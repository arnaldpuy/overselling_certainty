


# PRELIMINARY FUNCTIONS #######################################################

# Load the packages
sensobol::load_packages(c(
  "bibliometrix", "tidyverse", "data.table", "scales", "pdfsearch", "pdftools", 
  "openxlsx", "cowplot", "wesanderson", "sjmisc", "ggpubr", "tm", "syuzhet", 
  "qdapRegex", "tidytext", "igraph", "ggraph", "wordcloud2", "parallel", "maps", 
  "lsa", "LSAfun", "pheatmap", "ggrepel"))

# Create custom theme
theme_AP <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent",
                                           color = NA),
          legend.key = element_rect(fill = "transparent",
                                    color = NA), 
          strip.background = element_rect(fill = "white"), 
          legend.margin = margin(0.5, 0.1, 0.1, 0.1),
          legend.box.margin = margin(0.2,-4,-7,-7), 
          plot.margin = margin(3, 4, 0, 4), 
          legend.text = element_text(size = 8), 
          legend.key.width = unit(0.4, "cm"), 
          legend.key.height = unit(0.4, "cm"), 
          legend.title = element_text(size = 9)) 
}

# VECTOR WITH NAME OF MODELS ###################################################

water.models <- c("VIC", "TOPMODEL", "ORCHIDEE", "CLM4.5", "GR4J", "JULES-W1", 
                  "WaterGAP", "LPJmL", "PCR-GLOBWB", "H08","SACRAMENTO", 
                  "MHM", "MATSIRO", "DBHM", "CWatM", "MPI-HM")

models.vec <- paste(water.models, "_ref.bib", sep = "")
models.tolower <- tolower(water.models)

# FUNCTION TO CLEAN TEXT ######################################################

# Function to remove the name of models from text
removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}

# Function to remove punctuation, citations, numbers, stopwords in english, 
# bring to lowercase and strip whitespace, and especial characters, etc...
clear_text <- function(x, stem = TRUE) {
  
  y <- tolower(x)
  y <- str_replace_all(y, "[[:punct:]]", " ") # Remove punctuation characters
  y <- tm::removeNumbers(y)
  y <- tm::removeWords(y, stopwords::stopwords(language = "en"))
  y <- str_remove_all(y, "[^[\\da-zA-Z ]]")# Remove all non-alphanumerical
  y <- gsub("\\s[A-Za-z](?= )", " ", y, perl = TRUE) # Remove isolated letters
  #y <- tm::stripWhitespace(y)
  y <- str_squish(y)
  
  if (stem == TRUE) {
    y <- stemDocument(y) # Stem the document and keep only the root of the word
  }
  
  return(y)
}

# Function to extract the first 30 words before and after the mention of the 
# model name in the abstract

grab_text <- function(text, model) {
  vec <- paste("(( \\S+){30} ", tolower(model), "[[:punct:]\\s]*( \\S+){30})", sep = "")
  str_extract(text, vec)
}

# BIBLIOMETRIC ANALYSIS ########################################################

# Define vectors with keywords ------------------------------------------------

keywords_vec <- c("uncertainty", "sensitivity")
keywords_vec_stemmed <- stemDocument(keywords_vec)

output <- results <- years <- journals <- dt <- dt.clean <- list()

selected_cols <- c("title", "abstract", "keywords")

for (i in 1:length(water.models)) {
  
  output[[i]] <- convert2df(file = paste(water.models[i], "_ref.bib", sep = ""), 
                            dbsource = "wos", 
                            format = "bibtex")
  
  # Extract title --------------------------------------------------------------
  
  title <- output[[i]]$TI
  
  # Extract keywords ----------------------------------------------------------
  
  keywords <- gsub(";;", ";", output[[i]]$DE)
  keywords.plus <- gsub(";;", ";", output[[i]]$ID)
  
  # Create data.table ----------------------------------------------------------
  
  dt[[i]] <- data.table("WOS" = output[[i]]$UT, 
                        "title" = title,
                        "title.large" = tolower(title),
                        "year" = output[[i]]$PY,
                        "keywords" = keywords,
                        "abstract" = output[[i]]$AB, 
                        "abstract.large" = output[[i]]$AB) 
  
  dt.clean[[i]] <- copy(dt[[i]])
  
  # Clean text
  dt.clean[[i]][, (selected_cols):= lapply(.SD, function(x) 
    clear_text(x)), .SDcols = selected_cols] %>%
    .[, abstract.large:= tolower(abstract.large)]
  
  # Export data dirty and clean
  write.xlsx(dt[[i]], file = paste(water.models[i], "_bibliometric.xlsx", sep = ""))
  write.xlsx(dt.clean[[i]], file = paste(water.models[i], "_bibliometric_clean.xlsx", 
                                         sep = ""))
  
  # Retrieve analysis bibliometrix ---------------------------------------------
  
  results[[i]] <- biblioAnalysis(output[[i]], sep = ";")
  years[[i]] <- data.table(results[[i]]$Years)
  journals[[i]] <- data.table(results[[i]]$Sources) %>%
    .[, SO:= str_to_title(SO)] 
}

# Add names of models ----------------------------------------------------------

names(years) <- water.models
names(journals) <- water.models
names(dt.clean) <- water.models

# RETRIEVE THE FIRST 30 WORDS BEFORE AND AFTER MODEL NAME IN ABSTRACT ##########

for(i in names(dt.clean)) {
  
  dt.clean[[i]] <- dt.clean[[i]][, thirty.words:= grab_text(abstract.large, i)]
}

# ARRANGE DATA #################################################################

# Final dataset
full.dt <- rbindlist(dt.clean, idcol = "Model") %>%
  .[, year:= ifelse(year == 2023, 2022, year)] # Because
# eight papers were published Early Access end of 
# 2022, and ended up in 2023 issues. We count these papers
# as if published in 2022.

# Total number of studies
total.n <- full.dt[, .(Model, WOS)] %>%
  .[, .(total.papers = .N), Model] %>%
  .[order(-total.papers)]

sum(total.n$total.papers)

# Export
# fwrite(full.dt, "full.dt.csv")

# DESCRIPTIVE STATISTICS #######################################################

# IDENTIFY WHICH PAPERS ARE IN MORE THAN ONE MODEL ------------

# Number of papers in more than one model
n_occur <- data.frame(table(full.dt$WOS))
WOS.repeated <- data.table(n_occur[n_occur$Freq > 1,])
length(WOS.repeated$Var1) # number of repeated papers

# Fraction of repeated papers over the total
length(WOS.repeated$Var1) / nrow(full.dt) 

# How many papers are repeated twice, three times, etc...
WOS.repeated[, .(N.repeated.papers = .N), Freq]

# Extract which papers are repeated for which model
dt.sample.repeated <- full.dt[WOS %in% WOS.repeated$Var1] %>%
  .[, .(WOS, Model)] %>%
  .[order(WOS)]

dt.sample.repeated 

# Randomly retrieve only one of the repeated studies per model
set.seed(6)
dt.no.repeated <- dt.sample.repeated[,.SD[sample(.N, min(1,.N))], WOS]
dt.no.repeated.final <- full.dt[WOS %in% dt.no.repeated$WOS & Model %in% dt.no.repeated$Model]

# Make the final dataset without repeated papers across models
final.dt <- rbind(full.dt[!WOS %in% dt.no.repeated$WOS], dt.no.repeated.final)

# CHECK MENTIONS OF UNCERTAINTY IN THE ABSTRACT, KEYWORDS OR TITLE #############

out <- list()
for (i in 1:length(keywords_vec_stemmed)) {
  
  out[[i]] <- final.dt[, lapply(.SD, function(x) 
    str_detect(x, keywords_vec_stemmed[i])), .SDcols = (selected_cols)]
  
}

names(out) <- keywords_vec_stemmed

for (i in names(out)) {
  
  out[[i]] <- setnames(out[[i]], colnames(out[[i]]), paste(i, colnames(out[[i]]), 
                                                           sep = "."))
}

dada <- do.call(cbind, out) %>%
  .[, uncertainti:= rowSums(.[, 1:3]) > 0L] %>%
  .[, sensit:= rowSums(.[, 4:6]) > 0L]

final.dt <- cbind(final.dt, dada[, .(uncertainti, sensit)])




# Check which papers include "uncertainty" or "sensitivity" in the abstract
final.dt <- final.dt[, `:=` (uncertainti2 = str_detect(abstract, keywords_vec_stemmed[1]), 
                             sensit2 = str_detect(abstract, keywords_vec_stemmed[2]))] 

# EXPORT FINAL DATASETS #########################################################

setorder(final.dt, -Model, year)   
write.xlsx(final.dt, "final.dt.xlsx")
write.xlsx(final.dt[uncertainti == TRUE | sensit == TRUE], "uncertainty.dt.xlsx")















# SCREENING TO AVOID ERRORS OF COMMISSION ######################################

screening.dt <- final.dt[, .(WOS, Model, year, thirty.words)] 
setorder(screening.dt, -Model, year)     
write.xlsx(screening.dt, "screening.dt.xlsx")

###########################################
###########################################

# HERE IS WHERE WE HAVE TO FILTER THE FINAL.DT 
# DATASET TO RETRIEVE ONLY STUDIES THAT DO USE
# THE MODEL IN QUESTION.

###########################################
###########################################











# COUNT UNCERTAINTI AND SENSIT IN TITLE, ABSTRACT AND KEYWORDS #################

out <- list()
for(i in 1:length(keywords_vec_stemmed)) {
  out[[i]] <- final.dt[, lapply(.SD, function(x) str_detect(x, keywords_vec_stemmed[i])), 
                       .SDcols = (selected_cols)]
}

names(out) <- keywords_vec_stemmed

for(i in names(out)) {
  out[[i]] <- setnames(out[[i]], colnames(out[[i]]), paste(i, colnames(out[[i]]), 
                                                          sep = "."))
}

dada <- do.call(cbind, out)
dada[, uncertainti:= rowSums(dada[, 1:3]) > 0L]
dada[, sensit:= rowSums(dada[, 4:6]) > 0L]






# Fraction of papers with uncertainti and sensit in the abstract, title, 
# keywords ---------------------------------------------------

tmp <- lapply(out, function(x) x[, .(n = colSums(.SD)), .SDcols = (selected_cols)][
  , type:= selected_cols][
    , total.n:= nrow(final.dt)][, fraction:= n / total.n])

tmp

# Fraction of papers that do not include the words in the abstract but do 
# include them in keywords or title --------------------------------------------

tmp2 <- lapply(out, function(x) x[abstract == FALSE][title == TRUE | keywords == TRUE]) 

da <- rbindlist(tmp2, idcol = "word") %>%
  .[, n.row:= nrow(final.dt)]  %>%
  .[, .N, .(word, n.row)] %>%
  .[, fraction:= N / n.row]

da

######################################## PLOTS #################################
################################################################################

total.articles <- ggplot(total.n, aes(reorder(Model, total.papers), 
                                      total.papers)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(y = "Nº of articles", x = "") +
  theme_AP() +
  theme(legend.position = c(0.65, 0.35))

total.articles

# TOTAL NUMBER OF STUDIES THROUGH TIME #########################################

plot.time <- rbindlist(years, idcol = "Model") %>%
  .[, V1:= ifelse(V1 == 2023, 2022, V1)] %>%
  # For the reasons stated above
  .[, .N, V1] %>%
  ggplot(., aes(V1, N)) +
  geom_line(color = "blue") +
  labs(x = "Year", y = "Nº articles") +
  theme_AP() 

plot.time

# FRACTION OF STUDIES PER MODEL WITH UNCERTAINTI AND SENSIT* IN THE ABSTRACT ###

plot.n.keywords <- final.dt[, lapply(.SD, function(x) 
  sum(x) / .N), .SDcols = (keywords_vec_stemmed), .(Model)] %>%
  melt(., measure.vars = keywords_vec_stemmed) %>%
  ggplot(., aes(variable, value)) +
  geom_boxplot() + 
  labs(y = "Fraction articles", x = "") +
  scale_y_continuous(breaks = pretty_breaks(n = 3), 
                     limits = c(0, 1)) +
  scale_fill_discrete(name = "Word") +
  theme_AP() + 
  theme(legend.position = c(0.32, 0.8))

plot.n.keywords

# FRACTION OF STUDIES WITH UNCERTAINTI AND SENSIT* IN THE ABSTRACT
# BY MODEL #####################################################################

final.dt[, lapply(.SD, function(x) 
  sum(x) / .N), .SDcols = (keywords_vec_stemmed), Model] %>%
  melt(., measure.vars = keywords_vec_stemmed) %>%
  ggplot(., aes(reorder(Model, value), value, fill = variable)) +
  geom_bar(stat = "identity", 
           position = position_dodge(0.5)) + 
  labs(y = "Fraction articles", x = "") +
  scale_y_continuous(breaks = pretty_breaks(n = 3), 
                     limits = c(0, 1)) +
  scale_fill_discrete(name = "Word") + 
  coord_flip() +
  theme_AP() + 
  theme(legend.position = c(0.8, 0.5))

# Fraction of studies with both keywords in the abstract
final.dt[uncertainti == "TRUE" & sensit == "TRUE", .N] / full.dt[, .N]

# FRACTION OF STUDIES WITH WORDS UNCERTAINTI AND SENSIT IN THE 
# ABSTRACT, THROUGH TIME #######################################################

total.n.year <- final.dt[, .(total.n = .N), year]

plot.fraction.years <- final.dt[, .(WOS, uncertainti, sensit, year)] %>%
  melt(., measure.var = keywords_vec_stemmed) %>%
  .[value == TRUE, .N, .(year, variable)] %>%
  merge(., total.n.year, by = "year") %>%
  .[, fraction:= N / total.n] %>%
  ggplot(., aes(year, fraction, color = variable, group = variable)) +
  geom_line() + 
  scale_color_discrete(name = "Word") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Year", y = "Fraction articles") +
  theme_AP() +
  theme(legend.position = c(0.3, 0.66))

plot.fraction.years

# FRACTION OF STUDIES WITH WORDS UNCERTAINTI AND SENSIT IN THE 
# ABSTRACT, THROUGH TIME AND BY MODEL ##########################################

final.dt[, .N, .(year, Model)]

da <- final.dt[, .(WOS, uncertainti, sensit, year, Model)] %>%
  melt(., measure.var = keywords_vec_stemmed) %>%
  .[, .N, .(year, Model)] %>%
  ggplot(., aes(year, N, group = Model)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = pretty_breaks(n = 3)) +
  facet_wrap(~Model, scales = "free_y") +
  theme_AP() 

years.sa.ua <- final.dt[, .(WOS, uncertainti, sensit, year, Model)] %>%
  melt(., measure.var = keywords_vec_stemmed) %>%
  .[value == TRUE, .N, .(year, variable, Model)]

plot.fraction.years.model <- da + 
  geom_point(data = years.sa.ua, aes(year, color = variable), alpha = 0.4) +
  scale_color_discrete(name = "Word") +
  scale_x_continuous(breaks = pretty_breaks(n = 2))

plot.fraction.years.model


for(i in 1:length(type.models)) {
  
  da[[i]] <- full.dt[Type == type.models[i], 
                     .(WOS, uncertainti, sensit, year, Model)] %>%
    melt(., measure.var = keywords_vec_stemmed) %>%
    .[, .N, .(year, Model)] %>%
    ggplot(., aes(year, N, group = Model)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA), 
                       breaks = pretty_breaks(n = 3)) +
    facet_wrap(~Model, scales = "free_y") +
    theme_AP() + 
    ggtitle(names(type.models[i]))
  
  years.sa.ua[[i]] <- full.dt[Type == type.models[i], 
                              .(WOS, uncertainti, sensit, year, Model)] %>%
    melt(., measure.var = keywords_vec_stemmed) %>%
    .[value == TRUE, .N, .(year, variable, Model)]
  
  plot.fraction.years.model[[i]] <- da[[i]] + 
    geom_point(data = years.sa.ua[[i]], aes(year, color = variable), alpha = 0.4) +
    scale_color_discrete(name = "Word") 
}

plot.fraction.years.model

# MERGE PLOTS ##################################################################

top <- plot_grid(plot.time, plot.n.keywords, ncol = 2, labels = c("b", "c"), 
                 rel_widths = c(0.5, 0.5))
right <- plot_grid(top, plot.fraction.years, ncol = 1, labels = c("", "d"), 
                   rel_heights = c(0.52, 0.48))

plot_grid(total.articles, right, ncol = 2, rel_widths = c(0.38, 0.62), 
          labels = c("a", ""))

# PLOT JOURNALS ################################################################

tmp <- rbindlist(journals, idcol = "Model") 


tmp[, sum(N), SO] %>%
  .[order(-V1)] %>%
  .[1:20] %>%
  na.omit() %>%
  ggplot(., aes(reorder(SO, V1, sum), V1)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "", y = "Nº of articles") +
  theme_AP()

# RANK OF WORDS IN ABSTRACT ####################################################

# Create word cloud
first.n.words <- 100 # Check the most common n words

out <- Corpus(VectorSource(final.dt$abstract))
dtm <- tm::TermDocumentMatrix(out)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
word.count <- data.table(word = names(v), freq = v)

set.seed(6) # For reproducibility
plot.wordcloud <- wordcloud2(word.count[, head(.SD, first.n.words)], size = 0.5, color = "random-dark", 
                             shape = "circle")

plot.wordcloud 

# Check rank overall
rank.dt <- final.dt %>% 
  unnest_tokens(word, abstract) %>%
  .[, .N, word] %>%
  .[order(-N), .SD] %>%
  .[, rank := frank(-N, ties.method = "first")]

rank.dt[, head(.SD, first.n.words)] %>%
  print(n = Inf)

# Check rank for uncertainti and sensit
rank.dt[word %in% keywords_vec_stemmed]

# Check number of mentions of uncertainti and sensit in abstract per article
tmp <- final.dt %>% 
  unnest_tokens(word, abstract) %>%
  .[, .N, .(Model, word, WOS)] %>%
  .[order(-N), .SD, .(Model, WOS)] 


out <- tmp[word %in% keywords_vec_stemmed] %>%
  ggplot(., aes(N, fill = word)) +
  geom_bar(position = "identity", alpha = 0.5) +
  facet_wrap(~Model, ncol = 5) +
  scale_x_continuous(breaks = pretty_breaks(n = 4)) +
  labs(x = "Mentions in abstract", y = "Nº of papers") +
  theme_AP()

out

# Calculate ranks of words in abstract per model
freq.dt <- final.dt %>% 
  unnest_tokens(word, abstract) %>%
  .[, .N, .(Model, word)] %>%
  .[order(-N), .SD, Model] %>%
  .[, rank := frank(-N, ties.method = "first"), Model] 

# Plot the rank of uncertainti and sensit per model
freq.dt[word %in% keywords_vec_stemmed] %>%
  ggplot(., aes(N, fill = word)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  scale_color_discrete(name = "Word") +
  labs(x = "Rank", y = "Nº of models") +
  theme_AP()

# Print
dt <- freq.dt[word %in% keywords_vec_stemmed]
setorderv(dt, c("word", "N"))
dt  

######################## STUDY OF N-TOKENS #####################################
################################################################################

# Create function --------------------------------------------------------------
tokenize_fun <- function(dt, word, keywords, N.tokens) {
  
  # Create long dataset
  dt <- melt(dt, measure.vars = keywords)
  output <- dt[variable == word & value == TRUE]
  
  # Token analysis ------------------------------
  # We count the co-occurences of words without taking into account their order
  # within the n-token
  token.analysis <- output %>%
    unnest_tokens(bigram, abstract, token = "ngrams", n = N.tokens) %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    .[, `:=`(word1= pmin(word1, word2), word2 = pmax(word1, word2))] %>%
    count(word1, word2, Model, year, sort = TRUE) %>%
    unite(., col = "bigram", c("word1", "word2"), sep = " ")
  
  # Vector to retrieve only the bigrams with uncertainti or sensit 
  vec <- token.analysis[, str_detect(bigram, word)]
  
  # Final dataset
  output.dt <- token.analysis[vec]
  
  # Plot the q0 words most commonly 
  # associated with uncertainti and sensit ------
  plot.token <-  output.dt %>%
    .[, sum(n), bigram] %>%
    .[order(-V1)] %>%
    .[, head(.SD, 10)] %>%
    .[, bigram:= str_squish(str_remove(bigram, word))] %>%
    ggplot(., aes(reorder(bigram, V1, sum), V1)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_continuous(breaks = pretty_breaks(n = 3)) +
    theme_AP() +
    labs(y = "$n$", x = "") +
    ggtitle(word) +
    theme(legend.position = "none", 
          plot.title = element_text(size = 11)) 
  
  # Plot the 4 words most commonly associated with uncertainti and sensit 
  # and see their evolution through time --------
  vec.words <- output.dt[, sum(n), bigram] %>%
    .[order(-V1)] %>%
    .[, head(.SD, 4)] %>%
    .[, bigram:= str_squish(str_remove(bigram, word))] %>%
    .[, bigram]
  
  plot.token.year <- output.dt[, sum(n), .(year, bigram)] %>%
    .[, bigram:= str_squish(str_remove(bigram, word))] %>%
    .[bigram %in% vec.words] %>%
    ggplot(., aes(year, V1)) +
    geom_line() + 
    facet_wrap(~bigram) +
    scale_x_continuous(breaks = pretty_breaks(n = 3), 
                       guide = guide_axis(check.overlap = TRUE)) +
    scale_y_continuous(breaks = pretty_breaks(n = 3)) +
    theme_AP() +
    labs(x = "Year", y = "Nº of times") +
    ggtitle(word) + 
    theme(plot.title = element_text(size = 11), 
          axis.text.x = element_text(size = 8.5)) 
  
  # Plot the 4 words most commonly associated with uncertainti and sensit 
  # in each model -------------------------------
  
  plot.token.model <- token.analysis[vec] %>%
    .[, .(n = sum(n)), .(bigram, Model)] %>%
    .[order(-n), head(.SD, 5), Model] %>%
    .[, `:=` (bigram = str_squish(str_remove(bigram, word)), 
              Model = as.factor(Model))] %>%
    .[, bigram:= reorder_within(bigram, n, Model)] %>%
    ggplot(., aes(reorder(bigram, n, sum), n)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_AP() +
    labs(y = "$n$", x = "") +
    scale_x_reordered() +
    theme(legend.position = "none") +
    ggtitle(word) +
    facet_wrap(~Model, scales = "free", ncol = 3) 
  
  # Arrange and output --------------------------
  
  out <- list(token.analysis, plot.token, plot.token.year, plot.token.model)
  names(out) <- c("data", "token", "year", "model")
  
  return(out)
  
}

# RUN MODEL ####################################################################

N.tokens <- 2
token.dt <- list()

for (j in keywords_vec_stemmed) {
  
  token.dt[[j]] <- tokenize_fun(dt = final.dt, word = j, 
                                keywords = keywords_vec_stemmed, 
                                N.tokens = N.tokens)
  
}