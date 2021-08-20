
#load the data
results=read.csv("https://raw.githubusercontent.com/apreshill/bakeoff/master/data-raw/challenge_results.csv")

#load the packages
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.textmodels)
library(stopwords)
library(topicmodels)
library(tidytext)
library(tidyverse)


#so let's try the results with showstopper dishes 
results_complete=na.omit(results)
row.names(results_complete) <- 1:nrow(results_complete)

resultscorpus <- corpus(results_complete$showstopper)

summary(resultscorpus)
resultsDFM <- dfm(tokens(resultscorpus))
dim(resultsDFM)


# Remove stop words and perform stemming
library(stopwords)
#step 1: remove punctuation
resultsDFM <- dfm(tokens(resultscorpus,
                       remove_punct = T))
#step 2: remove stop words
resultsDFM <- dfm_remove(resultsDFM,stopwords("english"))

dim(resultsDFM)
topfeatures(resultsDFM)

#word cloud
textplot_wordcloud(resultsDFM,max_words=50)

# Explore document similarity for text25
#which is....Edd's Almond Sweet Pastry with Ginger and Chocolate  Raspberry Tart / Yorkshire Curd Tart  Chickpea and Salami, Red and Yellow Pepper, Asparagus Gruyere Quiche Canapés
results_complete %>% 
  filter(baker=="Edd") %>%
  select(episode,showstopper)

text_sim <- textstat_simil(resultsDFM, 
                           selection="text25",
                           margin="document",
                           method="correlation")
as.list(text_sim,n=5)

# Explore terms most similar to "orange"
term_sim <- textstat_simil(resultsDFM,
                           selection="orange", #note the stem
                           margin="feature",
                           method="correlation")
as.list(term_sim,n=5)


#topic modelling

# Explore the option with 4 topics - remember, need to specify
# You can explore with varying k numbers
results_LDA <- LDA(resultsDFM,k=4,control=list(seed=101))
results_LDA #doesn't tell you much
# Term-topic probabilities
results_LDA_td <- tidy(results_LDA)
results_LDA_td

#visualize most common terms in each topic
results_top_terms = results_LDA_td %>%
  group_by(topic) %>%
  top_n(8, beta) %>% #for each topic, find top 8 terms; beta is topic-term probability
  ungroup() %>%
  arrange(topic, -beta)

results_top_terms

#plot them
results_top_terms %>%
  mutate(term = reorder_within(term, beta,topic)) %>% 
  #reorder terms by beta, and here, topic is saying that they should be 
  #grouped by topic when they're considered
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + scale_x_reordered()

#view top 8 terms in each topic
results_LDA_term<-as.matrix(terms(results_LDA,8))
view(results_LDA_term)
  
  



