library(tidyverse) # The bible
library(tidytext) # The text bible
library(gutenbergr) # Jane Austen novels
library(udpipe) # For lemmatization
library(Hmisc) # For %nin%
library(ldatuning) # For computing the optimal number of topics
library(topicmodels) # For training a LDA topic model
library(wordcloud) # For wordclouds

# Downloading novels by Jane Austen
p <- gutenberg_download(105) %>% .$text %>% paste0(., collapse = " ")
pp <- gutenberg_download(1342) %>% .$text %>% paste0(., collapse = " ")
ss <- gutenberg_download(161) %>% .$text %>% paste0(., collapse = " ")

austen <- data.frame(id = c(1, 2, 3), 
                     text = c(p, pp, ss), 
                     stringsAsFactors = F)


# Downloading and loading the udpipe English model for lemmatization
model <- udpipe_download_model(language = "english")
model <- udpipe_load_model(model$file_model)

# Lemmatization
annotated <- udpipe_annotate(model, x = austen$text)
annotated <- as.data.frame(annotated)

# Summary
head(annotated)
str(annotated)
table(annotated$doc_id)

annotated %>% 
  count(upos) %>% 
  arrange(n) %>% 
  ggplot(aes(x = reorder(upos, -n), y = n)) + 
  geom_bar(stat = "identity", fill = "lightblue") + 
  theme(axis.title.x = element_blank())

# Concatenate PROPN so that "Jane" and "Austen" become "Jane Austen"
annotated_concat <- annotated %>%
  mutate(token_id = as.numeric(token_id),
         temp = ifelse(upos == "PROPN", NA, token_id)) %>% 
  tidyr::fill(temp) %>% 
  mutate(sk_id = ifelse(upos == "PROPN", temp + 1, token_id)) %>% 
  group_by(doc_id, paragraph_id, sentence_id, sentence, sk_id) %>% 
  summarise(lemma = paste0(lemma, collapse = " "),
            upos = first(upos)) %>% 
  ungroup()

stop <- tidytext::stop_words

dt <- annotated_concat %>% 
  filter(upos %in% c("NOUN", "VERB", "ADV", "ADJ", "PROPN"),
         lemma %nin% stop$word)

lemma_freq <- dt %>% 
  group_by(lemma, upos) %>% 
  summarise(frequency = n()) %>% 
  arrange(desc(frequency)) %>% 
  filter(frequency > 1)
  
lemma_freq %>% 
  head(., 15) %>% 
  ggplot(aes(x = reorder(lemma, -frequency), y = frequency)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  theme(axis.title.x = element_blank())

dt_lda <- dt %>% filter(lemma %in% lemma_freq$lemma,
                        lemma != "Chapter")
dtf <- document_term_frequencies(dt_lda, document = "doc_id", term = "lemma")
dtm <- document_term_matrix(x = dtf)


result <- FindTopicsNumber(
  dtm,
  topics = c(2, 3, 4, 5, 6, 7, 8, 10, 25),
  metrics = c("Griffiths2004", "CaoJuan2009",  "Deveaud2014", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 50L,
  verbose = TRUE
)

# Plot result
FindTopicsNumber_plot(result)

# Train a LDA Topic Model

control_LDA_Gibbs <- list(alpha = c(3, 50/6, 10), 
                          estimate.beta = TRUE,
                          verbose = 0, 
                          prefix = tempfile(), 
                          save = 0, 
                          keep = 0,
                          seed = as.integer(848), 
                          nstart = 1, 
                          best = TRUE,
                          delta = 0.1,
                          iter = 10000, 
                          burnin = 10, 
                          thin = 20)


set.seed(884422)
LDAmodel <- LDA(dtm, k = 6, method = "Gibbs", control = control_LDA_Gibbs)

topics_austen <- tidy(LDAmodel, matrix = "beta")

top_terms <- topics_austen %>%
  filter(term != "N", beta > 0.0009) %>% 
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(desc(beta), topic)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

df <- topics_austen %>% filter(topic == 6) %>%  mutate(freq = beta * 10000)
set.seed(1234)
wordcloud(words = df$term, freq = df$freq, 
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


document_austen <- tidy(LDAmodel, matrix = "gamma")

doc <- document_austen %>%
  filter(gamma > 0.0006) %>% 
  group_by(document, topic) %>%
  arrange(desc(gamma)) %>% 
  mutate(topic_label = paste("Topic", topic), 
         title = ifelse(document == "doc1", "Persuasion",
                        ifelse(document == "doc2", "Pride&Prejudice",
                                "Sense&Sensibility")), 
         gamma = gamma * 100)

doc %>%
  ggplot(aes(title, gamma, fill = factor(topic_label))) +
  geom_col(show.legend = TRUE)+
  theme(axis.title.x=element_blank()) +
  theme(legend.title=element_blank()) +
  ylab("Topic Proportion")+
  labs(title = "Proportion of topic per document") 


# Visualisarion for blog


doc <- document_austen %>%
  filter(gamma > 0.0006) %>% 
  group_by(document, topic) %>%
  arrange(desc(gamma)) %>% 
  mutate(topic_label = paste("Topic", topic), 
         title = ifelse(document == "doc1", "Document1",
                        ifelse(document == "doc2", "Document2",
                               "Document3")), 
         gamma = gamma * 100)

doc %>%
  ggplot(aes(title, gamma, fill = factor(topic_label))) +
  geom_col(show.legend = TRUE)+
  theme(axis.title.x=element_blank()) +
  theme(legend.title=element_blank()) +
  ylab("Topic Proportion")
          

df <- topics_austen %>% filter(topic == 6) %>%  mutate(freq = beta * 10000)
set.seed(1234)
wordcloud(words = df$term, freq = df$freq, 
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))