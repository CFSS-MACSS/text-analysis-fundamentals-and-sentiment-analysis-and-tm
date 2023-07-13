### This code is divided in four sections
# 1. LOAD/INSTALL LIBRARIES
# 2. LOAD AND PROCESS DATA
# 3. SENTIMENT ANALYSIS EXAMPLE USING BING DICTIONARY
# 4. SENTIMENT ANALYSIS EXAMPLE USING AFINN DICTIONARY

# This example is not in the book "Text Mining with R"
# For other examples see the book, especially chapter 2 and the case-study chapters



### 1. LOAD/INSTALL LIBRARIES 

library(tidyverse)
library(tidytext)
library(stringr)
library(forcats)
library(topicmodels)
library(ggwordcloud)


### 2.LOAD AND PROCESS DATA 
taylor_swift_lyrics <- read_csv("data/expanded_swift_lyrics.csv")

albums <- c("Taylor Swift", "Fearless", "Speak Now",  "Red", "1989", "Reputation", "Lover", "Folklore", "Evermore", "Midnights 3am Edition")

# put the albums in order!
taylor_swift_lyrics <- taylor_swift_lyrics  %>% 
  mutate(
    Album = factor(Album, levels = albums)
  )

#create shorter way to refer to the song for plotting purposes
taylor_swift_lyrics <- taylor_swift_lyrics %>% 
    mutate(song = word(Title, 1,2))



# combine albums into a list
ts_words <- taylor_swift_lyrics %>%
  # convert album to a factor
  mutate(Album = factor(Album)) %>%
  # tokenize the data frame
  unnest_tokens(word, Lyrics)
# check
ts_words

# most frequent words, by album (removing stop words)
ts_words %>%
  # delete stopwords
  anti_join(stop_words) %>%
  # summarize count
  count(word) %>%
  # get top 15 words  
  slice_max(order_by = n, n = 15) %>%
  mutate(word = reorder(word, n)) %>%
  # create barplot
  ggplot(aes(x = word, y = n)) +
  geom_col(color = "black") +
  scale_x_reordered() +
  labs(
    title = "Most frequent words in Taylor Swift Songs",
    x = NULL,
    y = "Word count"
  ) +
  coord_flip() +
  theme(legend.position = "none")
ggsave(path = "img/", "ts_frequent_words.png", width = 8)
ggsave(path = "../course-site/static/img/", "ts_frequent_words.png")



# most frequent words, by album (removing stop words)
ts_words %>%
  # delete stopwords
  anti_join(stop_words) %>%
  # summarize count per word per album
  count(Album, word) %>%
  # get top 15 words per album
  group_by(Album) %>%
  slice_max(order_by = n, n = 15) %>%
  mutate(word = reorder_within(word, n, Album)) %>%
  # create barplot
  ggplot(aes(x = word, y = n, fill = Album)) +
  geom_col(color = "black") +
  scale_x_reordered() +
  labs(
    title = "Most frequent words in Taylor Swift Songs",
    x = NULL,
    y = "Word count"
  ) +
  facet_wrap(facets = vars(Album), scales = "free", nrow = 2) +
  coord_flip() +
  theme(legend.position = "none")
ggsave(path = "img/", "ts_frequent_words_album.png", width = 8)
ggsave(path = "../course-site/static/img/", "ts_frequent_words_album.png", width = 10)

### 3. SENTIMENT ANALYSIS EXAMPLE USING BING DICTIONARY

# generate data frame with sentiment derived from the Bing dictionary
(ts_bing <- ts_words %>%
    inner_join(get_sentiments("bing")))

# using the Bing dictionary:
# visualize the most frequent positive/negative words in the all series and for each album
# see reorder_within() and scale_x_reordered(): https://juliasilge.com/blog/reorder-within/

## for all albums
ts_bing %>%
  # generate frequency count for each word and sentiment
  group_by(sentiment) %>%
  count(word) %>%
  # extract 10 most frequent pos/neg words
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 10) %>%
  # prep data for sorting each word independently by facet
  mutate(word = reorder_within(word, n, sentiment)) %>%
  # generate the bar plot
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  # used with reorder_within() to label the axis tick marks
  scale_x_reordered() +
  facet_wrap(facets = vars(sentiment), scales = "free_y", nrow = 2) +
  labs(
    title = "Sentiment of words used in Taylor Swift Albums",
    x = NULL,
    y = "Number of occurences in all eight albums (working on the rest!!)"
  ) +
  coord_flip()
ggsave(path = "img/", "ts_sentiment.png")
ggsave(path = "../course-site/static/img/", "ts_sentiment.png")


## for each album
ts_pos_neg_album <- ts_bing %>%
  # generate frequency count for each album, word, and sentiment
  group_by(Album, sentiment) %>%
  count(word) %>%
  # extract 10 most frequent pos/neg words per album
  group_by(Album, sentiment) %>%
  slice_max(order_by = n, n = 10)

## positive words
ts_pos_neg_album %>%
  filter(sentiment == "positive") %>%
  mutate(word = reorder_within(word, n, Album)) %>%
  ggplot(aes(word, n)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(facets = vars(Album), scales = "free_y", nrow = 2) +
  labs(
    title = "Positive words used in Taylor Swift albums",
    x = NULL,
    y = "Number of occurences"
  ) +
  coord_flip()
ggsave(path = "img/", "ts_positive_words_album.png")
ggsave(path = "../course-site/static/img/", "ts_positive_words_album.png")

## negative words
ts_pos_neg_album %>%
  filter(sentiment == "negative") %>%
  mutate(word = reorder_within(word, n, Album)) %>%
  ggplot(aes(word, n)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(facets = vars(Album), scales = "free_y", nrow = 2) +
  labs(
    title = "Negative words used in Taylor Swift albums",
    x = NULL,
    y = "Number of occurences"
  ) +
  coord_flip()
ggsave(path = "img/", "ts_negative_words_album.png")
ggsave(path = "../course-site/static/img/", "ts_negative_words_album.png") #for slides easy access


### 4. SENTIMENT ANALYSIS EXAMPLE USING AFINN DICTIONARY

# Generate data frame with sentiment derived from the AFINN dictionary
(ts_afinn <- ts_words %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(Album, song))

# for further practice (at home): 
# repeat (and adapt) the code above, using the AFINN dictionary instead
# the code below shows some additional analyses you can perform (with any dictionary)


# Visualize which words in the AFINN sentiment dictionary appear most frequently


set.seed(123) # ensure reproducibility of the wordcloud
ts_afinn %>%
  # count word frequency 
  ungroup() %>%
  count(word) %>%
  # keep only top 200 words for wordcloud
  slice_max(order_by = n, n = 200) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(70, 30))) %>%
  ggplot(aes(label = word, size = n, angle = angle)) +
  geom_text_wordcloud(rm_outside = TRUE) +
  scale_size_area(max_size = 10) +
  ggtitle("Most frequent 200 tokens in Taylor Swift Lyrics") +
  theme_minimal()
ggsave(path = "img/", "ts_wordcloud.png")
ggsave(path = "../course-site/static/img/", "ts_wordcloud.png")

# Visualize positive/negative sentiment using BING dictionary
ts_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c( "gray70", "gray20"),
                   max.words = 100) +
  ggtitle("Most frequent Positive (dark gray) and Negative (light gray) tokens in Taylor Swift Lyrics") +
  theme_minimal()
ggsave(path = "img/", "ts_wordcloud_pos_neg.png")
ggsave(path = "../course-site/static/img/", "ts_wordcloud_pos_neg.png")


# Visualize positive/negative sentiment for each album using AFINN dictionary
ts_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(Album, song) %>%
  summarize(value = sum(value)) %>%
  ggplot(mapping = aes(x = song, y = value, fill = Album)) +
  geom_col() +
  facet_wrap(facets = vars(Album), scales = "free_x", nrow = 2) +
  labs(
    title = "Emotional arc of Taylor Swift albums",
    subtitle = "AFINN sentiment dictionary",
    x = "Title",
    y = "Emotional score"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "img/", "ts_sentiment_album.png")
ggsave(path = "../course-site/static/img/", "ts_sentiment_album.png", width = 10)



## Diving more into frequency:

album_words <- ts_words %>%
  anti_join(stop_words) %>%
  # summarize count
  count(Album, word, sort = TRUE)

total_words <- album_words %>% 
  group_by(Album) %>% 
  summarize(total = sum(n))

total_ts_words <- left_join(album_words, total_words)
freq_by_rank <- total_ts_words %>% 
  group_by(Album) %>%  
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

# This is interesting in that we can see that for some albums (Red & 1989)
# The top words are used much much more often
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = Album)) + 
  geom_line(linewidth = 1.1, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()


album_tf_idf <- album_words %>%
  bind_tf_idf(word, Album, n) %>% 
  arrange(desc(tf_idf))


album_tf_idf %>%
  group_by(Album) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Album, ncol = 5, scales = "free") +
  labs(x = "tf-idf", y = NULL)
ggsave(path = "img/", "ts_idf_words.png", width = 8)
ggsave(path = "../course-site/static/img/", "ts_idf_words.png", width = 10)


## GETTING CRAZY! BIGRAMS!
ts_bigrams <- taylor_swift_lyrics %>%
  unnest_tokens(bigram, Lyrics, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

bigrams_separated <- ts_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts


bigrams_filtered %>%
  filter(word2 != word1) %>%
  count(Album, word1, word2, sort = TRUE)

# Note: not sure how much I'll use the above since these are songs, 
# but I plan to revisit this if/when I break the albums into eras!

#############################
## TOPIC MODELS

ts_words_count <- ts_words %>%
  # delete stopwords
  anti_join(stop_words) %>%
  group_by(Album, song) %>%
  # summarize count
  count(word) %>%
  mutate(
    Album_song = str_c(Album, song, sep = "_")
  )

# cast_dtm()
ts_td <- cast_dtm(ts_words_count, Album_song, word, n)
ts_td

# conduct LDA
ts_lda2 <- LDA(ts_td, k = 2, control = list(seed = 1234))
ts_lda3 <- LDA(ts_td, k = 3, control = list(seed = 1234))
ts_lda10 <- LDA(ts_td, k = 10, control = list(seed = 1234))
ts_lda2

# look at our topics
ts_topics <- tidy(ts_lda2, matrix = "beta")
ts_topics


## Let's explore the top terms in our topics!
ts_top_terms <- ts_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ts_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
labs(
  title = "Most frequent words in Taylor Swift Songs (two topics",
  x = NULL,
  y = "Word count"
) +
  theme(legend.position = "none")
ggsave(path = "img/", "ts_frequent_words_topics.png", width = 8)
ggsave(path = "../course-site/static/img/", "ts_frequent_words_topics.png", width = 8)


# could also explore the greatest difference in the two:
beta_wide <- ts_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_wide



## A DEEPER DIVE INTO THE SWIFTIEVERSE
# note: i manually ran this with 2, 3, and 10 to get the plots 
# but iddn't want to replicate that code here for space / concision

albums_gamma <- tidy(ts_lda10, matrix = "gamma")
albums_gamma

albums_gamma <- albums_gamma %>%
  separate(document, c("Album", "song"), sep = "_", convert = TRUE)

albums_gamma %>%
  mutate(Album = factor(Album, levels = albums)) %>%
  ggplot(aes(Album, gamma, color = Album, label = song)) +
  geom_point(position = "jitter") +
  geom_label() +
  facet_wrap(~ topic) +
  labs(x = "Topic", y = expression(gamma))
ggsave(path = "img/", "ts_10topics.png", width = 8)
ggsave(path = "../course-site/static/img/", "ts_10topics.png", width = 8)


## BONUS!! DEALING WITH MESSY DATA
# the data I had for the original lyrics was already cleaned. for your own fun (!), 
# there's a bonus folder in data that shows the cleaning process + files

### ACKNOWLEDGMENTS

#Based on code by Sabrina Nardin and using data from Kaggle
