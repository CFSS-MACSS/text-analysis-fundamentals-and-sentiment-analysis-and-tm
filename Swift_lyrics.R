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
  # convert book to a factor
  mutate(Album = factor(Album)) %>%
  # tokenize the data frame
  unnest_tokens(word, Lyrics)
# check
ts_words


# most frequent words, by album (removing stop words)
ts_words %>%
  # delete stopwords
  anti_join(stop_words) %>%
  # summarize count per word per book
  count(Album, word) %>%
  # get top 15 words per book
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
  facet_wrap(facets = vars(Album), scales = "free") +
  coord_flip() +
  theme(legend.position = "none")
ggsave(path = "img/", "ts_frequent_words.png", width = 8)

### 3. SENTIMENT ANALYSIS EXAMPLE USING BING DICTIONARY

# generate data frame with sentiment derived from the Bing dictionary
(ts_bing <- ts_words %>%
    inner_join(get_sentiments("bing")))

# using the Bing dictionary:
# visualize the most frequent positive/negative words in the all series and for each book
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
  facet_wrap(facets = vars(sentiment), scales = "free_y") +
  labs(
    title = "Sentiment of words used in Taylor Swift Albums",
    x = NULL,
    y = "Number of occurences in all eight books (working on the rest!!)"
  ) +
  coord_flip()

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
  facet_wrap(facets = vars(Album), scales = "free_y") +
  labs(
    title = "Positive words used in Taylor Swift albums",
    x = NULL,
    y = "Number of occurences"
  ) +
  coord_flip()

## negative words
ts_pos_neg_album %>%
  filter(sentiment == "negative") %>%
  mutate(word = reorder_within(word, n, Album)) %>%
  ggplot(aes(word, n)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(facets = vars(Album), scales = "free_y") +
  labs(
    title = "Negative words used in Taylor Swift albums",
    x = NULL,
    y = "Number of occurences"
  ) +
  coord_flip()


### 4. SENTIMENT ANALYSIS EXAMPLE USING AFINN DICTIONARY

# Generate data frame with sentiment derived from the AFINN dictionary
(ts_afinn <- ts_words %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(Album, song))

# for further practice (at home): 
# repeat (and adapt) the code above, using the AFINN dictionary instead
# the code below shows some additional analyses you can perform (with any dictionary)


# Visualize which words in the AFINN sentiment dictionary appear most frequently
library(ggwordcloud)

set.seed(123) # ensure reproducibility of the wordcloud
ts_afinn %>%
  # count word frequency across books
  ungroup() %>%
  count(word) %>%
  # keep only top 100 words for wordcloud
  slice_max(order_by = n, n = 100) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(70, 30))) %>%
  ggplot(aes(label = word, size = n, angle = angle)) +
  geom_text_wordcloud(rm_outside = TRUE) +
  scale_size_area(max_size = 15) +
  ggtitle("Most frequent tokens in Taylor Swift Lyrics") +
  theme_minimal()



# Visualize positive/negative sentiment for each album using AFINN dictionary
ts_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(Album, song) %>%
  summarize(value = sum(value)) %>%
  ggplot(mapping = aes(x = song, y = value, fill = Album)) +
  geom_col() +
  facet_wrap(facets = vars(Album), scales = "free_x") +
  labs(
    title = "Emotional arc of Taylor Swift albums",
    subtitle = "AFINN sentiment dictionary",
    x = "Title",
    y = "Emotional score"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))


## BONUS!! DEALING WITH MESSY DATA
# the data I had for the original lyrics was already cleaned. for your own fun (!), 
# there's a bonus folder in data that shows the cleaning process + files

### ACKNOWLEDGMENTS

#Based on code by Sabrina Nardin and using data from Benjamin Soltoff and Kaggle
