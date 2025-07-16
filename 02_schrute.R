# https://cran.r-project.org/web/packages/schrute/vignettes/theoffice.html

library(tidyverse)
library(tidytext)

## install.packages("schrute")
library(schrute)

# load our data
office <- schrute::theoffice

# tokenize
office_tokenized <- office %>%
  tidytext::unnest_tokens(word, text)

# kick out stop words
stop_words <- tidytext::stop_words

office_tokenized_ns <- office_tokenized %>%
  anti_join(stop_words, by = "word")


# look at common words:
office_tokenized_ns %>%
  count(word, sort = TRUE) %>%
  filter(n > 400) %>%
  mutate(word = stats::reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_minimal()


# get sentiment by character:
office_tokenized_ns %>% inner_join(get_sentiments("afinn")) %>%
group_by(character, season) %>% 
  summarise(sent=mean(value, na.rm = T), n=n()) %>% 
  arrange(desc(n)) %>% head(n=20) %>% 
  arrange(desc(sent)) %>%
  ggplot(aes(x = character, y = sent, fill = character)) + 
  geom_bar(stat = "identity") + facet_wrap(vars(season)) + theme(legend.position="none")
