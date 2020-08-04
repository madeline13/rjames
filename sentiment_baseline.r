library(tidyr)
library(dplyr)
library(stringr)
library(gutenbergr)

usa_const <- gutenberg_download(c(5))
usa_const

usa_const %>% # gutenbergr tokenization
  unnest_tokens(word, text) #multigrams-----------------inner_join(?)

usa <- corpus(usa_const)

usa_corpus <- dfm(usa, remove_punct = TRUE, remove_numbers = TRUE)
usa_toks <- tokens(
  usa,
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  split_hyphens = FALSE,
  include_docvars = TRUE,
  padding = TRUE
)

get_sentiments("afinn")

df <- data.frame(matrix(unlist(usa_toks), nrow=5349, byrow=T), stringsAsVectors=TRUE) #want stringsAsVectors for methods to work
names(df)[1] <- "word"

usa_sentiment <- df %>%
  inner_join (get_sentiments("afinn")) %>%
  group_by(index = "word")

usa_theta <- usa_sentiment %>%
  summarise(sentiment = sd(value, na.rm = TRUE)) %>% #get std deviation
  mutate(method = "AFINN")

#if sd > 1.30, emotional content is non-neutral; that means fewer terms cluster around a mean that approaches 0 with larger text samples
# citation afinn ---Finn Årup Nielsen A new ANEW: Evaluation of a word list for sentiment analysis in microblogs. Proceedings of the ESWC2011 Workshop on 'Making Sense of Microposts': Big things come in small packages 718 in CEUR Workshop Proceedings 93-98. 2011 May. http://arxiv.org/abs/1103.2903.
