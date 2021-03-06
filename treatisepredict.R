library(tm)
library(tidyr)
library(tidytext)
library(dplyr)
library(stringr)
library(quanteda)
library(gutenbergr)
library(readtext)
library(tune)
library(workflows)
library(tidymodels)
library(e1071)
library(mlbench)
library(naivebayes)

treatise_text <- gutenberg_download(7370, mirror = "http://mirrors.xmission.com/gutenberg/")
treatise_text

treatise_stripped <- tail(treatise_text, n = -49L)
treatise_stripped

treatise_stripped %>% # gutenbergr tokenization
  unnest_tokens(word, text) #multigrams-----------------inner_join(?)

treatise <- corpus(treatise_stripped)

treatise_corpus <- dfm(treatise, remove_punct = TRUE, remove_numbers = TRUE)
treatise_corpus

treatise_toks <- tokens(
  treatise,
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  split_hyphens = FALSE,
  include_docvars = TRUE,
  padding = FALSE
)

treatise_toks

dicttags <- dictionary(list(tags = c("will", "shall", "may", "to", "not")))
head(dfm(treatise_corpus, dictionary = dicttags))

be <- dictionary(list(tags = c("be")))
head(dfm(treatise_corpus, dictionary = be))

  treatise_clean <- 
  if (dicttags == "will" || dicttags == "shall" || dicttags == "may") {
    kwic(
      treatise_toks,
      be,
      window = 1, 
      valuetype = c("fixed"),
      separator = " ",
      case_insensitive = TRUE,
      remove_punct = TRUE
    )
  } else {
    kwic(
      treatise_toks,
      be,
      window = 1, 
      valuetype = c("fixed"),
      separator = " ",
      case_insensitive = TRUE,
      remove_punct = TRUE
    )
  }
treatise_clean

treatise_data <- as.data.frame(treatise_clean)

treatise_train <- treatise_data[1:30, ]
treatise_test <- treatise_data[31:100, -1]

### 1) General usage via formula interface
treatise_nb <- naive_bayes(pre ~ ., treatise_train, laplace = 1)
summary(treatise_nb)
