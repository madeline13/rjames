library(tm)
library(gsl)
library(tidyr)
library(tidytext)
library(tidyverse)
library(dplyr)
library(stringr)
library(quanteda)
library(gutenbergr)
library(readtext)
library(tidymodels)
library(mlr)
library(ggplot2)
library(textrecipes)
library(tokenizers)
library(quanteda.textmodels)
library(caret)
library(topicmodels)

kjv_text <- gutenberg_download(10, mirror = "http://mirrors.xmission.com/gutenberg/")

kjv_text %>% # gutenbergr tokenization
  unnest_tokens(word, text, token = "words")

write.csv(kjv_text, "kjv.csv")

kjv_read <- read.csv("modifiedkjvData.csv")

kjv <- corpus(kjv_read$text)
kjv2 <- Corpus(VectorSource(kjv_read$text))
kjv_tdm <- TermDocumentMatrix(kjv2)


kjv_toks <- tokens(
  kjv,
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  split_hyphens = FALSE,
  include_docvars = TRUE,
  padding = FALSE,
)

dicttags <- dictionary(list(tags = c("will", "shall", "may", "to", "not")))

be <- dictionary(list(tags = c("be", "is", "was", "have")))

kjv_clean <- 
  if (dicttags == "will" || dicttags == "shall" || dicttags == "may") {
    kwic(
      kjv_toks,
      be,
      window = 5, 
      valuetype = c("fixed"),
      case_insensitive = TRUE,
      remove_punct = TRUE
    )
  } else {
    kwic(
      kjv_toks,
      be,
      window = 5, 
      valuetype = c("fixed"),
      case_insensitive = TRUE,
      remove_punct = TRUE
    )
  }

write_csv(kjv_clean, "modifiedkjvData3.csv")
kjv_read2 <- read.csv("modifiedkjvData3.csv")

kjv_be <- Corpus(VectorSource(kjv_read2$pre))
kjv_be_tdm <- TermDocumentMatrix(kjv2)

kjv_pre <- as.character(kjv_read2$pre)
kjv_post <- as.character(kjv_read2$post)

kjv_future_data <- kjv_read2 %>%
  dplyr::filter(kjv_pre %in% c("will", "may", "would")) %>%
  select(docname, from, to, pre, keyword, post, pattern) %>%
  mutate(
    post = NULL,
    pattern = NULL,
    from = NULL,
    to = NULL)
  
kjv_moral_data <- kjv_read2 %>%
  dplyr::filter(kjv_pre %in% c("must", "should", "shall")) %>%
  select(docname, from, to, pre, keyword, post, pattern) %>%
  mutate(
    post = NULL,
    pattern = NULL,
    from = NULL,
    to = NULL)

kjvfuture <- Corpus(VectorSource(kjv_future_data$pre))
kjv_future_tdm <- TermDocumentMatrix(kjvfuture)

kjvmoral <- Corpus(VectorSource(kjv_read2$pre))
kjv_moral_tdm <- TermDocumentMatrix(kjvmoral)

# LDA--compare future with moral, with be, with baseline
# may need fewer topics; would all need the same number?

kjv_model <- LDA(kjv_tdm, control = list(seed = 10), k = 15)

kjv_be_model <- LDA(kjv_be_tdm, control = list(seed = 10), k = 15)

kjv_future_model <- LDA(kjv_future_tdm, control = list(seed = 10), k = 15)

kjv_moral_model <- LDA(kjv_moral_tdm, control = list(seed = 10), k = 15)