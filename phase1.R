library(quanteda)
library(readtext)
library(gutenbergr)
library(magrittr)
library(tidyverse)
library(viridis)
library(patchwork)
library(igraph)
library(ggraph)
library(colormap)
library(dplyr)

require(quanteda)

dir.create("tmp")

download.file(url = "http://www.gutenberg.org/cache/epub/26659/pg26659.txt",
              destfile = "tmp/The Will to Believe_William James_2009_English.txt")
dataframe <- readtext("tmp/*.txt",
                      docvarsfrom = "filenames",
                      docvarnames = c("title", "author",
                                      "year uploaded", "language"),
                      dvsep = "_",
                      encoding = "UTF-8")
unlink("tmp", recursive = TRUE)

doc.corpus <- corpus(dataframe)
summary(doc.corpus)

dfm_phil <- dfm(doc.corpus, remove_punct = TRUE, remove_numbers = TRUE)

# should I add more keys to these?
dicttags <- dictionary(list(tags = c("because", "that", "if", "difference between", "therefore", "then", "to")))
head(dfm(doc.corpus, dictionary = dicttags))

dictops <- dictionary(list(tags = c("are", "or", "not", "nor", "and", "but")))
head(dfm(doc.corpus, dictionary = dictops))


will_to_believe <- 
  if (dicttags == "because" || dicttags == "if" || dicttags == "therefore") { # have to refine (change the tags here)
  kwic(
    doc.corpus,
    dictops,
    window = 3, #put the smaller window before larger
    valuetype = c("fixed"),
    separator = " ",
    case_insensitive = TRUE,
    remove_punct = TRUE
  )
} else {
  kwic(
    doc.corpus,
    dictops,
    window = 5,
    valuetype = c("fixed"),
    separator = " ",
    case_insensitive = TRUE,
    remove_punct = TRUE
  )
}

#make data frame manipulable
phil_frame <- data.frame(will_to_believe)

prenodes = phil_frame %>% 
  select(pre)
phil_link = phil_frame %>%
  select(keyword)
postnodes = phil_frame %>%
  select(post)

#plot
src <- c(prenodes) 
edges <- c(phil_link)
target <- c(postnodes)

networkData <- data.frame(src, edges, target)
simpleNetwork(networkData)
