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
library(networkD3)

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
# make into compound tokens--which dictionary?
toks <- tokens(doc.corpus)

kw_multiword1 <- kwic(toks, pattern = phrase(c('because', 'if', 'therefore', 'then', 'if', 'to')))
kw_multiword2 <- kwic(toks, pattern = phrase(c("are", "or", "not", "nor", "and", "but")))

#make each token a corpus
corpus(kw_multiword1)
idemes <- kw_multiword1


# make Sankey
# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c(idemes), 
  target=c(idemes), 
  value=c(1)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# !!!!! With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

