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


dictions <- function(){
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
}
# make into compound tokens--which dictionary?
toks <- tokens(doc.corpus)

kw_multiword1 <- kwic(toks, pattern = phrase(c('because', 'if', 'therefore', 'then', 'if', 'to')))
kw_multiword2 <- kwic(toks, pattern = phrase(c("are", "or", "not", "nor", "and", "but")))


operators <- function(kw_multiword2){
  if (dicttags == "because" || dicttags == "if" || dicttags == "therefore" || dicttags == "then" || dicttags == "if" || dicttags == "to") {
    print (kw_multiword2)
  } 
}


#make each token a corpus
corpus(kw_multiword2)
idemes <- kw_multiword2

operators()



# make Sankey
# create nodes data by determining all unique nodes found in your data
node_names <- unique(c(as.character(idemes$source), as.character(idemes$target)))
nodes <- data.frame(name = node_names)

# create links data by matching the source and target values to the index of the
# node it refers to in the nodes data frame
links <- data.frame(source = match(kw_multiword2$source, node_names) - 1,
                    target = match(kw_multiword2$target, node_names) - 1)


# !!!!!!!!-as.matrix(?)
sankeyNetwork(Links = "links", Nodes = "nodes", Source = "source", 
              Target = "target")

