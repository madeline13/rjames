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

#   (modify the sources and targets) https://www.data-to-viz.com/graph/arc.html 
links= data.frame( 
  source=c(kw_multiword1),
  target=c(kw_multiword2)
)

# Transform to a igraph object
mygraph <- graph_from_data_frame(links)

# Make the usual network diagram
p1 <-  ggraph(mygraph) + geom_edge_link(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +geom_node_point( color="#69b3a2", size=5) +geom_node_text( aes(label=name), repel = TRUE, size=8, color="#69b3a2") +theme_void() +theme(legend.position="none",plot.margin=unit(rep(2,4), "cm")) 

# Make a cord diagram
p2 <-  ggraph(mygraph, layout="linear") + 
  geom_edge_arc(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +
  geom_node_point( color="#69b3a2", size=5) +
  geom_node_text( aes(label=name), repel = FALSE, size=8, color="#69b3a2", nudge_y=-0.1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(rep(2,4), "cm")
  ) 

p2

