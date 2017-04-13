library(wordcloud)
library(RColorBrewer)
library(tokenizers)
library(dplyr)
library(tm)

# Assume the only non-word frequency column in word.matrix is 
# SPEAKER
getCharacterCloud <- function(char.name, word.matrix, exclude=NULL,
                              scale=c(8, .5), min.freq=3,
                              max.words=100, colors="black"){
  char.rows <- word.matrix %>% 
    filter(SPEAKER == char.name) %>%
    select(-SPEAKER)
  char.words <- colSums(x = char.rows, na.rm=TRUE) %>%
    .[!(names(.) %in% exclude)]
  wordcloud(names(char.words), char.words, scale, min.freq, max.words, 
            random.order=FALSE, colors=colors)
}

##################

# so many ways to define stopwords
# which do we like best?
stopwds1 <- tokenizers::stopwords("en")
stopwds2 <- tm::stopwords("en")
stopwds3 <- tm::stopwords("SMART")

pal <- brewer.pal(6,"BuGn") %>%
  .[-(1:3)]
png("RhodaWordCloud_test.png")
getCharacterCloud("Rhoda", waves.text.words[-ncol(waves.text.words)], 
                  c(stopwds3), scale=c(4, .5), max.words=100, 
                  colors=pal)
dev.off()

pal2 <- brewer.pal(6,"OrRd") %>%
  .[-(1:2)]
png("JinnyWordCloud_test.png")
getCharacterCloud("Jinny", waves.text.words[-ncol(waves.text.words)], 
                  c(stopwds3), scale=c(4, .5), max.words=100, 
                  colors=pal2)
dev.off()
  