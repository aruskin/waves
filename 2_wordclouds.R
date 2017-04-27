source('1_preprocess_text.R') # make dataset
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
png("Plots/RhodaWordCloud_test.png")
getCharacterCloud("Rhoda", waves.text.words[-ncol(waves.text.words)], 
                  c(stopwds3), scale=c(4, .5), max.words=100, 
                  colors=pal)
dev.off()

pal2 <- brewer.pal(6,"OrRd") %>%
  .[-(1:2)]
png("Plots/JinnyWordCloud_test.png")
getCharacterCloud("Jinny", waves.text.words[-ncol(waves.text.words)], 
                  c(stopwds3), scale=c(4, .5), max.words=100, 
                  colors=pal2)
dev.off()

png("Plots/NevilleWordCloud_test.png")
getCharacterCloud("Neville", waves.text.words[-ncol(waves.text.words)], 
                  c(stopwds3), scale=c(3, .5), max.words=100, 
                  colors=pal2)
dev.off()

png("Plots/BernardWordCloud_test.png")
getCharacterCloud("Bernard", waves.text.words[-ncol(waves.text.words)], 
                  c(stopwds3), scale=c(3, .5), max.words=100, 
                  colors=pal2)
dev.off()

png("Plots/SusanWordCloud_test.png")
getCharacterCloud("Susan", waves.text.words[-ncol(waves.text.words)], 
                  c(stopwds3), scale=c(3, .5), max.words=100, 
                  colors=pal)
dev.off()

png("Plots/LouisWordCloud_test.png")
getCharacterCloud("Louis", waves.text.words[-ncol(waves.text.words)], 
                  c(stopwds3), scale=c(3, .5), max.words=100, 
                  colors=pal)
dev.off()


pal3 <- brewer.pal(6,"Blues") %>%
  .[-(1:2)]
png("Plots/WavesWordCloud_test.png")
getCharacterCloud("WAVES", waves.text.words[-ncol(waves.text.words)], 
                  c(stopwds3), scale=c(4, .5), max.words=100, 
                  colors=pal3)
dev.off()



