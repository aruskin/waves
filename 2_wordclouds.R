library(wordcloud)
library(RColorBrewer)
library(tokenizers)

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
            colors=colors)
}

##################

charNames <- c("bernard", "louis", "neville", "jinny", "rhoda", "susan", 
               "percival")

pronouns <-c("i", "me", "my", "you", "your", "yours", "she", "her", "hers",
             "he", "his", "him", "we", "our", "ours", "they", "their", 
             "theirs", "us", "them")

stopwds <- tokenizers::stopwords("en")

pal <- brewer.pal(6,"BuGn") %>%
  .[-(1:3)]

getCharacterCloud("Rhoda", waves.text.words[-ncol(waves.text.words)], 
                  stopwds, 100, pal)
getCharacterCloud("Rhoda", waves.text.words[-ncol(waves.text.words)], 
                  c(stopwds, pronouns), scale=c(4, .5), max.words=100, 
                  colors=pal)





  