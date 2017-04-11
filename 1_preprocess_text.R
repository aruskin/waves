# PREPROCESSING

# 1. Download text--techincally The Waves is not public domain in the US yet,
#   but it is in Australia and Canada

# For reference: https://www.r-bloggers.com/reading-html-pages-in-r-for-text-processing/
library(XML)
doc.html <- htmlTreeParse('http://gutenberg.net.au/ebooks02/0201091h.html',
                         useInternal = TRUE)

library(dplyr)
# Separate into pargaraphs, replace linebreaks and return characters
doc.text <-  unlist(xpathApply(doc.html, '//p', xmlValue)) %>%
  gsub('\\n', ' ', doc.text) %>%
  gsub('\r', '', doc.text)

# Find indices with weird A circumflex
bad.lines <- which(doc.text == doc.text[1])
waves.text <- doc.text[-bad.lines]

#Still have some Project Gutenberg related text in there at beginning and end
head(waves.text)
waves.text <- waves.text[2:length(waves.text)]

tail(waves.text)
waves.text <- waves.text[1:(length(waves.text)-2)]

# Now should have vector where each entry is a paragraph. Yay!

# 2. Identify speakers
# Luckily for us, The Waves has a super consistent style in this regard
# (almost as if Woolf had future programmers in mind...)

library(stringr)
name_locs <- str_locate(pattern = "\' said [A-Z][a-z]*. \'", waves.text)
id <- sapply(1:length(waves.text), 
             function(i) substr(waves.text[i], 
                                name_locs[i, 1], 
                                name_locs[i, 2])) %>%
  gsub("\' said ", "", .) %>%
  gsub(". \'", "", .)

waves.passages <- grep("^[^\']", waves.text)
id[waves.passages] <- "WAVES" 
curr.speaker <- "WAVES"
# How we deal with multi-paragraph quotes from same speaker
for(i in 1:length(id)){
  if(!is.na(id[i])) curr.speaker <- id[i]
  else id[i] <- curr.speaker
}

waves.text <- gsub("\' said [A-Z][a-z]*. \'", " ", waves.text)


# 3. Turn into dataset with word counts for each excerpt
# (We can later turn these into frequencies if we want)
library(tokenizers)
all.words <- lapply(waves.text, tokenize_words) %>% unlist %>% unique %>% sort
waves.text.words <- lapply(waves.text, tokenize_words) %>%
  lapply(., table) %>%
  lapply(., function(x) {
    x <- as.data.frame(x)
    rownames(x) <- x$Var1
    x$Var1 <- NULL
    as.data.frame(t(x))
  }) %>%
  rbind.fill

waves.text.words$speaker <- id
waves.text.words$id <- paste0(id, 1:length(id))

