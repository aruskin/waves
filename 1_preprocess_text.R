# PREPROCESSING

# 1. Download text--techincally The Waves is not public domain in the US yet,
#   but it is in Australia and Canada

# For reference: https://www.r-bloggers.com/reading-html-pages-in-r-for-text-processing/
library(XML)
doc.html <- htmlTreeParse('http://gutenberg.net.au/ebooks02/0201091h.html',
                         useInternal = TRUE)

library(dplyr)
# Separate into pargaraphs, replace linebreaks and return characters
all.text <-  unlist(xpathApply(doc.html, '//p', xmlValue)) %>%
  gsub('\\n', ' ', .) %>%
  gsub('\r', '', .)

# Non-spoken passages are in italics; should have 40 paragraphs
italic.text <- unlist(xpathApply(doc.html, '//p//i', xmlValue)) %>%
  gsub('\\n', ' ', .) %>%
  gsub('\r', '', .)
# Look at source--there's a </i><i> in the middle of a paragraph here
italic.text[6] <- paste(italic.text[6], italic.text[7])
italic.text[7] <- NA
# Ouch, this one is even more of a mess
italic.text[31] <- paste(italic.text[31:35], collapse=" ")
italic.text[32:35] <- NA

# Find indices with weird A circumflex
bad.lines <- which(all.text == all.text[1])
waves.text <- all.text[-bad.lines]

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
name_locs <- str_locate(pattern = "\' said [A-Z][a-z]*[:punct:]", waves.text)
id <- sapply(1:length(waves.text), 
             function(i) substr(waves.text[i], 
                                name_locs[i, 1], 
                                name_locs[i, 2])) %>%
  gsub("\' said ", "", .) %>%
  gsub("[[:punct:]]", "", .)

waves.passages <- which(waves.text %in% italic.text)

id[waves.passages] <- "WAVES" 
curr.speaker <- "WAVES"
# How we deal with multi-paragraph quotes from same speaker
for(i in 1:length(id)){
  if(!is.na(id[i])) curr.speaker <- id[i]
  else id[i] <- curr.speaker
}

waves.text <- gsub("\' said [A-Z][a-z]*[[:punct:]][\']*", " ", waves.text)


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
  bind_rows

waves.text.words$SPEAKER <- id
waves.text.words$ID <- paste0(id, 1:length(id))

