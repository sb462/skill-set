library(dplyr)
library(wordcloud)
library(tm)
library(RColorBrewer)

db <- src_sqlite('../input/database.sqlite', create = F)

db_controversial <- db %>%
  tbl('May2015') %>%
  filter(controversiality ==1, subreddit %in% c("news","worldnews")) %>%
  select(body) %>%
  collect() %>%
  data.frame() 

# lets do custom cleaning of the text since tm takes so long and often not correct
stop_words <- stopwords("SMART")
sublist <- paste0("\\b(", paste0(stop_words, collapse="|"), ")\\b")

clean_func <- function(text){
  # remove URLs
  ctext <- gsub("http[[:alnum:][:punct:]]*","",text, fixed = FALSE, perl = TRUE )
  # remove special characters
  ctext <- gsub("[^[:alnum:][:space:]']", " ", ctext,fixed = FALSE, perl = TRUE)
  # to lowercase
  ctext <- tolower(ctext)
  # remove stopwords
  ctext <- gsub(sublist, " ", ctext, fixed=FALSE, perl=TRUE)
  #ctext <- gsub("['[:alpha:]+]", " ", ctext, fixed=FALSE, perl=TRUE)
  # removing leftover ' if any
  ctext <- gsub("\\s\'\\w*\\s", " ", ctext, fixed=FALSE, perl=TRUE) 
  
  # remove extra whitespaces
  ctext <- gsub("[[:blank:]]+", " ",ctext, fixed=FALSE, perl=TRUE)
  return(ctext)
}

db_text <- db_controversial %>%
  mutate(clean_text = clean_func(body)) %>%
  select(clean_text)

corp <- Corpus(VectorSource(db_text$clean_text))    
dtm <- DocumentTermMatrix(corp,
                          control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
dtm_sparse <- removeSparseTerms(dtm, 0.99)
post_words <- as.data.frame(as.matrix(dtm_sparse))
print("matrix done")

total_words <- data.frame(words = colnames(post_words),
                          counts = colSums(post_words))


#Set up output so it is in a named png

png("news.png")

wordcloud(words = total_words$words,
          freq=total_words$counts, 
          max.words = 100,
          color = brewer.pal(8,"Dark2"))

dev.off()    
