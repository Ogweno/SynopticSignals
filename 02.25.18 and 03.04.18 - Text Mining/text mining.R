#library(qdap) # frequency of terms can't get without Java.
library(tm) # create corpus
library(wordcloud)

# read in file
bbc2 <- read.table("C:\\Users\\wol3\\Downloads\\Health-News-Tweets\\Health-Tweets\\bbchealth.txt", sep = "|", header = FALSE, stringsAsFactors = FALSE)

# checking what's wrong with text file
write.csv(bbc2, "C:\\Users\\wol3\\Downloads\\Health-News-Tweets\\Health-Tweets\\bbccheck.csv")

# read in file as CSVs after correcting the delimiter issues. 
bbc <- read.csv("C:\\Users\\wol3\\Downloads\\Health-News-Tweets\\Health-Tweets\\bbchealth.csv", header = TRUE, stringsAsFactors = FALSE)
cnn <- read.csv("C:\\Users\\wol3\\Downloads\\Health-News-Tweets\\Health-Tweets\\cnnhealth.csv", header = FALSE, stringsAsFactors = FALSE)
fox <-read.csv("C:\\Users\\wol3\\Downloads\\Health-News-Tweets\\Health-Tweets\\foxnewshealth.csv", header = FALSE, stringsAsFactors = FALSE)
kaiser <-read.csv("C:\\Users\\wol3\\Downloads\\Health-News-Tweets\\Health-Tweets\\KaiserHealthNews.csv", header = FALSE, stringsAsFactors = FALSE)
nyt <- read.csv("C:\\Users\\wol3\\Downloads\\Health-News-Tweets\\Health-Tweets\\nytimeshealth.csv", header = FALSE, stringsAsFactors = FALSE)

# structure
head(bbc)

#need to create column headers for the other datasets
colnames(cnn) <- c("ID", "Date", "Tweet")
colnames(fox) <- c("ID", "Date", "Tweet")
colnames(kaiser) <- c("ID", "Date", "Tweet")
colnames(nyt) <- c("ID", "Date", "Tweet")

#checking structure
str(cnn)
str(kaiser)
str(nyt)

#checking first set of each to make sure aligning correctly.
head(bbc)
head(cnn)
head(fox)
head(kaiser)
head(nyt)

# just tweet column
bbc_tweets <- bbc$Tweet
cnn_tweets <- cnn$Tweet
fox_tweets <- fox$Tweet
kaiser_tweets <- kaiser$Tweet
nyt_tweets <- nyt$Tweet

head(bbc$Tweet)

# Corpus thing
# vector source. Changed this piece of code to run each dataset then compiled each wordcloud at the end.
source <- VectorSource(bbc_tweets)

# make volatile corpus
corpus <- VCorpus(source)


# print out 3rd tweet data
corpus[[3]]

# print out 3rd tweet content
corpus[[1]][1]

# Cleaning the corpus section. 
toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, toSpace, "http[[:alnum:][:punct:]]*")
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "video", "audio", "can", "health", "well"))
  corpus <- tm_map(corpus, stripWhitespace)
}


clean_new <- clean_corpus(corpus)

clean_new[[3]][1]

# create the dtm from the corpus: bbc_dtm
dtm <- DocumentTermMatrix(clean_new)

#convert to matrix
dtm_matrix <- as.matrix(dtm)

# print dimensions of matrix
dim(dtm_matrix)

# review portion
dtm_matrix[150:155, 10000:10005]

# Create TDM (transpose of DTM) more rows than columns
tdm <- TermDocumentMatrix(clean_new)

#convert to new matrix
tdm_matrix <- as.matrix(tdm)

# print dimensions
dim(tdm_matrix)

#review same portion
tdm_matrix[10000:10005, 150:155]

#rowSums
term_frequency <- rowSums(tdm_matrix)

#sort descending
term_frequency2 <- sort(term_frequency, decreasing = TRUE)

#bar plot
barplot(term_frequency2[1:10])

#print out top 10 to make sure it's correct
head(term_frequency2,10)

#create data frame needed for wordcloud
bbc_word_freqs <- data.frame(term = names(term_frequency2), num = term_frequency2)

#create word cloud
wordcloud(kaiser_word_freqs$term, word_freqs$num, max.words = 50, colors = blue_pal)
wordcloud(nyt_word_freqs$term, word_freqs$num, max.words = 50, colors = grey_pal)
wordcloud(cnn_word_freqs$term, word_freqs$num, max.words = 50, colors = red_pal)
wordcloud(bbc_word_freqs$term, word_freqs$num, max.words = 50, colors = dred_pal)
wordcloud(fox_word_freqs$term, word_freqs$num, max.words = 50, colors = dblue_pal)


# Creating the color sets for each wordcloud above. Greys - NYT, Blues - KFF, Fox, Reds - CNN, BBC
display.brewer.all()

green_pal <- brewer.pal(8, "Greens")
green_pal <- green_pal[-(1:4)]

grey_pal <- brewer.pal(8, "Greys")
grey_pal <- grey_pal[-(1:4)]

blue_pal <- brewer.pal(8, "Blues")
blue_pal <- blue_pal[-(1:4)]

dblue_pal <- brewer.pal(8, "Blues")
dblue_pal <- dblue_pal[-(1:6)]

red_pal <- brewer.pal(8, "Reds")
red_pal <- red_pal[-(1:4)]

dred_pal <- brewer.pal(8, "Reds")
dred_pal <- dred_pal[-(1:6)]
