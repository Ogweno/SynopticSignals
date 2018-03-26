library(tm) # create corpus
library(wordcloud)
library(plotrix)

# read in files
fox <-read.csv("C:\\Users\\Jeff\\Downloads\\Health-Tweet\\Health-Tweets\\foxnewshealth.csv", header = FALSE, stringsAsFactors = FALSE)
nyt <- read.csv("C:\\Users\\Jeff\\Downloads\\Health-Tweet\\Health-Tweets\\nytimeshealth.csv", header = FALSE, stringsAsFactors = FALSE)

# column names
colnames(fox) <- c("ID", "Date", "Tweet")
colnames(nyt) <- c("ID", "Date", "Tweet")

# Create fox news and nyt tweets
all_fox <- paste(fox$Tweet, collapse = " ")
all_nyt <- paste(nyt$Tweet, collapse = " ")

# All tweets
all_tweets <- c(all_fox, all_nyt)

# Convert to vector source
all_tweets_v <- VectorSource(all_tweets)

# Create the corpus
all_corpus <- VCorpus(all_tweets_v)

# Cleaning the corpus section. 
toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )

# create function to clean
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, toSpace, "http[[:alnum:][:punct:]]*")
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "video", "audio", "can", "health", "well"))
  corpus <- tm_map(corpus, stripWhitespace)
}

clean_tweets <- clean_corpus(all_corpus)

# creating Term Document Matrix
tweet_tdm <- TermDocumentMatrix(clean_tweets)

# convert to matrix
tweet_m <- as.matrix(tweet_tdm)

# print commonality cloud
commonality.cloud(tweet_m, colors = "steelblue1", max.words = 100)

# Giving columns distinct names
colnames(tweet_tdm) <- c("fox", "nyt")

# create all_m
tweet_m2 <- as.matrix(tweet_tdm)

# comparison.cloud not in common
comparison.cloud(tweet_m2, colors = c("orange", "blue"), max.words = 50)

# create common words
common_words <- subset(tweet_m, tweet_m[, 1] > 0 & tweet_m[, 2] > 0)

# create difference
difference <- abs(common_words[, 1] - common_words[, 2])



# combine common_words and difference
common_words <- cbind(common_words, difference, fox.freq = prop.table(common_words[,1])*100, nyt.freq = prop.table(common_words[,2])*100, total = common_words[,1] + common_words[,2])

# ordering the data
common_words <- common_words[order(common_words[, 6], decreasing = TRUE), ]

# creating top 25 df
top25_df <- data.frame(x = common_words[1:25, 4], y = common_words[1:25, 5],
                       labels = rownames(common_words[1:25, ]))

# create pyrmaid plot
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, gap = 0,
             top.labels = c("Fox", "Words", "NYT"),
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL)

# creating top 50 df
top50_df <- data.frame(x = common_words[1:50, 4], y = common_words[1:50, 5],
                       labels = rownames(common_words[1:50, ]))

# create pyrmaid plot
pyramid.plot(top50_df$x, top50_df$y,
             labels = top50_df$labels, gap = 0,
             top.labels = c("Fox", "Words", "NYT"),
             main = "Words in Common", laxlab = seq(from = 0, to = 3, by = 0.5), 
             raxlab = seq(from = 0, to = 3, by = 0.5), unit = "%")

write.csv(common_words, "C:\\Users\\Jeff\\Downloads\\Health-Tweet\\Health-Tweets\\check.csv")


