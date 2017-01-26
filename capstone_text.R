library('NLP')
library('RWeka')
library('tm')
library('SnowballC')
library('wordcloud')
library('stringi')
library('stringr')

News <- readLines("en_US.news.txt", encoding = "UTF-8", skipNul=TRUE)
Twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
Blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)

## Task 0: Understanding the Problem

# Basic readings and analysis on Twitter data:
twitterWords <- stringi::stri_count_words(Twitter)
summary(twitterWords)
range(twitterWords)

twitterStats <- stringi::stri_stats_general(Twitter)
twitterStats
range(twitterStats)

sampleReading_twitter <- Twitter[sample(1:length(Twitter), 30000)]
twitterStatsSR <- stringi::stri_stats_general(sampleReading_twitter)
twitterStatsSR

# Basic readings and analysis on News data:
newsWords <- stringi::stri_count_words(News)
summary(newsWords)
range(newsWords)

newsStats <- stringi::stri_stats_general(News)
newsStats
range(newsStats)

sampleReading_news <- News[sample(1:length(News), 30000)]
newsStatsSR <- stringi::stri_stats_general(sampleReading_news)
newsStatsSR

# Basic readings and analysis on Blogs data:
blogsWords <- stringi::stri_count_words(Blogs)
summary(blogsWords)
range(blogsWords)

blogsStats <- stringi::stri_stats_general(Blogs)
blogsStats
range(blogsStats)

sampleReading_blogs <- Blogs[sample(1:length(Blogs), 30000)]
blogsStatsSR <- stringi::stri_count_words(sampleReading_blogs)
blogsStatsSR

# Setting seed for reproducibility purposes and sampling the data sets

set.seed(148)
Twitter.sample <- sample(Twitter, length(Twitter)*0.10, replace = FALSE)

News.sample <- sample(News, length(News)*0.10, replace = FALSE)

Blogs.sample <- sample(Blogs, length(Blogs)*0.10, replace = FALSE)

# Inspecting the provided Datasets

summary(nchar(News.sample))
hist(News.sample)

# some info about the data (MB and lines) and summaries 
format(object.size(News), units = "MB")
length(News)

format(object.size(Twitter), units = "MB")
length(Twitter)

format(object.size(Blogs), units = "MB")
length(Blogs)

summary(Blogs)
summary(nchar(Blogs))
summary(Twitter)
summary(nchar(Twitter))
summary(News)
summary(nchar(News))

## Task 1: Getting and cleaning the data

# Creating a Subset Corpus, Cleaning Data and Reviewing Basic Data Summaries.
# Data Cleaning includes: 1)Removing; numbers, punctuation, foreign characters, 
# whitespaces, words not in the English language,  and 2) Converting to lowercase


# new data set with samples of all three datas together
new.data <- c(News.sample,Twitter.sample,Blogs.sample)
corpus <- Corpus(VectorSource(new.data))
remove.decimals <- function(x) {gsub("([0-9]*)\\.([0-9]+)", "\\1 \\2", x)}
remove.hashtags <- function(x) { gsub("#[a-zA-z0-9]+", " ", x)}
remove.noneng <- function(x) {gsub("\\W+", " ",x)}
corpus <- tm_map(corpus, remove.decimals)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, remove.noneng)
corpus <- tm_map(corpus, remove.hashtags)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# only twitter sample data to avoid out of memory bust while sequencing
corpus.twitter <- Corpus(VectorSource(Twitter.sample))
remove.decimals <- function(x) {gsub("([0-9]*)\\.([0-9]+)", "\\1 \\2", x)}
remove.hashtags <- function(x) {gsub("#[a-zA-Z0-9]+", " ", x)}
remove.noneng <- function(x) {gsub("\\W+", " ", x)}
corpus.twitter <- tm_map(corpus.twitter, remove.decimals)
corpus.twitter <- tm_map(corpus.twitter, removeNumbers)
corpus.twitter <- tm_map(corpus.twitter, remove.noneng)
corpus.twitter <- tm_map(corpus.twitter, remove.hashtags)
corpus.twitter <- tm_map(corpus.twitter, stripWhitespace)
corpus.twitter <- tm_map(corpus.twitter, removePunctuation)
corpus.twitter <- tm_map(corpus.twitter, tolower)
corpus.twitter <- tm_map(corpus.twitter, removeWords, stopwords("english"))

corpus.twitter <- tm_map(corpus.twitter, removeWords, profanity)

# Remove profanity and rude words. From the dataset FrontGateMedia (font: www.FrontGateMedia.com) which presents more than 700 such words.
# These words will be used as stopwords. 

profanity <- read.csv("Terms-to-Block.csv")
profanity <- profanity[-c(1:3),]
profanity <- rep(profanity$Your.Gateway.to.the.Chrisitan.Audience)

corpus <- tm_map(corpus, removeWords, profanity)


## Task 2: Exploratory Data Analysis & Task 3: Modeling

# One-Gram Tokenization

one.gram.toke <- NGramTokenizer(corpus.twitter, Weka_control(min = 1, max = 1))
one.g <- data.frame(table(one.gram.toke))
one.g.sort <- one.g[order(one.g$Freq, decreasing = TRUE),]

one.g.sort[1:20,]

wordcloud(one.g.sort[,1], freq = one.g.sort[,2], scale = c(5,1), random.order = F, rot.per = 0.5, min.freq = 100, colors = brewer.pal(8, "Dark2"))

# Two-Gram Tokenization

two.gram.toke <- NGramTokenizer(corpus.twitter, Weka_control(min = 2, max = 2))
two.g <- data.frame(table(two.gram.toke))
two.g.sort <- two.g[order(two.g$Freq, decreasing = TRUE),]

two.g.sort[1:20,]

# Three-Gram Tokenization
three.gram.toke <- NGramTokenizer(corpus.twitter, Weka_control(min = 3, max = 3))
three.g <- data.frame(table(three.gram.toke))
three.g.sort <- three.g[order(three.g$Freq, decreasing = TRUE),]

three.g.sort[1:20,]

# Creating a wordcloud
par(mfrow = c(1,2))
wordcloud(two.g.sort[,1], freq = two.g.sort[,2], scale = c(5,1), random.order = F, rot.per = 0.5, min.freq = 100, colors = brewer.pal(8, "Dark2"))
wordcloud(three.g.sort[,1], freq = three.g.sort[,2], scale = c(5,1), random.order = F, rot.per = 0.5, min.freq = 100, colors = brewer.pal(8, "Dark2"))

# To choose a value for n in an n-gram model, it is necessary to find the right trade off between the stability of
# the estimate against its appropriateness. This means that
# three-gram is a common choice with large training corpora (millions of words), 
# whereas a two-gram is often used with smaller ones, such as the ones we are working with.


## Task 4: Prediction Model

# Partition the Data into Train and Test Dataset.
library('caret')

inTrain <- createDataPartition(y=corpus.twitter$classe, p=0.7, list=FALSE)
training <- corpus.twitter[inTrain,]
testing <- corpus.twitter[-inTrain,]

# Conditional Inference Trees
# Uses an implementation of conditional inference trees which embed tree-structured regression models into a well defined theory of conditional inference procedures.
library('party')

# To train the data set;
train.ctree <- ctree(Author~., data = training, controls = ctree_control(maxsurrogate = 2))
plot(train.ctree)

# Prediction applied on testing and training data set using the trained model:
testing.pred.ctree <- predict(train.ctree, testing)
training.pred.ctree <- predict(train.ctree, training)

# Misclassification Matrix
# Test Data Prediction:
misClassTest <- table("Predict"=testing.pred.ctree, "Actual"=testing$Author)

# Train Data Prediction:
missClassTrain <- table("Predict"=training.pred.ctree, "Actual"=training$Author)

# Accuracy based on Acceptance criteria
Accuracy.ctree <- (100-mean(c((nrow(testing)-sum(diag(misClassTest)))/nrow(testing)),(nrow(training)-sum(diag(missClassTrain)))/nrow(training)))
Accuracy.ctree



