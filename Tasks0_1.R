library('tm')
library('ggplot2')
library('openNLP')
library('RWeka')
library('slam')
library('knitr')
library('quanteda')
library("stringi")
library('filehash')
library('scales')
library('rJava')
library('wordcloud')

## Task 0: Understanding the Problem

News <- readLines("en_US.news.txt", encoding = "UTF-8", skipNul=TRUE)
Twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
Blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)

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
# whitespaces, words not in the English language, profanity and 2) Converting to lowercase

data.sample <- sample(Blogs, length(Blogs) * 0.005)
Corpus <- VCorpus(VectorSource(data.sample))
ToSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
Corpus <- tm_map(Corpus, ToSpace, "(f|ht)tp(s?://(.*)[.][a-z]+)")
Corpus <- tm_map(Corpus, ToSpace, "@[^\\s]+")
Corpus <- tm_map(Corpus, tolower)
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))
Corpus <- tm_map(Corpus, removePunctuation)
Corpus <- tm_map(Corpus, removeNumbers)
Corpus <- tm_map(Corpus, stripWhitespace)
Corpus <- tm_map(Corpus, PlainTextDocument)

-
# An N-Gram refers to the number of words in a string.
# I focuse on a Tri-gram model. The basic building blocks of that model are Uni-grams, Bi-grams, and Tri-grams.

# One-Gram Tokenization.
uni.gram.toke <- NGramTokenizer(Corpus, Weka_control(min = 1, max = 1))
uni.g <- data.frame(table(uni.gram.toke))
uni.g.sorted <- uni.g[order(uni.g$Freq, decreasing = TRUE),]
uni.g.sorted[1:30,]


## Task 2: Exploratory Data Analysis

# Two-Gram Tokenization.
two.gram.toke <- NGramTokenizer(Corpus, Weka_control(min = 2, max = 2))
two.g <- data.frame(table(two.gram.toke))
two.g.sort <- two.g[order(two.g$Freq, decreasing = TRUE),]


# Three-Gram Tokenization.
three.gram.toke <- NGramTokenizer(Corpus, Weka_control(min = 3, max = 3))
three.g <- data.frame(table(three.gram.toke))
three.g.sort <- three.g[order(three.g$Freq, decreasing = TRUE),]


# Most popular non-stop words in the n-gram samples:
head(two.g.sort, n=10)
two.g.sort[1:30,]

head(three.g.sort, n=10)
three.g.sort[1:30,]

## Creating a wordcloud

par(mfrow = c(1,2))
wordcloud(two.g.sort[,1], freq = two.g.sort[,2], scale = c(5,1), random.order = F, rot.per = 0.5, min.freq = 100, colors = brewer.pal(8, "Dark2"))
wordcloud(three.g.sort[,1], freq = three.g.sort[,2], scale = c(5,1), random.order = F, rot.per = 0.5, min.freq = 100, colors = brewer.pal(8, "Dark2"))



# C:/Users/Marcelo/Documents/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/Coursera/Data_Science/10_Data-Science-Capstone/Coursera-SwiftKey/final/en_US/
# options(java.parameters = "-Xmx4g")
# options(java.parameters = "-XX: MaxPermSize")

# Quizz 1 - Q.3

fileName <- "en_US.blogs.txt"
con <- file(fileName, open = 'r')
lineBlogs <- readLines(con)
longBlogs <- length(lineBlogs)
close(con)

fileName <- "en_US.news.txt"
con <- file(fileName, open = 'r')
lineNews <- readLines(con)
longNews <- length(lineNews)
close(con)

fileName <- "en_US.twitter.txt"
con <- file(fileName, open = 'r')
lineTwitter <- readLines(con)
longTwitter <- length(lineTwitter)
close(con)



require(stringi)
longBlogs<-stri_length(lineBlogs)
max(longBlogs)

longNews <- stri_length(lineNews)
max(longNews)

longTwitter <- stri_length(lineTwitter)
max(longTwitter)

# Q.4

loveTwitter <- grep("love", lineTwitter)
length(loveTwitter)

hateTwitter <- grep("hate", lineTwitter)
length(hateTwitter)

dividelovehate <- length(loveTwitter)/length(hateTwitter)

# Q.5

biostatsTwitter <- grep("biostats", lineTwitter)
lineTwitter[biostatsTwitter]

# Q.6
senteceTwitter <- grep("A computer once beat me at chess, but it was no match for me at kickboxing", lineTwitter)
length(senteceTwitter)

