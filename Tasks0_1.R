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

# An N-Gram refers to the number of words in a string.
# I focuse on a Tri-gram model. The basic building blocks of that model are Uni-grams, Bi-grams, and Tri-grams.

# One-Gram Tokenization.
uni.gram.toke <- NGramTokenizer(Corpus, Weka_control(min = 1, max = 1))
uni.g <- data.frame(table(uni.gram.toke))
uni.g.sorted <- uni.g[order(uni.g$Freq, decreasing = TRUE),]
uni.g.sorted[1:30,]



# C:/Users/Marcelo/Documents/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/Coursera/Data_Science/10_Data-Science-Capstone/Coursera-SwiftKey/final/en_US/
# options(java.parameters = "-Xmx4g")
# options(java.parameters = "-XX: MaxPermSize")