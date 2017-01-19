libraries <- c('tm', 'ggplot2', 'openNLP', 'RWeka', 'slam', 'knitr')
lapply(libraries, require, character.only = TRUE)


## Set global options
options(stringsAsFactors = FALSE)
options(scipen = 999)
opts_chunk$set(cache = TRUE)

data <- c('blogs', 'news', 'twitter')
sample_data <- paste0(data, '.sample10')
pathname <- "~Coursera/Data_Science/10_Data-Science-Capstone/Coursera-SwiftKey/final/en_US"

## Modifing by taking out george carlin's 7 dirty words:

bad_word <- c("shit", "fuck", "cunt", "cocksucker", "motherfucker", "tits", "twat") 

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

# Removing unprintable characters: command line: tr -cd '/11/12/15/40-/176' <en_US.blogs.txt> en_US.blogs.filt.txt

