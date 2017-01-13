library('tm')
library('ggplot2')
library('openNLP')
library('RWeka')
library('slam')
library('knitr')

## Task 0: Understanding the Problem

News <- readLines("en_US.news.txt", encoding = "UTF-8", skipNul=TRUE)
Twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
Blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)

# some info about the data (MB and lines)
format(object.size(News), units = "MB")
length(News)

format(object.size(Twitter), units = "MB")
length(Twitter)

format(object.size(Blogs), units = "MB")
length(Blogs)

# C:/Users/Marcelo/Documents/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/Coursera/Data_Science/10_Data-Science-Capstone/Coursera-SwiftKey/final/en_US/