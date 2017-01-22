libraries <- c('tm', 'ggplot2', 'openNLP', 'RWeka', 'slam', 'knitr')
lapply(libraries, require, character.only = TRUE)


## Set global options
options(stringsAsFactors = FALSE)
options(scipen = 999)
opts_chunk$set(cache = TRUE)

data <- c('blogs', 'news', 'twitter')
sample_data <- paste0(data, '.sample10')
write(sample_data)
pathname <- "C:/Users/Marcelo/Documents/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/Coursera/Data_Science/10_Data-Science-Capstone/Coursera-SwiftKey/final/en_US"

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

# Creating functions for reading and writing file

readfunction <- function(type, path, add_name = "", action = "r", output = NULL){
  read_file <- sprintf("%sen_US.%s.txt", path, type)
  read_file2 <- sprintf("%sen_US.%s.txt", path, type, add_name)
  if (action == "r"){
    con <- file(read_file, "r")
    feed_lines <- readLines(con, skipNul = T)
    close(con)
    return(list(feed = type, lines = feed_lines))
  }
  if (action == "w"){
    con <- file(read_file2, "w")
    feed_lines <- writeLines(text = output, con = con)
    close(con)
  }
}

en_US.blogs
"%en_US.%.txt"

# Create a sample

linesSample <- lapply(sample_data, readfunction, path = pathname, action = "r")



ls_sample <- lapply(feed_sample, feedStdIO, path = pathname, action = "r")
# str(ls_sample)
blog <- ls_sample[[1]][[2]]
news <- ls_sample[[2]][[2]]
twit <- ls_sample[[3]][[2]]