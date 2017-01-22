

News <- readLines("en_US.news.txt", encoding = "UTF-8", skipNul=TRUE)
Twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
Blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)


set.seed(48)
News.sample <- sample(News, length(News)*0.10, replace = FALSE)
Twitter.sample <- sample(Twitter, length(Twitter)*0.10, replace = FALSE)
Blogs.sample <- sample(Blogs, length(Blogs)*0.10, replace = FALSE)

summary(nchar(News.sample))
hist(News.sample)


library(tm)
library(SnowballC)
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


two.gram.toke <- NGramTokenizer(corpus, Weka_control(min = 2, max = 2))
two.g <- data.frame(table(two.gram.toke))
two.g.sort <- two.g[order(two.g$Freq, decreasing = TRUE),]