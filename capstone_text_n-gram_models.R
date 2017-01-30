library('tm')   
library('filehash')

# Using virtual corpus to read the three data sets (I created 3 folders and associated one corpus per folder:

Twitter <- VCorpus(DirSource("twitter", encoding = "UTF-8"), readerControl = list(language="en"))

Blogs <- VCorpus(DirSource("blogs", encoding = "UTF-8"), readerControl = list(language="en"))

News <- VCorpus(DirSource("news", encoding = "UTF-8"), readerControl = list(language="en"))

# To split corpora into train, devtest, and test sets I decided to randomize the order of each corpus and 
# then set the ratio to 60% in order to get the training set and divide the rest into testing and devtest sets.

# Twitter
set.seed(148)
perm.twitter <- sample(Twitter[[1]][[1]], length(Twitter[[1]][[1]]))
TwitR <- round(0.6*length(perm.twitter))
twitterTrain <- perm.twitter[1:TwitR]
remain <- perm.twitter[-(1:TwitR)]

DEV <- round(0.5*(length(remain)))
twitterDevTest <- remain[1:DEV]
twitterTest <- remain[-(1:DEV)]

write(twitterTrain, "twitterTrain.txt")
write(twitterDevTest, "twitterDevTest.txt")
write(twitterTest, "twitterTest.txt")

# Blogs
set.seed(149)
perm.blog <- sample(Blogs[[1]][[1]], length(Blogs[[1]][[1]]))
BlogR <- round(0.6*length(perm.blog))
blogsTrain <- perm.blog[1:BlogR]
remain2 <- perm.blog[-(1:BlogR)]

DEV2 <- round(0.5*length(remain2))
blogDevTest <- remain2[1:DEV2]
blogTest <- remain[-(1:DEV2)]

write(blogsTrain, "blogsTrain.txt")
write(blogDevTest, "blogDevTest.txt")
write(blogTest, "blogTest.txt")

# News
set.seed(150)
perm.news <- sample(News[[1]][[1]], length(News[[1]][[1]]))
NewsR <- round(0.6*length(perm.news))
newsTrain <- perm.news[1:NewsR]
remain3 <- perm.news[-(1:NewsR)]

DEV3 <- round(0.5*length(remain3))
newsDevTest <- remain3[1:DEV3]
newsTest <- remain3[-(1:DEV3)]

write(newsTrain, "newsTrain.txt")
write(newsDevTest, "newsDevtest.txt")
write(newsTest, "newsTest.txt")

#### restart from executive Summary: https://github.com/jgendron/datasciencecoursera/blob/master/NLP-A%20Model%20to%20Predict%20Word%20Sequences.Rmd

profanity <- read.csv("Terms-to-Block.csv")
profanity <- profanity[-c(1:3),]
profanity <- rep(profanity$Your.Gateway.to.the.Chrisitan.Audience)

# Writes final, processed corpus to disc for building n-grams
new.data <- c(News.sample,Twitter.sample,Blogs.sample)
corpus <- Corpus(VectorSource(new.data))
remove.decimals <- function(x) {gsub("([0-9]*)\\.([0-9]+)", "\\1 \\2", x)}
remove.hashtags <- function(x) { gsub("#[a-zA-z0-9]+", " ", x)}
remove.noneng <- function(x) {gsub("\\W+", " ",x)}

corpus <- PCorpus(DirSource("en_US",
                            encoding="UTF-8",mode="text"),dbControl=list(dbName="lastCorpus.db",
                                                                         dbType="DB1"))
for (j in seq(corpus)) {
  corpus <- tm_map(corpus, remove.decimals)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, remove.noneng)
  corpus <- tm_map(corpus, remove.hashtags)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeWords, profanity)
}




write(corpus,"./corpusTrain.txt") 
corpus <- tm_map(corpus, tolower); dbInit("lastCorpus.db")

#package to create N-Grams
library('tau') 

#pulls out the text element from the corpus
CORP <- c(corpus[[1]][[1]])

ngram <- function(n) {
  textcnt(CORP, method = "string", n = as.integer(n),
          split = "[[:space:][:digit:]]+", decreasing = T)
}

one.gram <- ngram(1)
one.gram.df <- data.frame(Uni = names(one.gram), counts = unclass(one.gram))

one.gram.df$Uni <- as.character(one.gram.df$Uni)
one.gram.df$counts <- as.numeric(one.gram.df$counts)

# Removes the "words" <eos> and <num> from one-gram table:
one.gram.df <- one.gram.df[which(one.gram.df$Uni!="<eos>"),]
one.gram.df <- one.gram.df[which(one.gram.df$Uni!="<num>"),]

length.one.gram <- length(one.gram.df$Uni)


### not done
lengthUni<-length(unigramDF$Uni) #253,921 unigrams

# Builds frequency of frequency table for Good-Turing smoothing
uni.freqfreq<-data.frame(Uni=table(unigramDF$counts))

write.csv(unigramDF,"unigramDF.csv") #2,620 frequencies
write.csv(uni.freqfreq,"uni.freqfreq.csv")
rm(unigramDF,uni.freqfreq,CORP,myCorpus)
