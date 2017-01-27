library('tm')   
library('filehash')

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
