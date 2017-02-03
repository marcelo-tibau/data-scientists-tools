library('tm')   
library('filehash')
library('tau')

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

## Data cleaning:

# Set the dataset to remove profanity and rude words. From the dataset FrontGateMedia (font: www.FrontGateMedia.com) which presents more than 700 such words.
# These words will be used as stopwords later on. 

profanity <- read.csv("Terms-to-Block.csv")
profanity <- profanity[-c(1:3),]
profanity <- rep(profanity$Your.Gateway.to.the.Chrisitan.Audience)

# Directing the source to the "trainings" dataset. In order to do that, I added a folder "training" 
# and included the Train.txt (blogs, news, twitter) files in it.
# I also and created the folder "modified" to hold in-process cleaning data.

Corpus <- PCorpus(DirSource("training", encoding = "UTF-8", mode = "text"),
                  dbControl = list(dbName="Corpus.db", dbType="DB1"))


# Cleaning steps:
# to convert to lower case, separate hyphenated and slashed words, convert symbol to apostrophe, 
# provide progress to user and create end of sentence markers:

Corpus <- tm_map(Corpus, content_transformer(tolower)); dbInit("Corpus.db")

for(j in seq(Corpus)) {
  Corpus[[j]][[1]] <- gsub("-", " ", Corpus[[j]][[1]])
  Corpus[[j]][[1]] <- gsub("/", " ", Corpus[[j]][[1]])
  Corpus[[j]][[1]] <- gsub("<>", "\\'", Corpus[[j]][[1]])
  print("3 of 18 transformations complete")
  Corpus[[j]][[1]] <- gsub("\\. |\\.$","  <EOS> ", Corpus[[j]][[1]])
  Corpus[[j]][[1]] <- gsub("\\? |\\?$","  <EOS> ", Corpus[[j]][[1]])
  Corpus[[j]][[1]] <- gsub("\\! |\\!$","  <EOS> ", Corpus[[j]][[1]])
  print("6 of 18 transformations complete") 
}

# To write corpus to permanent disc

write(Corpus[[1]][[1]], "./modified/CorpusTrain.txt")

# Reads back, tranforms various ASCII codes to appropriate language, removes all punctuation except apostrophe and <> symbols in <EOS>
# removes web site URLs, removes all single letters except "a" and "i":
Corpus <- PCorpus(DirSource("modified", encoding = "UTF-8", mode = "text"),
                  dbControl = list(dbName="halfCorpus.db", dbType="DB1"))

for(j in seq(Corpus)) {
  Corpus[[j]][[1]] <- gsub("<85>"," <EOS> ", Corpus[[j]][[1]])
  Corpus[[j]][[1]] <- gsub("<92>","'", Corpus[[j]][[1]])
  Corpus[[j]][[1]] <- gsub("\\&", " and ", Corpus[[j]][[1]])
  print("9 of 18 transformations complete")
  Corpus[[j]][[1]] <- gsub("[^[:alnum:][:space:]\'<>]", " ", Corpus[[j]][[1]])
  Corpus[[j]][[1]] <- gsub(" www(.+) ", " ", Corpus[[j]][[1]])
  Corpus[[j]][[1]] <- gsub(" [b-hj-z] "," ", Corpus[[j]][[1]])
  print("12 of 18 transformations complete")
  }

write(Corpus[[1]][[1]], "./modified/CorpusTrain.txt")

# Remove apostrophes introduced by transformations, errant codes in < > brackets, eplaces numbers with a number marker <NUM> for context
# and the errant <> brackets remaining:

Corpus <- PCorpus(DirSource("modified", encoding="UTF-8", mode = "text"), dbControl = list(dbName="lastCorpus.db", dbType="DB1"))

for(j in seq(Corpus)) {
  Corpus[[j]][[1]] <- gsub(" ' "," ", Corpus[[j]][[1]])
  Corpus[[j]][[1]] <- gsub("\\' ", " ", Corpus[[j]][[1]])
  Corpus[[j]][[1]] <- gsub(" ' ", " ", Corpus[[j]][[1]])
  print("15 of 18 transformations complete")
  Corpus[[j]][[1]] <- gsub("<[^EOS].+>"," ", Corpus[[j]][[1]])
  Corpus[[j]][[1]] <- gsub("[0-9]+"," <NUM> ", Corpus[[j]][[1]])
  Corpus[[j]][[1]] <- gsub("<>"," ", Corpus[[j]][[1]])
  print("18 of 18 transformations complete") 
}


# Codes to remove numbers and the "dbInit" function from filehash package to compresses data in RAM.
Corpus <- tm_map(Corpus, removeNumbers); dbInit("lastCorpus.db")

# Codes to remove errant 's symbols not as contractions, close brackets starting a word and white spaces such as line breaks>
Corpus[[1]][[1]] <- gsub(" 's"," ", Corpus[[1]][[1]])
Corpus[[1]][[1]] <- gsub(">[a-z]"," ", Corpus[[1]][[1]])

Corpus <- tm_map(Corpus, stripWhitespace); dbInit("lastCorpus.db") 


# Code to Write final, processed corpus to disc for building n-grams
write(Corpus[[1]][[1]], "./modified/CorpusTrain.txt")

## One-Gram Model
# Codes uses CorpusTrain.txt to generate list of all 1-gram (unigrams). The library tau is also used.

Corpus <- PCorpus(DirSource("modified", encoding="UTF-8", mode = "text"), dbControl = list(dbName="aggCorpus.db", dbType="DB1"))

# Define a source to pulls out the text element from the list Corpus:
CORP <- c(Corpus[[1]][[1]])

# Creation of a ngram function
n.gram <- function(n) {
  textcnt(CORP, method = "string", n = as.integer(n),
          split = "[[:space:][:digit:]]+", decreasing = T)
}


# Codes to build the one-gram model
one.gram <- n.gram(1)
one.gram.DF <- data.frame(Uni = names(one.gram), counts = unclass(one.gram))

one.gram.DF$Uni <- as.character(one.gram.DF$Uni)
one.gram.DF$counts <- as.numeric(one.gram.DF$counts)

# Codes to remove the "words" <eos> and <num> from one.gram data frame

one.gram.DF <- one.gram.DF[which(one.gram.DF$Uni !="<eos>"),]
one.gram.DF <- one.gram.DF[which(one.gram.DF$Uni !="<num>"),]

# Some exploratory analysis:
length(one.gram.DF$Uni)

wordcloud(one.gram.DF[,1], freq = one.gram.DF[,2], scale = c(5,1), random.order = F, rot.per = 0.5, min.freq = 100, colors = brewer.pal(8, "Dark2"))

# Code to build a frequency table for Good-Turing smoothing:
one.freq.t <- data.frame(Uni=table(one.gram.DF$counts))

# write to csv files to speedy the process later:
write.csv(one.gram.DF, "one.gram.DF.csv")
write.csv(one.freq.t, "one.freq.t.csv")

## Two-Gram Model
# Codes to build the two-gram model. Here we reset the Corpus in order to define a new database to two.gram:

Corpus <- PCorpus(DirSource("modified", encoding="UTF-8", mode = "text"), dbControl = list(dbName="twogramCorpus.db", dbType="DB1"))

CORP <- c(Corpus[[1]][[1]])

# Codes to set the number of loop runs to process 10,000 docs per run:
step <- trunc(length(CORP)/10000)
remain <- length(CORP)-(step * 10000)
CORPport <- CORP[1:remain]

# The two-gram model
two.gram <- n.gram(2)
names(two.gram) <- gsub("^\'","", names(two.gram))
two.gram.df <- data.frame(Bi = names(two.gram), counts = unclass(two.gram))
names(two.gram.df) <- c("Bi", "counts")

# Codes to remove the "words" <eos> and <num> from the two-gram database:
eost <- grepl("<eos>", two.gram.df$Bi)
two.gram.df <- two.gram.df[!eost,]
numt <- grepl("<num>", two.gram.df$Bi)
two.gram.df <- two.gram.df[!numt,]

# Code to write the N:n=step dataframe
write.csv(two.gram.df, "two.gram.def.csv")

# Codes to remove the processed docs from corpus, provide user progress, remove the "words" <eos> and <num> from the table and create a loop to process the 10000 docs.
CORP <- CORP[-(1:remain)]

for (i in 1:(step-1)) {
  CORPport <- CORP[1:10000]
  two.gram <- n.gram(2)
  names(two.gram) <- gsub("^\'","", names(two.gram))
  temp.two.gram.df <- data.frame(Bi = names(two.gram), counts = unclass(two.gram))
  print(paste("Iteration", i, "of", step))
  name <- paste("two.gram.df", (i+1), ".csv", sep = "")
  eost <- grepl("<eos>", temp.two.gram.df$Bi)
  temp.two.gram.df <- temp.two.gram.df[!eost, ]
  numt <- grepl("<num>", temp.two.gram.df$Bi)
  temp.two.gram.df <- temp.two.gram.df[!numt,]
  write.csv(temp.two.gram.df, name)
  two.gram.df <- rbind(two.gram.df, temp.two.gram.df)
  two.gram.df <- aggregate(two.gram.df$counts, list(Bi=two.gram.df$Bi), sum)
  names(two.gram.df) <- c("Bi", "counts")
  CORP <- CORP[-(1:10000)]
}

two.gram.df$Bi <- as.character(two.gram.df$Bi)
two.gram.df$counts <- as.numeric(two.gram.df$counts)  
two.gram.df$Uni <- sub(" .*","", two.gram.df$Bi)

# Codes to build frequency table for Good-Turing smoothing:

two.freq.t <- data.frame(Bi=table(two.gram.df$counts))
write.csv(two.gram.df, "two.gram.df.csv")
write.csv(two.freq.t, "two.freq.t.csv")





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