library('tm')   
library('filehash')
library('tau')
library('wordcloud')

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


## Three-gram
# Codes to build the three-gram model. The deal here is to break large corpus into chunks to deal
# with limitations imposed by RAM in R

Corpus <- PCorpus(DirSource("modified", encoding = "UTF-8", mode = "text"), dbControl = list(dbName="threegramCorpus.db", dbType="DB1"))

CORP <- c(Corpus[[1]][[1]])

# Codes to determine the number of loop runs to process to 10,000 docs per run and write the first of N:n=step dataframes:

step <- trunc(length(CORP)/10000)

for (i in 1:(step)) {
  CORPport <- CORP[1:10000]
  three.gram <- n.gram(3)
  names(three.gram) <- gsub("^\'","", names(three.gram))
  three.gram.df <- data.frame(Tri = names(three.gram), counts = unclass(three.gram))
  print(paste("Iteration", i, sep = " "))
  name <- paste("three.gram.df", i, ".csv", sep = "")
  eost <- grepl("<eos>", three.gram.df$Tri)
  three.gram.df <- three.gram.df[!eost,]
  numt <- grepl("<num>", three.gram.df$Tri)
  three.gram.df <- three.gram.df[!numt,]
  write.csv(three.gram.df, name)
  CORP <- CORP[-(1:10000)]
}

CORPport <- CORP

# To repeat above loop for the remaining documents (partial step)
three.gram <- n.gram(3)
names(three.gram) <- gsub("^\'","", names(three.gram))
three.gram.df <- data.frame(Tri = names(three.gram), counts = unclass(three.gram))
names(three.gram.df) <- c("Tri", "counts")
name <- paste("three.gram.df", i+1, ".csv", sep = "")
eost <- grepl("<eos>", three.gram.df$Tri)
three.gram.df <- three.gram.df[!eost,]
numt <- grepl("<num>", three.gram.df$Tri)
three.gram.df <- three.gram.df[!numt, ]
write.csv (three.gram.df,name)

# Codes to aggregate dataframes in groups to develop one three-gram dataframe:
# First round aggregation

for (k in seq(1, 46, 5)) {
  key <- paste("three.gram.df", k, ".csv", sep = "")
  print(k)
  three.gram.df <- read.csv(file = key)
  three.gram.df <- three.gram.df[,-1]
  for (m in 1:4) {
    name <- paste("three.gram.df", (k+m), ".csv", sep = "")
    temp <- temp[, -1]
    three.gram.df <- merge(three.gram.df, temp, by.x = "Tri", by.y = "Tri", all = TRUE)
    print(k+m)
    rm(temp)
  }
  out <- paste("merged.three.g", k, ".csv", sep = "")
  counts <- three.gram.df[, 2:ncol(three.gram.df)]
  three.gram.df$counts <- rowSums(counts, na.rm = TRUE)
  three.gram.df <- three.gram.df[, -(2:(ncol(three.gram.df)-1))]
  write.csv(three.gram.df, out)
  rm(three.gram.df, out)
}

k = k+5

key <- paste("three.gram.df", k, ".csv", sep = "")
print(k)
three.gram.df <- read.csv(file = key)
three.gram.df <- three.gram.df[, -1]
for (m in 1:3) {
  name <- paste("three.gram.df", (k+m), ".csv", sep = "")
  temp <- read.csv(name)
  temp <- temp[, -1]
  three.gram.df <- merge(three.gram.df, temp, by.x = "Tri", by.y = "Tri", all = TRUE)
  print(k+m)
  rm(temp)
}

out <- paste("merged.three.g", k, ".csv", sep = "")
counts <- three.gram.df[,2:ncol(three.gram.df)]
three.gram.df$counts <- rowSums(counts, na.rm = TRUE)
three.gram.df <- three.gram.df[,-(2:(ncol(three.gram.df)-1))]
write.csv(three.gram.df, out)
rm(three.gram.df, counts)

# Second round aggregation

for(p in c(1, 11, 21, 31, 41)) {
  m1 <- paste("merged.three.g", p, ".csv", sep = "")
  m6 <- paste("merged.three.g", (p+5), ".csv", sep = "")
  print(m1)
  print(m6)
  m1 <- read.csv(m1)[,-1]
  m6 <- read.csv(m6)[,-1]
  super <- merge(m1, m6, by.x="Tri", by.y="Tri", all=TRUE)
  rm(m1,m6)
  counts <- super[,2:ncol(super)]
  super$counts <- rowSums(counts, na.rm = TRUE)
  super <- super[, -(2:(ncol(super)-1))]
  sup.out <- paste("super.three.g", p, ".csv", sep = "")
  write.csv(super, sup.out)
  rm(super, counts)
}

super.three.51 <- read.csv("merged.three.g51.csv")[,-1]
write.csv(super.three.51, "super.three.51.csv")
rm(super.three.51)

# Third round aggregation       

for(p in c(1, 21, 41)) {
  m1 <- paste("super.three.51", p, ".csv", sep = "")
  m6 <- paste("super.three.51", (p+10), ".csv", sep = "")
  print(m1)
  print(m6)
  m1 <- read.csv(m1)[,-1]
  m6 <- read.csv(m6)[,-1]
  super <- merge(m1, m6, by.x="Tri", by.y="Tri", all=TRUE)
  rm(m1, m6)
  counts <- super[, 2:ncol(super)]
  super$counts <- rowSums(counts, na.rm = TRUE)
  super <- super[, -(2:(ncol(super)-1))]
  sup.out <- paste("super.three.2-", p, ".csv", sep = "")
  write.csv(super, sup.out)
  rm(super, counts)
}

# Final phases to aggregartion process to build a three-gram dataframe
# Phase 1

Al <- read.csv("super.three.2-1.csv")[,-1]
Al2plus <- Al[which(Al$counts>=2),]
Alsingles <- Al[which(Al$counts==1),]
rm(Al)

Be <- read.csv("super.three.2-21.csv")[,-1]
Be2plus <- Be[which(Be$counts>=2),]
Besingles <- Be[which(Be$counts==1),]
rm(Be)

# Codes to create aggregated dataframes 
three.gram.df <- merge(Al2plus, Be2plus, by.x="Tri", by.y="Tri", all=TRUE)
rm(Al2plus, Be2plus)
write.csv(three.gram.df, "three.gram.df.csv")
rm(three.gram.df)

singles <- merge(Alsingles, Besingles, by.x="Tri", by.y="Tri", all=TRUE)
rm(Alsingles, Besingles)
write.csv(singles, "AlBesinglesInterim.csv")

counts <- singles[,2:ncol(singles)]
singles$counts <- rowSums(counts, na.rm = TRUE)
singles <- singles[, -(2:(ncol(singles)-1))]
rm(counts)

singleBlend <- whics(singles$counts>1)
singleAdd3 <- singles[singleBlend,]
write.csv(singleAdd3, "add2three1.cvs")
rm(singleAdd3)singleBlend
singles <- singles[-singblend, ]
rm(singleBlend)

half <- trunc(length(singles$counts)/2)
half2 <- length(singles$counts)-half

singlesFirstHalf <- singles[1:half,]
write.csv(singlesFirstHalf, "firstHalf.csv")
rm(singlesFirstHalf)

singlesSecondHalf <- singles[-(1:half),]
write.csv(singlesSecondHalf, "secondHalf.csv")
rm(singles)

# Phase 2

Ga <- read.csv("super.three.2-41.csv")[,-1]  
Ga2plus <- Ga[which(Ga$counts>=2),]
Gasingles <- Ga[which(Ga$counts==1),] 
rm(Ga)

three.gram.df <- read.csv("three.gram.df.csv")
three.gram.df <- three.gram.df[,-1]
three.gram.df <- merge(three.gram.df, Ga2plus, by.x="Tri", by.y="Tri", all=TRUE)
rm(Ga2plus)

counts <- three.gram.df[,2:ncol(three.gram.df)]
three.gram.df$counts <- rowSums(counts, na.rm = TRUE)
three.gram.df <- three.gram.df[, -(2:(ncol(three.gram.df)-1))]
rm(counts)
write.csv(three.gram.df, "three.gram.df.csv")
rm(three.gram.df)

singles <- merge(singlesSecondHalf, Gasingles, by.x="Tri", by.y="Tri", all=TRUE)
rm(singlesSecondHalf)

counts <- singles[, 2:ncol(singles)]
singles$counts <- rowSums(counts, na.rm = TRUE)
singles <- singles[, -(2:(ncol(singles)-1))]
rm(counts)

singleBlend <- which(singles$counts>1)
singleAdd3 <- singles[singleBlend,]
write.csv(singleAdd3, "add2three2.csv")
rm(singleBlend, singles)

singles <- singles[-singleBlend,]
write.csv(singles, "half.real.singles2.csv")
rm(singleBlend, singles)

firstHalf <- read.csv("firstHalf.csv")
firstHalf <- firstHalf[,-1]
singles <- merge(firstHalf, Gasingles, by.x="Tri", by.y="Tri", all=TRUE)
rm(Gasingles, firstHalf)

counts <- singles[,2:ncol(singles)]
singles$counts <- rowSums(counts, na.rm = TRUE)
singles <- singles[,-(2:(ncol(singles)-1))]
rm(counts)

singleBlend <- which(singles$counts>1)
singleAdd3 <- singles[singleBlend,]
write.csv(singleAdd3, "add2three3.csv")
rm(singleAdd3)

singles <- singles[-singleBlend,]
rm(singleBlend)
write.csv(singles, "half.real.singles1.csv")
rm(singles)

# Codes to bind it all together

three.gram.df <- read.csv("three.gram.df.csv")
three.gram.df <- three.gram.df[,-1]

one <- read.csv("add2three1.csv")
one <- one[-1]
three.gram.df <- rbind(three.gram.df, one)
rm(one)

two <- read.csv("add2three2.csv")
two <- two[-1]
three.gram.df <- rbind(three.gram.df, two)
rm(two)

three <- read.csv("add2three3.csv")
three <- three[-1]
three.gram.df <- rbind(three.gram.df, three)
rm(three)

write.csv(three.gram.df, "three.gram.df.csv")

# Codes to prepare the dataset
three.gram.df$Tri <- as.character(three.gram.df$Tri)
three.gram.df$counts <- as.numeric(three.gram.df$counts)
two.gram <- sub(" ","@@@@",three.gram.df$Tri)
two.gram <- sub(" .*","", two.gram)
three.gram.df$Bi <- sub("@@@@"," ", two.gram)
rm(two.gram)

three.gram.df$Uni <- sub(".* ","", three.gram.df$Bi)
three.gram.df$w3 <- sub(".* ","", three.gram.df$Tri)

# Codes to build frequency table for Good-Turing smoothing

three.freq.t <- data.frame(Tri=table(three.gram.df$counts))
write.csv(three.gram.df[,-1], "three.gram.df.csv")
write.csv(three.freq.t, "three.freq.t.csv")
rm(three.freq.t, three.gram.df)

# Codes to create dataframe of singleton three-grams 

single1 <- read.csv("half.real.singles1.csv")
single1 <- single1[,-1]

single2 <- read.csv("half.real.singles2.csv")
single2 <- single2[,-1]

single1 <- rbind(single1, single2)
write.csv(single1, "single.three.gram.csv")
rm(single1, single2)








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
