### LOAD LIBRARIES
library(tm)
library(RWeka)

### GATHERING THE DATA

# Create a new Directory if necessary
if(!file.exists("NLP_data")){
	dir.create("NLP_data")
	}

# Download the NLP Capstone Data Set
fileURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(fileURL, destfile = "./NLP_data/NLP_Dataset.zip", mode = "wb")

# Unzip the File
unzip(zipfile = "./NLP_data/NLP_Dataset.zip", exdir = "./NLP_data")

# Navigate through the Capstone dataset. Notice there are 4 folders for 4 different languages: German, English, Finnish, Russian
# Navigate to the folder that contains the data in the English language
setwd("C:\\Users\\574996\\Desktop\\R_Assignments\\NLP_data\\final\\en_US")

# Open a connection in binary mode (I am using a Windows computer) and read in the "Blogs" file. Afterwards, close the connection.
conBlog <- file("en_US.blogs.txt", "rb")
Blogs <- readLines(conBlog)
close(conBlog)

# Open a connection in binary mode (I am using a Windows computer) and read in the "News" file. Afterwards, close the connection.
conNew <- file("en_US.news.txt", "rb")
News <- readLines(conNew)
close(conNew)

# Open a connection in binary mode (I am using a Windows computer) and read in the "Twitter" file. Afterwards, close the connection.
conTwitter <- file("en_US.twitter.txt", "rb")
Twitter <- readLines(conTwitter)
close(conTwitter)


### SAMPLING AND CLEANING THE DATA

# Given the size of all of the files, we will take a 1% sample from each file

# First, set the seed for reproducibility
set.seed(223334444)

# Sample the files
blogsSample <- sample(Blogs, round(length(Blogs)*0.01,digits = 0))
newsSample <- sample(News, round(length(News)*0.01,digits = 0))
twitterSample <- sample(Twitter, round(length(Twitter)*0.01,digits = 0))

# Combine the files together into one single text file
textFileSample <- paste(blogsSample,newsSample,twitterSample)


### CLEANING DATA

# From the newly created text file, create a corpus
textFileVectorSource <- VectorSource(textFileSample)
corpus <- VCorpus(textFileVectorSource)

# From the newly created corpus: 
# Convert all uppercase letters to lowercase letters, remove punctuation, remove numbers, and strip the whitespace
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)


# Next, we will need to remove all sources of profanity from our corpus

# First, we will gather a list of profanity 
# A list of profanity words can be gathered from the following website: http://www.cs.cmu.edu/~biglou/resources/bad-words.txt
conProfanity <- url("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", "r")
Profanity <- readLines(conProfanity)
close(conProfanity)

# Remove all of the text that contain profanity
corpus <- tm_map(corpus, removeWords, Profanity)

# Also, remove all text that contains stopwords in English
corpus <- tm_map(corpus, removeWords, stopwords("english"))

### N-GRAM MODEL CREATION

## CREATE A UNIGRAM

# Tokenize a Unigram
unigram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

# Create a Term Document Matrix
unigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = unigram))

# Find all words in the Unigram Matrix with a frequency of at least 90
unigramFreqTerms <- findFreqTerms(unigramMatrix, lowfreq = 90)
unigramMatrix <- unigramMatrix[unigramFreqTerms,]

# Convert the unigram matrix variable to an actual matrix
unigramMatrix <- as.matrix(unigramMatrix)

# Determine the frequency of each of the words in the Unigram Matrix
unigramTermFreq <- rowSums(unigramMatrix)

# Create a data frame for the unigram matrix for which there are 2 variables:
# The words themselves ("word") and their frequencies ("count")
unigramTable <- data.frame(word = names(unigramTermFreq), count = unigramTermFreq)

# Order the terms by most frequent to least frequent
unigramTable <- unigramTable[order(-unigramTable$count), ]

## SAVE THE UNIGRAM TO AN R-DATA FILE

save(unigramTable,file="unigramData.RData")


## CREATE A BIGRAM

# Tokenize a Bigram
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

# Create a Term Document Matrix
bigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = bigram))

# Find all word pairs in the Bigram Matrix with a frequency of at least 90
bigramFreqTerms <- findFreqTerms(bigramMatrix, lowfreq = 90)
bigramMatrix <- bigramMatrix[bigramFreqTerms,]

# Convert the bigram matrix variable to an actual matrix
bigramMatrix <- as.matrix(bigramMatrix)

# Determine the frequency of each of the word pairs in the Bigram Matrix
bigramTermFreq <- rowSums(bigramMatrix)

# Create a data frame for the bigram matrix for which there are 2 variables:
# The word pairs themselves ("words") and their frequencies ("count")
bigramTable <- data.frame(words = names(bigramTermFreq), count = bigramTermFreq)

# Order the terms by most frequent to least frequent
bigramTable <- bigramTable[order(-bigramTable$count), ]

## SAVE THE BIGRAM TO AN R-DATA FILE

save(bigramTable,file="bigramData.RData")


## CREATE A TRIGRAM

# Tokenize a Trigram
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

# Create a Term Document Matrix
trigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = trigram))

# Find all word triplets in the Trigram Matrix with a frequency of at least 25
trigramFreqTerms <- findFreqTerms(trigramMatrix, lowfreq = 25)
trigramMatrix <- trigramMatrix[trigramFreqTerms,]

# Convert the trigram matrix variable to an actual matrix
trigramMatrix <- as.matrix(trigramMatrix)

# Determine the frequency of each of the word triplets in the Trigram Matrix
trigramTermFreq <- rowSums(trigramMatrix)

# Create a data frame for the trigram matrix for which there are 2 variables:
# The word triplets themselves ("words") and their frequencies ("count")
trigramTable <- data.frame(words = names(trigramTermFreq), count = trigramTermFreq)

# Order the terms by most frequent to least frequent
trigramTable <- trigramTable[order(-trigramTable$count), ]

## SAVE THE TRIGRAM TO AN R-DATA FILE

save(trigramTable,file="trigramData.RData")


## CREATE A QUADGRAM

# Tokenize a Quadgram
quadgram <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# Create a Term Document Matrix
quadgramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = quadgram))

# Find all word quartets in the Quadgram Matrix with a frequency of at least 10
quadgramFreqTerms <- findFreqTerms(quadgramMatrix, lowfreq = 10)
quadgramMatrix <- quadgramMatrix[quadgramFreqTerms,]

# Convert the quadgram matrix variable to an actual matrix
quadgramMatrix <- as.matrix(quadgramMatrix)

# Determine the frequency of each of the word quartets in the Quadgram Matrix
quadgramTermFreq <- rowSums(quadgramMatrix)

# Create a data frame for the quadgram matrix for which there are 2 variables:
# The word quartets themselves ("words") and their frequencies ("count")
quadgramTable <- data.frame(words = names(quadgramTermFreq), count = quadgramTermFreq)

# Order the terms by most frequent to least frequent
quadgramTable <- quadgramTable[order(-quadgramTable$count), ]

## SAVE THE QUADGRAM TO AN R-DATA FILE

save(quadgramTable,file="quadgramData.RData")


## CREATE A QUNIGRAM

# Tokenize a Qunigram
Qunigram <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

# Create a Term Document Matrix
QunigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = Qunigram))

# Find all word quintets in the Qunigram Matrix with a frequency of at least 5
QunigramFreqTerms <- findFreqTerms(QunigramMatrix, lowfreq = 5)
QunigramMatrix <- QunigramMatrix[QunigramFreqTerms,]

# Convert the Qunigram matrix variable to an actual matrix
QunigramMatrix <- as.matrix(QunigramMatrix)

# Determine the frequency of each of the word quintets in the Qunigram Matrix
QunigramTermFreq <- rowSums(QunigramMatrix)

# Create a data frame for the Qunigram matrix for which there are 2 variables:
# The word quintets themselves ("words") and their frequencies ("count")
QunigramTable <- data.frame(words = names(QunigramTermFreq), count = QunigramTermFreq)

# Order the terms by most frequent to least frequent
QunigramTable <- QunigramTable[order(-QunigramTable$count), ]

## SAVE THE QUNIGRAM TO AN R-DATA FILE

save(QunigramTable,file="QunigramData.RData")


