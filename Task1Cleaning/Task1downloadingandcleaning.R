#The purpose of part 1 of the capstone project is to download and explore
#data for the english characters. 

#create a directory
if (!file.exists("data")) {
  dir.create("data")
}

#download the data
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(url,destfile = "Coursera-SwiftKey.zip", mode="wb")
unzip("Coursera-SwiftKey.zip", exdir = "data")

#download the english files from the data
blogs.txt <- "./data/final/en_US/en_US.blogs.txt"
news.txt  <- "./data/final/en_US/en_US.news.txt"
twitter.txt <- "./data/final/en_US/en_US.twitter.txt"  

#record file size
blogsSize   <- file.size(blogs_file) / (1024^2)
newsSize    <- file.size(news_file) / (1024^2)
twitterSize <- file.size(twitter_file) / (1024^2)

#read the data files
blogs   <- readLines(blogs_file)
news    <- readLines(news_file)
twitter <- readLines(twitter_file) 

#number of lines in each file type
blogsLines   <- length(blogs)
newsLines    <- length(news)
twitterLines <- length(twitter)

#number of characters in each file type
blogsChar   <- nchar(blogs)
newsChar    <- nchar(news)
twitterChar <- nchar(twitter)
blogsCharSum   <- sum(blogsChar)
newsCharSum    <- sum(newsChar)
twitterCharSum <- sum(twitterChar)

#number of words in each file 
blogsWords <- str_count(blogs)
newsWords  <- str_count(news)
twitterWords <- str_count(news)
blogsWords <- sum(blogsWords)
newsWords  <- sum(newsWords)
twitterWords <- sum(twitterWords)
#create a summary for all the stats
summary <- data.frame(
  f_names = c("blogs", "news", "twitter"),
  f_size = c(blogsSize, newsSize, twitterSize),
  f_lines = c(blogsLines, newsLines, twitterLines),
  n_char = c(blogsCharSum, newsCharSum, twitterCharSum),
  n_words = c(blogsWords, newsWords,twitterWords)
)

kable(summary)


##sample the data and save
set.seed(091305)
sampleSize = 0.01

blogSample<- sample(blogs, length(blogs) * sampleSize, replace = FALSE)
newsSample<- sample(news, length(news) * sampleSize, replace = FALSE)
twitterSample<- sample(twitter, length(twitter) * sampleSize, replace = FALSE)

#remove any latin characters or ASCII
blogSample <- iconv(blogSample, "latin1", "ASCII", sub = "")
newsSample <- iconv(newsSample, "latin1", "ASCII", sub = "")
twitterSample <- iconv(twitterSample, "latin1", "ASCII", sub = "")
  
#combine data sets into single data set
dataSample <- c(blogSample, newsSample, twitterSample)
datafilename <- "data/final/en_US/en_US.sample.txt"

dataSampleLines <- length(dataSample)
dataSampleWords<- sum(str_count(dataSample))

##clean the sample data. Remove URLs, convert to lowercase, 
##remove punctuation, remove numbers, remove whitespace, remove profanity
##convert to a plain text document

cleanSample <- Corpus(VectorSource(dataSample))

#remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
cleanSample <- tm_map(cleanSample, content_transformer(removeURL))

#remove punctuation, numbers
removeSymbol <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
cleanSample <- tm_map(cleanSample, content_transformer(removeSymbol))

#turn all values to lowercase
cleanSample <- tm_map(cleanSample, content_transformer(tolower))

#remove profanity, first download file for profanity list
##download https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/en as txt file
profanity<- read.table("profanity.txt", header = FALSE, sep = "\n")
cleanSample<-tm_map(cleanSample, removeWords, profanity[,1])

#remove stopwords
cleanSample <- tm_map(cleanSample, removeWords, stopwords("english"))
cleanSample <- tm_map(cleanSample, removeWords, stopwords("SMART"))

#remove whitespace
cleanSample <- tm_map(cleanSample, stripWhitespace)

saveRDS(cleanSample, file = "./data/final/en_US/clean_sample.rds" )

##preliminary data analysis in order to find the most frequent words and tokenize
doctermCorpus <- DocumentTermMatrix(cleanSample)
newDoctermCorpus <- removeSparseTerms(doctermCorpus,sparse = 0.993)
colS <- colSums(as.matrix(newDoctermCorpus))

#count top 10 most frequent words, 
library(data.table)
doctermFeatures <- data.table(name = attributes(colS)$names, count = colS)

#plot the top 10 most frequent words
library(ggplot2)

ggplot(doctermFeatures[count>1150],aes(name, count)) +
  geom_bar(stat = "identity",color='black') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

