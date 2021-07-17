library(twitteR)
library(tm)
library(wordcloud)
library(syuzhet)
library(ggplot2)
library(dplyr)

#Setup twitter oauth using access token
consumerKey <- "iticSogt5Kux9AqXFWh6d3pFI"
consumerSecret <- "iEXSEadVhIxDSxzSFGiwAaJeiAYfmNfQTmgKu8dDeZqDCwbpcQ"
accessToken <- 	"4923758112-7jJIzLRnYGJzUqfTKTca3WurySlevy77tGAm0Yo"
accessSecret <- "cCH1sQr68r5mUyItaKK5pffKXhEGqig1MxyM8VSAqHnmP"
setup_twitter_oauth(consumerKey, consumerSecret, accessToken,accessSecret)

#Loading Tweets on “Lockdown” contain 1000 tweets
searchnew<-searchTwitter ("Lockdown", n=1000,lang="en") 
searchnew

search_text_vec=sapply(searchnew, function(x) x$getText()) 
search_text_vec

#Data Cleaning
search_tm=Corpus(VectorSource(search_text_vec))
inspect(search_tm)

#Remove Punctuations
search_tm_level1=tm_map(search_tm,removePunctuation)

#Remove Numbers
search_tm_clean=tm_map(search_tm_level1,removeNumbers)

#Remove Blank Spaces
search_tm_clean=tm_map(search_tm_clean,stripWhitespace)

#Remove Stopwords
search_tm_clean=tm_map(search_tm_clean,removeWords,stopwords("english"))

#remove emoticons
remove1<-function(x) gsub("[^\x01-\x7F]", "", search_tm_clean)
search_tm_clean=tm_map(search_tm_clean,content_transformer(remove1))

#convert Tweets to Lower Case
search_tm_clean=tm_map(search_tm_clean,content_transformer(tolower))

#Remove URL
removeURL<-function(x) gsub('http[[:alnum:]]*', '',x)
search_tm_clean=tm_map(search_tm_clean,content_transformer(removeURL))

tdm<-TermDocumentMatrix(search_tm_clean)

tdm
tdm<-as.matrix((tdm))

#Word Cloud
w<-sort(rowSums(tdm),decreasing=TRUE)
set.seed(222)
wordcloud(words=names(w),freq=w,
          random.order = F,min.freq = 3,
          colors=brewer.pal(8,'Dark2'),
          scale=c(5,0.3))

#Obtain Sentiments
s<-get_nrc_sentiment(as.character(search_tm_clean))
head(s)

#Barplot
barplot(colSums(s),las=2,col=rainbow(10),ylab = "count",
        main = "Sentimental analysis")

