library(twitteR)
library(ROAuth)
library(httpuv)
library(base64enc)
# authentication
user <- OAuthFactory$new(consumerKey="_______________________________________________",#fill ur's consumer API key
                         consumerSecret="____________________________________________",#fill ur's consumer API secret key
                         requestURL="https://api.twitter.com/oauth/request_token",
                         accessURL="https://api.twitter.com/oauth/access_token",
                         authURL="https://api.twitter.com/oauth/authorize")
#save(user,file = "twitter authentication user.Rdata")
#getwd()

setup_twitter_oauth("_____________________________________",#consumer API key
                    "_____________________________________",#consumer secret key
                    "_____________________________________",#access token 
                    "____________________________________")#access token secret


# twitter extraction 
tweets_ext <-userTimeline("SrBachchan",n=1000)
tweets_df <- twListToDF(tweets_ext)
write.csv(tweets_df,"SrBachchan.csv")
getwd()


################ Text mining ################################

library(SnowballC)
library(tm)
library(wordcloud)
library(topicmodels)
library(RColorBrewer)

text <- read.csv(file.choose())
View(text)
document <- Corpus(VectorSource(text$text))
inspect(document[10])
#function to clean the corpus
tospace <- function(x,pattern)gsub(pattern,"",x)
document <- tm_map(document,tospace,"/")
document <- tm_map(document,tospace,"@")
document <- tm_map(document,tospace,"\\|")
document <- tm_map(document,tospace,"#")

# converting to lowerspace 
document <- tm_map(document,tolower)
# removing numbers
document <- tm_map(document,removeNumbers)
# removing stopwords
document <- tm_map(document,removeWords,stopwords("english"))

# removing punctuations
document <- tm_map(document,removePunctuation)

# removing whitespace
document <- tm_map(document,stripWhitespace)
inspect(document[[10]])

#document <- tm_map(document,stemDocument)

inspect(document[10])
################# document term matrix #####################
doctm <- TermDocumentMatrix(document)
dim(doctm)

ctdm <- as.DocumentTermMatrix(doctm)
rowtotals <- apply(ctdm,1,sum)
ctdm.new <- ctdm[rowtotals>0,]
lda <- LDA(ctdm.new,10)
terms <- terms(lda,5)
terms

topic <- terms(lda)
tab <- table(names(topic),unlist(topic))
head(tab)
library(cluster)
library(dendextend)
cluster <- hclust(dist(tab),method = "ward.D2")
col_bran <- color_branches(cluster,k=3)
plot(col_bran)

#NLP.......
library(textcat)
table(textcat(document))
consider <- c(which(textcat(document)=="english"))
documen2 <- document[consider]
documen3 <- as.character(documen2)
library(syuzhet)
elon_tweets <- get_sentences(documen3)

#sentimental analysis
sentiments <- c("syuzhet","afinn","bing","nrc","stanford","custom")
a <- NULL
sent_list <- NULL
for(i in sentiments[1:4]){
  sent_list[[i]] <- get_sentiment(documen3,method = i)
  a[[i]] <- table(get_sentiment(documen3,method = i))
}
a
sent_list
#plot bing
plot(sent_list$bing,type = "l",main="plot bing ")
abline(h=0,col="red")
abline(h=1,col="blue")
abline(h=2,col="yellow")
abline(h=3,col="forestgreen")

#plot nrc
plot(sent_list$nrc,type="l",main="plot nrc")
abline(h=0,col="red")
abline(h=1,col="blue")
abline(h=2,col="yellow")
abline(h=3,col="forestgreen")

nrc_data <- get_nrc_sentiment(documen3)

#emotions plot
barplot(sort(colSums(prop.table(nrc_data))),cex.names = 0.8,main="emotion plot",col =1:8)


#wordcloud
freq <- rowSums(as.matrix(doctm))
length(freq)
ord <- order(freq,decreasing = TRUE)
freq[head(ord)]
freq[tail(ord)]

df <- data.frame(word =names(freq),freq = freq)
windows()
wordcloud(words = df$word,freq = df$freq,min.freq = 3,max.words = 100,random.order = F,col = brewer.pal(20,"Dark2"))
findFreqTerms(doctm,lowfreq = 8)
findAssocs(doctm,terms ="uuuu",corlimit =0.3)
head(df,10)

barplot(df[1:10,]$freq,names.arg = df[1:10,]$word,col="forestgreen",main= "most used terms",ylab ="word frequency")



#uuuu is most used term by Amitabh Bachchan (an Indian film actor)
