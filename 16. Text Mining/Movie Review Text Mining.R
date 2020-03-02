################################ MOVIE REVIEW ################################################
library(rvest)
library(XML)
library(magrittr)

url <- "https://www.imdb.com/title/tt4154796/reviews?ref_=tt_urv"
IMDB_avengers <- NULL
for(i in 1:10){
  murl <- read_html(as.character(paste(url,i,sep = "=")))
  rev <- murl%>%
    html_nodes(".show-more__control")%>%
    html_text()
  IMDB_avengers <- c(IMDB_avengers,rev)
}
length(IMDB_avengers)
length(unique(IMDB_avengers))
IMDB_avengers[duplicated(IMDB_avengers)]
write.table(unique(IMDB_avengers),"avengers.txt",row.names = F)
Avengers <- readLines("avengers.txt")


#####cleaning unwanted "https://"..........
df_avengers <- gsub(pattern = "http.*",replacement = "",x=Avengers)
df_avengers <- gsub("https.*","",df_avengers)
df_avengers <- gsub('\""','',df_avengers)

library(textcat)
table(textcat(df_avengers))
df_avengers[which(textcat(df_avengers)=="norwegian")]
consider <- c(which(textcat(df_avengers)!="norwegian"))
df_avengers <- df_avengers[consider]
length(df_avengers)


# text mining 
library(tm)
library(NLP)
stopword <- readLines(file.choose())
View(stopword)
aveng_corp <- Corpus(VectorSource(df_avengers))
aveng_corp <- tm_map(aveng_corp,removePunctuation)

### removing stop words
aveng_corp <- tm_map(aveng_corp,removeWords,stopword)
aveng_corp <- tm_map(aveng_corp,removeNumbers)
aveng_corp <- tm_map(aveng_corp,stripWhitespace)

# creating term document matrix
aveng_tdm <- TermDocumentMatrix(aveng_corp)

# converting tdm to dtm
aveng_dtm <- t(aveng_tdm)
rowtotal <- apply(aveng_dtm,1,sum)
aveng_dtm2 <- aveng_dtm[rowtotal>3,]
aveng_dtm2$dimnames$Terms


# LDA
library(topicmodels)
aveng_LDA <- LDA(x=aveng_dtm2,10)
aveng_LDA_terms <- terms(aveng_LDA,5)
aveng_LDA_terms

topic <- terms(aveng_LDA)
table <- table(names(topic),unlist(topic))
head(table)
library(cluster)
library(dendextend)
cluster <- hclust(dist(table),method = "ward.D2")
colr <-color_branches(cluster,k=3) 
plot(colr)

#NLP
library(syuzhet)
aveng <- get_sentences(df_avengers)
class(aveng)

#sentiment analysis
sentiments <- c("syuzhet","afinn","bing","nrc","stanford","custom")
A <- NULL
sentimentlist <- NULL
for(i in sentiments[1:4]){
  sentimentlist[[i]] <- get_sentiment(aveng,method = i)
  A[[i]] <- table(get_sentiment(aveng,method = i))
}
A
sentiments

#plot for NRC

plot(sentimentlist$nrc,type = "l",main = "NRC plot",xlab = "Time",ylab = "Emotion")
abline(h=0,col ="red",lwd=2)
abline(h=1,col ="blue",lwd =1)
abline(h=2,col ="yellow",lwd=1)
abline(h=4,col ="green",lwd=1)

#to extract the sentences with most negative emotions
negative <- sentimentlist$bing[which.min(sentimentlist$bing)]
aveng[which(sentimentlist$bing==negative)]

#to extract the sentences with most positive emotions
positive <- sentimentlist$bing[which.max(sentimentlist$bing)]
aveng[which(sentimentlist$bing==positive)]

#percent based fig
percentvalues <- get_percentage_values(sentimentlist$bing)
plot(percentvalues,type = "l",main = "percentage baced means",xlab = "TIME",ylab = "emotion value",col ="blue")
ft_values <- get_transformed_values(sentimentlist$bing,low_pass_size = 2,x_reverse_len = 100,scale_vals = T,scale_range = F)
plot(ft_values,col =ifelse(ft_values>0,"red","blue"))


#Emotion plot
nrc_data <- get_nrc_sentiment(aveng)
barplot(sort(colSums(prop.table(nrc_data))),horiz = F,cex.names = 0.8,col = 1:8)



## Word cloud
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
freq <- (rowSums(as.matrix(aveng_tdm)))
length(freq)
ord <- order(freq,decreasing = TRUE)
head(freq)


data_frame <- data.frame(word =names(freq),freq= freq)
windows()
wordcloud(words = data_frame$word,freq = data_frame$freq,min.freq = 3,max.words = 150,random.order = F,colors = brewer.pal(10,"Dark2"))
findFreqTerms(aveng_dtm,lowfreq = 5)
findAssocs(aveng_dtm,terms = "avengers",corlimit = 0.2)
head(data_frame,10)

barplot(data_frame[1:10,]$freq,names.arg = data_frame[1:10,]$word,col = "navyblue")