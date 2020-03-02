library(rvest)
library(XML)
library(magrittr)
library(tm)
# Review extraction
url <- "https://www.amazon.in/OnePlus-McLaren-Limited-256GB-Storage/product-reviews/B07HGKGDHH/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
McLaren <- NULL
for(i in 1:10){
  murl <- read_html(as.character(paste(url,i,sep = "=")))
  rev <- murl%>%
    html_nodes(".review-text")%>%
    html_text()
  McLaren <- c(McLaren,rev)
}
write.table(McLaren,"McLaren.txt",row.names = F)

oneplus7T <- unique(readLines("McLaren.txt"))

#cleaning https://
df_oneplus7T <- gsub(pattern = "http.*",replacement = "",x=oneplus7T)
df_oneplus7T <- gsub("http.","",df_oneplus7T)
df_oneplus7T <- gsub("#.*","",df_oneplus7T)
df_oneplus7T <- gsub("@.*","",df_oneplus7T)

library(textcat)
table(textcat(df_oneplus7T))
df_oneplus7T[which(textcat(df_oneplus7T)=="norwegian")]
consider <- c(which(textcat(df_oneplus7T)!="norwegian"))
df_oneplus7T <- df_oneplus7T[consider]

#stopwords
stop <- readLines(file.choose())
McLaren_corp <- Corpus(VectorSource(df_oneplus7T))
McLaren_corp <- tm_map(McLaren_corp,removePunctuation)
McLaren_corp <- tm_map(McLaren_corp,removeWords,stop)
McLaren_corp <- tm_map(McLaren_corp,stripWhitespace)

#TDM
McLaren_tdm <- TermDocumentMatrix(McLaren_corp)

#convert tdm to dtm
McLaren_dtm <- t(McLaren_tdm)
rowtotals <- apply(McLaren_dtm,1,sum)
McLaren_dtm2 <- McLaren_dtm[rowtotals>3,]
McLaren_dtm2$dimnames$Terms

#LDA
library(topicmodels)
McLaren_LDA <- LDA(x=McLaren_dtm2,10)
McLaren_LDA_terms <- terms(McLaren_LDA,5)
McLaren_LDA_terms
topics <- terms(McLaren_LDA)
table <- table(names(topics),unlist(topics))
library(cluster)
library(dendextend)
cluster <- hclust(dist(table),method = "ward.D2")
colr <- color_branches(cluster,k=4)
plot(colr)

#NLP
library(syuzhet)
McLarens <- get_sentences(df_oneplus7T)
class(McLarens)

#sentiment analysis
sentiments <- c("syuzhet","afinn","bing","nrc","custom")
A <- NULL
sentimentlist <- NULL
for(i in sentiments[1:4]){
  sentimentlist[[i]] <- get_sentiment(McLarens,method = i)
  A[[i]] <- table(get_sentiment(McLarens,method = i))
}
A
sentimentlist

#plot for NRC
plot(sentimentlist$nrc,type = "l",main = "NRC plot")
abline(h=0,col ="red")
abline(h=1,col ="blue")
abline(h=2,col="yellow")

# sentences with negative emotion values
negative <- sentimentlist$nrc[which.min(sentimentlist$nrc)]
McLarens[which(sentimentlist$nrc==negative)]

#sentences with positive emotions values
positive <- sentimentlist$nrc[which.max(sentimentlist$nrc)]
McLarens[which(sentimentlist$nrc==positive)]

#emotion plot
nrc_data <- get_nrc_sentiment(McLarens)
barplot(sort(colSums(prop.table(nrc_data[,1:8]))),cex.names = 0.8,col = 1:8)

#word cloud
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

freq <- rowSums(as.matrix(McLaren_tdm))
length(freq)
order <- order(freq,decreasing = TRUE)
head(freq)
data_frame <- data.frame(word=names(freq),freq=freq)
data_frame <- data_frame[-8,]
windows()
wordcloud(words = data_frame$word,freq = data_frame$freq,min.freq = 2,max.words = 200,random.order = F,colors = brewer.pal(10,"Dark2"))
findFreqTerms(McLaren_dtm,lowfreq = 5)
findAssocs(McLaren_dtm,terms = "fast",corlimit = 0.3)
head(data_frame,10)
barplot(data_frame[(1:11),]$freq,names.arg = data_frame[(1:11),]$word,col = "navyblue")