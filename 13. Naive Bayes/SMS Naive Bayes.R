sms <- read.csv(file.choose())
View(sms)
str(sms)
sms$type <- factor(sms$type)
table(sms$type)

#building a corpus using text mining
library(tm)
library(textcat)
table(textcat(x=sms$type),sms$type)#latvian messages are spam

#creating corpus
sms_corpus <- Corpus(VectorSource(sms$text))

#clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus,tolower)
corpus_clean <- tm_map(corpus_clean,removeNumbers)
corpus_clean <- tm_map(corpus_clean,removeWords,stopwords())
corpus_clean <- tm_map(corpus_clean,removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)


# creating a Document term matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)

#splitting data to test and train
set.seed(101)
split <- sample(1:nrow(sms),nrow(sms)*0.7,F)
tr_sms <- sms[split,]
ts_sms <- sms[-split,]

#splitting th document term matrix
tr_dtm <- sms_dtm[split,]
ts_dtm <- sms_dtm[-split,]

#splitting the corpus
tr_cor <- corpus_clean[split]
ts_cor <- corpus_clean[-split]

round(prop.table(table(tr_sms$type))*100)


library(wordcloud)
windows()
wordcloud(tr_cor,min.freq = 10,max.words = 100,colors = ifelse(sms$type=="spam","red","green"),
          random.order = F)
#green for ham message and red for most used

# reducing the number of columns based on lowest frequency
freq <- findFreqTerms(x=tr_dtm,lowfreq = 10)
tr_dtm= DocumentTermMatrix(tr_cor,list(dictionary = freq))
ts_dtm= DocumentTermMatrix(ts_cor,list(dictionary = freq))

nrow(tr_dtm)
dim(tr_dtm)
nrow(ts_dtm)
dim(ts_dtm)

counts=function(x){
  x= ifelse(x>0,1,0)
  x= factor(x,levels = c(0,1), labels = c("no","yes"))
}

tr_dtm = apply(tr_dtm,MARGIN = 2,counts);table(tr_dtm)
ts_dtm =apply(ts_dtm,MARGIN = 2,counts);table(ts_dtm)


################# naive bayes #############################
library(e1071)
model <- naiveBayes(tr_dtm,tr_sms$type)
pred <- predict(model,ts_dtm)
table(pred,ts_sms$type)
mean(pred==ts_sms$type)                   # acc = 0.9736
library(gmodels)
CrossTable(pred,ts_sms$type,prop.chisq = F,prop.r = F,prop.t = F)

model2 <- naiveBayes(tr_dtm,tr_sms$type,laplace = 1)
pred2 <- predict(model2,ts_dtm)
mean(pred2==ts_sms$type)                 # acc = 0.9730
CrossTable(pred2,ts_sms$type,prop.chisq = F,prop.r = F,prop.t = F)