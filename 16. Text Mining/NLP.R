install.packages("tm")
library(tm)

install.packages("slam")
library(slam)

install.packages("topicmodels")
library(topicmodels)

x <- readLines("C:/Datasets_BA/TextMining/NLP/modi.txt")
x
length(x)

mydata.corpus <- Corpus(VectorSource(x))

mydata.corpus <- tm_map(mydata.corpus, removePunctuation)

my_stopwords <- c(stopwords('english'),"brothers", "sisters", "the", "due", "are", "not", "for", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then")

mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)

mydata.corpus <- tm_map(mydata.corpus, removeNumbers)

mydata.corpus <- tm_map(mydata.corpus, stripWhitespace)

## build a term-document matrix
mydata.dtm3 <- TermDocumentMatrix(mydata.corpus)
mydata.dtm3

dim(mydata.dtm3)

# dtm <- as.DocumentTermMatrix(mydata.dtm3)
# dtm <- DocumentTermMatrix(mydata.corpus)
dtm <- t(mydata.dtm3)

rowTotals <- apply(dtm, 1, sum)
?apply

dtm.new   <- dtm[rowTotals > 0, ]
dim(dtm.new)

lda <- LDA(dtm.new, 10) # find 10 topics
?LDA

term <- terms(lda, 20) # first 5 terms of every topic
term

tops <- terms(lda)
?terms
tb <- table(names(tops), unlist(tops))
tb <- as.data.frame.matrix(tb)
?unlist

cls <- hclust(dist(tb), method = 'ward.D2') #ward is absolute distance
?hclust
par(family = "HiraKakuProN-W3")
plot(cls)
?par
######

####################### Emotion mining ##############################

install.packages("syuzhet")
library("syuzhet")

my_example_text <- readLines("C:/Datasets_BA/TextMining/NLP/lotr.txt")

s_v <- get_sentences(my_example_text)
class(s_v)
str(s_v)
head(s_v)

sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)

afinn_s_v <- get_sentiment(s_v, method = "afinn")
head(afinn_s_v)

nrc_vector <- get_sentiment(s_v, method="nrc")
head(nrc_vector)

sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

# plot
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive

# more depth
poa_v <- my_example_text
poa_sent <- get_sentiment(poa_v, method="bing")
plot(
  poa_sent, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# percentage based figures
percent_vals <- get_percentage_values(poa_sent)

plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values <- get_transformed_values(
  poa_sent, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(s_v)
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')
# subset

sad_items <- which(nrc_data$sadness > 0)
head(s_v[sad_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:10]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)

############
############

####################### Structured data extraction (NER) ##################
install.packages("rJava")
library(rJava)
install.packages("NLP")
library(NLP)
install.packages("openNLP")
library(openNLP)
install.packages("RWeka")
library(RWeka)
install.packages("magrittr")
library(magrittr)

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
library(openNLPmodels.en)

bio <- readLines("C:/Datasets_BA/TextMining/NLP/modi.txt")
bio <- paste(bio, collapse = " ")
bio <- as.String(bio)

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
bio_annotations <- annotate(bio, list(sent_ann, word_ann))

class(bio_annotations)
head(bio_annotations)
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)
sents(bio_doc) %>% head(2)
words(bio_doc) %>% head(10)

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
bio_annotations <- annotate(bio, pipeline)
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)

entities <- function(doc, kind) {
     s <- doc$content
     a <- annotations(doc)[[1]]
     if(hasArg(kind)) {
          k <- sapply(a$features, `[[`, "kind")
          s[a[k == kind]]
     } else {
          s[a[a$type == "entity"]]
     }
}

entities(bio_doc, kind = "person")

entities(bio_doc, kind = "location")

entities(bio_doc, kind = "organization")
