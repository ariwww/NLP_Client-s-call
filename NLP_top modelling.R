library(data.table)
library(readxl)
library(dplyr)
#install.packages("tidytext")
library(tidytext)
library(stringr)

library(tidyr)
#install.packages("readxl")
library(readxl)
#install.packages("quanteda")
library(quanteda)
#install.packages("stm")
library(stm)
library(ggplot2)
#install.packages("textmineR")
library(textmineR)
#install.packages("tm")
library(tm)
#install.packages("readr")
library(readr)
#install.packages("sqldf")
library(sqldf)
sapply(data, class)

dt1 <- read_csv(file.choose()) #data is Data_Cleaned_Topics.csv

dt2 <- dt1[,-c(36:56)]
dt <- subset(dt2, dt2$WorkOrderStatus_Desc == "CLOSED")

str(dt)
Door <- subset(dt, dt$ServiceType_Name =="DOORS & WINDOWS")
new_Door_Windows <- filter(Door,Func_Burdened_Cost < 700)
summary(new_Door_Windows)

###### PREPROCESSING  ######
#Doors Windows
str(new_Door_Windows) #19787 observations

Doors_NLP_bgis <-  select(new_Door_Windows,Description_Document)#19787 observations

Doors_new_corpus <- Corpus(VectorSource(new_Door_Windows$Description_Document))
str(Doors_new_corpus)

install.packages("RWeka")
library(RWeka)
# Removing Punctuation
Doors_new <- tm_map(Doors_new_corpus,removePunctuation)
#removing numbers
Doors_new <- tm_map(Doors_new,removeNumbers)
# Removing Stop words
Doors_new <- tm_map(Doors_new, removeWords, stopwords(kind = "english")) #English
Doors_new <- tm_map(Doors_new, removeWords, stopwords(kind = "SMART")) #English
Doors_new <- tm_map(Doors_new, removeWords, stopwords(kind = "french")) #English
Doors_new <- tm_map(Doors_new, tolower) #
Doors_new <- tm_map(Doors_new, stripWhitespace) #
Doors_new <- tm_map(Doors_new, stemDocument) #
#remove some unintersting words
words_to_remove <- c("pleas","the","client","name", "told","over","more","other","have","last","with","this","that","such","when","been","says","will","also","where","why","would","today")
Doors_new <- tm_map(Doors_new,removeWords,words_to_remove)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2)) #ngram

str(Doors_new)
#Term Frequency weighting
Doors_dtm <- DocumentTermMatrix(Doors_new, control=list(tokenize=BigramTokenizer, minDocFreq=2, minWordLength=2))
Doors_dtm

#Frequency 
findFreqTerms(Doors_dtm, lowfreq = 10)

par(mar=c(5,6,4,2),mgp=c(4,1,0))
par(mar=c(7,8,6,5))
dev.off()
barplot(d[1:10,]$freq, las = 1, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
word.freq <- sort(colSums(as.matrix(Doors_dtm)), decreasing = T)
word.freq
#install.packages("Rgraphviz")
#library(Rgraphviz)
#install.packages("graph")
#library(graph)
#plot(Doors_dtm, term = freq.terms, corThreshold = 0.12, weighting = T)

#library(wordcloud)
wordcloud(Doors_new,colors=rainbow(10),max.words=200)
set.seed(12333)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# Remove sparse terms similar to dim()
sparse = removeSparseTerms(Doors_dtm, 0.95)
dim(sparse)
#class(sparse)
#str(sparse)
#Remove the empty rows/ descriptions
new <- which(rowSums(as.matrix(sparse))<1)
sparse <- sparse [-new,] ##since removing some rows here,
dim(sparse)
#Clustering
#distMatrix <- dist(scale(sparse.1))
#fit <- hclust(distMatrix, method = "ward.D")
#plot(fit)
#rect.hclust(fit, k = 30)


## Run LDA
#install.packages("topicmodels")
library(topicmodels)
Doors.lda<-LDA(sparse,control = list(alpha = 0.1), 45)
str(Doors.lda)
lda_inf.1 <- posterior(Doors.lda, Doors_dtm) # get the top weights - list


#install.packages("doParallel")
#library(doParallel)
#library(ggplot2)
#library(scales)
#perplexity(Doors.lda, new_Door_Windows)
#perplexity(lda,dataset,)
#print  topics output
output<- terms(Doors.lda, 10 ,0.001)

#convert lda to a data frame
topics.Dr<-data.frame(lda_inf.1['topics'], stringsAsFactors = FALSE)
str(topics.Dr) #12505 observations

#new_Door_Windows1 <-new_Door_Windows[-new,]
dim(new_Door_Windows)#keep the same length
#str(new_Door_Windows)
#combine the topics with the original dataset
Doors.full = cbind(new_Door_Windows,topics.Dr)
str(Doors.full)
write.csv(Doors.topic, file = "Doors.topic1.K45.csv", row.names=FALSE)

######################################################################################################
####ELECTRICAL

#dt$ServiceType_Name <- ifelse(dt$ServiceType_Name == "ELECTRICAL REPAIRS", "ELECTRICAL",dt$ServiceType_Name)
summary(dt$ServiceType_Name)
str(dt)

ELECRTICAL <- subset(dt, dt$ServiceType_Name =="ELECTRICAL_REPAIRS") 
dim(ELECRTICAL) #13647 obs
new_ELECRTICAL <- filter(ELECRTICAL,Func_Burdened_Cost < 2000)
new_ELECRTICAL <- filter(new_ELECRTICAL,Func_Burdened_Cost > 50 )
dim(new_ELECRTICAL)  #13375

###### PREPROCESSING  ######


ELECRTICALs_NLP_bgis <-  select(new_ELECRTICAL,Description_Document)#19787 observations
ELECRTICALs_new_corpus <- Corpus(VectorSource(new_ELECRTICAL$Description_Document))

#install.packages("RWeka")
library(RWeka)
# Removing Punctuation
ELECRTICALs_new <- tm_map(ELECRTICALs_new_corpus,removePunctuation)
#removing numbers
ELECRTICALs_new <- tm_map(ELECRTICALs_new,removeNumbers)
# Removing Stop words
ELECRTICALs_new <- tm_map(ELECRTICALs_new, removeWords, stopwords(kind = "english")) #English
ELECRTICALs_new <- tm_map(ELECRTICALs_new, removeWords, stopwords(kind = "SMART")) #English
ELECRTICALs_new <- tm_map(ELECRTICALs_new, removeWords, stopwords(kind = "french")) #English
ELECRTICALs_new <- tm_map(ELECRTICALs_new, tolower) #
ELECRTICALs_new <- tm_map(ELECRTICALs_new, stripWhitespace) #
ELECRTICALs_new <- tm_map(ELECRTICALs_new, stemDocument) #
words_to_remove <- c("pleas","the","client","name", "told","over","more","other","have","last","with","this","that","such","when","been","says","will","also","where","why","would","today")
ELECRTICALs_new <- tm_map(ELECRTICALs_new,removeWords,words_to_remove)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2)) #ngram

str(ELECRTICALs_new)
#Term Frequency weighting
ELECRTICALs_dtm <- DocumentTermMatrix(ELECRTICALs_new, control=list(tokenize=BigramTokenizer, minDocFreq=2, minWordLength=2))
dim(ELECRTICALs_dtm)

#Frequency 
findFreqTerms(ELECRTICALs_dtm)
word.freq <- sort(colSums(as.matrix(ELECRTICALs_dtm)), decreasing = T)
word.freq
#install.packages("Rgraphviz")
#library(Rgraphviz)
#install.packages("graph")
#library(graph)
#plot(ELECRTICALs_dtm, term = freq.terms, corThreshold = 0.12, weighting = T)

library(wordcloud)
wordcloud(ELECRTICALs_new,colors=rainbow(10),max.words=200)

# Remove sparse terms similar to dim()
sparse = removeSparseTerms(ELECRTICALs_dtm, 0.95) #13375 obs
dim(sparse)

#Remove the empty rows/ descriptions
new <- which(rowSums(as.matrix(sparse))<1)
str(new)
sparse.1 <- sparse [-new,] ##since removing some rows here,
dim(sparse) #13375 obs


#Clustering
#distMatrix <- dist(scale(sparse.1))
#fit <- hclust(distMatrix, method = "ward.D")
#plot(fit)
#rect.hclust(fit, k = 30)


## Run LDA
#install.packages("topicmodels")
library(topicmodels)
ELECRTICALs.lda<-LDA(sparse.1,control = list(alpha = 0.1), 35)
lda_inf.E <- posterior(ELECRTICALs.lda, ELECRTICALs_dtm) # get the top weights - list


#install.packages("doParallel")
#library(doParallel)
#library(ggplot2)
#library(scales)
#perplexity(ELECRTICALs.lda, new_ELECRTICAL_Windows)
#perplexity(lda,dataset,)
#print  topics output
output<- terms(ELECRTICALs.lda, 10 ,0.001)

#convert lda to a data frame
topics.Dr<-data.frame(lda_inf.E['topics'], stringsAsFactors = FALSE)
dim(topics.Dr) #13375 observations

#new_ELECRTICAL_Windows1 <-new_ELECRTICAL_Windows[-new,]
#dim(new_ELECRTICAL)#keep the same length

#combine the topics with the original dataset
ELECRTICALs.full= cbind(new_ELECRTICAL, topics.Dr)
str(ELECRTICALs.full)
write.csv(ELECRTICALs.full, file = "ELECRTICAL.topic2.K35.csv", row.names=FALSE)


######################################################################################################
####HVAC MAINTENANCE


MAINTENANCE <- subset(dt, dt$ServiceType_Name =="HVAC_MAINTENANCE") 
dim(MAINTENANCE) #28,184 obs
new_MAINTENANCE <- filter(MAINTENANCE,Func_Burdened_Cost < 20000)
new_MAINTENANCE <- filter(new_MAINTENANCE,Func_Burdened_Cost > 50 )
dim(new_MAINTENANCE)  #27,774

###### PREPROCESSING  ######


MAINTENANCE_NLP_bgis <-  select(new_MAINTENANCE,Description_Document)#19787 observations
MAINTENANCE_new_corpus <- Corpus(VectorSource(MAINTENANCE_NLP_bgis$Description_Document))


###### PREPROCESSING  ######
#MAINTENANCE Windows
dim(new_MAINTENANCE) #27,774 observations
#install.packages("RWeka")
library(RWeka)
# Removing Punctuation
MAINTENANCE_new <- tm_map(MAINTENANCE_new_corpus,removePunctuation)
#removing numbers
MAINTENANCE_new <- tm_map(MAINTENANCE_new,removeNumbers)
# Removing Stop words
MAINTENANCE_new <- tm_map(MAINTENANCE_new, removeWords, stopwords(kind = "english")) #English
MAINTENANCE_new <- tm_map(MAINTENANCE_new, removeWords, stopwords(kind = "SMART")) #English
MAINTENANCE_new <- tm_map(MAINTENANCE_new, removeWords, stopwords(kind = "french")) #English
MAINTENANCE_new <- tm_map(MAINTENANCE_new, tolower) #
MAINTENANCE_new <- tm_map(MAINTENANCE_new, stripWhitespace) #
MAINTENANCE_new <- tm_map(MAINTENANCE_new, stemDocument) #
words_to_remove <- c("pleas","the","client","name", "told","over","more","other","have","last","with","this","that","such","when","been","says","will","also","where","why","would","today")
MAINTENANCE_new <- tm_map(MAINTENANCE_new,removeWords,words_to_remove)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2)) #ngram
print()
str(MAINTENANCE_new)
#Term Frequency weighting
MAINTENANCE_dtm <- DocumentTermMatrix(MAINTENANCE_new, control=list(tokenize=BigramTokenizer, minDocFreq=2, minWordLength=2))
print(MAINTENANCE_dtm)

#IDF Inverse Document Frequency is not for LDA#
dtmm <-DocumentTermMatrix(MAINTENANCE_new,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))) 
dtmm

#Frequency 
findFreqTerms(MAINTENANCE_dtm)
word.freq <- sort(colSums(as.matrix(MAINTENANCE_dtm)), decreasing = T)
word.freq
#install.packages("Rgraphviz")
#library(Rgraphviz)
#install.packages("graph")
#library(graph)
#plot(MAINTENANCE_dtm, term = freq.terms, corThreshold = 0.12, weighting = T)

library(wordcloud)
wordcloud(MAINTENANCE_new,colors=rainbow(10),max.words=200)
# Remove sparse terms similar to dim()
sparse = removeSparseTerms(MAINTENANCE_dtm, 0.95) #13265 obs
dim(sparse) #27774
#Remove the empty rows/ descriptions
new <- which(rowSums(as.matrix(sparse))<1)
dim(new)
sparse <- sparse [-new,] ##since removing some rows here,
dim(sparse) #26213 obs


#Clustering
#distMatrix <- dist(scale(sparse.1))
#fit <- hclust(distMatrix, method = "ward.D")
#plot(fit)
#rect.hclust(fit, k = 30)


## Run LDA
#install.packages("topicmodels")
library(topicmodels)
MAINTENANCE.lda<-LDA(sparse,control = list(alpha = 0.1), 35)
lda_inf.E <- posterior(MAINTENANCE.lda, MAINTENANCE_dtm) # get the top weights - list


#install.packages("doParallel")
#library(doParallel)
#library(ggplot2)
#library(scales)
#perplexity(MAINTENANCE.lda, new_ELECRTICAL_Windows)
#perplexity(lda,dataset,)
#print  topics output
output<- terms(MAINTENANCE.lda, 10 ,0.001)

#convert lda to a data frame
topics.Dr<-data.frame(lda_inf.E['topics'], stringsAsFactors = FALSE)
dim(topics.Dr) #27774 observations

#combine the topics with the original dataset
MAINTENANCE.full= cbind(new_MAINTENANCE, topics.Dr)
str(MAINTENANCE.full)
write.csv(MAINTENANCE.full, file = "MAINTENANCE.topic1.K35.csv", row.names=FALSE)


######################################################################################################
#####################################HVAC REPAIR

summary(dt$ServiceType_Name)
str(dt)

REPAIRS <- subset(dt, dt$ServiceType_Name =="HVAC_REPAIRS") 
dim(REPAIRS) #25001 obs
new_REPAIRS <- filter(REPAIRS,Func_Burdened_Cost < 1500)
new_REPAIRS <- filter(new_REPAIRS,Func_Burdened_Cost > 50 )
dim(new_REPAIRS)  #23090

###### PREPROCESSING  ######


REPAIRS_NLP_bgis <-  select(new_REPAIRS,Description_Document)#19787 observations
REPAIRS_new_corpus <- Corpus(VectorSource(new_REPAIRS$Description_Document))

#install.packages("RWeka")
library(RWeka)
# Removing Punctuation
REPAIRS_new <- tm_map(REPAIRS_new_corpus,removePunctuation)
#removing numbers
REPAIRS_new <- tm_map(REPAIRS_new,removeNumbers)
# Removing Stop words
REPAIRS_new <- tm_map(REPAIRS_new, removeWords, stopwords(kind = "english")) #English
REPAIRS_new <- tm_map(REPAIRS_new, removeWords, stopwords(kind = "SMART")) #English
REPAIRS_new <- tm_map(REPAIRS_new, removeWords, stopwords(kind = "french")) #English
REPAIRS_new <- tm_map(REPAIRS_new, tolower) #
REPAIRS_new <- tm_map(REPAIRS_new, stripWhitespace) #
REPAIRS_new <- tm_map(REPAIRS_new, stemDocument) #
words_to_remove <- c("pleas","the","client","manag","approv","thank","could","request","the","name", "told","over","more","other","have","last","with","this","that","such","when","been","says","will","also","where","why","would","today")
REPAIRS_new <- tm_map(REPAIRS_new,removeWords,words_to_remove)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2)) #ngram

str(REPAIRS_new)
#Term Frequency weighting
REPAIRS_dtm <- DocumentTermMatrix(REPAIRS_new, control=list(tokenize=BigramTokenizer, minDocFreq=2, minWordLength=2))
dim(REPAIRS_dtm) #23090


#Frequency 
findFreqTerms(REPAIRS_dtm)
word.freq <- sort(colSums(as.matrix(REPAIRS_dtm)), decreasing = T)
class(word.freq)
top.words <- data.frame(word.freq)
write.csv(top.words, file = "top.words.csv", row.names=FALSE)

#install.packages("Rgraphviz")
#library(Rgraphviz)
#install.packages("graph")
#library(graph)
#plot(REPAIRS_dtm, term = freq.terms, corThreshold = 0.12, weighting = T)

library(wordcloud)
wordcloud(REPAIRS_new,colors=rainbow(10),max.words=200)

# Remove sparse terms similar to dim()
sparse = removeSparseTerms(REPAIRS_dtm, 0.95) #13375 obs
dim(sparse)

#Remove the empty rows/ descriptions
new <- which(rowSums(as.matrix(sparse))<1)
str(new)
sparse.1 <- sparse [-new,] ##since removing some rows here,
dim(sparse.1) #22648 obs


#Clustering
#distMatrix <- dist(scale(sparse.1))
#fit <- hclust(distMatrix, method = "ward.D")
#plot(fit)
#rect.hclust(fit, k = 30)


## Run LDA
#install.packages("topicmodels")
library(topicmodels)
REPAIRS.lda<-LDA(sparse.1,control = list(alpha = 0.1), 35)
lda_inf.E <- posterior(REPAIRS.lda, REPAIRS_dtm) # get the top weights - list


#install.packages("doParallel")
#library(doParallel)
#library(ggplot2)
#library(scales)
#perplexity(REPAIRS.lda, new_ELECRTICAL_Windows)
#perplexity(lda,dataset,)
#print  topics output
output<- terms(REPAIRS.lda, 10 ,0.001)
output

#convert lda to a data frame
topics.Dr<-data.frame(lda_inf.E['topics'], stringsAsFactors = FALSE)
dim(topics.Dr) #23090 observations

#new_ELECRTICAL_Windows1 <-new_ELECRTICAL_Windows[-new,]
#dim(new_ELECRTICAL)#keep the same length

#combine the topics with the original dataset
REPAIRS.full= cbind(new_REPAIRS, topics.Dr)
str(REPAIRS.full)
write.csv(REPAIRS.full, file = "REPAIRS.topic2.K35.csv", row.names=FALSE)


######################################################################################################
####INTERIOR_LIGHTING_REPAIRS

#dt$ServiceType_Name <- ifelse(dt$ServiceType_Name == "ELECTRICAL REPAIRS", "ELECTRICAL",dt$ServiceType_Name)
summary(dt$ServiceType_Name)
str(dt)

INTERIOR <- subset(dt, dt$ServiceType_Name =="INTERIOR_LIGHTING_REPAIRS") 
dim(INTERIOR) #22062 obs
new_INTERIOR <- filter(INTERIOR,Func_Burdened_Cost < 6000)
new_INTERIOR <- filter(new_INTERIOR,Func_Burdened_Cost > 50 )
dim(new_INTERIOR) #20455

new_INTERIOR <- new_INTERIOR[-new,]

###### PREPROCESSING  ######
#ELECRTICALs_NLP_bgis <-  select(new_ELECRTICAL,Description_Document)#19787 observations
INTERIOR_new_corpus <- Corpus(VectorSource(new_INTERIOR$Description_Document))

#install.packages("RWeka")
library(RWeka)
# Removing Punctuation
INTERIOR_new <- tm_map(INTERIOR_new_corpus,removePunctuation)
#removing numbers
INTERIOR_new <- tm_map(INTERIOR_new,removeNumbers)
# Removing Stop words
INTERIOR_new <- tm_map(INTERIOR_new, removeWords, stopwords(kind = "english")) #English
INTERIOR_new <- tm_map(INTERIOR_new, removeWords, stopwords(kind = "SMART")) #English
INTERIOR_new <- tm_map(INTERIOR_new, removeWords, stopwords(kind = "french")) #English
INTERIOR_new <- tm_map(INTERIOR_new, tolower) #
INTERIOR_new <- tm_map(INTERIOR_new, stripWhitespace) #
INTERIOR_new <- tm_map(INTERIOR_new, stemDocument) #
words_to_remove <- c("pleas","the","client","name", "told","over","more","other","have","last","with","this","that","such","when","been","says","will","also","where","why","would","today")
INTERIOR_new <- tm_map(INTERIOR_new,removeWords,words_to_remove)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2)) #ngram

dim(INTERIOR_new)
#Term Frequency weighting
INTERIOR_dtm <- DocumentTermMatrix(INTERIOR_new, control=list(tokenize=BigramTokenizer, minDocFreq=2, minWordLength=2))
dim(INTERIOR_dtm) #20455

#Frequency 
findFreqTerms(INTERIOR_dtm)
word.freq <- sort(colSums(as.matrix(INTERIOR_dtm)), decreasing = T)
word.freq
#install.packages("Rgraphviz")
#library(Rgraphviz)
#install.packages("graph")
#library(graph)
#plot(INTERIOR_dtm, term = freq.terms, corThreshold = 0.12, weighting = T)
dev.off()
library(wordcloud)
wordcloud(INTERIOR_new,colors=rainbow(10),max.words=200)


# Remove sparse terms similar to dim()
sparse = removeSparseTerms(INTERIOR_dtm, 0.95) #20455 obs
dim(sparse)

#Remove the empty rows/ descriptions
new <- which(rowSums(as.matrix(sparse))<1)
str(new)
sparse.1 <- sparse [-new,] ##since removing some rows here,
dim(sparse.1) #20220 obs


#Clustering
#distMatrix <- dist(scale(sparse.1))
#fit <- hclust(distMatrix, method = "ward.D")
#plot(fit)
#rect.hclust(fit, k = 30)


## Run LDA
#install.packages("topicmodels")
library(topicmodels)
INTERIOR.lda<-LDA(sparse.1,control = list(alpha = 0.1), 35)
lda_inf.E <- posterior(INTERIOR.lda) # get the top weights - list


#install.packages("doParallel")
#library(doParallel)
#library(ggplot2)
#library(scales)
#perplexity(INTERIOR.lda, new_ELECRTICAL_Windows)
#perplexity(lda,dataset,)
#print  topics output
output<- terms(INTERIOR.lda, 10 ,0.001)

#convert lda to a data frame
topics.Dr<-data.frame(lda_inf.E['topics'], stringsAsFactors = FALSE)
dim(topics.Dr) #20220 observations

#new_ELECRTICAL_Windows1 <-new_ELECRTICAL_Windows[-new,]
#dim(new_ELECRTICAL)#keep the same length

#combine the topics with the original dataset
INTERIOR.full= cbind(new_INTERIOR, topics.Dr)
dim(INTERIOR.full)
write.csv(INTERIOR.full, file = "INTERIOR.topic1.K35.csv", row.names=FALSE)


######################################################################################################
####PLUMBING MAINTENANCE

#dt$ServiceType_Name <- ifelse(dt$ServiceType_Name == "ELECTRICAL REPAIRS", "ELECTRICAL",dt$ServiceType_Name)
summary(dt$ServiceType_Name)
str(dt)

PLUMBING <- subset(dt, dt$ServiceType_Name =="PLUMBING_MAINTENANCE") 
dim(PLUMBING) #18265 obs
new_PLUMBING <- filter(PLUMBING,Func_Burdened_Cost < 7000)
new_PLUMBING <- filter(new_PLUMBING,Func_Burdened_Cost > 50 )
dim(new_PLUMBING) #17662

new_PLUMBING <- new_PLUMBING[-new,]

###### PREPROCESSING  ######
#PLUMBING_NLP_bgis <-  select(new_ELECRTICAL,Description_Document)#19787 observations
PLUMBING_new_corpus <- Corpus(VectorSource(new_PLUMBING$Description_Document))

#install.packages("RWeka")
library(RWeka)
# Removing Punctuation
PLUMBING_new <- tm_map(PLUMBING_new_corpus,removePunctuation)
#removing numbers
PLUMBING_new <- tm_map(PLUMBING_new,removeNumbers)
# Removing Stop words
PLUMBING_new <- tm_map(PLUMBING_new, removeWords, stopwords(kind = "english")) #English
PLUMBING_new <- tm_map(PLUMBING_new, removeWords, stopwords(kind = "SMART")) #English
PLUMBING_new <- tm_map(PLUMBING_new, removeWords, stopwords(kind = "french")) #English
PLUMBING_new <- tm_map(PLUMBING_new, tolower) #
PLUMBING_new <- tm_map(PLUMBING_new, stripWhitespace) #
PLUMBING_new <- tm_map(PLUMBING_new, stemDocument) #
words_to_remove <- c("pleas","the","client","name", "told","over","more","other","have","last","with","this","that","such","when","been","says","will","also","where","why","would","today")
PLUMBING_new <- tm_map(PLUMBING_new,removeWords,words_to_remove)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2)) #ngram

dim(PLUMBING_new)
#Term Frequency weighting
PLUMBING_dtm <- DocumentTermMatrix(PLUMBING_new, control=list(tokenize=BigramTokenizer, minDocFreq=2, minWordLength=2))
dim(PLUMBING_dtm) #17662

#Frequency 
findFreqTerms(PLUMBING_dtm)
word.freq <- sort(colSums(as.matrix(PLUMBING_dtm)), decreasing = T)
word.freq
#install.packages("Rgraphviz")
#library(Rgraphviz)
#install.packages("graph")
#library(graph)
#plot(PLUMBING_dtm, term = freq.terms, corThreshold = 0.12, weighting = T)

library(wordcloud)
wordcloud(PLUMBING_new,colors=rainbow(10),max.words=200)

# Remove sparse terms similar to dim()
sparse = removeSparseTerms(PLUMBING_dtm, 0.95) #20455 obs
dim(sparse)

#Remove the empty rows/ descriptions
new <- which(rowSums(as.matrix(sparse))<1)
str(new)
sparse.1 <- sparse [-new,] ##since removing some rows here,
dim(sparse.1) #17051 obs


#Clustering
#distMatrix <- dist(scale(sparse.1))
#fit <- hclust(distMatrix, method = "ward.D")
#plot(fit)
#rect.hclust(fit, k = 30)


## Run LDA
#install.packages("topicmodels")
library(topicmodels)
PLUMBING.lda<-LDA(sparse.1,control = list(alpha = 0.1), 35)
lda_inf.E <- posterior(PLUMBING.lda) # get the top weights - list


#install.packages("doParallel")
#library(doParallel)
#library(ggplot2)
#library(scales)
#perplexity(PLUMBING.lda, new_ELECRTICAL_Windows)
#perplexity(lda,dataset,)
#print  topics output
output<- terms(PLUMBING.lda, 10 ,0.001)

#convert lda to a data frame
topics.Dr<-data.frame(lda_inf.E['topics'], stringsAsFactors = FALSE)
dim(topics.Dr) #17051 observations

#new_ELECRTICAL_Windows1 <-new_ELECRTICAL_Windows[-new,]
#dim(new_ELECRTICAL)#keep the same length

#combine the topics with the original dataset
PLUMBING.full= cbind(new_PLUMBING, topics.Dr)
str(PLUMBING.full)
write.csv(PLUMBING.full, file = "PLUMBING.topic1.K35.csv", row.names=FALSE)

#top words for each topics - 35
library(tidytext)
ap_topics <- tidy(REPAIRS.lda, matrix = "beta")
ap_topics
library(ggplot2)
library(dplyr)
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#dtm with fre words
dtm <- TermDocumentMatrix(REPAIRS_new)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#word clouds
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#Show most frequent terms and their frequencies in a bar plot.
dtmm <-DocumentTermMatrix(REPAIRS_new,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))) 

high.freq=tail(sort(d$freq),n=10)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 

ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")


library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))


library(ggplot2)

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()