library(tm)
library(wordcloud)
library(Rgraphviz)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(plyr)
library(ggplot2)
library(RTextTools)
library(e1071)
//Read the table again abnd then generate the word cloud
x <- read.table('5.txt', header=FALSE, sep='\t')
unique(x$V1)
x <- x[which(x$V1 %in% c('feedback','service', 'complaint')),]
nrow(x)

source <- VectorSource(x$V2)
corpus <- Corpus(source)

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))


set.seed(10)
x <- read.table('5.txt', header=FALSE, sep='\t')
x.rand <- x[sample(1:nrow(x)),]
x.rand <- x.rand[which(x.rand$V1 %in% c('feedback','service', 'complaint')),]
source <- VectorSource(x.rand$V2)
corpus <- Corpus(source)


mat <- DocumentTermMatrix(corpus)

mat4 <- weightTfIdf(mat)
mat4 <- as.matrix(mat4)

classifier <- naiveBayes(mat4[1:568,], x.rand$V1[1:568])
predicted <- predict(classifier, mat4[569:710,])
table(as.character(x.rand$V1[569:710]), as.character(predicted))

recall_accuracy(as.character(x.rand$V1[569:710]), as.character(predicted))

container <- create_container(mat, x.rand$V1, trainSize=1:568,testSize=569:710, virgin=FALSE)
model <- train_model(container, 'TREE',kernel='linear')
results <- classify_model(container, model)
table(as.character(x.rand$V1[569:710]), as.character(results[,"TREE_LABEL"]))

recall_accuracy(x.rand$V1[569:710], results[,"TREE_LABEL"])

container <- create_container(mat, x.rand$V1, trainSize=1:568,testSize=569:710, virgin=FALSE)
model <- train_model(container, 'SVM',kernel='linear')
results <- classify_model(container, model)
table(as.character(x.rand$V1[569:710]), as.character(results[,"SVM_LABEL"]))




