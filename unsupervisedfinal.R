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

x <- read.table('5.txt', header=FALSE, sep='\t')
unique(x$V1)
x <- x[which(x$V1 %in% c('complaint','service', 'feedback')),]
nrow(x)

source <- VectorSource(x$V2)
corpus <- Corpus(source)

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))


mat <- DocumentTermMatrix(corpus)
mat4 <- weightTfIdf(mat)
mat4 <- as.matrix(mat4)


norm_eucl <- function(m)
  m/apply(m,1,function(x) sum(x^2)^.5)
mat_norm <- norm_eucl(mat4)


set.seed(5)
k <- 3
kmeansResult <- kmeans(mat_norm, k)


kmeansResult$cluster[1:5]

count(kmeansResult$cluster)

result <- data.frame('actual'=x$V1, 'predicted'=kmeansResult$cluster)
result <- result[order(result[,1]),]

result$counter <- 1
result.agg <- aggregate(counter~actual+predicted, data=result, FUN='sum')

result.agg

ggplot(data=result.agg, aes(x=actual, y=predicted, size=counter)) + geom_point()
