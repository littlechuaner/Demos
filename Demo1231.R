library(ggplot2)
library(klaR)
library(sqldf)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
movies <- read.csv(file.choose(),stringsAsFactors = TRUE)
str(movies)
###
movies$Year <- as.numeric(movies$Year)
movies$Metascore <- as.numeric(movies$Metascore)
movies$Vote <- as.numeric(movies$Vote)
movies$Runtime <- as.numeric(movies$Runtime)
###
goodmovies<-subset(movies,movies$Score>=7)
plainmovies<-subset(movies,movies$Score<7)
goodmovies$Score <- 1
plainmovies$Score <- 0
movies_new <- rbind(goodmovies,plainmovies)
###
oldmovies <- subset(movies_new,movies_new$Year<1980)
middlemovies <- subset(movies_new,movies_new$Year>1980 & movies_new$Year<2000 )
newmovies <- subset(movies_new,movies_new$Year>2000 )
oldmovies$Year <- 1
middlemovies$Year <- 2
newmovies$Year <- 3
movies_new <- rbind(oldmovies,middlemovies,newmovies)
goodmovies<-subset(movies_new,movies_new$Score == 1)
plainmovies<-subset(movies_new,movies_new$Score == 0)
###
a <- colnames(movies_new)
ggplot(goodmovies, aes(goodmovies[,3]) ) + 
  geom_bar(aes(fill = as.factor(goodmovies[,3]))) + 
  scale_fill_discrete(name=a[3]) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Age",y= "Frequency", title = "Good Movies" )
###
ggplot(plainmovies, aes(plainmovies[,3]) ) + 
  geom_bar(aes(fill = as.factor(plainmovies[,3]))) + 
  scale_fill_discrete(name=a[3]) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Age",y= "Frequency", title = "Plain Movies" )
###
directors <- Corpus(VectorSource(goodmovies$Director))
description <- Corpus(VectorSource(goodmovies$Description))
genres <- Corpus(VectorSource(goodmovies$Genre))
###
docs <- description
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(0002)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.5, 
          colors=brewer.pal(8, "Dark2"))
###
docs <- directors
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(0002)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.5, 
          colors=brewer.pal(8, "Dark2"))
###
docs <- genres
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(0002)
wordcloud(words = d$word, freq = d$freq, min.freq = 0,
          max.words=500, random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))
directors <- Corpus(VectorSource(plainmovies$Director))
description <- Corpus(VectorSource(plainmovies$Description))
genres <- Corpus(VectorSource(plainmovies$Genre))
###
docs <- description
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(0003)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=600, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))
###
docs <- directors
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(0001)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.5, 
          colors=brewer.pal(8, "Dark2"))
###
docs <- genres
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(0002)
wordcloud(words = d$word, freq = d$freq, min.freq = 0,
          max.words=500, random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))
###
set.seed(0002)
data <- movies_new[,c(3:5,7,9,10)]
index <- sample(1:2, size = nrow(data), replace = TRUE, prob = c(0.7,0.3))
train_data <- data[index == 1,]
test_data <- data[index == 2,]
model1 <- glm(formula = train_data$Score ~ ., data =train_data, family = 'binomial')
summary(model1)
###
prob <- predict(object = model1, newdata= test_data, type = 'response')
pred <- ifelse(prob >= 0.8, 'yes','no')
pred <- factor(pred, levels =c('no','yes'), order = TRUE)
f <- table(test_data$Score, pred)
f
p <- (f[1,1]+f[2,2])/(f[1,1]+f[2,2]+f[1,2]+f[2,1])
p
###
train_data <- na.omit(train_data)
train_data$Score <- as.factor(train_data$Score)
model2<- NaiveBayes(formula=Score~.,data=train_data)
summary(model2)
###
pre <- predict(model2, newdata = test_data)
pred <- ifelse(pre$posterior[,2] >= 0.8, 'yes','no')
f <- table(test_data$Score, pred)
f
#Accuracy
p <- (f[1,1]+f[2,2])/(f[1,1]+f[2,2]+f[1,2]+f[2,1])
p
###
diamonds
p <- ggplot(diamonds, aes(factor(cut), price))
p + geom_violin()
###
p <- ggplot(diamonds, aes(depth, price))
p + geom_point()
###
library(geomnet)
madmen
library(geomnet)
data(madmen)
ggplot(data = madmen$edges, aes(from_id = Name1, to_id = Name2)) +
  geom_net(colour = "darkred", layout.alg = "circle", labelon = TRUE, 
           size = 15, directed = TRUE, vjust = 0.5, labelcolour = "grey80",
           arrowsize = 0.4, linewidth = 0.5, arrowgap = 0.05,
           selfloops = TRUE, ecolour = "grey40") + 
  theme_net() 
###
ggplot(data = madmen$vertices, aes(from_id = label, to_id = Gender)) +
  geom_net(colour = "darkred", layout.alg = "circle", labelon = TRUE, 
           size = 15, directed = TRUE, vjust = 0.5, labelcolour = "grey80",
           arrowsize = 0.4, linewidth = 0.5, arrowgap = 0.05,
           selfloops = TRUE, ecolour = "grey40") + 
  theme_net() 
###
data <- read.csv(file.choose())
model <- lm(data = data, formula = unlist(data[1])~.)
summary(model)

