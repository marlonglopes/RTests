
emails = read.csv("emails.csv", stringsAsFactors=FALSE)

str(emails)


max(nchar(emails$text))


which.min(nchar(emails$text))

which.max(nchar(emails$text))
emails$text[2651]




library(tm)


#1) Build a new corpus variable called corpus.

corpus = Corpus(VectorSource(emails$text))

#2) Using tm_map, convert the text to lowercase.

corpus = tm_map(corpus, tolower)

#3) Using tm_map, remove all punctuation from the corpus.

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, PlainTextDocument)

#4) Using tm_map, remove all English stopwords from the corpus.

corpus = tm_map(corpus, removeWords, stopwords("english"))

#5) Using tm_map, stem the words in the corpus.

corpus = tm_map(corpus, stemDocument)


#6) Build a document term matrix from the corpus, called dtm.

dtm = DocumentTermMatrix(corpus)




These steps can be accomplished by running:

corpus = Corpus(VectorSource(emails$text))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)

dtm


spdtm = removeSparseTerms(dtm, 0.95)

spdtm


#emailsSparse = spdtm    make.names

emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))


emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

csEmails = colSums(emailsSparse) 

which.max(csEmails)

sort(colSums(emailsSparse))

which.max(colSums(emailsSparse))

str(emailsSparse)


#add dependent variable
emailsSparse$spam = emails$spam

#How many word stems appear at least 5000 times in the ham emails in the dataset? Hint: in this and the next question, remember not to count the dependent variable we just added.
sort(colSums(subset(emailsSparse, spam == 0)))

#How many word stems appear at least 1000 times in the spam emails in the dataset?

sort(colSums(subset(emailsSparse, spam == 1)))

subset(emailsSparse, spam == 1)

sort(colSums(subset(emailsSparse, spam == 1)))




#to Predict

library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)


emailsSparse$spam = as.factor(emailsSparse$spam)

set.seed(123)

library(caTools)

spl = sample.split(emailsSparse$spam, 0.7)

train = subset(emailsSparse, spl == TRUE)

test = subset(emailsSparse, spl == FALSE)




#These models can be trained with the following code:

spamLog = glm(spam~., data=train, family="binomial")

spamCART = rpart(spam~., data=train, method="class")

spamRF = randomForest(spam~., data=train)





#These probabilities can be obtained with:

predTrainLog = predict(spamLog, type="response")

predTrainCART = predict(spamCART)[,2]

predTrainRF = predict(spamRF, type="prob")[,2]

#To check the number of probabilities with these characteristics, we can use:

table(predTrainLog < 0.00001)

table(predTrainLog > 0.99999)

table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)


#How many variables are labeled as significant (at the p=0.05 level) in the logistic regression summary output?

summary(spamLog)


prp(spamCART)


#What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?

table(train$spam, predTrainLog > 0.5)

The accuracy is (3052+954)/nrow(train).


#What is the training set AUC of spamLog?

predictionTrainLog = prediction(predTrainLog, train$spam)

as.numeric(performance(predictionTrainLog, "auc")@y.values)


#What is the training set accuracy of spamCART

table(train$spam, predTrainCART > 0.5)

predictionTrainCART = prediction(predTrainCART, train$spam)

as.numeric(performance(predictionTrainCART, "auc")@y.values)


#What is the training set accuracy of spamRF

table(train$spam, predTrainRF > 0.5)

And then the accuracy is (3013+914)/nrow(train)

predictionTrainRF = prediction(predTrainRF, train$spam)

as.numeric(performance(predictionTrainRF, "auc")@y.values)



#Obtain predicted probabilities for the testing set for each of the models,


The predicted probabilities can be obtained with:

predTestLog = predict(spamLog, newdata=test, type="response")

predTestCART = predict(spamCART, newdata=test)[,2]

predTestRF = predict(spamRF, newdata=test, type="prob")[,2]



This can be obtained with:

table(test$spam, predTestLog > 0.5)

Then the accuracy is (1257+376)/nrow(test)


predictionTestLog = prediction(predTestLog, test$spam)

as.numeric(performance(predictionTestLog, "auc")@y.values)


table(test$spam, predTestCART > 0.5)

Then the accuracy is (1228+386)/nrow(test)



predictionTestCART = prediction(predTestCART, test$spam)

as.numeric(performance(predictionTestCART, "auc")@y.values)



table(test$spam, predTestRF > 0.5)

Then the accuracy is (1290+385)/nrow(test)


predictionTestRF = prediction(predTestRF, test$spam)

as.numeric(performance(predictionTestRF, "auc")@y.values)





# Build a CART model

library(rpart)
library(rpart.plot)

spamCART = rpart(spam~., data=train, method="class")

prp(smapCART)


# Video 6

# Make predictions on the test set

pred = predict(spamCART, newdata=test)
pred[1:10,]
pred.prob = pred[,2]

# Compute accuracy

table(test$spam, pred.prob >= 0.5)








library(tm)

corpusTitle = Corpus(VectorSource(trials$title))

corpusTitle = tm_map(corpusTitle, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))

corpusAdded = tm_map(corpusAdded, stemDocument)

dtmAdded = DocumentTermMatrix(corpusAdded)

#Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions, and call the new matrix sparseAdded. How many terms appear in sparseAdded?
#Remove 0.997 that are the most ocurrences

sparseAdded = removeSparseTerms(dtmAdded, 0.997)


wordsAdded = as.data.frame(as.matrix(sparseAdded))

colnames(wordsAdded) = paste("A", colnames(wordsAdded))