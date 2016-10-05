trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)

From summary(nchar(trials$abstract)) or max(nchar(trials$abstract)), we can read the maximum length.


table(nchar(trials$abstract) == 0) or sum(nchar(trials$abstract) == 0), we can find the number of missing abstracts.





Below we provide the code for corpusTitle; only minor modifications are needed to build corpusAbstract.

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

These can be read from str(dtmTitle) and str(dtmAbstract). Other than str(), the dim() or ncol() functions could have been used. If you used fileEncoding="latin1" when reading in the datafile, you'll have a few extra terms in dtmAbstract, but you should get the answer correct.

We can compute the column sums and then identify the most common one with:

csAbstract = colSums(dtmAbstract)

which.max(csAbstract)



colnames(dtmTitle) = paste0("T", colnames(dtmTitle))

colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))


dtm = cbind(dtmTitle, dtmAbstract)



set.seed(144)

spl = sample.split(dtm$trial, 0.7)

train = subset(dtm, spl == TRUE)

test = subset(dtm, spl == FALSE)


trialCART = rpart(trial~., data=train, method="class")

prp(trialCART)


predTrain = predict(trialCart)[,2]

summary(predTrain)


We can compare the predictions with threshold 0.5 to the true results in the training set with:

table(train$trial, predTrain >= 0.5)

From this, we read the following confusion matrix (rows are true outcome, columns are predicted outcomes):

FALSE TRUE

0 631 99

1 131 441

We conclude that the model has training set accuracy (631+441)/(631+441+99+131), sensitivity 441/(441+131) and specificity 631/(631+99).



The testing set predictions can be obtained and compared to the true outcomes with:

predTest = predict(trialCART, newdata=test)[,2]

table(test$trial, predTest >= 0.5)

This yields the following confusion matrix:

FALSE TRUE

0 261 52

1 83 162

From this, we read that the testing set accuracy is (261+162)/(261+162+83+52).

The AUC can be determined using the following code:

library(ROCR)

pred = prediction(predTest, test$trial)

as.numeric(performance(pred, "auc")@y.values)



