rawdata <- read.csv(file.choose())
dim(rawdata)
names(rawdata)
summary(rawdata)
str(rawdata)

cdata<- read.csv(file.choose())

nas = sapply(rawdata, function(x) sum(is.na(x)))
nas

missing.percent = (nas*100)/(nrow(rawdata))
missing.percent

# Treatment of Missing Values

library("mice")
treated <- mice(rawdata, maxit = 50, printFlag = F)
cdata <- mice::complete(treated)


#  Conversion of data

cdata$Education <- as.factor(rawdata$Education)
cdata$Personal.Loan <- as.factor(rawdata$Personal.Loan)
cdata$Securities.Account <- as.factor(rawdata$Securities.Account)
cdata$CD.Account <- as.factor(rawdata$CD.Account)
cdata$Online <- as.factor(rawdata$Online)
cdata$CreditCard <- as.factor(rawdata$CreditCard)

cdata$Experience..in.years. <- abs(cdata$Experience..in.years.)


# ReChecking after treatment
summary(cdata)
str(cdata)


write.csv(cdata, "E:\\New folder\\Downloads\\cdata.csv")
library("xlsx")

# EDA
hist(cdata$Age..in.years.)
hist(cdata$Experience..in.years.)
hist(cdata$Income..in.K.month.)
hist(cdata$CCAvg)
hist(cdata$Mortgage)

dev.off()
par(mfrow=c(4,2))
graphics.off()


par(mfrow=c(1,2))
hist(rawdata$CCAvg,xlab = "Credit card avg", main = "Credit card avg")
boxplot(rawdata$CCAvg, horizontal = T, xlab = "Credit card avg", main = "Credit card avg")



uv <- cdata[,c(2,3,4,6,7,9)]
boxplot(uv, horizontal = T)

dev.off()
par(mfrow=c(4,2))
barplot(table(cdata$Family.members), main = "Barplot of No. of family members",
        xlab = "No. of members", ylab = "Count")
barplot(table(cdata$Education), main = "Barplot of Education levels",
        xlab = "Levels", ylab = "Count")
barplot(table(cdata$Personal.Loan), main = "Barplot of Personal LOans",
        xlab = "Levels", ylab = "Count")
barplot(table(cdata$Securities.Account), main = "Barplot of Securities Account",
        xlab = "Levels", ylab = "Count")
barplot(table(cdata$Personal.Loan), main = "Barplot of Certificate of Deposit",
        xlab = "Levels", ylab = "Count")
barplot(table(cdata$Personal.Loan), main = "Barplot of Online Banking Facilities",
        xlab = "Levels", ylab = "Count")
barplot(table(cdata$Personal.Loan), main = "Barplot of Credit cards issued by bank",
        xlab = "Levels", ylab = "Count")

boxplot(cdata$Personal.Loan~ cdata$Age..in.years.+cdata$Experience..in.years., horizontal = TRUE)

library(lattice)
histogram(~cdata$Personal.Loan | cdata$Income..in.K.month.)

# Corrplot

library(corrplot)
data.cor = cor(cdata[,c(2,3,4,5,6)])
corrplot(data.cor, method = "number")

barplot(cdata, height = cdata$Family.members)
table(cdata$Family.members)


# Checking VIF with regression
library(car)
attach(cdata)

model = lm(Personal.Loan ~ Experience..in.years. +
             Income..in.K.month.+ Family.members + CCAvg + 
             Education + Mortgage, data = cdata)

vif(model)


# Hierarchical Clustering 

rawdata.Scaled = scale(rawdata[,c(2,3,4,6,7,9)])

head(rawdata.Scaled)

distMatrix = dist(x=rawdata.Scaled, method = "euclidean") 
print(distMatrix, digits = 3)

cluster <- hclust(distMatrix, method = "average")
plot(cluster, labels = as.character(rawdata[,1]))

rect.hclust(cluster, k=3, border="red")

rawdata$Cluster <- cutree(cluster, k=3)

custProfile = aggregate(rawdata[, -c(1,5)],list(rawdata$Cluster),FUN="mean")
custProfile$Frequency = as.vector(table(rawdata$Cluster))
View(custProfile)

# K means clustering

km <- read.csv(file.choose(), header = T)
scaled.km <- scale(km[, -c(1,2,5)])

wssplot <- function (data, nc = 5, seed = 1234){
        wss <- c()
        for (i in 1:nc){
        set.seed(seed)
        wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type = 'b', xlab = 'NUmber of Clusters',
     ylab = "Within groups sum of squares")}

wssplot(scaled.km, nc = 15)


kmeans.clus <- kmeans(x=scaled.km, centers = 3, nstart = 100)

tmp <- as.data.frame(scaled.km)
km$Cluster <- kmeans.clus$cluster
view(tmp)

aggr = aggregate(km[,-c(1,2,5)],list(km$Cluster), mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(km$Cluster)),
                            aggr[,-1])

View(clus.profile)

library(fpc)
plotcluster(scaled.km, kmeans.clus$cluster)

library(cluster)
clusplot(scaled.km, kmeans.clus$cluster,
         color = T, shade = T, labels = 2, lines = 1)

#  Decision Tree Algorithm

## Data Import
rawdata <- read.csv(file.choose(), header = T)

rawdata$Education <- as.factor(rawdata$Education)
rawdata$Personal.Loan <- as.factor(rawdata$Personal.Loan)
rawdata$Securities.Account <- as.factor(rawdata$Securities.Account)
rawdata$CD.Account <- as.factor(rawdata$CD.Account)
rawdata$Online <- as.factor(rawdata$Online)
rawdata$CreditCard <- as.factor(rawdata$CreditCard)
rawdata$Family.members <- as.factor(rawdata$Family.members)

set.seed(100)

indices= sample(1:nrow(rawdata), 0.7*nrow(rawdata))

train_data = rawdata[indices,]
test_data = rawdata[-indices,]

str(train_data)
str(test_data)


## installing rpart package for CART
## install.packages("rpart")
## install.packages("rpart.plot")


## loading the library
library(rpart)
library(rpart.plot)


## Target Rate 
sum(train_data$Personal.Loan)/3500
sum(test_data$Personal.Loan)/1500

## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=70, minbucket = 25, cp = 0, xval = 5)

## calling the rpart function to build the tree

m1 <- rpart(formula = Personal.Loan ~ ., data = train_data[,-1], method = "class", control = r.ctrl)
m1

#Plot the tree
rpart.plot(m1)

## to find how the tree performs and derive cp value
printcp(m1, digits = 23)
plotcp(m1)


## Pruning Code
ptree<- prune(m1, cp=0.28, "CP")
printcp(ptree)
rpart.plot(ptree)


View(CTDF.dev)

## Scoring syntax & confustion matrix
test_data$predict.class <- predict(m1, test_data, type="class")
library(caret)
confmat.train = table (test_data$predict.class, test_data$Personal.Loan)
confusionMatrix(confmat.train, positive = '1', mode = 'everything')

train_data$predict.score <- predict(m1, train_data)


View(train)
head(train)

test_data$predict.class <- predict(m1, test_data, type="class")
test_data$predict.score <- predict(m1, test_data)

View(test)


test$predict.score



#  Confusion Matrix

library(caret)

confmat.train = table (train_data$predict.class, train_data$Personal.Loan)
confusionMatrix(confmat.train, positive = '1', mode = 'everything')




# Random Forest Algorithm

library(randomForest)
set.seed (123)

indices= sample(1:nrow(rawdata), 0.7*nrow(rawdata))

train_data = rawdata[indices,]
test_data = rawdata[-indices,]


RF_train <- randomForest(as.factor(train_data$Personal.Loan) ~ ., data = train_data[,-c(1,5)], 
                   ntree=500, mtry = 3, nodesize = 10,
                   importance=TRUE)


print(RF_test)

getTree(RF_train, 1, labelVar=TRUE)

# check oob for both train and test

## List the importance of the variables.
impVar <- round(randomForest::importance(RF_train), 2)
impVar[order(impVar[,3], decreasing=TRUE),]


# Checking the required number of trees

plot(RF_train, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")

RF_train$err.rate


RF_train$importance

# Income, education are very improtant for building the trees. Mean dec accuracy is higher




# Tune RF
set.seed(123)
tRF <- tuneRF(x = train_data[,-c(1,2,5,10)], 
              y=as.factor(train_data$Personal.Loan),
              mtryStart = 3, 
              ntreeTry=501, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 50, 
              importance=TRUE
)

# 3 is correct for mtry


# Scoring Matrix and confusion matrix

test_data$predict.class <- predict(tRF, test_data, type="class")
test_data$predict.score <- predict(tRF, test_data, type="prob")
head(train_data)
library(caret)
confmat.test = table (test_data$predict.class, test_data$Personal.Loan)
confusionMatrix(confmat.test, positive = '1', mode = 'everything')


test_data$predict.class <- predict(tRF, test_data, type="class")
test_data$predict.score <- predict(tRF, test_data, type="prob")
head(test)
class(test$predict.score)

#  Confusion Matrix

library(caret)

confmat.train = table (train_data$predict.class, train_data$predict.score)
confusionMatrix(confmat.train, positive = '1', mode = 'everything')


# KS Stat
library(ROCR)
pred <- prediction(test_data$predict.score[,2], test_data$Personal.Loan)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

install.packages("InformationValue")
library(InformationValue)
ks_stat(test_data$Personal.Loan, test_data$predict.class, returnKSTable = T)

ks_plot(test_data$Personal.Loan, test_data$predict.class)

## Area Under Curve
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

## Gini Coefficient
library(ineq)
gini = ineq(test_data$predict.score[,2], type="Gini")
gini
  



