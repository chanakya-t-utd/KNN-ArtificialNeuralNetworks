install.packages("kknn")
# Initialize variables
library(plyr)
library(kknn)

setwd("C:/Users/Sudha Thyagarajan/Desktop/Machine Learning/project 3 submission")
data = read.table("datatraining.txt",header=TRUE,sep=",")

samplesize = 0.7 * nrow(data)
set.seed(10)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

datatrain = data[ index, ]
datatest = data[ -index, ]

data <- data[, sapply(data, is.numeric)]

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))
data<-scaled
set.seed(50)
k = 100
RMSE.NN = NULL

df3<-data.frame()
df4<-data.frame()
trainlength<-data.frame()
df1<-data.frame()
acclist = NULL
i=1
set.seed(007)
for(j in seq(1000,nrow(data)-20,1000)){
  for (i in 1:k) {
    index = sample(1:nrow(data),j)
    
    trainNN = data[index,]
    testNN = data[-index,]
    datatest = data[-index,]
    
    knn <- kknn(formula = formula(Occupancy~.), train = trainNN, test = testNN, k = 8, distance = 1)
    fit<- fitted(knn)
    dt<-table(testNN$Occupancy,fit)
    accuracy<-sum(diag(dt))/sum(dt)
    accuracy<-as.numeric(accuracy*100)
    acclist[i]<-accuracy
  }
  df1<-median(acclist)
  trainlength<-nrow(trainNN)
  df3<-cbind(df1,trainlength)
  df4<-rbind(df4,df3)
}

plot(df4$df1~df4$trainlength,type = "l", xlab ="length of training set", 
     ylab = "median Accuracy", main = "Variation of Accuracy with length of training set" )

##With the above plot we can understand maximum accuracy is obtained with 220 rows of training data set.
df3c<-data.frame()
df4c<-data.frame()
trainlengthc<-data.frame()
df1c<-data.frame()
acclistc = NULL
clusterno = NULL
kc=100
set.seed(001)
##Change in accuracy with change in clusters
for(j in 1:15){
  for (i in 1:kc) {
    index = sample(1:nrow(data),6000)
    trainNNc = data[index,]
    testNNc = data[-index,]
    datatestc = data[-index,]
    knnc <- kknn(formula = formula(Occupancy~.), train = trainNNc, test = testNNc, k = j, distance = 1)
    fitc<- fitted(knnc)
    dtc<-table(testNNc$Occupancy,fitc)
    accuracyc<-sum(diag(dtc))/sum(dtc)
    accuracyc<-as.numeric(accuracyc*100)
    acclistc[i]<-accuracyc
  }
  df1c<-median(acclistc)
  trainlengthc<-nrow(trainNNc)
  clusterno <- j
  df3c<-cbind(df1c,trainlengthc,clusterno)
  df4c<-rbind(df4c,df3c)
}

plot(df4c$df1c~df4c$clusterno,type='l',xlab="No of Clusters",ylab="Median Accuracy from n-fold validation", main = "Accuracy Vs No of clusters")
