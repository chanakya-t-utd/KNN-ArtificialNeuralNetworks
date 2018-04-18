
setwd("C:/Users/meera.kulkarni.BEST/Desktop/UTD/Machine_Learning/Project2")

# Loading data

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
#-----------------------------------------------------------------------------------------------------------------------------------
#Basic Neural Network Implementation
install.packages("neuralnet")
library(neuralnet)

trainNN = scaled[index , ]
testNN = scaled[-index , ]

set.seed(2)
NN = neuralnet(Occupancy ~ Temperature + Humidity + Light + CO2, trainNN,
             hidden = 3 , linear.output = T, act.fct = 'tanh') #Change act.fct to vary differentiating function, hidden to layers
plot(NN)

predict_testNN = compute(NN, testNN[,c(1:4)])
predict_testNN = (predict_testNN$net.result * (max(data$Occupancy) - min(data$Occupancy))) 
                   + min(data$Occupancy)
for(i in 1:nrow(predict_testNN)){
 if(predict_testNN[i]<0.5){
     predict_testNN[i]<-0
   }
   else
     predict_testNN[i]<-1
 }
CM<-table(predict_testNN,testNN$Occupancy) #Confusion Matrix

#-------------------------------------------------------------------------------------------------------------------------------------
#Testing impact of varying training set size
set.seed(50)
k = 100


df3<-data.frame()
df4<-data.frame()
trainlength<-data.frame()
df1<-data.frame()
acclist = NULL
i=1

for(j in seq(1000,nrow(data),1000)){
  for (i in 1:k) {
    index = sample(1:nrow(data),10) #varying proportion of train data
    
    trainNN = data[index,]
    testNN = data[-index,]
    datatest = data[-index,]
    
    NN = neuralnet(Occupancy ~ Temperature + Humidity + Light + CO2, trainNN, 
                   hidden = 3 , linear.output = T, act.fct = 'logistic')
    predict_testNN = compute(NN, testNN[,c(1:4)])
    
    
    predict_testNN = (predict_testNN$net.result * (max(data$Occupancy) - min(data$Occupancy))) 
    + min(data$Occupancy)
    
    dt<-table(predict_testNN,testNN$Occupancy)
    accuracy<-sum(diag(dt))/sum(dt)
    accuracy<-as.numeric(accuracy*100)
    acclist[i]<-accuracy
  }
  df1<-median(acclist)
  trainlength<-j
  df3<-cbind(df1,trainlength)
  df4<-rbind(df4,df3)
}

plot(df4$df1~df4$trainlength,type = "l", xlab ="length of training set", 
     ylab = "median Accuracy", main = "Variation of Accuracy with length of training set" )

#-------------------------------------------------------------------------------------------------------------------------------------
#Testing impact of varying hidden layers
set.seed(50)
k = 1

trainNN = scaled[index , ]
testNN = scaled[-index , ]

df3<-data.frame()
df4<-data.frame()
layers<-data.frame()
df1<-data.frame()
acclist = NULL
i=1

for(j in seq(7,8,2)){
  for (i in 1:k) {

    NN = neuralnet(Occupancy ~ Temperature + Humidity + Light + CO2, trainNN, 
                   hidden = j , linear.output = T, act.fct = 'logistic')
    predict_testNN = compute(NN, testNN[,c(1:4)])
    
    
    predict_testNN = (predict_testNN$net.result * (max(data$Occupancy) - min(data$Occupancy))) 
    + min(data$Occupancy)
    
    dt<-table(predict_testNN,testNN$Occupancy)
    accuracy<-sum(diag(dt))/sum(dt)
    accuracy<-as.numeric(accuracy*100)
    acclist[i]<-accuracy
  }
  df1<-median(acclist)
  layers<-j
  df3<-cbind(df1,layers)
  df4<-rbind(df4,df3)
}

plot(df4$df1~df4$layers,type = "l", xlab ="number of layers", 
     ylab = "median Accuracy", main = "Variation of Accuracy with number of layers" )


