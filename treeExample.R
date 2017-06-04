library(data.tree)
source("D:\\FourthYear\\SecondTerm\\MachineLearning\\DecisionTree.R")

# library(data.tree)
actualData <- read.csv("D:\\FourthYear\\SecondTerm\\MachineLearning\\MachineLearning\\ML-Assi1\\Problem1 Dataset\\house-votes-84.data.csv")
#actualData <- actualData[actualData.complete.cases(actualData)]
actualData <- na.omit(actualData)
for(i in 2:ncol(actualData[1,])){
  if(!is.na(sum(actualData[,i] == 'y')) && sum(actualData[,i] == 'y')> sum(actualData[,i]=='n')){
    actualData[,i][actualData[,i] == '?'] <- 'y'
  }else{
    actualData[,i][actualData[,i] == '?'] <- 'n'
  }
}
spilt=1/4
indx = -1
train<-actualData[1:(spilt*nrow(actualData)),]
test<-actualData[-(1:(spilt*nrow(actualData))),]
mx<- gain(train,2)
for(i in 3:ncol(train[1,])){
  colGain <- gain(train,i)
  if(mx < colGain){
    mx = colGain
    indx = i
  }
  # print(colGain)
}
x <- colnames(actualData)[indx]
root <- Node$new(x)
vst <- data.frame(matrix(0,ncol = 2, nrow =length(train) ))
tree(root,indx,vst,TRUE,TRUE)
print(root)
#print(indx)
# <- train[train[,indx] == 'y',]
# print(firstChild)
print(mx)

