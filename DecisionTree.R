
entropy<-function(rep, demo){ #entropy for each column 
  # print("entropy function")
  logDemo<-log2(demo)
  logRep<-log2(rep)
  # print(log2(demo))
  # print(is.infinite(log2(rep)))
  if(is.infinite(log2(rep))){
    # print ("enta d5lt ?")
    logRep<-0
  }  
  if(is.infinite(log2(demo)))
  {
    print ("enta d5lt ?")
    logDemo<-0 
  }
  # print(rep)
  # print(log2(rep))
  # print((-rep*log2(rep)))
  # print(demo)
  # print(log2(demo))
  
  return (-rep*logRep-demo*logDemo)
} 

probability <- function (dat,colNum,choise,output){ # prob
    prob<-sum(dat[,colNum] == choise & dat[,1] == output)
    # print (colNum)
    # print(dat[,colNum])
    # print("prob ")
    # print(dat[,colNum] == choise & dat[,1] == output)
    # print("-------------------------------------------------------")
  return(prob)
  
}
gain <- function(dataa , colNum){
  yesRepbProb <- probability(dataa, colNum,'y', 'republican')
  yesDemoProb <- probability(dataa, colNum,'y', 'democrat')
  noRepProb <- probability(dataa, colNum,'n', 'republican')
  noDemoProb <- probability(dataa, colNum,'n', 'democrat')
   # print(" yes Rep prob ")
   # print(yesRepbProb)
   # print(" yes Demo prob ")
   # print(yesDemoProb)
   # print(" no Rep prob ")
   # print(noRepProb)
   # print(" no Demo prob ")
   # print(noDemoProb)
   # print("*******************************************")
  # parent 
  repParent <- yesRepbProb  + noRepProb
  demoParent <- yesDemoProb + noDemoProb
  # entropy of parent
  parentEntropy <- entropy(repParent/(repParent+demoParent), demoParent/(repParent+demoParent))
  print(repParent/(repParent+demoParent))
  print(demoParent/(repParent+demoParent))
  print("yes entropy attributes ")
  
  yesEntropy <- entropy(yesDemoProb/(yesDemoProb+yesRepbProb),yesRepbProb/(yesDemoProb+yesRepbProb))
  print("summation")
  print((yesDemoProb+yesRepbProb))
   print("yesEntropy")
  print(yesEntropy)
  noEntropy <- entropy(noDemoProb/(noDemoProb+noRepProb), noRepProb/(noDemoProb+noRepProb))
  print("summation no entroy")
  print(noDemoProb+noRepProb)
  print("noEntropy")
  print(noEntropy)
  gn <- parentEntropy-((yesDemoProb+yesRepbProb)/(repParent+demoParent))*yesEntropy -((noDemoProb+noRepProb)/(repParent+demoParent))*noEntropy 
  
  return (gn)
}
library(data.tree)
tree <- function(branch, indx, vst,flagYes,flagNo){ 
  vst[indx][1] = TRUE 
  vst[indx][2] = TRUE 
  ########### YES BRANCH ########################
  if(flagYes){
  rootYes<- train[train[,indx] == 'y',]
  yesRepbProb <- probability(rootYes, indx,'y', 'republican')
  yesDemoProb <- probability(rootYes, indx,'y', 'democrat')
  yesChild<- branch$AddChild("yes")
  indx1 = -1
  # parent <- branch$
  for(i in 2:ncol(rootYes)){
    if(i != indx & !vst[i][1] ){
      colGain <- gain(dataa = rootYes, colNum = i)
      if(mx < colGain){
        mx = colGain
        indx1 = i
      }
    }
  }
  
  tmpBranch <- yesChild$AddChild(colnames(actualData)[indx1])
  if(yesRepbProb==0|yesDemoProb==0) flagYes=FALSE
  if(yesRepbProb==0) tmpBranch <- yesChild$AddChild('democrat')
  else if(yesDemoProb==0) tmpBranch <- yesChild$AddChild('republican')
  tree(tmpBranch, indx1,vst,flagYes,flagNo)
  }
 
  
  ################# NO BRANCH ##################
  if(flagNo){
  noChild <- root$AddChild("no")
  rootNo<- train[train[,indx] == 'n',]
  noRepProb <- probability(rootNo, indx,'n', 'republican')
  noDemoProb <- probability(rootNo, indx,'n', 'democrat')
  indx2 = -1
  for(i in 2:ncol(rootNo)){
    if(i != indx & !vst[i][2]){
      colGain <- gain(dataa = rootNo, colNum = i)
      if(mx < colGain){
        mx = colGain
        indx2 = i
      }
    }
  }
  tmpBranch2 <- noChild$AddChild(colnames(actualData)[indx2])
  if(noDemoProb==0|noRepProb==0) flagNo=FALSE
  if(noRepProb==0) tmpBranch <- yesChild$AddChild('democrat')
  else if(noDemoProb==0) tmpBranch <- yesChild$AddChild('republican')
  tree(tmpBranch2, indx2,vst,flagYes,flagNo)
  }
 
}
 # root <- Node$new("yes")
 # root <- Node$new("no")
 # print(root)
