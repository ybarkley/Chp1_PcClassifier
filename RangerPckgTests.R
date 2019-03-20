

#select rows one at a time to include as training data
#use remaining as test data

PEL = 1:8                # designated groupID's assigned for each pelagic group 
NWHI  = 14:17   # designated groupID's assigned for each NWHI group, #20 was absorbed into #19 (June 2017)     
MHI = 31:34

grid = expand.grid(c(1:8), c(14:17), c(31:34))
grid = expand.grid(PEL, NWHI, MHI)  #grid of all possible combinations


formPc <- as.formula(paste("population ~ ", paste(names(trainsub)[7:53],collapse="+")))
testsub <- NULL
trainsub <- NULL
results <- NULL
for(combo in 1:nrow(grid)) {
  
  row <- as.numeric(grid[combo,])
  testsub=droplevels(subset(pcdata, pcdata$group==row))  
  trainsub=droplevels(subset(pcdata, pcdata$group!=row))
  # Don't need this
  # testall = rbind(testall, testsub)
  # trainall = rbind(trainall, trainsub)
  
  
  fit <- randomForest(formPc, data=trainsub, ntree=1000, importance=TRUE, replace=F, mtry=5, norm.votes=F, confusion=T, keep.inbag=T, proximity=T, keep.forest=T, scale = F)
  
  prediction <-predict(fit, newdata=testsub, predict.all = F, type='response') 
  testsub$rightPred <- as.character(prediction) == as.character(testsub$population)
  
  results.temp <- table(testsub$population, prediction)  # conf matrix of individual whistle counts
  results = results + results.temp 
  
}



require(rfUtilities)
require(randomForest)
set.seed(1234)	
data(iris)
iris$Species <- as.factor(iris$Species) 
( rf.mdl <- randomForest(iris[,1:4], iris[,"Species"], ntree=501) )
( rf.perm <- rf.significance(rf.mdl, iris[,1:4], nperm=99, ntree=501) ) 


############

require(ranger)
require(climbeR)

#Train
rangerfit <- ranger(population ~ ., data=train[c(1,7:31)], num.trees=3000, importance='permutation', replace=F, mtry=7, oob.error = T, classification = T)

RFfit <- randomForest(population ~., data=train[c(1,7:31)], ntree=3000, importance=TRUE, replace=F, mtry=7, norm.votes=F, confusion=T, keep.inbag=F, proximity=T, keep.forest=T, scale = F) 

#Prediction
rangerpred <- predict(rangerfit, test)
table(test$population, rangerpred$predictions)

RFpred <- predict(RFfit, test, predict.all = T, type='response')
table(test$population, RFpred$aggregate)


#Using climbeR functions
ranger.result <- getAndPlotMetric(rangerfit)
ranger.evaldata <- ranger.result$subtree_metrics
ranger.plot2vs1 <- ranger.result$so_vs_fo_plot
ranger.plotsplits <- ranger.result$ns_vs_fo_plot



###############
myiris <- cbind(iris[1:4], matrix(runif(96 * nrow(iris)), nrow(iris), 96)) 
result <- rfcv(myiris, iris$Species, cv.fold=3) 
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))
