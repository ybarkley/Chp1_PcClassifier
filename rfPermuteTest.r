#20180723

#Interested in the importance of groups of variables, eg slopes, frequencies, number of steps, etc.


group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(importance(rf.obj, 2)[g, ]*var.share(rf.obj, g))
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}


library(rfPermute)

data("symb.metab")
rf <- randomForest(formPc, pcdata.pm, proximity = TRUE, oob.prox=T)
exptdErrRate(rf)
classConfInt(rf)
plotVotes(rf)
proximityPlot(rf)

library(caret)
rfFuncs$fit <- function(x,y,first,last,...) { library(randomForest); randomForest(x,y, importance = first, ntree=if(first) 10000 else 1000) } 
pcdata.pm <- read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles/ROCCA Towed Data/BigRoccaFile_PM_TowedAll_EDIT_20180717.csv'

mypm <- cbind(pcdata.pm[7:32], matrix(runif(96 * nrow(iris)), nrow(iris), 96))
results <- replicate(5, rfcv(mypm[,c(7:32)], pcdata.pm$population, simplify = F)
error.cv <- sapply(results, "[[", "error.cv")
matplot(results[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type="l",
        lwd=c(2, rep(1, ncol(error.cv))), col=1, lty=1, log="x",
        xlab="Number of variables", ylab="CV Error")


myiris <- cbind(iris[1:4], matrix(runif(96 * nrow(iris)), nrow(iris), 96))
result <- replicate(5, rfcv(myiris, iris$Species), simplify=FALSE)
error.cv <- sapply(result, "[[", "error.cv")
matplot(result[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type="l",
        lwd=c(2, rep(1, ncol(error.cv))), col=1, lty=1, log="x",
        xlab="Number of variables", ylab="CV Error")
