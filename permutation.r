# NOT RUN {
# A regression model using the ozone example
data(airquality)
ozone.rfP <- rfPermute(Ozone ~ ., data = airquality, ntree = 100, na.action = na.omit, nrep = 50, num.cores=1)

# Plot the null distributions and observed values.
layout(matrix(1:6, nrow = 2))
plotNull(ozone.rfP) 
layout(matrix(1))
#}

#test out permutation using sample
pcdata_perm <- pcdata.pm[c(501:550, 1001:1050),]
> df2 <- df1[sample(nrow(df1)),]
df2 <- pcdata_perm[sample(pcdata_perm[, 1]),]
