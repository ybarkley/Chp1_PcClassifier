pcdata.pel <- droplevels(subset(pcdata.pm, population == 'pel'))


pcdata.mhi <- droplevels(subset(pcdata.pm, population == 'mhi'))


means.mhi<-as.data.frame(colMeans(pcdata.mhi[, 7:54]))
means.pel<-as.data.frame(colMeans(pcdata.pel[, 7:54]))
means.pm <- cbind("MHI" = means.mhi, "PEL" = means.pel[,1])

df_mhi = droplevels(subset(pcplot, pcplot$population=='mhi'))
posslope_mhi = mean(df_mhi$FREQPOSSLOPEMEAN)
plot(as.factor(df_mhi$group), df_mhi$FREQPOSSLOPEMEAN, xlab="Encounter ID", ylab="Mean Positive Slope", main="Main Hawaiian Island Population")
plot(as.factor(df_mhi$group), df_mhi$FREQQUARTER3, xlab="Encounter ID", ylab="Mean Positive Slope", main="Main Hawaiian Island Population")
plot(as.factor(df_mhi$group), df_mhi$FREQSWEEPFLATPERCENT, xlab="Encounter ID", ylab="% of the whistle with zero slope ", main="Main Hawaiian Island Population")

plot(pcdata.pel$FREQSPREAD, pcdata.mhi$FREQSPREAD)
plot(pcdata.mhi$DURATION)
poop <- lm(DURATION ~ population, pcdata.mhi)


anova(poop)

#standard deviation of a variable for each encounter?

varsOnly <- pcdata.pm[, c(1,3, 7:54)]



library(data.table)
vars.dt <-data.table(varsOnly)
df3<-setDF(vars.dt[, as.list(unlist(lapply(.SD, function(x) list(mean=mean(x),median=median(x), sd=sd(x), IQR=IQR(x), quantile(x, 0.25, type=2), quantile(x, 0.75, type=2), range=range(x))))),
          by="EncounterID", .SDcols=colnames(varsOnly[,3:48])])
population<- c('mhi','mhi','mhi','mhi','mhi','nwhi','nwhi','nwhi','nwhi','pel','pel','pel','pel','pel','pel','pel','pel')
df3 <- cbind(population, df3)

df.mean_sd <- setDF(vars.dt[, as.list(unlist(lapply(.SD, function(x) list(mean=mean(x), sd=sd(x))))), by="EncounterID", .SDcols=varsOnly[,2:48]])

df.median_IQR <- setDF(vars.dt[, as.list(unlist(lapply(.SD, function(x) list(median=median(x), IQR=IQR(x), quantile(x, 0.25, type=2), quantile(x, 0.75, type=2), range=range(x))))), by="EncounterID", .SDcols=varsOnly[,2:48]])
                          
                                



#plotting histograms for each variable in each encounter
varsOnly_long<-melt(varsOnly, id.vars=1, variable.name = 'Response', value.name = 'value')



plotHistFunc <- function(x, na.rm = TRUE, ...) {
  nm <- names(x)
  for (i in seq_along(nm)) {
    plots <-ggplot(x,aes_string(x = nm[i])) + geom_histogram(alpha = .5,fill = "dodgerblue", bins = 30)
    ggsave(plots,filename=paste("myplot",nm[i],".png",sep=""))
  }
}

p<-ggplot(varsOnly_long,aes(x = value)) + 
  facet_wrap(~Response,scales = "free_x") + 
  geom_histogram()
p<-p+labs(title="Response Variables")
p+theme(axis.text=element_text(size=4), 
        axis.title=element_text(size=8, face='bold'),
        text=element_text(size=8),
        plot.title=element_text(size=12))

scatter<-ggplot(varsOnly_long,aes(x = EncounterID, y = value)) + 
  geom_point()+
  facet_wrap(~Response,scales = "free_x") + 
  labs(title="Response Variables")+
  theme(axis.text=element_text(size=5), 
        axis.title=element_text(size=8, face='bold'),
        text=element_text(size=8),
        plot.title=element_text(size=8))



ggplot(subset(m, variable == "x"), aes(w, value)) + geom_line() 
p <- ggplot(m, aes(w, value)) + geom_line(aes(colour=variable)
d_ply(m, .(variable), function(d) p %+% d, .print=TRUE)

plot(value~EncounterID, varsOnly_long)



# plot(df3$FREQABSSLOPEMEAN.sd, df3$EncounterID, y_axis)
library(ggplot2)
ggplot()

