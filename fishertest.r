Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
              dimnames = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
                              satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS")))

fisher.test(Job) # 0.7827
fisher.test(Job, simulate.p.value = TRUE, B = 1e5) # also close to 0.78


TeaTasting <-
  matrix(c(3, 1, 1, 3),
         nrow = 2,
         dimnames = list(Guess = c("Milk", "Tea"),
                         Truth = c("Milk", "Tea")))
fisher.test(TeaTasting, alternative = "greater")
## => p = 0.2429, association could not be established

MP6 <- rbind(
  c(1,2,2,1,1,0,1),
  c(2,0,0,2,3,0,0),
  c(0,1,1,1,2,7,3),
  c(1,1,2,0,0,0,1),
  c(0,1,1,1,1,0,0))

#Convert table to matrix
library(MASS)
confmatrixTEST <- rbind(
  c(6081, 4300, 4619),
  c(4537, 5364, 5099),
  c(3854, 3405, 7741)
)

confmatrixTEST <- rbind(
  c(.658,	.341),
  c(.294,	.706))

confmatrixTEST <- rbind(
  c(988,	512),
  c(441,	1059))


chisq.test(confmatrixTEST)

attributes(Conf.Mat.pm)$class <- "matrix"
class(Conf.Mat.pm)
results.encB <- results.enc[-c(1,2), -c(1,2)]

fisher.test(confmatrixTEST, alternative='two.sided')#, simulate.p.value = TRUE)


#Try the G-test for larger values
#http://www.biostathandbook.com/gtestind.html#chivsg
#G-values are additive, which means they can be used for more elaborate statistical designs. G–tests are a subclass of likelihood ratio tests, a general category of tests that have many uses for testing the fit of data to mathematical models
library(DescTools)

observed = c(6081, 5364, 7741)    # observed frequencies
expected = c(0.33, 0.33, 0.33)      # expected proportions

GTest(x=observed,
      p=expected,
      correct="none")


expected.count = sum(observed)*expected
G = 2 * sum(observed * log(observed / expected.count))





