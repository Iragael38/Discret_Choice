# GPA ex from Greene and : https://stats.idre.ucla.edu/r/dae/logit-regression/
# A researcher is interested in how variables, such as GRE (Graduate Record Exam scores), GPA (grade point average) and prestige of the undergraduate institution, effect admission into graduate school. The response variable, admit/don't admit, is a binary variable


#install.packages("aod")

library(aod)
# Import/download Data from ucla website
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)

# Save data on your computer & re-import
setwd("C:\\Users\\ijoly.INRA\\Desktop\\Formation_DCM\\TEX_RNW\\Rscripts")
write.csv(file="C:\\Users\\ijoly.INRA\\Desktop\\Formation_DCM\\TEX_RNW\\Rscripts\\binary.csv", mydata,  row.names = FALSE)
inputData <- read.csv("binary.csv")
###
# Summary Stat
summary(mydata)
sapply(mydata, sd)
xtabs(~admit + rank, data = mydata)
# rank variable as factor (categorial variable)
mydata$rank <- factor(mydata$rank)
# Logit estimation
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)

# CI with s.e. obtained with profiled LogLik
confint(mylogit)
# with est. s.e.
confint.default(mylogit)

# Wald test of overall effect of the rank variable significance
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
# Wald test of equal coef for rank2 and rank3
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

# Odd Ratio
exp(coef(mylogit))
exp(cbind(OR = coef(mylogit), confint(mylogit)))


# PRediction
newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link", se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata3)
library(ggplot2)
ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
       ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
       size = 1)
