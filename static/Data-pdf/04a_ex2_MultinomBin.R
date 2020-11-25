# Example: Entering high school students make program choices among general program, vocational program and academic program. Their choice might be modeled using their writing score and their social economic status.

# Description of the data
# For our data analysis example, we will expand the third example using the hsbdemo data set. Let's first read in the data.

# ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
# Save data & re-import
setwd("G:\\IRA\\888_ESE UPMF\\2019_C2ES_M2_Econometrie3\\Scripts and Dataframes")
#write.csv(file="C:\\Users\\ijoly.INRA\\Desktop\\Formation_DCM\\TEX_RNW\\Rscripts\\hsbdemo.csv", ml,  row.names = FALSE)
ml <- read.csv("hsbdemo.csv")
###


# The data set contains variables on 200 students. The outcome variable is prog, program type. The predictor variables are social economic status, ses, a three-level categorical variable and writing score, write, a continuous variable. Let's start with getting some descriptive statistics of the variables of interest.

with(ml, table(ses, prog))

with(ml, do.call(rbind, tapply(write, prog, function(x) c(Mean = mean(x), SD = sd(x)))))


summary(ml)
# Multinom
library(nnet)
ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ ses + write, data = ml)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors  ;  z
p <- (1 - pnorm(abs(z), 0, 1)) * 2  ;  p

########## Same with Mlogit
library(mlogit)
table(ml$prog2)
# Dataframe Tranformation for Mlogit
Dat <- mlogit.data(ml, choice = "prog2", shape = "wide", alt.levels = c("academic","general","vocation ") ,id.var = "id")
# Multinomial Logit with individual characteristic (no alt. attributes)                     
mlog <- mlogit( data=Dat, prog2 ~ 1 | ses + write)
summary(mlog)
# Coefficients comparison
coefficients(test)
coefficients(mlog)
##########
# Risk ratio
exp(coef(test))
# Predicted proba for sample
head(pp <- fitted(test))


########"
# PRediction
dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(test, newdata = dses, "probs")

dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), write = rep(c(30:70),  3))

## store the predicted probabilities for each value of ses and write
pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
by(pp.write[, 3:5], pp.write$ses, colMeans)

## melt data set to long for ggplot2
library(data.table);library(ggplot2)
lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
head(lpp)  # view first few rows


## plot predicted probabilities across write values for each level of ses
## facetted by program type
ggplot(lpp, aes(x = write, y = probability, colour = ses)) + geom_line() + facet_grid(variable ~ ., scales = "free")




mlog <- mlogit( data=Dat, prog2 ~ 1 | female+ ses + write +schtyp + math + read + science + honors + awards)
summary(mlog)

cor(ml[,6:10])
