#### EXERCISE from Paul Allison : logistic regression using SAS, 2nd Ed.
#
DF <- read.csv(file = "G:/IRA/888_ESE UPMF/2019_C2ES_M2_Econometrie3/Scripts and Dataframes/02_Penalty.txt", sep = " ", dec = '.', header = T)

# 147 death penalty, in NewJersey state
# defendant was convicter of 1rst degree murder
# a penalty trial was conducted to determine whether the defendant would # receive a sentence of death or life imprisonment
# DEATH : 1 for death sentence / 0 for a life sentence
# BLACKD: 1 if the defendant was black, otherwise 0
# WHITVIC: 1 if the victim was white, otherwise 0
# SERIOUS: a rating of the seriousness of the crime, as evaluated by
# a panel of judges (average rankings between 1-15)
# CULP: 5 denotes high culpability and 1 denotes low culpability, based 
# on aggravating and mitigating circumstances)
# SERIOUS2: a 5 points rating scale of the seriousness of the crime



# Objectives: model death
summary(DF)
library(gmodels)
CrossTable(DF$death, DF$blackd) 
mytable <- xtabs(~death+blackd+whitvic, data=DF)
ftable(mytable) # print table
summary(mytable) # chi-square test of indepedence 


#1rst model with blackd + whitvic 
glm1 <- glm(death ~ blackd + whitvic + serious, data=DF, x = TRUE, 
    family = binomial(link = "logit"))
summary(glm1)

# 2nd model with factor: culp / reflevel:5
DF$culp <- factor(DF$culp)
DF$culp = relevel(DF$culp, ref=5)

glm2 <- glm(death ~ blackd + whitvic + culp, data=DF, x = TRUE, 
            family = binomial(link = "logit"))
summary(glm2)

# Marginal effect: aggregated version
# dp/dx = beta p(1-p)
addmargins(table(DF$death,DF$blackd))
p <- sum(DF$death==1)/length(DF$death)
Mef <- p*(1-p) * coefficients(glm1)[-1]
round( Mef,3)
# marginal effects at mean
# Logit # xb*:
betas<-t(data.frame(coef(glm1))) ; betas
xmean <- c(1, mean(DF$blackd), mean(DF$whitvic), mean(DF$serious))
print("XBetas:")
xb_logit <- sum(xmean*betas) ; xb_logit
# Slopes (at mean): Lambda(mean(xb))*(b)
print("Slopes:")
logit_slopes <- dlogis(xb_logit)*betas
round( logit_slopes,3)

# 3rd model: with multiplicative variables

glm3 <- glm(death ~ blackd * whitvic + culp, data=DF, x = TRUE, 
            family = binomial(link = "logit"))
summary(glm3)



# Convergence problem: extrem s.e. !
# see for discussion : 
#  https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression

glm4 <- glm(death ~ culp + serious, data=DF[DF$blackd==0,], x = TRUE, family = binomial(link = "logit"))
summary(glm4)
## Solution: bayesian estimation
library(arm)
fit <- bayesglm(death ~ culp + serious, data=DF, family="binomial", prior.df=5)

display(fit)

# normal prior
fit.2 <- bayesglm(death ~ culp + serious, data=DF, family="binomial", prior.scale=2.5, prior.df=Inf)  # normal prior with scale 2.5
display(fit.2)

# Multicolinearity Diag
library(DescTools)
# Values of vif up to 5 are usually interpreted as uncritical, 
# values above 5 denote a considerable multicollinearity.
VIF(glm(death ~ blackd + whitvic + culp, data=DF, x = TRUE, 
        family = binomial(link = "logit")))

# Goodness of fit
summary(glm3)
# Deviance: D = 2 sum_j (O_j)log(O_j/E_j)
# O_j: observed freq
# E_j: expected freq in cell j

library(ResourceSelection)
hl <- hoslem.test(glm3$y, fitted(glm3), g=5) # choose: g>p+1
hl

cdplot(factor(death)~blackd, data=DF) 
