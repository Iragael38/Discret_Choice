##########
#   SCRIPT 01_Introduction
##########

#' Half of the students have experienced inferencial test with \texttt{R} ?
#' \\ Our sample : $n=20$, \texttt{R} users :$n_1=8$

 prop.test(8,20,p=.5,alternative="two.sided",
     conf.level=0.95,correct=TRUE)


prop.test(8,20,p=.4,alternative="greater",
           conf.level=0.99,correct=FALSE)


Ruser <- matrix(c(435,147,375,134),nrow=2,byrow=TRUE)
dimnames(Ruser) <- list(c("Female","Male"),c("Yes","No"))
names(dimnames(Ruser)) <- c("Gender","R.User")
Ruser


round(Ruser / sum(Ruser), 3)
rowtot <- apply(Ruser,1,sum)
coltot <- apply(Ruser,2,sum)
sweep(Ruser,1,rowtot,"/")
sweep(Ruser,2,coltot,"/")


Gender <- c("Female","Female","Male","Male")
R.user <- c("Yes","No","Yes","No")
Count <- c(435,147,375,134)
Ruserdf <- data.frame(Gender,R.user,Count)
Ruserdf
rm(Gender, R.user, Count)
names(dimnames(Ruserdf)) <- c("Gender","R user")
tab1 <- tapply(Ruserdf$Count,list(Ruserdf$Gender,
                          Ruserdf$R.user),c);tab1


prop.test(Ruser)


prop.test(tab1)


tab1
R.test <- prop.test(tab1)
names(R.test)
p1 <- 147/(147+435); p2 <- 134/(134+375)
p1;p2


R.test$estimate


#' Odds of "No" over "Yes"

odds <- R.test$estimate/(1-R.test$estimate) 
names(odds) <- c("Odds1: F","Odds2: M")  ; odds


OR <- odds[1]/odds[2]
names(OR) <- c("OR")  ; OR

#' The odds of success (not R-user) are "lower" for exposed individuals (women) than for not exposed (men)


theta <- odds[1]/odds[2]
ASE <- sqrt(sum(1/tab1))
ASE
logtheta.CI <- log(theta) + c(-1,1)*1.96*ASE
logtheta.CI
exp(logtheta.CI)


source(file = "Rscripts\\oddsratio.R")
odds.ratio(tab1)

##########
# Smokers and exercise

library(MASS) ;  library(gplots)

tbl = table(survey$Smoke, survey$Exer) ; tbl

balloonplot(t(tbl), main ="Smokers and Exercise"
        ,xlab ="Exercise", ylab="Smoke"
        , label = FALSE, show.margins = FALSE)

chisq.test(tbl)


fisher.test(tbl)













###################
# Solution to Agresti exercise
#' Fatality results for children under age 18 who were passengers in auto accidents in Florida in 2008 according to whether the child was wearing a seat belt

SeatBelt <- c("No","No","Yes","Yes")
Accident <- c("Fatal","Nonfatal","Fatal","Nonfatal")
Count <- c(54 , 10325, 25 , 51790)
Accdf <- data.frame(SeatBelt,Accident,Count)
Accdf

rm(SeatBelt,Accident,Count)
names(dimnames(Accdf)) <- c("SeatBelt","Accident")
tab1 <- tapply(Accdf$Count,list(Accdf$SeatBelt,
Accdf$Accident),c);tab1


R.test <- prop.test(tab1)
odds <- R.test$estimate/(1-R.test$estimate)
names(odds) <- c("Odds1: F","Odds2: NF") ; odds
OR <- odds[1]/odds[2]
names(OR) <- c("OR") ; OR


theta <- odds[1]/odds[2]
ASE <- sqrt(sum(1/tab1))
# ASE
ASE
logtheta.CI <- log(theta) + c(-1,1)*1.96*ASE
# IC log(theta)
logtheta.CI
# IC(OR)
exp(logtheta.CI)


source(file = "Rscripts\\oddsratio.R")
odds.ratio(tab1)
