# http://people.stern.nyu.edu/wgreene/Microeconometrics.htm
# Here is a description of the data in the clogit dataset : as already noted, this is a
# choice-based survey of the intercity mode choices of 210 Australians, gathered by
# David Hensher. There are 4 modes, in order: air, train, bus, car. Each individual is
# represented by 4 rows of data, one row for each mode. (This is the so-called "long"
# form of the data).
# See variable description at the end of file

library(foreign)
clogit <- read.table("G:\\IRA\\888_ESE UPMF\\2019_C2ES_M2_Econometrie3\\Scripts and Dataframes\\clogit.csv" ,header = T, sep=",")

summary(clogit)
str(clogit)

# indicate the choice set by a vector (1,2,3,4) x 210 times, with alt. names
clogit$mode.ids<-factor(rep(1:4,210),labels=c("air","train","bus","car"))

# MLOGIT PACKAGE
#  choice variable "choice="
# the values of "alt.var="
# panel dataset "id.var="
# if the choice set varies "chid.var="
library(mlogit)
# Data Format for Mlogit
CLOGIT<-mlogit.data(clogit,shape="long",choice="mode", alt.var="mode.ids")
# Estimation 1
res1<-mlogit(MODE~TTME+GC, data=clogit, shape="long",  alt.var="mode.ids")
# Same estimation, in shape=long
res2 <- mlogit(MODE~TTME+GC,data=CLOGIT)
summary(res1);summary(res2)
# With travel time vs waiting time
res12<-mlogit(MODE~INVT+GC, data=clogit, shape="long",  alt.var="mode.ids")
summary(res12)
# Specifications
## Crossed effect of income with mode
res3<-mlogit(MODE~TTME+GC | HINC, data=CLOGIT)
summary(res3)
## 
res4<-mlogit(MODE~TTME | HINC | GC, data=CLOGIT)
summary(res4)


res42<-mlogit(MODE~GC | HINC | INVT, data=CLOGIT)
summary(res42)


## CAteg with ref level
res5<-mlogit(MODE~TTME+GC,data=CLOGIT,reflevel="car",
             R=500,halton=NA,probit=TRUE,print.level=1)
summary(res5)
# We see estimates of the Choleski factor.  
# We deduce estimated covariance
L1 <- matrix(0, 3, 3)
L1[!upper.tri(L1)] <- c(1, coef(res5)[6:10])
## Multiplying L1 by its transpose gives 
L1 %*% t(L1)

###############################
# MIXED LOGIT

res6<-mlogit(MODE~TTME+GC,data=CLOGIT,reflevel="car", rpar=c("air:(intercept)"="n","bus:(intercept)"="n", "train:(intercept)"="n"), R=500,halton=NA,print.level=1)
summary(res6)

by(CLOGIT$TTME, CLOGIT$mode.ids, summary)

# Variable Description
# mode 0/1 variable, with a 1 indicating the position of the mode selected.
# ttme terminal waiting time, minutes
# invc in-vehicle cost, minutes
# invt in-vehicle time, minutes
# gc generalized cost = invc + (invt x value-of-time)
# chair dummy, = 1 if chosen mode is air
# hinc household income, in thousands of $AUS
# psize travelling party size
# indj ????2 for not-air ; otherwise the same as mode
# indi dummy for air / not-air : ????1 for the other modes
# aasc alternative-specific dummy for air mode
# tasc alternative-specific dummy for train mode
# basc alternative-specific dummy for bus mode
# casc alternative-specific dummy for car mode
# psizea psize x aasc
# hinca household income, "attached" to the air mode
# z tasc + basc + basc = Dummy variable for Not Air
# nij 1 if aasc = 1 and 3 if aasc = 0
# ni 2 = number of branches in tree


inclass1<-mlogit(MODE~ GC + PSIZEA |  + HINC | INVT, data=CLOGIT)
summary(inclass1)

inclass1<-mlogit(MODE~ GC  | PSIZEA + HINC | INVT, data=CLOGIT)
summary(inclass1)

inclass1<-mlogit(MODE~ GC  | PSIZE + HINC | INVT, data=CLOGIT)
summary(inclass1)

inclass2<-mlogit(MODE~ GC  | HINC | INVC, data=CLOGIT)
summary(inclass2)

inclass3<-mlogit(MODE~ TTME  | HINC | INVC + INVT , data=CLOGIT)
summary(inclass3)


summary(CLOGIT$INVT)
