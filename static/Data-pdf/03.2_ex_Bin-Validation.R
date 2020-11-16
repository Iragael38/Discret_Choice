# Example Problem
# 
# Lets try and predict if an individual will earn more than $50K (ABOVE50K) using logistic regression based on demographic variables available in the adult data. In this process, we will:
#   
# Import the data
# Check for class bias
# Create training and test samples
# Build logit models and predict on test data
# Do model diagnostics

# 1

setwd("G:\\IRA\\888_ESE UPMF\\2020_C2ES_M2_Econometrie3\\Rscripts")
inputData <- read.csv("adult.csv")
head(inputData)

# 
table(inputData$ABOVE50K)

logitMod <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, data=inputData, family=binomial(link="logit"))

summary(logitMod)

table(inputData$RELATIONSHIP)

# To analyse prediction capacity
### Create Training Data and Test Data
input_ones <- inputData[which(inputData$ABOVE50K == 1), ]  # all 1's
input_zeros <- inputData[which(inputData$ABOVE50K == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 
# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

## Estimate and Predict

prop.table(table(trainingData$ABOVE50K))
prop.table(table(testData$ABOVE50K))

logitMod <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, data=trainingData, family=binomial(link="logit"))

summary(logitMod)

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores


# test for the full model against the 0-model
glm0 <- glm(ABOVE50K ~ 1 , data=trainingData, family=binomial(link="logit"))
anova(glm0, logitMod, test="Chisq")

# IC
confint(logitMod)

# Odd Ratio interpretation
exp(logitMod$coefficients)


# Wald.test: Occupation=farm-fishing
library(aod)
wald.test(b=coef(logitMod), Sigma=vcov(logitMod), Terms=13)
# Wald Test: Rel:Not-in-family = Rel: Other-relative
restr <- rbind(c(0, -1, 1, 0,0,0, rep(0,17)))
wald.test(b = coef(logitMod), Sigma = vcov(logitMod), L = restr)

# VIF diagnostic
library(car)
vif(logitMod)

# Classification Diagnostic
#install.packages("InformationValue")
library(InformationValue)
# optimal cut-off proba that minimizes the misclassification error for the above model
optCutOff <- optimalCutoff(testData$ABOVE50K, predicted)[1] 
# percentage mismatch of predicted vs actuals
misClassError(testData$ABOVE50K, predicted, threshold = optCutOff)

plotROC(testData$ABOVE50K, predicted)
# Sensitivity: proportion of non-events that are correctly predicted
# Specificity: proportion of events that are correctly predicted

Concordance(testData$ABOVE50K, predicted)
# % of true positive
sensitivity(testData$ABOVE50K, predicted, threshold = optCutOff)
# % of true negative
specificity(testData$ABOVE50K, predicted, threshold = optCutOff)

confusionMatrix(testData$ABOVE50K, predicted, threshold = optCutOff)

# ***

classDF <- data.frame(response = logitMod$y, predicted = round(fitted(logitMod),0))
xtabs(~ predicted + response, data = classDF)
                                                             
ClassTab <- xtabs(~ predicted + response, data = classDF)
(ClassTab[1,1] + ClassTab[2,2] ) / sum(ClassTab)                                                             
                                                             
predpr <- predict(logitMod,type=c("response"))
library(pROC)
roccurve <- roc(trainingData$ABOVE50K ~ predpr)
plot(roccurve)
auc(roccurve)
