setwd("/Users/Brian/Documents/R")
library (caret)
library(ggplot2)
library(leaps)
library(hydroGOF)  # for function rmse()

wq = read.csv("winequality-red.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")
View(wq)

# 
# Compute Pearson r-values and plot regression lines for acid-related variables and pH
#
cor.test(wq$pH, wq$fixed.acidity, method="pearson")
ggplot(wq, aes(y=pH,x=fixed.acidity)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) 

cor.test(wq$pH, wq$volatile.acidity, method="pearson")
ggplot(wq, aes(y=pH,x=volatile.acidity)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) 

# The sulfur-dioxide variables are uncorrelated with the sulphates variable
cor.test(wq$sulphates, wq$free.sulfur.dioxide, method="pearson")
cor.test(wq$sulphates, wq$total.sulfur.dioxide, method="pearson")


# 
# Do exhaustive search to find the best variable combination for predicting y in linear regression
# Note: adjusted r^2 discards new variables that don't actually reduce r^2
# 
rs = regsubsets(quality ~., data=wq, nvmax = 11, nbest=2)
plot(rs, scale="adjr2")

# With just 7 variables
#rs2 = regsubsets(quality ~., data=wq, nvmax = 7, nbest=5)
#plot(rs2, scale="adjr2")

# 
# Now try greedy feature selection 
#

# First, generate the training and test datasets
roundDown = ((nrow(wq)-1)/2) * 2
training = wq[seq(1,roundDown,2), ]
testing  = wq[seq(2,roundDown,2), ]

# Create a sequence of the variable names
rsVars = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", 
        "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")

greedyFeatureSel = function(varList) {
  # We want to repeatedly run multivariate regression with (selVars U {trialVar}) to see if it improves RMSE.
  # If so, add trialVar to selVars; otherwise, drop trialVar. 
  selVars = c("quality")
  selRMSE = .Machine$double.xmax     # largest real value on the machine
  while (length(varList) > 0) {
    # Randomly pick a new variable from the remaining ones (varList)
    numLeft = length(varList)
    trialIdx = sample(1:numLeft, 1)  # one number randomly from 1:numLeft
    trialVar = varList[trialIdx]
    # Train the linear regression model with (selVars U {trialVar})
    trialVarSet = c(selVars, trialVar)
    trialModel = train(data=training[, trialVarSet], quality ~ ., method="lm", metric="RMSE")
    predQ = predict(trialModel, newdata=testing[, trialVarSet])
    trialRMSE = rmse(predQ, testing$quality, na.rm = TRUE)
    if ((trialRMSE < selRMSE) & (abs(selRMSE-trialRMSE)/selRMSE > 0.005)) {
      # Add trialVar to selVars
      selVars = c(selVars, trialVar)
      selRMSE = trialRMSE
      message("  Adding ", trialVar, ": RMSE now ", format(selRMSE))
    } else {
      message("  NOT adding ", trialVar)
      if (trialRMSE >= selRMSE) {
        message("  NOT adding ", trialVar, 
                " - trialRMSE(", format(trialRMSE), ") >= currRMSE(", format(selRMSE), ")")
      } else {
        message("  NOT adding ", trialVar, 
                " - new RMSE(", format(trialRMSE), ") within 0.05% of currRMSE(", format(selRMSE), ")")
      }
    }
    # remove trailVar from varList
    varList = varList[-trialIdx]   
  }
  message("Finished: RMSE = ", format(selRMSE))
  message("Final model variables:")
  cat(trialVarSet, sep=", ")
  message(" ")
  return(selVars)
}

greedyFeatureSel(rsVars)
