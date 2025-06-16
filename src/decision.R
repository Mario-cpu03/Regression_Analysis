### Decision Script

# The decision script is used to
# estimate parameters from each Regression Model formulated
# in the "regressionModel.R" script, in order to evaluate 
# those candidate models and choose the most appropriate one

# AIC/BIC, overall F-Test and p-values from individual t-tests will be 
# the model's selection criteria, adopted while using backward 
# stepwise regression to drop non-significant predictors in each model
# as advised by the the principle of Parsimony (Occam's Razor principle)

resultsModel <- "~/Desktop/Regression_Analysis/results/models"

## A FIRST EVALUATION OF THE CANDIDATE MODELS

pdf("results/models/CompleteModel.pdf", width=15, height=15)
plot(completeModel)
dev.off()

pdf("results/models/AlternativeModel1.pdf", width=15, height=15)
plot(alternativeModel1)
dev.off()

pdf("results/models/AlternativeModel2.pdf", width=15, height=15)
plot(alternativeModel2)
dev.off()

pdf("results/models/AlternativeModel3.pdf", width=15, height=15)
plot(alternativeModel3)
dev.off()

## BACKWARD SELECTION 

# Perform a backward selection on each model
completeModelBack <- step(completeModel, direction = "backward", trace = FALSE)
summary(completeModelBack)
# Residuals Evaluation
cMBRes <- residuals(completeModelBack)
# Shapiro - Wilks test on residuals
shapiro.test(cMBRes) #p-value = 0.108 > 0.05 --> normality verified
# MSE
cmbMSE <- mean(residuals(completeModelBack)^2)
# SD Residuals
cmbS <- sd(residuals(completeModelBack))
# SQE
cmbSQE <- sum(residuals(completeModelBack)^2)
# MeanSQE
cmbMSQE <- cmbMSE / (length(residuals(completeModelBack)) - length(coef(completeModelBack)))
# Residual's histogram
png(filename = paste0(resultsModel, "Histogram_res_Complete.png"))
hist(cMBRes,
     main = "Histogram of the Complete Model's residuals",
     xlab = "Residuals",
     col = "blue",
     border = "black"
     )
# The resulting model exactly matches the first alternative model proposed
# without applying the backward selection

alternativeModel1Back <- step(alternativeModel1, direction = "backward", trace = FALSE)
summary(alternativeModel1)
# Residuals evaluation
aMB1Res <- residuals(alternativeModel1Back)
# Shapiro - Wilks test on residuals
shapiro.test(aMB1Res) #p-value = 0.108 > 0.05 --> normality verified
# MSE
amb1MSE <- mean(residuals(alternativeModel1Back)^2)
# SD Residuals
amb1S <- sd(residuals(alternativeModel1Back))
# SQE
amb1SQE <- sum(residuals(alternativeModel1Back)^2)
# MeanSQE
amb1MSQE <- amb1MSE / (length(residuals(alternativeModel1Back)) - length(coef(alternativeModel1Back)))
# Residual's histogram
png(filename = paste0(resultsModel, "Histogram_res_AM1.png"))
hist(aMB1Res,
     main = "Histogram of the first Alternative Model's residuals",
     xlab = "Residuals",
     col = "blue",
     border = "black"
)
# The resulting model exactly matches the first alternative model proposed
# without applying the backward selection

alternativeModel2Back <- step(alternativeModel2, direction = "backward", trace = FALSE)
summary(alternativeModel2)
# Residuals Evaluation
aMB2Res <- residuals(alternativeModel2Back)
# Shapiro - Wilks test on residuals
shapiro.test(aMB2Res) #p-value = 0.2022 > 0.05 --> normality verified
# MSE
amb2MSE <- mean(residuals(alternativeModel2Back)^2)
# SD Residuals
amb2S <- sd(residuals(alternativeModel2Back))
# SQE
amb2SQE <- sum(residuals(alternativeModel2Back)^2)
# MeanSQE
amb2MSQE <- amb2MSE / (length(residuals(alternativeModel2Back)) - length(coef(alternativeModel2Back)))
# Residual's histogram
png(filename = paste0(resultsModel, "Histogram_res_AM2.png"))
hist(aMB2Res,
     main = "Histogram of the second Alternative Model's residuals",
     xlab = "Residuals",
     col = "blue",
     border = "black"
)
# The resulting model exactly matches the second alternative model proposed
# without applying the backward selection

alternativeModel3Back <- step(alternativeModel3, direction = "backward", trace = FALSE)
summary(alternativeModel3)
# Residuals Evaluation
aMB3Res <- residuals(alternativeModel3Back)
# Shapiro - Wilks test on residuals
shapiro.test(aMB3Res) #p-value = 0.2157 > 0.05 --> normality verified
# MSE
amb3MSE <- mean(residuals(alternativeModel3Back)^2)
# SD Residuals
amb3S <- sd(residuals(alternativeModel3Back))
# SQE
amb3SQE <- sum(residuals(alternativeModel3Back)^2)
# MeanSQE
amb3MSQE <- amb3MSE / (length(residuals(alternativeModel3Back)) - length(coef(alternativeModel3Back)))
# Residual's histogram
png(filename = paste0(resultsModel, "Histogram_res_AM3.png"))
hist(aMB3Res,
     main = "Histogram of the third Alternative Model's residuals",
     xlab = "Residuals",
     col = "blue",
     border = "black"
)
# The resulting model exactly matches the third alternative model proposed
# without applying the backward selection

## AKAIKE INFORMATION CRITERION

# AIC evaluation in order to choose the best data-adapting model and predicting model
aicCM <- AIC(completeModel)
aicCMB <- AIC(completeModelBack)
aicM1 <- AIC(alternativeModel1)
aicM2 <- AIC(alternativeModel2)
aicM3 <- AIC(alternativeModel3)

# Minimun AIC selcection
arr <- c(aicCM,aicCMB,aicM1,aicM2,aicM3)
selectValAic <- min(arr)

## BAYESIAN INFORMATION CRITERION

# BIC evaluation in order to choose the best data-adapting model
bicCM <- BIC(completeModel)
bicCMB <- BIC(completeModelBack)
bicM1 <- BIC(alternativeModel1)
bicM2 <- BIC(alternativeModel2)
bicM3 <- BIC(alternativeModel3)

# Minimun BIC selcection
arr <- c(bicCM,bicCMB,bicM1,bicM2,bicM3)
selectValBic <- min(arr)

## SELECTED MODEL

# Given the R^2 results
# the high significance of the p-values of each parameter 
# the lowest BIC and AIC score
# and the high F-Statistic value (see results/models/Backward.txt), 
# alternativeModel3 (which is identical to alternativeModelBack3)
# is the chosen model. A more profund view may be found in "results/models/Chosen.txt"
selectedModel <- capture.output(summary(alternativeModel3Back)) 
#selectedModel <- capture.output(summary(alternativeModel3)) 
selectedAIC <- capture.output(cat(selectValAic))
selectedBIC <- capture.output(cat(selectValBic))
selectedMSE <- capture.output(cat(amb3MSE))
selectedS <- capture.output(cat(amb3S))
selectedSQE <- capture.output(cat(amb3SQE))
selectedMSQE <- capture.output(cat(amb3MSQE))
  
# Persistence Logic
decisionOutput <- c( "Multiple Regression Model Chosen:\n",
                     selectedModel, "\n",
                     "AIC:", selectedAIC, "\n",
                     "BIC:", selectedBIC, "\n",
                     "MSE:", selectedMSE, "\n",
                     "SD residuals:", selectedS, "\n",
                     "SQE:", selectedSQE, "\n",
                     "MSQE:", selectedMSQE)
writeLines(decisionOutput, "results/models/chosen/Chosen.txt")

