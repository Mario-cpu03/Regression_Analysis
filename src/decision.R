### Decision Script

# The decision script is used to
# estimate parameters from each Regression Model formulated
# in the "regressionModel.R" script, in order to evaluate 
# those candidate models and choose the most appropriate one

# AIC/BIC, overall F-Test and p-values from individual t-tests will be 
# the model's selection criteria, adopted while using backward 
# stepwise regression to drop non-significant predictors in each model
# as advised by the the principle of Parsimony (Occam's Razor principle)

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
# The resulting model exactly matches the first alternative model proposed
# without applying the backward selection

alternativeModel1Back <- step(alternativeModel1, direction = "backward", trace = FALSE)
summary(alternativeModel1)
# The resulting model exactly matches the first alternative model proposed
# without applying the backward selection

alternativeModel2Back <- step(alternativeModel2, direction = "backward", trace = FALSE)
summary(alternativeModel2)
# The resulting model exactly matches the second alternative model proposed
# without applying the backward selection

alternativeModel3Back <- step(alternativeModel3, direction = "backward", trace = FALSE)
summary(alternativeModel3)
# The resulting model exactly matches the third alternative model proposed
# without applying the backward selection

## AKAIKE INFORMATION CRITERION

# AIC evaluation in order to choose the best data-adapting model and predicting model
aicCM <- AIC(completeModel)
aicCMB <- AIC(completeModelBack)
aicM1 <- AIC(alternativeModel1)
aicM2 <- AIC(alternativeModel2)
aicM3 <- AIC(alternativeModel3)

arr <- c(aicCM,aicCMB,aicM1,aicM2,aicM3)
selectValAic = Inf
for (i in 1:length(arr)) {
  if (arr[i] < selectValAic)
    selectValAic = arr[i]
}

## BAYESIAN INFORMATION CRITERION

# BIC evaluation in order to choose the best data-adapting model
bicCM <- BIC(completeModel)
bicCMB <- BIC(completeModelBack)
bicM1 <- BIC(alternativeModel1)
bicM2 <- BIC(alternativeModel2)
bicM3 <- BIC(alternativeModel3)

arr <- c(bicCM,bicCMB,bicM1,bicM2,bicM3)
selectValBic = Inf
for (i in 1:length(arr)) {
  if (arr[i] < selectValBic)
    selectValBic = arr[i]
}

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

# Persistence Logic
decisionOutput <- c( "Multiple Regression Model Chosen:\n",
                     selectedModel, "\n",
                     "AIC:", selectedAIC, "\n",
                     "BIC:", selectedBIC)
writeLines(decisionOutput, "results/Chosen.txt")

