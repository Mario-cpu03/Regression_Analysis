## Descriptive Analysis Script

# The descriptiveAnalysis script is used to 
# analyze all the variables in the training set using
# descriptive statistics methods, aiming to obtain
# crucial characteristic quantities about the dependent 
# variable and the predictor variables.
# A graphical evaluation first will contribute to our analysis
# by exposing quantiles and outliers; a mathematical evaluation 
# then will provide precious informations about mean and variance.
# The following script will be useful and necessary to the correlation analysis phase.

# GRAPHICAL EVALUATION

# Histogram of every variable
for (i in 1:8){
  hist(dataRaw[, i], 
       main = names(dataRaw)[i],
       xlab = names(dataRaw)[i],
       col = "blue",
       border = "black"
  )
}

# General view of each variable's box-plot
boxplot(dataRaw)

# Box-plot of every variable in search for eventual outliers
for (i in 1:8){
  boxplot(dataRaw[, i], main = names(dataRaw)[i])
}

# MATHEMATICAL EVALUATION 

# Summary of each variable
cat("\nDescriptive statistics for each variable:")
for (i in 1:8){
  cat("\nVariable: ", names(dataRaw)[i])
  cat("\nMean: ", mean(dataRaw[, i]))
  cat("\nVariance: ", var(dataRaw[, i]))
  cat("\nStandard Deviation: ", sd(dataRaw[, i]))
  cat("\nMin: ", min(dataRaw[, i]))
  cat("\nMax: ", max(dataRaw[, i]))
  cat("\nQuantiles (25%, 50%, 75%): ", quantile(dataRaw[, i], probs = c(0.25, 0.5, 0.75)))
  cat("\nInterquartile Range (IQR): ", IQR(dataRaw[, i]))
  cat("\n\n")
}
