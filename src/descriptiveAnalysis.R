### Descriptive Analysis Script

# The descriptiveAnalysis script is used to 
# analyze all the variables in the training set using
# descriptive statistics methods, aiming to obtain
# crucial characteristic quantities about the dependent 
# variable and the predictor variables.

# A graphical evaluation first will contribute to our analysis
# by exposing quantiles and outliers; a mathematical evaluation 
# then will provide precious informations about mean and variance.

# The following script will be useful and necessary to the correlation analysis phase.

# directory containing every relevant result
resultsBox <- "~/Desktop/Regression_Analysis/results/boxplots/"
resultsHist <- "~/Desktop/Regression_Analysis/results/histograms/"

## GRAPHICAL EVALUATION

# Histogram of Every Variable
for (i in 1:8){
  png(filename = paste0(resultsHist, "histogram_", names(dataRaw)[i], ".png"))
  hist(dataRaw[, i], 
       main = names(dataRaw)[i],
       xlab = names(dataRaw)[i],
       col = "blue",
       border = "black"
  )
  dev.off()
}

# General View of each Variable's Box-plot
png(filename = paste0(resultsBox, "boxplot_all_variables.png"))
boxplot(dataRaw)
dev.off()

# Box-plot of every Variable in search for eventual Outliers
for (i in 1:8){
  png(filename = paste0(resultsBox, "boxplot_", names(dataRaw)[i], ".png"))
  boxplot(dataRaw[, i], main = names(dataRaw)[i])
  dev.off()
}

## MATHEMATICAL EVALUATION 

# Summary of each Variable
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

#Saving a summary object in a dedicated text file
summaryDataSet <- capture.output(summary(dataRaw))
writeLines(summaryDataSet, "results/CharacterizedDataSet.txt")
