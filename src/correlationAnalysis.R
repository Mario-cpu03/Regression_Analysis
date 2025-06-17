### Correlation Analysis Script

# The correlationAnalysis script is used to
# evaluate the correlation between each couple of
# variables in the training set.

# The quantification of the correlation between each pair of
# variables is central, in order to avoid problems in model estimation.
# Linearly or quadratic dependent variables (multi-collinearity) may 
# cause problems in the Covariance Matrix of the model 
# (specifically while trying to compute it's inverted)

# Note that relevant results regarding correlation and hypothesis tests
# result about the Polynomial Regression Models will be found in the
# "results/" directory. 

# The output of the following script will be a processed data set
# in which every correlation will be resolved 
# (if considered statistically relevant)

# directory containing every relevant result
results <- "~/Desktop/Regression_Analysis/results/"

## CORRELATION IN THE TRAINING SET

# Computing Correlation
correlation <- cor(dataRaw)

# Graphic Evaulation
pdf("results/heatmaps/heatmap_raw_dataset.pdf", width = 12, height = 12)
corrplot::corrplot(correlation, 
                   method = "color",
                   col = colorRampPalette(c("blue", "white", "red"))(200),
                   type = "upper",       
                   tl.col = "black",
                   tl.srt = 45,
                   number.cex = 0.7)   
dev.off()

# Correlation Matrix Creation
correlationMTX <- Hmisc::rcorr(as.matrix(dataRaw)) #???
corr <- correlationMTX$r
cat("\nCorrelation Matrix:\n")
print(corr)

# Multicollinearity measurement, 
# m.cor <- cor(dataRaw)
# 1/(1-m.cor^2)

## HYPOTHESIS TEST

# Let the risk level be alpha=0.05 and the null and alternative hypothesis be:
# H0: corr = 0    HA: corr != 0

# Given the correlation matrix we evaluate the associated p-values in order
# to prove the statistical relevance of the results

# Computing p-values
pValue <- correlationMTX$P
# Persistence Logic
write.csv(data.frame(pValue), "results/Correlation_PVALUES.csv")

# Graphic Evaluation
# Visualization of the relationships
#png(filename = "results/scatter-plot_All.png")
pdf("results/scatterplots/scatter-plot_All.png", width = 12, height = 12)
pairs(dataRaw, upper.panel = NULL)
GGally::ggpairs(dataRaw)
dev.off()

# Analytic Evaluation
# A closer look on the most interesting relationships
# with a Polynomial Regression Model  to evaluate the statistical relevance 
# y_VideoQuality <-> x1_ISO
plot(dataRaw$x1_ISO, dataRaw$y_VideoQuality, main = "ISO - Video Quality",xlab='x1_ISO',ylab='y_VideoQuality')
modelPoly <- lm(y_VideoQuality ~ poly(x1_ISO, 3), data = dataRaw)
yx1 <- capture.output(summary(modelPoly))
# p-value higly relevant --> y_VideoQuality = b_0 + b_1 x1_ISO + b_2 x1_ISO^2 + b_3 x1_ISO^3

# y_VideoQuality <-> x2_FRatio
plot(dataRaw$x2_FRatio, dataRaw$y_VideoQuality, main = "Focal Ratio - Video Quality",xlab='x2_FRatio',ylab='y_VideoQuality')
modelPoly <- lm(y_VideoQuality ~ poly(x2_FRatio, 2), data = dataRaw)
yx2 <- capture.output(summary(modelPoly))
# p-value higly relevant --> y_VideoQuality = b_0 + b_1 x2_FRatio + b_2 x2_FRatio^2

# y_VideoQuality <-> x3_TIME
plot(dataRaw$x3_TIME, dataRaw$y_VideoQuality, main = "Time - Video Quality",xlab='x3_TIME',ylab='y_VideoQuality')
modelPoly <- lm(y_VideoQuality ~ poly(x3_TIME, 2), data = dataRaw)
yx3 <- capture.output(summary(modelPoly))
# p-value higly relevant --> y_VideoQuality = b_0 + b_1 x3_TIME + b_2 x3_TIME^2

# y_VideoQuality <-> x5_CROP
plot(dataRaw$x5_CROP, dataRaw$y_VideoQuality, main = "Crop Factor - Video Quality",xlab='x5_CROP',ylab='y_VideoQuality')
modelPoly <- lm(y_VideoQuality ~ poly(x5_CROP, 1), data = dataRaw)
yx5 <- capture.output(summary(modelPoly))
# p-value higly relevant --> y_VideoQuality = b_0 + b_1 x5_CROP

# x4_MP <-> x7_PixDensity
plot(dataRaw$x4_MP, dataRaw$x7_PixDensity, main = "MegaPixels - Density",xlab='x4_MP',ylab='x7_PixDensity')
modelPoly <- lm(x7_PixDensity ~ poly(x4_MP, 1), data = dataRaw)
x4x7 <- capture.output(summary(modelPoly))
# p-value higly relevant --> x7_PixDensity = b_0 + b_1 x4_MP

# x4_MP <-> x2_FRatio
plot(dataRaw$x4_MP, dataRaw$x2_FRatio, main = "MegaPixels - Focal Ratio",xlab='x4_MP',ylab='x2_FRatio')
modelPoly <- lm(x2_FRatio ~ poly(x4_MP, 1), data = dataRaw)
x4x2 <- capture.output(summary(modelPoly))
# Too much randomness in the observation and way
# too high p-value on the Intercept parameter. 
# The correlation is statistically relevant on the b_1 parameter, but the dataset may be too small 
# or contain too much variability for strong conclusions to be drawn.
# For this reasons those two predictors still remain into the dataset
# Nonetheless due to the p-value we can affirm:
# p-value relevant --> x2_FRatio = b_0 + b_1 x4_MP

# x5_CROP <-> x7_PixDensity
plot(dataRaw$x5_CROP, dataRaw$x7_PixDensity, main = "Crop Factor - Density",xlab='x5_CROP',ylab='x7_PixDensity')
modelPoly <- lm(x7_PixDensity ~ poly(x5_CROP, 2), data = dataRaw)
x5x7 <- capture.output(summary(modelPoly))
# p-values are way too high on the Intercept and b_0 parameter.
# Nevertheless due to the p-value we can affirm:
# p-value higli relevant --> x7_PixDensity = b_2 x5_CROP^2

# x2_FRatio <-> x7_PixDensity
plot(dataRaw$x2_FRatio, dataRaw$x7_PixDensity, main = "Focal Ratio - Density",xlab='x2_FRatio',ylab='x7_PixDensity')
modelPoly <- lm(x7_PixDensity ~ poly(x2_FRatio, 1), data = dataRaw)
x2x7 <- capture.output(summary(modelPoly))
# Too much randomness in the observation and way
# too high p-value on the Intercept parameter. 
# The correlation is statistically relevant on the b_1 parameter, but the dataset may be too small 
# or contain too much variability for strong conclusions to be drawn.
# Nonetheless due to the p-value we can affirm:
# p-value relevant --> x7_PixDensity = b_1 x2_FRatio

## SUMMARY

correlationOutput <- c("Summary of statistically relevant correlations:\n", 
                       yx1, "\n",
                       yx2, "\n",
                       yx3, "\n",
                       yx5, "\n",
                       x4x7, "\n",
                       x4x2, "\n",
                       x2x7, "\n", 
                       x5x7, "\n",
                       "Please note that each correlation found in the dataset is statistically relevant but not each one of them represent a real problem for the Covariance Matrix of the regression model.\n",
                       "Let x4x2 be an example: there is a relevant correlation but not a preoccupying collinearity such that it would justify the exclusion of one of them from the data set.")
writeLines(correlationOutput, "results/Polynomial_Regression.txt")

## PROCESSED TRAINING SET - PERSISTENCE LOGIC

# Eliminating x7_PixDensity from the Training Set
# due to its high correlation with x4_MP and other predictors
processedDataSet <- dataRaw[, c("y_VideoQuality","x1_ISO","x2_FRatio","x3_TIME","x4_MP", "x5_CROP", "x6_FOCAL")]
processedData <- write.csv(processedDataSet, "data/DataSet_gruppo4-PROCESSED.csv")

# Graphic evaluation of the Processed Training Set
correlationProcessed <- cor(processedDataSet)
pdf("results/heatmaps/heatmap_processed_dataset.pdf", width = 12, height = 12)
corrplot::corrplot(correlationProcessed, 
                   method = "color",
                   col = colorRampPalette(c("blue", "white", "red"))(200),
                   type = "upper",       
                   tl.col = "black",
                   tl.srt = 45,
                   number.cex = 0.7)   
dev.off()
