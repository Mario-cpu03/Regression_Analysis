### Correlation Analysis Script

# The correlationAnalysis script is used to
# evaluate the correlation between each couple of
# variables in the training set.

# The quantification of  correlation between each pair of
# variables is central, in order to avoid problems in model estimation.
# Linearly or quadratic dependent variables (multicollinearity) may 
# cause problems in the Covariance Matrix of the model 
# (specifically while trying to compute it's inverted)

# The output of the following script will be a processed data set
# in which every correlation will be resolved 
# (if considered statistically relevant)

results <- "~/Desktop/Regression_Analysis/results/"

## CORRELATION IN THE TRAINING SET

# Computing Correlation
correlation <- cor(dataRaw)

# Graphic Evaulation
png(filename = paste0(results, "heatmap_raw_dataset.png"))
corrplot::corrplot(correlation, 
                   method = "color",     # Usa colori per rappresentare la correlazione
                   col = colorRampPalette(c("blue", "white", "red"))(200), # Scala di                                                                       # colori
                   type = "upper",       # Mostra solo la parte superiore della matrice
                   tl.col = "black",     # Colore del testo delle etichette
                   tl.srt = 45,          # Ruota le etichette dei nomi
                   number.cex = 0.7)   
dev.off()

# Correlation Matrix Creation
correlationMTX <- Hmisc::rcorr(as.matrix(dataRaw)) #???
corr <- correlationMTX$r
cat("\nCorrelation Matrix:\n")
print(corr)



# Processed Training Set Persistence Logic
processedData <- write (processed_data, "data/DataSet_gruppo4-PROCESSED.csv");