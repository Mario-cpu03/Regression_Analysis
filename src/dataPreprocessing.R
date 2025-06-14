### Data Preprocessing Script

# The data preprocessing script is used as a first step
# for an explorative data analysis.
# The following script aims to check if the dataset is clean.
# Moreover, we will obtain information about the variables domains and modalities,
# that is, with respect to the values they can assume.

## PRELIMINARY CONTROLS

cat("Verifying if the data set is clean...")

# Check on the first rows of the training set
cat("\nheader:")
headerData <- head(dataRaw)
print(headerData)

# Check on the column names of the traing set
# expected output: y_VideoQuality, x1_ISO, x2_FRatio, x3_TIME, x4_MP, x5_CROP, x6_FOCAL, x7_PixDensity  
cat("\nresponse variable and predictors/potential regressors variables:\n")
namesData <- names(dataRaw)
print(namesData)

## STRUCTURE CONTROL

# Check on the dataset dimension
cat("\ndataset dimension:")
dimension <- dim(dataRaw)
print(dimension)

# Check on the variables domain
# expected output: no non-numeric variables
cat("\nvariables domain:\n")
str(dataRaw)
