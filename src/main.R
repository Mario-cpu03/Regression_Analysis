### Main Script

# The main script is used to read code from
# "src/dataPreprocessing.R", "src/descriptiveAnalysis.R" and all the 
# other scripts that may be found in the
# "src/" directory.

# The training set which is in the interest of the
# following exmination may be found in the "data/" directory named as it follows:
# "data/DataSet_gruppo4-RAW.csv".

# After running of at least the first 3 scripts, in the same directory as the RAW one,
# it can be found the processed data set, produced as
# an output of the "src/correlationAnalysis.R" script, named as it follows:
# "data/DataSet_gruppo4-PROCESSED.csv".


# Setting the correct working directory:
# (delete the content and insert a different directory if you cloned the repository from GitHub
# or delete "#" if the path is the same)
setwd("~/Desktop/Regression_Analysis")

# Import the Data Set:
dataRaw <- read.csv("data/DataSet_gruppo4-RAW.csv")

## EXPLORATIVE ANALYSIS:

source("./src/dataPreprocessing.R")
source("./src/descriptiveAnalysis.R")
source("./src/correlationAnalysis.R")

# Import the Processed Data Set:
processedData <- read.csv("data/DataSet_gruppo4-PROCESSED.csv")

## LINEAR MODEL CREATION:

source("./src/regressionModel.R")
source("./src/decision.R")
