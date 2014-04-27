# This is a program that will read in test and training data from a zip file. 
# First take the zip file located here:
#
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
# and extract it to a folder in your computer. After that, change the setwd to look into the root 
# directory where the test and training files are located.
#
setwd("F:\\coursera\\gettingandcleaning\\PA\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset")
#
#Read in the Training and testing datasets for each type and appends the test and training datasets.
#
x.Train <- read.table("train/X_train.txt")
x.Test  <- read.table("test/X_test.txt")
x_Both  <- rbind(x.Train, x.Test)

y.Train <- read.table("train/y_train.txt")
y.Test  <- read.table("test/y_test.txt")
y_Both  <- rbind(y.Train, y.Test)

subj.Train <- read.table("train/subject_train.txt")
subj.Test  <- read.table("test/subject_test.txt")
subj_Both  <- rbind(subj.Train, subj.Test)
#
# The features file in the root directory contains information about the type of variables 
# contained within each dataset. This will help us obtain the columns desired for our tidy data set
# A vector that contains the rows of the features that either have "mean" or "std" in the name will be selected
#
feat <- read.table("features.txt")
mean.or.std.feat <- grep("-mean\\(\\)|-std\\(\\)", feat[, 2])
#
# Takes the columns that were selected above (66 columns with either mean or std in the name) 
# from the x dataset created earlier (561 total columns) 
#
x_Both <- x_Both[, mean.or.std.feat]
names(x_Both) <- feat[mean.or.std.feat, 2]
names(x_Both) <- gsub("\\(|\\)", "", names(x_Both))
#
# Takes the y dataset above which just had numbers depicting each activity and replaces it with the 
# activity specified for each number in the activity_labels file.
#
act <- read.table("activity_labels.txt")
act[, 2] = gsub("_", "", tolower(as.character(act[, 2])))
y_Both[,1] = act[y_Both[,1], 2]
names(y_Both) <- "activity"
#
# Need to merge the subject file activity file and results file together. Confirm that all files 
# have the same number of obcervations before merging the datasets (should be 10299)
#
dim(x_Both)[1]
dim(y_Both)[1]
dim(subj_Both)[1]
#
#The Subject dataset contains the subject for which the row is referring too (1-30) change the name 
# of the column to be Subject and then merge with the other two datasets
#
names(subj_Both) <- "subject"
merged <- cbind(subj_Both, y_Both, x_Both)
write.table(merged, "merged_Data.txt")
#
# Looks at the number of unique Subjects (30) and Activities (6) and creates a data table that contains
# 180 rows, one for each possible combination
#
u_Subj <- unique(merged$subject)
u_Act <- unique(merged$activity)
numCols <- dim(merged)[2]
result <- merged[1:(length(u_Subj)*length(u_Act)), ]
#
# Loops through each unique subject and activity to get the rows that are secific to each pair and calculates 
# the mean for each column in the data set 
#
row <- 1
for (s in c(1:length(u_Subj))) {
  for (a in c(1:length(u_Act))) {
    result[row, 1]<- u_Subj[s]
    result[row, 2]<- u_Act[a]
    tmp <- merged[merged$subject==u_Subj[s] & merged$activity==u_Act[a],]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row <- row+1
  }
}
#
# Writes table to setwd directory of the mean for each subject and activity by each column type 
# selected from the script above
#
write.table(result, "dataset_averages.txt")
