## required libraries
require("plur")

## download data
url <-"http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,"DS.zip")

## unzip data
unzip("DS.zip")

## load data to R
Xtest <- read.table("UCI HAR Dataset/test/X_test.txt")
Ytest <- read.table("UCI HAR Dataset/test/y_test.txt")
Stest <- read.table("UCI HAR Dataset/test/subject_test.txt")
Xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")
Ytrain <- read.table("UCI HAR Dataset/train/y_train.txt")
Strain <- read.table("UCI HAR Dataset/train/subject_train.txt")

labels <- read.table("UCI HAR Dataset/activity_labels.txt", colClasses="character")
features <- read.table("UCI HAR Dataset/features.txt", colClasses="character")

## Remove excess data (we need only measurements on the mean() and str())
v1 <- grepl("mean()", features[, 2], fixed=T)
v2 <- grepl("std()", features[, 2], fixed=T)
v <- v1 | v2

Xtest <- Xtest[, v]
Xtrain <- Xtrain[, v]

## Merge subject, activity and measurements data
Xtest <- cbind(Stest, Ytest, Xtest)
Xtrain <- cbind(Strain, Ytrain, Xtrain)

## Merge training and test sets
Data <- rbind(Xtest, Xtrain)

## Name data
names(Data) <- c("Subject", "Level", features[v,2])
labels[, 1] <- as.integer(labels[, 1])
names(labels) <- c("Level", "Activity")

## Name activities like in the activity_labels.txt
Data <- arrange(join(Data, labels), Level)

## Create new directory for lots of files with average values of variables
dir.create("Means_of_variables")

## Create new data set for average values (it will be a list)
DataMean <-sapply(features[v,2],function(x) NULL)

## Compute average values for each activity and each subject
for (i in 1:sum(v)){
     DataMean[[i]] <- tapply(Data[, i+2], list(factor(Data$Subject),factor(Data$Activity)), mean)
     ## Write values to the file (individual file for each variable)
     write.table(DataMean[[i]], paste("Means_of_variables/Mean_of_", features[i,2], ".txt", sep=''))
}

## Sort data before saving
Data <- arrange(Data, Data$Subject)

## Write data to the .txt files
write.table(Data, "Tidy_data.txt")
write.table(DataMean, "Tidy_data_mean.txt")

## Write data to the .csv files
write.csv(Data, "Tidy_data.csv")
write.csv(DataMean, "Tidy_data_mean.csv")
