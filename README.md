title: "README.Rmd"
# Getting and Cleaning Data Course Project #

## Script short description ##
There is only one script was used (you can find it in the run_analysis.R file). The script download the zip file, unzip it, load pack of data files in R variables. After that the script clean data (remove excess data), merge data sets and name columns. Than it names the activities? computes average values and writes data to the set of files.

## Detailed descrition ##

### Data loading ###

First, let's download required packages an data file. 

```r
## required libraries
require(plyr)
```

```
## Loading required package: plyr
```

```r
## download data
url <-"http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,"DS.zip")
```

Let's unzip it in the folder "UCI HAR Dataset" (The folder is inside the .zip file).


```r
## unzip data
unzip("DS.zip")
```

Now it's time to load data into R variables. There are 6 data files we should read. X_test.txt and X_train.txt contain measured values for testing and training sets. Y_test.txt and Y_train.txt contain values of activity variables (it's just numbers). Files subject_test.txt and subject_train.txt contains information about subjects of research. Subjects are people but coded by numbers from 1 to 30. Also there are two auxiliary files: activity_labels.txt and features.txt. The first one contains activity decoding data, and the second contains names of the variables.


```r
## load data to R
Xtest <- read.table("UCI HAR Dataset/test/X_test.txt")
Ytest <- read.table("UCI HAR Dataset/test/y_test.txt")
Stest <- read.table("UCI HAR Dataset/test/subject_test.txt")
Xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")
Ytrain <- read.table("UCI HAR Dataset/train/y_train.txt")
Strain <- read.table("UCI HAR Dataset/train/subject_train.txt")

labels <- read.table("UCI HAR Dataset/activity_labels.txt", colClasses="character")
features <- read.table("UCI HAR Dataset/features.txt", colClasses="character")
```

### Data cleaning ###

We should only use the measurements on the mean and standard deviation for each measurement. As Xtest and Xtrain are heavy variables let's remove excess data from it (almost 90% of data). During cleaning I've assumed that meanFreq() and stdFreq() don't contain measurements on the mean and standard deviation. So we need only valiables with mean() and std(). The grepl function is just what we need.


```r
## Remove excess data (we need only measurements on the mean() and str())
v1 <- grepl("mean()", features[, 2], fixed=T)
v2 <- grepl("std()", features[, 2], fixed=T)
v <- v1 | v2

Xtest <- Xtest[, v]
Xtrain <- Xtrain[, v]
```

### Data merging ###

Now we can merge all the data and name the columns.

```r
## Merge subject, activity and measurements data
Xtest <- cbind(Stest, Ytest, Xtest)
Xtrain <- cbind(Strain, Ytrain, Xtrain)

## Merge training and test sets
Data <- rbind(Xtest, Xtrain)

## Name data
names(Data) <- c("Subject", "Level", features[v,2])
labels[, 1] <- as.integer(labels[, 1])
names(labels) <- c("Level", "Activity")
```

Also let's convert activity codes into descriptive activity names. I've used names from the activity_labels.txt: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING.

```r
## Name activities like in the activity_labels.txt
Data <- arrange(join(Data, labels), Level)
```

```
## Joining by: Level
```

### Mean values calculation and file writing
Now we can calculate the average of each variable for each activity and each subject. For each variable we get matrix 30x6 with mean values. Let's put it in the list variable full of matricies. This variable will be written in the main file later. But, as i think, we should save all the matricies in the separate .tct files. So let's create new directory and save these files there.

```r
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
```

### Saving the data ###

It's time to save data.framework with tidy data (let's sort it before) and the list with mean values. I've used 2 formats. Txt for txt-lovers and csv for everybody else.

```r
## Sort data before saving
Data <- arrange(Data, Data$Subject)

## Write data to the .txt files
write.table(Data, "Tidy_data.txt")
write.table(DataMean, "Tidy_data_mean.txt")

## Write data to the .csv files
write.csv(Data, "Tidy_data.csv")
write.csv(DataMean, "Tidy_data_mean.csv")
```
