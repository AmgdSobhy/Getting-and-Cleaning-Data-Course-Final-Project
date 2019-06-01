#run_analysis.R is a script for getting and cleaning the data of UCI dataset collected using Samsung Galaxy S smartphone

# First - Downlading Data File and Unzipping It

setwd("C:/Users/Amgds/Documents/CleaningDataProject")
if (!file.exists("UCI HAR Dataset.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                      file.path(getwd(),"UCI HAR Dataset.zip"))
}

if (!file.exists("UCI HAR Dataset")) {
        unzip("UCI HAR Dataset.zip")
}


# Second - Loading Data Files into R

##clearing Global Enviroment to Prevent any Errors

remove(list = ls())

## Setting Working Directory

setwd(file.path(getwd(),"UCI HAR Dataset"))

## Loading data.table Package that Has Faster read.table Function

library(data.table)

## Reading Train Data

trainingSubjects <- read.table(file.path(getwd(), "train", "subject_train.txt"))
trainingValues <- read.table(file.path(getwd(), "train", "X_train.txt"))
trainingActivity <- read.table(file.path(getwd(), "train", "y_train.txt"))

## Reading Test Data

testSubjects <- read.table(file.path(getwd(), "test", "subject_test.txt"))
testValues <- read.table(file.path(getwd(), "test", "X_test.txt"))
testActivity <- read.table(file.path(getwd(), "test", "y_test.txt"))

## Reading Features and Activity Lables
features <- read.table(file.path(getwd(), "features.txt"), as.is = TRUE)
activities <- read.table(file.path(getwd(), "activity_labels.txt"))


# Third - 1. Merges the training and the test sets to create one data set. 

X_data <- rbind(cbind(trainingSubjects, trainingValues, trainingActivity),
                cbind(testSubjects, testValues, testActivity))

## Nameing the Dataset Using Features Data

colnames(X_data) <- c("subjects", features[,2], "activity")

## Removing Original Datasets after Creating a One Dataset that Combines them All
        ### this will help saving memory
rm(trainingSubjects,trainingValues,trainingActivity,testSubjects,testValues,testActivity,features)

# Fourth - 2.Extracts only the measurements on the mean and standard deviation for each measurement.

## creating a vector with only needed columns names

new_columns <- grepl("mean|std|subjects|activity",colnames(X_data))

## Subsetting Based on the New Filtered Columns 

X_data <- X_data[,new_columns]

# Fifth - 3. Uses descriptive activity names to name the activities in the data set

X_data$activity <- factor(X_data$activity, levels = activities[, 1], labels = activities[, 2])

# Sixth - 4. Appropriately labels the data set with descriptive variable names

## get column names
Corrected <- colnames(X_data)

## remove special characters
Corrected <- gsub("[\\(\\)-]", "", Corrected)

## expand abbreviations and clean up names
Corrected <- gsub("^f", "frequencyDomain", Corrected)
Corrected <- gsub("^t", "timeDomain", Corrected)
Corrected <- gsub("Acc", "Accelerometer", Corrected)
Corrected <- gsub("Gyro", "Gyroscope", Corrected)
Corrected <- gsub("Mag", "Magnitude", Corrected)
Corrected <- gsub("Freq", "Frequency", Corrected)
Corrected <- gsub("mean", "Mean", Corrected)
Corrected <- gsub("std", "StandardDeviation", Corrected)

## correct typo
Corrected <- gsub("BodyBody", "Body", Corrected)

## use new labels as column names
colnames(X_data) <- Corrected

# Seventh - 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

## Grouping the Dataset Based on Subject and Activity

library(dplyr)
DataMean <- X_data %>% 
        group_by(subjects, activity) %>%
        summarise_each(funs(mean))

## output to file "tidy_data.txt"
write.table(DataMean, "tidy_data.txt", row.names = FALSE,quote = FALSE)