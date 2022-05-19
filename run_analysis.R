##  Project for getting and cleaning data class ----
##  You should create one R script called run_analysis.R that does the following. 
##      Merges the training and the test sets to create one data set.
##      Extracts only the measurements on the mean and standard deviation for 
##      each measurement. 
##      Uses descriptive activity names to name the activities in the data set
##      Appropriately labels the data set with descriptive variable names. 
##      From the data set in step 4, creates a second, independent tidy data 
##      set with the average of each variable for each activity and each subject.
library(data.table)
library(dplyr)
##  Take a look at the metadata
##  Part 1 Merges the training and the test sets to create one data set ----
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
head(featureNames)
head(activityLabels)
View(featureNames)
View(activityLabels)
## get the training data (in 3 places)
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
##  Get the test data (in 3 places)
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
##  Merge Train and Test data sets into one set with 3 parts 
##  (suject, activity, and features)
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)
names(features)
##  add names to the columns using metadata (Features)
colnames(features) <- t(featureNames[2])
##  Merge the three files into one and store in completeData
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)
##  Part 2 Extracts only the measurements on the mean and standard deviation for ---- 
##  each measurement. 
##  Get the columns that have std or mean and subset them
columnsMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
##  Add activity and subject columns to the data. 
requiredColumns <- c(columnsMeanSTD, 562, 563)
dim(completeData)
##  extract the selected columns and save as extractedData
extractedData <- completeData[,requiredColumns]
dim(extractedData)
names(extractedData)
##  Part 3 Uses descriptive activity names to name the activities in ----
##  the data set
## change the data type from numeric to character for activity field then use
##  metadta to get the names
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
        extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
##  Factor the activity variable
extractedData$Activity <- as.factor(extractedData$Activity)
##  Part 4 - Appropriately labels the data set with descriptive variable names
names(extractedData)
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)
##  Part 5 - From the data set in step 4, creates a second, independent ----
##  tidy data set with the average of each variable for each 
##  activity and each subject
##  make subject a factor variable.
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)
##  Get the average for each acctivity and subject and save as tidyData, then 
##  order it, then use fwrite or write.table to save it into a txt file.
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

