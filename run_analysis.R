#############################################################################
#
# 
#   run_analysis.R
#   Running this script will download the zip file containing the Samsung Galaxy data
#   from the UCI machine learning library and produce the Tiny txt file for 
#   the Getting and Cleaning Data Course Project.
#

library(dplyr)


zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}


dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

#The following reads and sets up the training and testing datasets from the zipfile.

trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


# STEP 1: Merges the training and the test sets to create one data set.


humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

colnames(humanActivity) <- c("subject", features[, 2], "activity")

#STEP 2: Extracts only the measurements on the mean and standard deviation 
#for each measurement.

columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

humanActivity <- humanActivity[, columnsToKeep]



#STEP 3:Uses descriptive activity names to name the activities in the data set

humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])


#STEP 4 - Appropriately label the data set with descriptive variable names

humanActivityCols <- colnames(humanActivity)

humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

colnames(humanActivity) <- humanActivityCols


# STEP 5 - From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)