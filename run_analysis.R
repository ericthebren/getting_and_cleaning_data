library(plyr)
library(dplyr)
library(lubridate)
library(data.table)
library(tidyr)


    
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, "dataset1.zip")
    unzip("dataset1.zip")

##Requirement 1: Merges the training and the test sets to create one data set
    ## Get Activity and Features Data
    activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c('activityid', 'activity'))
    activityLabels[,2] <- as.character(activityLabels[,2])
    features <- read.table("UCI HAR Dataset/features.txt", col.names = c('featureid', 'featurename'))
    features[,2] <- as.character(features[,2])
    
    ## Limit Features Data to STD or Mean
    featuresGood <- grep("*[Mm]ean*|*[Ss]td*", features[,2])
    featuresnames<- features[featuresGood, 2]
    subfeatures<-  features[featuresGood,]

    ##load test datasets
    subject_test <-  read.table("UCI HAR Dataset/test/subject_test.txt", col.names = c('subject'))
    data_test <-  read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$featurename)
    activity_test <-  read.table("UCI HAR Dataset/test/Y_test.txt", col.names = c('activityid'))

    ## load train datasets
    subject_train <-  read.table("UCI HAR Dataset/train/subject_train.txt", col.names = c('subject'))
    data_train <-  read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$featurename)
    activity_train <-  read.table("UCI HAR Dataset/train/Y_train.txt", col.names = c('activityid'))

    ##Merge datasets
    data_trainandtest <- rbind(data_train, data_test)    
    activity_trainandtest <- rbind(activity_train, activity_test)
    subject_trainandtest <- rbind(subject_train, subject_test)
    colnames(data_trainandtest) <- features$featurename

    subject_activity <- cbind(subject_trainandtest, activity_trainandtest)
    AllData <- cbind(subject_activity, data_trainandtest)

        
##Requirement 2: Extracts only the measurements on the mean and standard deviation for each measurement.
    mergemeansd <- subset(AllData, ,grep("*[Mm]ean*|*[Ss]ubject*|*[Aa]ctivity*|*[Ss]td*", colnames(AllData), value = TRUE))

    
##Requirement 3: Uses descriptive activity names to name the activities in the data set
    mergemeansd <- merge(activityLabels, mergemeansd, by="activityid", all.x=TRUE)

    
##Requirement 4: Appropriately labels the data set with descriptive variable names   
    ## clean up features names
    colnames(mergemeansd) <- gsub("*[Mm]ean*", "Mean", colnames(mergemeansd))
    colnames(mergemeansd) <- gsub("*[Ss][Tt][Dd]*", "StandardDev", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("^[Tt]", "Time", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*[Bb]ody[Bb]ody", "Body", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*[Bb]ody*", "Body", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("^[Ff]", "Freq", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("^[Aa]ngle", "Angle", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*[Gg]ravity*", "Gravity", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*[Gg]yro*", "Gyroscope", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*[Mm]ag*", "Magnitude", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*[Ac]cc*", "Accelerometer", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*|*", "", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*_*", "", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*-*", "", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*\\(*", "", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*\\)*", "", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*,*", "", colnames(mergemeansd) )
    colnames(mergemeansd) <- gsub("*\\.*", "", colnames(mergemeansd) )

    ## group by subject and activity, provide mean for each measurement
    MeanMeasure <- aggregate(. ~subject + activity, mergemeansd, mean)
    MeanMeasure <- MeanMeasure[order(MeanMeasure$subject, MeanMeasure$activity), ]

    
##Requirement 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    ## write to tidy.csv
    write.table(MeanMeasure, file = "tidy.csv", row.names = FALSE)