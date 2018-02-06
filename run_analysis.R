# Peer-graded Assignment: Getting and Cleaning Data Course Project
# Read in https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# Merge train and test datasets (X,y and subject) into one dataset dt_all
# select only mean() and std() variables
# label all variables appropriately
# create a new dataset (dt_all2) which takes the means of selected variables in dt_all by subject and activity



## Using data.table instead of data frame (much quicker, quicker syntax and memory efficient)
library(data.table)
wd <- getwd()
setwd(wd)

## url for dataset from Coursera
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "fuci.zip"


## download zip file and save as "fuci.zip" if not already on system
if (!file.exists(file)) {
        download.file(url, file)      
}



## unzip .zip file and extract all files to fuci dir in current working directory
#unzip(file, exdir = "fuci", list = TRUE)  ## uncomment to list files in archive
unzip(file, exdir = "fuci")


## read in names of features
## features are a list called "V2" - as automatically called by data.table
## create a vector of feature names
dt_features <- fread("fuci/UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
featurenames <- dt_features[, V2]

## read in X_train, y_train and subject_train and store as data tables
## and label variables appropriately using col.names
dt_X_train <- fread("fuci/UCI HAR Dataset/train/X_train.txt", stringsAsFactors = FALSE, col.names = featurenames)
dt_y_train <- fread("fuci/UCI HAR Dataset/train/y_train.txt", stringsAsFactors = FALSE, col.names = "Activity")
dt_subject_train <- fread("fuci/UCI HAR Dataset/train/subject_train.txt", stringsAsFactors = FALSE, col.names="SubjectID")


## read in X_test, y_test and subject_test and store as data tables
## and label variables appropriately using col.names
dt_X_test <- fread("fuci/UCI HAR Dataset/test/X_test.txt", stringsAsFactors = FALSE, col.names = featurenames)
dt_y_test <- fread("fuci/UCI HAR Dataset/test/y_test.txt", stringsAsFactors = FALSE, col.names = "Activity")
dt_subject_test <- fread("fuci/UCI HAR Dataset/test/subject_test.txt", stringsAsFactors = FALSE, col.names="SubjectID")

## bind SubjectID, X and Y together for train and test
dt_train <- cbind(dt_subject_train, dt_X_train, dt_y_train)
dt_test <- cbind(dt_subject_test, dt_X_test, dt_y_test)

## bind train and test together on rows
dt_all <- rbind(dt_train, dt_test)

## extract variable names containing mean or stand deviation
## i.e. search for mean() or std()
meanstdvarnames <- grep("mean[()]|std[()])", names(dt_all), value=TRUE)

## vector of variables to keep
## keep mean and std and also SubjectID and Activity label
keepvars <- c("SubjectID", meanstdvarnames, "Activity")

## keep only these variables
dt_all <- dt_all[, keepvars, with=FALSE]


## Rename "Activity" as per dt_activitylabels 
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING

## has 2 columns (V1 = Activity Number as per dt_all and V2 has label)
dt_activitylabels <- fread("fuci/UCI HAR Dataset/activity_labels.txt")

## creates a vector with the row indices in dt_activitylabels (V1) to match "Activity" for each observation in dt_all
activityindex <- match(dt_all[, Activity], dt_activitylabels[, V1])

## relabel "Activity" using this vector
dt_all[, Activity := dt_activitylabels[activityindex, V2]]

## create new dataset (dt_all2) with mean of all mean() and std() variable by SubjectID and Activity
## data.table makes this quick to do
dt_all2 <- dt_all[, lapply(.SD, mean), .SDcols = meanstdvarnames, by=c("SubjectID", "Activity")]

## quick sort (by reference only) by SubjectID and Activity
setorderv(dt_all2, c("SubjectID", "Activity"))

## rename columns to indicate they are now averages
oldnames <- names(dt_all2)

avgpaste <- function(x) {
        if (!grepl("SubjectID|Activity", x)) {
                paste0("Average_", x)
              
        } else {
               x
        }
        
}

names(dt_all2) <- sapply(oldnames, avgpaste)

## Remove () from variable names
names(dt_all2) <- gsub("[()]", "", names(dt_all2))


## Replace "-" from variable names and replace with "_" 
names(dt_all2) <- gsub("[-]", "_", names(dt_all2))


## Output final dataset to .txt file, columns separated by space
write.table(dt_all2, file="final_clean_dataset.txt", row.names=F)


## clean up - delete everything except required datasets
keepdata <- c("dt_all", "dt_all2")
rm(list= setdiff(ls(), keepdata))





