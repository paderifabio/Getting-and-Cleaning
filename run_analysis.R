#### Getting and Cleaning Data ####

# Final project #

# Fabio Paderi, 2017-10-11 

library(dplyr)
library(reshape2)

# create directory
if(!file.exists("data")) {
    dir.create("data")
    }
# download file
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile = "data/UCI HAR Dataset.zip")

# unzip and setting the right directory
unzip("data/UCI HAR Dataset.zip", exdir = "./data")
setwd("./data")

# load data
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
activitylables <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

# create a train and a test dataset
train <- cbind(subject_train, y_train, x_train)
test <- cbind(subject_test, y_test, x_test)

# merge the training and the test sets to create one data set.
dataset <- rbind(train, test)

# adding features names

feature_names <- as.vector(features[, 2]) 
colnames(dataset) <- c("id", "activity_labels", feature_names)

# replace activity codes with activity lables

activity_names <- as.vector(activitylables[, 2])
dataset$activity_labels <- factor(dataset$activity_labels, 
                                  levels = c(1:6),
                                  labels = activity_names)

# extract only the measurements on the mean and standard deviation for each measurement.

col_index <- grep("id|activity|mean|std", names(dataset))

newdata <- dataset[, col_index]

newdata <- select(newdata, -contains("Freq"))

# tidy variable names

names(newdata) <- gsub(pattern = "\\(\\)", replacement = "", x = names(newdata))
names(newdata) <- gsub(pattern = "-", replacement = "_", x = names(newdata))
names(newdata) <- gsub(pattern = "BodyBody", replacement = "Body", x = names(newdata))

# creating the new dataset with the average of each variable for each activity and each subject.

melt_data <- melt(newdata, id.vars = c("id", "activity_labels"))
mean_data <- dcast(melt_data, formula = id + activity_labels ~ variable, fun.aggregate = mean)

# save the dataset

write.table(mean_data, file = "tidy_dataset.txt")






