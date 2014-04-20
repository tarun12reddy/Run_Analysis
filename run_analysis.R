
#Path of current directory
dir <- getwd()

#Download the zip file from HTTP server as zip file
#download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", paste(dir, "/UCI HAR Dataset.zip", sep = ""), method = "curl")

#Unzip the zip file and save it in current directory
#unzip("~/Desktop/Dropbox/Coursera/Data Science Specialization/Course 3 - Getting and Cleaning Data/UCI HAR Dataset.zip", exdir = dir)

#Reading data from within the directory
setwd(paste(dir, "/UCI HAR Dataset", sep = ""))
activity_labels <- read.table("activity_labels.txt", stringsAsFactors = FALSE)
features <- read.table("features.txt")

#Reading data from the test directory
setwd(paste(dir, "/UCI HAR Dataset/test", sep = ""))
test_Y <-  read.table("y_test.txt")
test_X <-  read.table("X_test.txt", col.names = features$V2)

#Reading data from the train directory
setwd(paste(dir, "/UCI HAR Dataset/train", sep = ""))
train_X <-  read.table("X_train.txt", col.names = features$V2)
train_Y <-  read.table("y_train.txt")


#Moving to current directory because the R script is in current directory
setwd(dir)

#Problem 1: Combining train and test data bases
totaldata <- rbind(test_X, train_X)

#Problem2: Extracting only the mean and std columns
mean <- "mean()"
std <- "std()"
#checking for mean() and std() in names of the columnns of test and train
test_dataset <- c(grep(mean,names(test_X)), grep(std, names(test_X)))
train_dataset <- c(grep(mean,names(train_X)), grep(std, names(train_X)))
total_Y <- rbind(test_Y, train_Y)
totaldata_mean_std <- rbind(test_X[test_dataset], train_X[train_dataset])

#a <- merge(total_Y, activity_labels, by = "V1", sort = "FALSE", all.x = "TRUE")

#Problem3: Attaching a column with the acitivity labels to the above data
for (i in 1: length(total_Y[, 1])){
  for (j in 1: length(activity_labels[, 1])){
       if (total_Y[i, 1] == activity_labels[j, 1]){
           total_Y[i, 2] <- activity_labels[j, 2]
}
}
}

labeled_data <- cbind(totaldata_mean_std, total_Y[, 2])

names(labeled_data)[80] <- paste("labels")

#Problem4: Calculating the average for each of the labels for each of the subject
final.labels <- c("LAYING","SITTING","STANDING","WALKING", "WALKING_DOWNSTAIRS","WALKING_UPSTAIRS")
output <- matrix(nrow = 79, ncol = 6)
for (i in 1:79){
  output[i, 1:6] <- tapply(as.numeric(labeled_data[, i]), as.factor(labeled_data[, 80]), FUN = "mean", na.rm = "TRUE")
}
final.output <- output[1:79, 1:6]
final.subjects <- as.character(matrix(nrow = 79, ncol=1))
for (i in 1: 79){
final.subjects[i] <- names(totaldata_mean_std)[i]
}
#Gives the output of average matrix along with row names and column names in seperate text files
write.table(final.output, "~/Desktop/Dropbox/Coursera/Data Science Specialization/Course 3 - Getting and Cleaning Data/final.output.txt", sep="\t")
write.table(final.subjects, "~/Desktop/Dropbox/Coursera/Data Science Specialization/Course 3 - Getting and Cleaning Data/final.subjects.txt", sep="\t")
write.table(final.labels, "~/Desktop/Dropbox/Coursera/Data Science Specialization/Course 3 - Getting and Cleaning Data/final.labels.txt", sep="\t")