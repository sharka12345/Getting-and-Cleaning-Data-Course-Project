# run_analysis.R
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Check for required libraries
if (!"reshape2" %in% installed.packages()) {
  install.packages("reshape2")
}
library("reshape2")

#Read the Files
X_test <- read.table('./test/X_test.txt')
y_test <- read.table('./test/y_test.txt')

X_train <- read.table('./train/X_train.txt')
y_train <- read.table('./train/y_train.txt')

subject_test <- read.table('./test/subject_test.txt')
subject_train <- read.table('./train/subject_train.txt')

labels <- read.table('activity_labels.txt')
features <- read.table('features.txt')

# 1. Merge training and test sets to a single data set
data <- rbind(X_train,X_test)
subject_data <- rbind(subject_train, subject_test)
activity_data <- rbind(y_train, y_test)

#2. Extract only mean and sd measurements
msd <- grep("mean() | std()", features[,2])
data <- data[,msd]

# 3. and 4. Descriptions and Labels
tidied_names <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(data) <- tidied_names[msd]
names(subject_data) <- "Subject"
names(activity_date) <- "Activity"

# Combine data
data <- cbind(subject_data, activity_data, data)

#5. Create Tidy Data Set
# Uses reshape2
coredata <- melt(data,(id.vars=c("Subject","Activity")))
data2 <- dcast(coredata, Subject + Activity ~ variable, mean)
names(data2)[-c(1:2)] <- paste("[mean of]" , names(data2)[-c(1:2)] )
write.table(data2, "tidied_average_data.txt", sep = ",")

