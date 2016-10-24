#Loads the data.table library.
##Downloads the raw data from the cloudfront.net site and extract the zip file to the working directory.
library(data.table)
fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists('./UCI HAR Dataset.zip')){
        download.file(fileurl,'./UCI HAR Dataset.zip', mode = 'wb')
        unzip("UCI HAR Dataset.zip")
}
#Read the text files and create variables that contain the raw trainging data. Then create a data frame of the raw data. 
##Variable names were created to match as accuaratly as possible the variables in the raw data. 
##The idea is to maintain consistency during per review or replication in real world situations.
features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])

X.train <- read.table('./UCI HAR Dataset/train/X_train.txt')
Y.train <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
Subject.train <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

training.data <-  data.frame(Subject.train, Y.train, X.train)
names(training.data) <- c(c('subject', 'activity'), features)

X.test <- read.table('./UCI HAR Dataset/test/X_test.txt')
Y.test <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
Subject.test <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

test.data <-  data.frame(Subject.test, Y.test, X.test)
names(test.data) <- c(c('subject', 'activity'), features)
#Merges training.data and test.data variables to create one data set called tt.merged.data
tt.merged.data <- rbind(test.data, training.data)
#Creates a variable called mean.sd which is the output of the grep search function for the mean and standard deviation for each column measurement?
##The mean.sd.data variable inserts the mean and standard deviation in the tt.merged.data variable.
mean.sd <- grep('mean|std', features)
mean.sd.data <- tt.merged.data[,c(1,2,mean.sd + 2)]
#Descriptive activity names are applied to name the activities in the new data set.
activity.labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
activity.labels <- as.character(activity.labels[,2])
mean.sd.data$activity <- activity.labels[mean.sd.data$activity]
#Labels the data set with descriptive variable names using the gsub command.
name.new <- names(mean.sd.data)
name.new <- gsub("[(][)]", "", name.new)
name.new <- gsub("^t", "TimeDomain_", name.new)
name.new <- gsub("^f", "FrequencyDomain_", name.new)
name.new <- gsub("Acc", "Accelerometer", name.new)
name.new <- gsub("Gyro", "Gyroscope", name.new)
name.new <- gsub("Mag", "Magnitude", name.new)
name.new <- gsub("-mean-", "_Mean_", name.new)
name.new <- gsub("-std-", "_StandardDeviation_", name.new)
name.new <- gsub("-", "_", name.new)
names(mean.sd.data) <- name.new
#The tidy.data variable addresses the requirments for tidy data of this particular data set which has now added the mean and standard deviation.
##Finall, the table is written to a text file called tiday_data.
tidy.data <- aggregate(mean.sd.data[,3:81], by = list(activity = mean.sd.data$activity, subject = mean.sd.data$subject),FUN = mean)
write.table(x = tidy.data, file = "tidy_data.txt", row.names = FALSE)