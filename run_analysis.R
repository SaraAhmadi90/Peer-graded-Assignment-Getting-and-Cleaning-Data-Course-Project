library(dplyr)
#reading the feature names for x_train and a_test data
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","feature_names"))
#reading the activity text file
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

## reading train data:
X_Train<- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature_names)
Y_Train<- read.table("UCI HAR Dataset/train/Y_train.txt",col.names = "code")
## reading the test data:
X_Test<- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature_names)
Y_Test<- read.table("UCI HAR Dataset/test/Y_test.txt", col.names = "code")
#reading the subjects for both test and train dataset
Subject_Test <-read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
Subject_Train <-read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

#Part1 - merges train and test data in one dataset (full dataset at the end)
X<-rbind(X_Test, X_Train)
Y<-rbind(Y_Test, Y_Train)
Subject<-rbind(Subject_Test, Subject_Train)
Merged_Data <- cbind(Subject, Y, X)
#Part2
Tidy_Data <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
#part3 decoding the code colum to the original descriptive names in activity text file
Tidy_Data$code <- activities[Tidy_Data$code, 2]
#part4 Replace the names in data set with names from activity labels
names(Tidy_Data)[2] = "activity"
names(Tidy_Data)<-gsub("Acc", "Accelerometer", names(Tidy_Data))
names(Tidy_Data)<-gsub("Gyro", "Gyroscope", names(Tidy_Data))
names(Tidy_Data)<-gsub("BodyBody", "Body", names(Tidy_Data))
names(Tidy_Data)<-gsub("Mag", "Magnitude", names(Tidy_Data))
names(Tidy_Data)<-gsub("^t", "Time", names(Tidy_Data))
names(Tidy_Data)<-gsub("^f", "Frequency", names(Tidy_Data))
names(Tidy_Data)<-gsub("tBody", "TimeBody", names(Tidy_Data))
names(Tidy_Data)<-gsub("-mean()", "Mean", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("-std()", "STD", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("-freq()", "Frequency", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("angle", "Angle", names(Tidy_Data))
names(Tidy_Data)<-gsub("gravity", "Gravity", names(Tidy_Data))
#part5
Final_Data <- Tidy_Data %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(Final_Data, "Final_Data.txt", row.name=FALSE)

