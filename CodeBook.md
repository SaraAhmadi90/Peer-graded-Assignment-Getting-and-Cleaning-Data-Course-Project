#Code Book and the description of the codes

# Peer-graded-Assignment-Getting-and-Cleaning-Data-Course-Project
Dataset is extracted under the folder called UCI HAR Dataset.
#reading the feature names for x_train and x_test data that are available in UCI HAR Dataset folder, i name the first column to n and feature colomns to feature_names 
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","feature_names")) # size 561*2

# reading the activity text file, i named the first column to code, which is the coded version of the activities in second columns that i called activities.(coding the categorical variable "activity")
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity")) #size 6*2

## reading the train data for X(features) and Y (class of activities) to create X_Train and Y_Train variables, againe i named the columns in X_Train "feature_names" from the  features dataset that was created befor and columns in Y_Train to "code")
X_Train<- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature_names) # size 7352* 561  
Y_Train<- read.table("UCI HAR Dataset/train/Y_train.txt",col.names = "code")  # size 7352* 1
## reading the test data: the above-mentioned process is repeated for test data set as well to create X_Test and Y_Test variables
X_Test<- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature_names) # size 2947*561 
Y_Test<- read.table("UCI HAR Dataset/test/Y_test.txt", col.names = "code") # size 2947*1
# reading the subjects for both test and train dataset to create Subject_Test and Subject_Train variables , i called the column names to subject, this column contains numbers from 1 to 30 that is the coded version of who performed those activities
Subject_Test <-read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject") # size 2947 * 1 
Subject_Train <-read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject") # size 7352 * 1  
# Part1 - merges train and test data in one dataset (full dataset at the end)
# The rbind function combines the two dataset X_Test and X_Train into X, the two dataset Y_Test and Y_Train into Y, and Subject_Test and Subject_Train to Subject by rows
X<-rbind(X_Test, X_Train) # size 10299 * 561 
Y<-rbind(Y_Test, Y_Train) # size 10299 * 1
Subject<-rbind(Subject_Test, Subject_Train) # size 10299 * 1
# the final merged data set is combined by rows from Subject, Y and X datasets.
Merged_Data <- cbind(Subject, Y, X) # size 10299*563 

# part2:select function keeps only the variables we mention after the first argumnet which is subject and subsets the merged_Data dataset to extract only the measurements on the mean and standard deviation and the i assigned it to Tidy_Data variable.
Tidy_Data <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
# part4: Replace the names in data set with names from activity labels:
# all the numbers in code column of the Tidy_Data replaced with corresponding activity taken from second column of the  activities variable by gsub function which performs replacement of the first and all matches respectively
# code column (second column) in Tidy_Data was renamed into activities
names(Tidy_Data)[2] = "activity" 
# All Acc in column’s name replaced by Accelerometer
names(Tidy_Data)<-gsub("Acc", "Accelerometer", names(Tidy_Data)) # size 10299 * 88 
# All Gyro in column’s name replaced by Gyroscope
names(Tidy_Data)<-gsub("Gyro", "Gyroscope", names(Tidy_Data))
# All BodyBody in column’s name replaced by Body
names(Tidy_Data)<-gsub("BodyBody", "Body", names(Tidy_Data))
# All Mag in column’s name replaced by Magnitude
names(Tidy_Data)<-gsub("Mag", "Magnitude", names(Tidy_Data)) 
# All ^t in column’s name replaced by Tim
names(Tidy_Data)<-gsub("^t", "Time", names(Tidy_Data)) 
# All ^f in column’s name replaced by Frequency
names(Tidy_Data)<-gsub("^f", "Frequency", names(Tidy_Data)) 
# All tBody in column’s name replaced by TimeBody
names(Tidy_Data)<-gsub("tBody", "TimeBody", names(Tidy_Data)) 
# All -mean() in column’s name replaced by Mean
names(Tidy_Data)<-gsub("-mean()", "Mean", names(Tidy_Data), ignore.case = TRUE)
# All -std() in column’s name replaced by STD
names(Tidy_Data)<-gsub("-std()", "STD", names(Tidy_Data), ignore.case = TRUE) 
# All -freq in column’s name replaced by Frequency
names(Tidy_Data)<-gsub("-freq()", "Frequency", names(Tidy_Data), ignore.case = TRUE) 
# All angel in column’s name replaced by Angle
names(Tidy_Data)<-gsub("angle", "Angle", names(Tidy_Data))
# All gravity in column’s name replaced by Gravity
names(Tidy_Data)<-gsub("gravity", "Gravity", names(Tidy_Data))  
# part5:Final_Data is created  Tidy_Data is summerized by taking the means of each variable for each activity and each subject, after being groupped by subject and activity by group_by function and final_Data is created and written by write.table function (the whole process is done by pipeline technique).
Final_Data <- Tidy_Data %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(Final_Data, "Final_Data.txt", row.name=FALSE) # size 180 *88 

