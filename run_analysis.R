## 1.Merges the training and the test sets to create one data set
## 2.Extracts only the measurements on the mean and standard deviation for each measurement
## 3.Uses descriptive activity names to name the activities in the data set
## 4.Appropriately labels the data set with descriptive variable names
## 5.From the data set in step 4, creates a second, independent tidy data set with the 
## average of each variable for each activity and each subject.

library(data.table)

# Read and merge data from UCI HAR Dataset and set variablenames
DT <- data.table( rbind( read.table('data//UCI HAR Dataset//train//X_train.txt'),
                         read.table('data//UCI HAR Dataset//test//X_test.txt') ) )

DT.names <- read.table('data//UCI HAR Dataset//features.txt', colClasses = "character")[,2]
setnames(DT,names(DT),DT.names)

subject <- data.table( subject=rbind( read.table('data//UCI HAR Dataset//train//subject_train.txt'),
                         read.table('data//UCI HAR Dataset//test//subject_test.txt') ) )
setnames(subject,names(subject),"subject")

activity <- data.table( activity=rbind( read.table('data//UCI HAR Dataset//train//y_train.txt'),
                         read.table('data//UCI HAR Dataset//test//y_test.txt') ) )
setnames(activity,names(activity),"activity")
activity$activity <- as.factor(activity$activity)
activity_labels <- read.table('data//UCI HAR Dataset//activity_labels.txt')
levels(activity$activity)<-activity_labels[[2]]

# Extract Mean and Std variables
msv <- grep("mean()|std()",DT.names) 
DT2 <- cbind(subject,activity,DT[,msv,with=False)]) # Notice with = FALSE to get colums by nr
# Tidy variable names
tidynames <- gsub("\\(\\)|\\-","",tolower(names(DT2)))
setnames(DT2,names(DT2),tidynames)

# Aggregate
agg <- aggregate(DT2[,3:81,with=FALSE],by=list(activity=DT2$activity,subject=DT2$subject),FUN=mean,na.rm=TRUE)

# Write tidy aggregated dataset to file
write.table(agg,file="aggregateducihar.txt",row.name=FALSE)
