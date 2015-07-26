## Merges the training and the test sets to create one dataset

train_data <- read.table("./train/X_train.txt") 
train_label <- read.table("./train/y_train.txt")
table(train_label)
train_subject <- read.table("./train/subject_train.txt")
test_data <- read.table("./test/X_test.txt")
test_label <- read.table("./test/y_test.txt") 
table(test_label) 
test_subject <- read.table("./test/subject_test.txt")
## join train and test data,label,subject 
joinData <- rbind(train_data, test_data)
dim(join_data)
join_label <- rbind(train_label, test_label)
dim(join_label) 
join_subject <- rbind(train_subject, test_subject)
dim(join_subject) 

## problem : 2 Extracts only the measurements on the mean and standard 

features <- read.table("./features.txt")
dim(features) 
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) 
join_data <- join_data[, meanStdIndices]
dim(join_data) 
names(join_data) <- gsub("\\(\\)", "", features[meanStdIndices, 2])
names(join_data) <- gsub("mean", "Mean", names(join_data)) 
names(join_data) <- gsub("std", "Std", names(join_data)) 
names(join_data) <- gsub("-", "", names(join_data))  

##prob3
activity <- read.table("./activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[join_label[, 1], 2]
join_label[, 1] <- activityLabel
names(join_label) <- "activity"

##PROBLEM 4
names(join_subject) <- "subject"
cleanedData <- cbind(join_subject, join_label, join_data)
## merge whole data and write withing merge_data.txt
write.table(cleanedData, "merged_data.txt")


##last

subjectLen <- length(table(join_subject)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    resulst_data[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
result_data
write.table(result_data, "avg_data.txt")