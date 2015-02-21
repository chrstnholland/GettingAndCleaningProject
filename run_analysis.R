## loading the requisite data, includes substituting indexed column names with meaningful names (Course Project Acceptance Criteria #4)
featuresDescriptor <- read.table("features.txt", sep = " " , header = FALSE)
colnames(featuresDescriptor) <- as.character(c("feature", "featureName"))
activityDescriptor <- read.table("activity_labels.txt", sep = " " , header = FALSE, col.names = c("activityIndex", "activity"))
df1 <- read.table("X_train.txt", sep = "", header = FALSE, col.names = make.names(featuresDescriptor[, 2]))
activityIndexLabelsTrain <- read.table("y_train.txt", sep = "\n", header = FALSE)
df2 <- read.table("X_test.txt", sep = "", header = FALSE, col.names = make.names(featuresDescriptor[, 2]))
activityIndexLabelsTest <- read.table("y_test.txt", sep = "\n", header = FALSE)
subjectLabelsTrain <- read.table("subject_train.txt", header = FALSE)
subjectLabelsTest <- read.table("subject_test.txt", header = FALSE)

## dropping all columns that do not contain mean or std values (Course Project Acceptance Criteria #2)
## alert! if you do not ignore the case you will miss 7 of the mean & std variables and only have 81 columns in your resulting dataset)
## the column names in the originating dataset have "Mean", "mean & "std"
findColumnsOfMeanValues <- grep("mean", ignore.case = TRUE, featuresDescriptor[, 2])
findColumnsOfStdValues <- grep("std", ignore.case = TRUE, featuresDescriptor[, 2])
keeps <- sort(append(findColumnsOfMeanValues, findColumnsOfStdValues))
df1 <- df1[, keeps]
df2 <- df2[, keeps]

## merging the subject label index to the observations (aka adding the participant info, a requirement for Course Project Acceptance Criteria #5)
df1$activityIndex <- as.factor(activityIndexLabelsTrain[, 1])
df1$subject <- as.factor(as.numeric(subjectLabelsTrain[, 1]))
df2$activityIndex <- as.factor(activityIndexLabelsTest[, 1])
df2$subject <- as.factor(as.numeric(subjectLabelsTest[, 1]))

## merging the test & training datasets into one (Course Project Acceptance Criteria #1)
df <- rbind(df1, df2)

## substituting descriptive activity names for the activity index number (Course Project Acceptance Criteria #3)
df <- merge(df, activityDescriptor, by.x = "activityIndex", by.y = "activityIndex")
df <- df[, 2:89]

##at this point the course project is complete through #4
## where the current value of the datafrome df = the data set where:
## 1. Merges the training and the test sets to create one data set
## 2. Extracts only the measurements on the mean and standard deviation for each measurement
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 

## now to make the tidy data frame to satisfy Acceptance Criteria #5

dfTidy<- data.frame()
subjectsList <- sort(as.numeric(unique(df$subject)))
subjectsListSequencer <- as.numeric(c(1:length(subjectsList)))
activityList <- sort(as.character(unique(df$activity)))
activityListSequencer <- as.numeric(c(1:length(activityList)))

for (each in seq_along(subjectsList)) {
  
    for (picker in seq_along(activityListSequencer)) {

      recordLocator <- (df$subject == each) & (df$activity == activityList[picker])
      workerDF <- df[recordLocator, 1:86]
      result <- lapply(workerDF, mean)
##      addingSubjectListItem <- c(subject = subjectsList[each], activity = activityList[picker])
##      result <- append(result, addingSubjectListItem)
      dfTidy <- rbind(dfTidy, result)
      
    }
  
}

## adding labels to the tidy data frame 
tidyLabels <- data.frame(subject = as.factor(as.numeric(sort(rep(1:30, length(activityList))))), activity = as.factor(rep(activityList, length(subjectsList))))
dfTidy <- cbind(tidyLabels, dfTidy)

## removing residual row names for the truly tidy
row.names(dfTidy) <- c(1:nrow(dfTidy))

## writing .txt file of tidy data fram
write.table(dfTidy, file = "tidyDataFrame.txt", row.name = FALSE)

##bBbBbBbBbB... that's all folks! dfTidy is the tidy data frame specified in Acceptance Criteria #5 and has been saved to .txt file