##install and load packages needed -> "data.table" and "reshape2"
if(!library(data.table, logical.return = TRUE))
{
        install.packages("data.table")
        library(data.table)
}
if(!library(reshape2, logical.return = TRUE))
{
        install.packages("reshape2")
        library(reshape2)
}

##download and unzip data from given zip file
path<-getwd()
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,file.path(path,"dataFiles.zip"))
unzip(zipfile="dataFiles.zip")


## Part 1: Merges the training and the test sets to create one data set.

## a. Read in all data (test, train, features, and activity labels)

## Main Folder file path
filePath<-file.path(path,"UCI HAR Dataset")

##Test
testData<-read.table(file.path(filePath,"test","X_test.txt",fsep="/"))
testActivities<-read.table(file.path(filePath,"test","y_test.txt",fsep="/"))
testSubjects<-read.table(file.path(filePath,"test","subject_test.txt",fsep="/"))

##Train
trainData<-read.table(file.path(filePath,"train","X_train.txt",fsep="/"))
trainActivities<-read.table(file.path(filePath,"train","y_train.txt",fsep="/"))
trainSubjects<-read.table(file.path(filePath,"train","subject_train.txt",fsep="/"))

##Features
allFeatures<-read.table(file.path(filePath,"features.txt",fsep="/"))

##Activity Labels
activityLabels<-read.table(file.path(filePath,"activity_labels.txt",fsep="/"))


## b. Assign column names before binding
colnames(testData)<-allFeatures[,2]
colnames(testActivities)<-"activityNumber"
colnames(testSubjects)<-"subjectNumber"

colnames(trainData)<-allFeatures[,2]
colnames(trainActivities)<-"activityNumber"
colnames(trainSubjects)<-"subjectNumber"

colnames(activityLabels)<-c("activityNumber","activityType")

## c. Bind the columns of the x_test,y_test,and subject_test (same for train data)
bindTest<-cbind(testSubjects,testActivities,testData)
bindTrain<-cbind(trainSubjects,trainActivities,trainData)

## d. Bind the rows of the test and train data
fullData<-rbind(bindTest,bindTrain)



## Part 2: Extracts only the measurements on the mean and standard deviation for each measurement.
targetFeatures<-allFeatures[grep("-(mean|std)\\(\\)",allFeatures[,2]),2]
fullData<-fullData[, c(1, 2, targetFeatures)]



## Part 3: Uses descriptive activity names to name the activities in the data set
activityLabels[,2]<-as.character(activityLabels[,2])
colnames(fullData)<-c("subject","activity",gsub("\\-|\\(|\\)","",as.character(targetFeatures)))

fullData$activity<-factor(fullData$activity,levels=activityLabels[,1],labels=activityLabels[,2])
fullData$subject<-as.factor(fullData$subject)



## Part 4: Appropriately labels the data set with descriptive variable names
colnames(fullData)<-c("subject","activity",gsub("\\-|\\(|\\)","",as.character(targetFeatures)))
fullData[,2]<-as.character(fullData[,2])


## Part 5: Create a second, independent tidy data set with the avg of each variable for each activity and each subject.
melted<-melt(fullData,id=c("subject","activity"))
finalMean<-dcast(melted,subject + activity ~ variable,mean)

write.table(finalMean,file=file.path("tidy.txt"),row.names=FALSE,quote=FALSE)




