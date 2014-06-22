##This script merges data from a number of .txt files and produces 
## A tidy data set which may be used for further analysis.
## First, load datasets
datafile <- "data.zip"
datadir  <- "UCI HAR Dataset"
if (!file.exists(datafile)){  
fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, datafile)
if (!file.exists(datadir)){ unzip(datafile)}
## Read all required .txt files and label the datasets
if (!file.exists(datadir)){ unzip(datafile)}
train.x<-read.table("./UCI HAR Dataset/train/X_train.txt")
test.x<-read.table(".//UCI HAR Dataset/test/X_test.txt")
train.y<-read.table("./UCI HAR Dataset/train/y_train.txt")
test.y<-read.table("./UCI HAR Dataset/test/y_test.txt")
Subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
Subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
activity_lables<-read.table("./UCI HAR Dataset/activity_labels.txt")
features<-read.table("./UCI HAR Dataset/features.txt")
##Combine the test data and the train data into one dataframe
all.x<-rbind(train.x,test.x)
##Extracts only the measurements refering to mean() or std() values
colnames(all.x) <- c(as.character(features[,2]))
Mean<-grep("mean()",colnames(all.x),fixed=TRUE)
SD<-grep("std()",colnames(all.x),fixed=TRUE)
MeanSD<-all.x[,c(Mean,SD)]
##Uses descriptive activity names to name the activities in the data set.
all.y<-rbind(train.y,test.y)
all.activity<-cbind(all.y,MeanSD)
colnames(all.activity)[1] <- "Activity"
##Add labels
activity_lables[,2]<-as.character(activity_lables[,2])
for(i in 1:length(all.activity[,1])){
  all.activity[i,1]<-activity_lables[all.activity[i,1],2]
##Create the second tidy data set
Subject_all<-rbind(Subject_train,Subject_test)
all<-cbind(Subject_all,all.activity)
colnames(all)[1] <- "Subject"
Tidy <- aggregate( all[,3] ~ Subject+Activity, data = all, FUN= "mean" )
for(i in 4:ncol(all)){  
  Tidy[,i] <- aggregate( all[,i] ~ Subject+Activity, data = all, FUN= "mean" )[,3]
}
colnames(Tidy)[3:ncol(Tidy)] <- colnames(MeanSD)
##Creates a Tidy data set
write.table(Tidy, file= "TidyDataSet.txt")

