library(tidyverse)
library(downloader)
library(data.table)

#Download file to data directory and and unzip file 
getwd()
if(!file.exists("data")){dir.create("data")}
fileUrl<-("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
download(fileUrl, dest="./data/assignment3.zip", mode="wb")
unzip (zipfile = "./data/assignment3.zip", exdir = "./data")

#Read in variable names, convert them to characters
testxvariables<-read.table("./data/UCI HAR Dataset/features.txt", sep = "")
variables<-variables %>% pull(2)
variables<-as.character(variables)

#Read in test dataset and assign variable names to column names
testx <- read.table("./data/UCI HAR Dataset/test/X_test.txt", sep = "", col.names = variables)

#Extracts only the measurements on the mean and std deviation for each measurement
testx <- testx %>% select(matches('mean|std', ignore.case = TRUE))

#Read in activity and rename them
testy <- read.table("./data/UCI HAR Dataset/test/Y_test.txt", sep = "", col.names=c("Activity"))
testy<-testy %>% mutate(Activity = ifelse(Activity==1, "walking", 
                                          ifelse(Activity==2, "walking_upstairs", 
                                                 ifelse(Activity==3, "walking_downstairs",
                                                        ifelse(Activity == 4, "sitting",
                                                               ifelse(Activity==5, "standing","laying"))))))

#Read in subject identifiers
subject_test<-read.table("./data/UCI HAR Dataset/test/subject_test.txt", sep = "", col.names = c("Subject"))

#Bind subject identifiers, activities, and measurements into one dataframe
testssubyx<-cbind(subject_test, testy, testx)


#Repeat the above for the training dataset
trainx <- read.table("./data/UCI HAR Dataset/train/X_train.txt", sep = "", col.names = variables)
trainx<-trainx %>% select(matches('mean|std', ignore.case = TRUE))
trainy <- read.table("./data/UCI HAR Dataset/train/Y_train.txt", sep = "", col.names = c("Activity"))
trainy<-trainy %>% mutate(Activity = ifelse(Activity==1, "walking", 
                                            ifelse(Activity==2, "walking_upstairs", 
                                                   ifelse(Activity==3, "walking_downstairs",
                                                          ifelse(Activity == 4, "sitting",
                                                                 ifelse(Activity==5, "standing","laying"))))))
subject_train<-read.table("./data/UCI HAR Dataset/train/subject_train.txt", sep = "", col.names = c("Subject"))
trainsubyx<-cbind(subject_train, trainy, trainx)


#Combine the training and test datasets
complete_dataset<-bind_rows(trainsubyx, testssubyx)

#Arrange by subject and activity
complete_dataset<-complete_dataset %>% arrange(Subject, Activity) 

#Calculate the average of each variable for each activity and each subject, added 'mean' to each variable name to
#indicate that this is a mean
Summary_complete_dataset <- complete_dataset %>% group_by(Subject, Activity) %>% summarise_all(list(mean=mean))
write.table(Summary_complete_dataset, file ="./data/UCI HAR Dataset/step5dataset.txt", row.names = FALSE)
