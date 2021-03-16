library(tidyverse)
setwd("../Desktop/UCI HAR Dataset/")

#First I created 3 objects: activity, subject and features in each object are the test and train data,
#merge with rbind
activity<-rbind(read.table(file.path("test", "Y_test.txt"),header = F),
                read.table(file.path("train","Y_train.txt"),header = F))
subject<-rbind(read.table(file.path("test", "subject_test.txt"),header = F),
               read.table(file.path("train", "subject_train.txt"),header = F))
features<-rbind(read.table(file.path("test", "X_test.txt"),header = F),
                read.table(file.path("train", "X_train.txt"),header = F))

###Then I merged these 3 objects in one data frame call all.data
all.data<-data.frame(features, subject,activity)

##Secondly I name the columns of all.data. first i saved the features.txt file in the object feat
feat<-read.table(file.path("features.txt"),head=FALSE)

#Then, I used feat to rename the columns, finally, i added the names "subject" and "activity"
colnames(all.data)<-c(feat$V2,"subject","activity")


#After that, i select only the variables with "mean" and "std" in the names of feat object
#these names are saved in names.feat object
names.feat<-c(feat$V2[grep("mean\\(\\)|std\\(\\)",feat$V2)],"subject","activity")

#I created an object only with the variables of all.data, that are exactly the names saved in names.feat
select.data<-select(all.data,names.feat)


#The next step is rename the column activity. The labels of the activities, are in labels.act

labels.act<- read.table(file.path("activity_labels.txt"),header = FALSE)

#The I used a for loop to evaluate each number in the column activity and replaced with the name
#of the activity saved in labels.act
for (i in 1:length(labels.act$V1)){
select.data[which(select.data$activity==labels.act[i,1]),68]<-labels.act[i,2]
}


##Next step is put a descriptive name to all variables. So i used the gsub function to search
#a pattern an replace it in each case:
#this replaces i made in the names.feat object
names.feat<-gsub("^t", "time", names.feat)
names.feat<-gsub("^f", "frequency", names.feat)
names.feat<-gsub("Acc", "Accelerometer", names.feat)
names.feat<-gsub("Gyro", "Gyroscope", names.feat)
names.feat<-gsub("Mag", "Magnitude", names.feat)
names.feat<-gsub("BodyBody", "Body",names.feat)

#When the labels are appropriately renamed, I used it for rename the colnames for select.data object
colnames(select.data)<-c(names.feat)

##Finally, I calculate the mean for each subject for each activity and 
#order the columns subject and activitu
data.agre<-aggregate(.~subject+activity,select.data,mean)
tidy.data<-data.agre %>% arrange(subject,activity)

##And I used the data frame tidy.data to write a txt file
write.table(tidy.data,"tidydata.txt",row.names = F)
