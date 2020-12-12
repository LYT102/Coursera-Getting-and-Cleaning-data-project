library(dplyr)
library(data.table)
library(stringr)

#Reading data from text file
dirsubjecttest<-"./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt"
dirsubjecttrain<-"./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt"

dirfeaturetest<-"./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt"
dirfeaturetrain<-"./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt"
dirfeatureinfo<-"./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt"

diractivitytest<-"./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt"
diractivitytrain<-"./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt"
diractivitylabels<-"./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt"

#Read text file data to table
subject_data_test<-read.table(dirsubjecttest)
subject_data_train<-read.table(dirsubjecttrain)

feature_data_test<-read.table(dirfeaturetest, header=FALSE)
feature_data_train<-read.table(dirfeaturetrain, header = FALSE)
feature_info_data<-read.table(dirfeatureinfo)

activity_data_test<-read.table(diractivitytest)
activity_data_train<-read.table(diractivitytrain)
activity_labels_data<-read.table(diractivitylabels)

###################
#Step 1 Merge Training and Test Table.

#row bind for subject data, feature data, and activity data
full_subject_data<-rbind(subject_data_test,subject_data_train)
full_feature_data<-rbind(feature_data_test,feature_data_train)
full_activity_data<-rbind(activity_data_test,activity_data_train)

#capture the name from feature info data
feature_header_names<-feature_info_data[,2]

#name for all the data set (feature,subject,and activity)
colnames(full_feature_data)<-feature_header_names
colnames(full_subject_data)<-c("SubjectID")
colnames(full_activity_data)<-c("Activity")

#create addition information on full data to identify test and training set
category_data<-data.frame("category" = c(1:10299))
category_data$category[1:2947]<-"test"
category_data$category[2948:10299]<-"train"


#Merge the full data with column bind for all data set
#Complete Step 1
full_data<-cbind(full_subject_data,full_activity_data,category_data,
                 full_feature_data)

######################
#Step 2 Extracts only the measurements on the mean and standard deviation for each measurement.

#Identify the pattern column names of full data using grep
colname<-grep("mean..|std..|Subject|Activity",colnames(full_data))

#create mean and std tidy data by the selected column names pattern from full data
#complete step 2
mean_std_data<-full_data[,colname]

######################
#step 3 Uses descriptive activity names to name the activities in the data set

#replace activity value with named factor levels
#complete step 3
mean_std_data$Activity<-factor(mean_std_data$Activity,
                                 levels = activity_labels_data[,1],
                                 labels = activity_labels_data[,2])


######################
#step 4 Appropriately labels the data set with descriptive variable names.

#getting the column names of mean and std tidy data to variable newcolname
newcolname<-colnames(mean_std_data)

#remove the pattern from the column name
newcolname<-gsub("//(//)","",newcolname)
newcolname<-gsub("//-","",newcolname)

#rename the column name properly
newcolname<-gsub("^t","TimeDomain",newcolname)
newcolname<-gsub("Freq","Frequency",newcolname)
newcolname<-gsub("^f","Frequency",newcolname)
newcolname<-gsub("Acc","Accelerometer",newcolname)
newcolname<-gsub("Gyro","Gyroscope",newcolname)
newcolname<-gsub("mean","Mean",newcolname)
newcolname<-gsub("std","StandardDeviation",newcolname)
newcolname<-gsub("Mag","Magnitude",newcolname)

#remove typo error by original data and modify name
newcolname<-gsub("BodyBody","Body",newcolname)
newcolname<-gsub("Frequencyuency","Frequency",newcolname)

#set the new name to mean and std tidy data
#complete step 4
colnames(mean_std_data)<-newcolname


######################
#Step 5 Creating final tidy data set

#group SubjectID with Activity from mean and std tidy data and set to variable tidydata
tidydata<-group_by(mean_std_data,SubjectID,Activity)
#summarise each varaible in tidydata with mean 
#complete step 5
tidydata<-summarise_each(tidydata, funs = mean)


#Extra tidy data set to txt format 
write.table(tidydata, file="./TidyDataSet.txt",row.names = FALSE)

