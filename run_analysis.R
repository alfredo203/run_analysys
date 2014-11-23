
library(dplyr)

##Reading all tables
subject_test<-tbl_df(read.table("/home/alfredo/Documentos/Cleaning Data week 3/UCI HAR Dataset/test/subject_test.txt"))
subject_train<-tbl_df(read.table("/home/alfredo/Documentos/Cleaning Data week 3/UCI HAR Dataset/train/subject_train.txt"))
x_test<-tbl_df(read.table("/home/alfredo/Documentos/Cleaning Data week 3/UCI HAR Dataset/test/X_test.txt"))
y_test<-tbl_df(read.table("/home/alfredo/Documentos/Cleaning Data week 3/UCI HAR Dataset/test/y_test.txt"))
y_train<-tbl_df(read.table("/home/alfredo/Documentos/Cleaning Data week 3/UCI HAR Dataset/train/y_train.txt"))
x_train<-tbl_df(read.table("/home/alfredo/Documentos/Cleaning Data week 3/UCI HAR Dataset/train/X_train.txt"))
features<-tbl_df(read.table("/home/alfredo/Documentos/Cleaning Data week 3/UCI HAR Dataset/features.txt"))

## 4.1 Appropriately labels the data set with descriptive variable names. 
y_test<-rename(y_test, Activity=V1)
y_train<-rename(y_train, Activity=V1)
subject_test<-rename(subject_test, Subject=V1)
subject_train<-rename(subject_train, Subject=V1)

## 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
search<-grep("mean", features$V2 ) ##Get which has mean
search2<-grep("std", features$V2) ## Get which has std
x_test<- x_test %>% select(c(search), c(search2)) ##select all variables with std and mean
x_train<-x_train %>% select(c(search), c(search2))

## 4.2 Select the names of the important variables (Names of step 2)
names1<- as.data.frame(features$V2[search])
names2<- as.data.frame(features$V2[search2])
names(names2)<-("features$V2[search]")
all.names<-rbind(names1, names2)    ##All column required names as a dataframe
names(all.names)<-("Col")

## 4.3 Input the names of the important variables on the data frames (Input names of step 2 table)
names(x_test)<-(all.names$Col)  ##Rename all columns of x test
names(x_train)<-(all.names$Col) ## Rename all columns of x train


## 1 Test and Train merges on X, Y and Subject (Now with correct names)
x<-rbind(x_train, x_test)
y<-rbind(y_train, y_test)
sub<-rbind(subject_train, subject_test)
xysub<-cbind(sub, y, x)

## 3 Uses descriptive activity names to name the activities in the data set
act<-tbl_df(read.table("/home/alfredo/Documentos/Cleaning Data week 3/UCI HAR Dataset/activity_labels.txt"))
names(act)<-c("Activity", "Name.of.Activity")

left_j<-left_join(act, xysub, by="Activity")

## 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Tidy<-left_j %>% 
  group_by(Name.of.Activity) %>%  
  summarise_each(funs(mean)) %>% 
  arrange(Activity) %>%
  rename(Activity.code=Activity) #%>% 
#select(-matches("Subject"))   #####IMPORTANT SUBJECT VARIABLE MUST BE DELETED, BECAUSE MEAN OF SUBJECT CODE DO NOT HAS RELEVANT INFORMATION
## I LEAVED SUBJECT CODE MEAN TO NOT MISS ANY INFORMATION IN THE ASSIGMENT :)

##Write file
write.table(Tidy, "run_analysis.txt", row.name=FALSE)
