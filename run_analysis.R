library(dplyr)
#reading the features list provided in a variable called featureslists
#I changed the name of the zip folder as "project" rather than"UCI HAR Dataset" for my convenience and ease to for accessing the data

featureslists<-read.table("./project/features.txt", col.names = c("no","features"))

#head(featureslists) #over here just checking what feautureslists actucally has and how it can be used 
# over here like the above , reading the activity_labels.txt, subject_train.txt, X_train.txt, y_train.txt in their respective variables as shown below
#y_train contains the activityid, subject_train contains the subjectid and the activitylabel contains the the activityid as well as the name of the activity
activitylabel<-read.table("./project/activity_labels.txt",col.names = c("activityid","activity"))
subject_train<-read.table("./project/train/subject_train.txt",col.names = "subjectid")
xtrain<-read.table("./project/train/X_train.txt")
ytrain<-read.table("./project/train/y_train.txt",col.names = "activityid")

#similarly, following all the same steps of train data for subject_test,xtest and ytest as shown below

subject_test<-read.table("./project/test/subject_test.txt",col.names = "subjectid")
xtest<-read.table("./project/test/X_test.txt")
ytest<-read.table("./project/test/y_test.txt",col.names = "activityid")


#Just checking for the dimensions of the xtrain and xtest in order to provide their columnnames using featureslist();"usually not required just did for for my convenience";
#dim(xtrain)
#dim(xtest)
colnames(xtest)<-featureslists[,2]
colnames(xtrain)<-featureslists[,2]

#merging all the training data to form new1 and test data to form new2 using cbind and forming a dinal dataset new3 in which we have to finally work using rbind
new1<-cbind(subject_train,ytrain,xtrain)
new2<-cbind(subject_test,ytest,xtest)
new3<-rbind(new1,new2)

#Extracting all the data which has std and mean measurements
names<-colnames(new3)
required<-(grepl("subjectid",names)|grepl("activityid",names)|grepl("mean..",names)|grepl("std..",names))
StdMean<-new3[,required == TRUE]

#finally providing the dataframe with the descriptive activity names using the merge function
merged<-merge(StdMean,activitylabel, by = "activityid", all = TRUE)

#Creating a second, independent tidy data set with the average of each variable for each activity and each subject
final<-aggregate(.~ subjectid + activityid,merged,mean)
final<-final[order(final$subjectid,final$activityid),]
final<-select(final,-activityid)


#writing the above tidy data set into a txt file
write.table(final,"final.txt",row.name = FALSE)
