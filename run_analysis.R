library(data.table)
library(dplyr)

##Import train data set
Train<-read.table(paste(c(getwd(),"train","X_train.txt"),collapse="/"),header=FALSE)
Train<-data.table(Train)

##Import train data set labels
Labels<-read.csv(paste(c(getwd(),"features.txt"),collapse="/"),header=FALSE,sep=" ")
names(Train)<-as.character(Labels$V2)

##Import Activity labels and make them self-descriptive
Activity<-read.table(paste(c(getwd(),"train","y_train.txt"),collapse="/"),header=FALSE)
ActivityLab<-read.table(paste(c(getwd(),"activity_labels.txt"),collapse="/"),header=FALSE)
ActivityLab<-ActivityLab$V2
ActivityLab<-as.character(ActivityLab)
DescriptiveF<-function(i) {return(ActivityLab[Activity$V1[i]])} 
ActDescription<-sapply(1:nrow(Activity),DescriptiveF)
Train<-Train[,ActivitlyLabels:=ActDescription]

##Import and append subject labels to Train data set
Subject<-read.table(paste(c(getwd(),"train","subject_train.txt"),collapse="/"),header=FALSE)
Train<-Train[,SubjectLabel:=Subject]

##Import test data set
Test<-read.table(paste(c(getwd(),"test","X_test.txt"),collapse="/"),header=FALSE)
Test<-data.table(Test)
names(Test)<-as.character(Labels$V2)

##append activity labels and subject labels to Test data set
Activity2<-read.table(paste(c(getwd(),"test","y_test.txt"),collapse="/"),header=FALSE)
DescriptiveF2<-function(i) {return(ActivityLab[Activity2$V1[i]])} 
ActDescription2<-sapply(1:nrow(Activity2),DescriptiveF2)
Test<-Test[,ActivitlyLabels:=ActDescription2]
Subject2<-read.table(paste(c(getwd(),"test","subject_test.txt"),collapse="/"),header=FALSE)
Test<-Test[,SubjectLabel:=Subject2]

##merge Test and Train data sets
Data<-rbind(Train,Test,use.names=TRUE)

##Subset Data to obtain data set wit only mean and standard deviation for each measurement
duplicates<-names(Data)[duplicated(names(Data))]
nonduplicates<-names(Data)[!names(Data)%in%duplicates]
Data2<-subset(Data,select=nonduplicates)
Data3<-select(Data2,contains("mean"))
Data4<-select(Data3,-contains("meanFreq"))
Data5<-select(Data4,-contains("angle"))
Data6<-select(Data2,contains("std"))
Data7<-cbind(Data5,Data6)
DataFinal<-cbind(Data$SubjectLabel,Data$ActivitlyLabels,Data7)
DataFinal<-rename(DataFinal,SubjectLabel=V1)
DataFinal<-rename(DataFinal,ActivityLabel=V2)
DataFinal<-arrange(DataFinal,SubjectLabel)

##Create second tidy data set with average of each variable for each activity and each subject
DataFinal<-data.table(DataFinal)
GroupedData<-DataFinal[,lapply(.SD,mean),by=list(SubjectLabel,ActivityLabel)]