#Read data sets and combine
testData=read.table("~/Downloads/UCI HAR Dataset/test/X_test.txt")
trainData=read.table("~/Downloads/UCI HAR Dataset/train/X_train.txt")
x=rbind(testData,trainData)
#garbage collection
rm(testData)
rm(trainData)
#read subjects and combine
testSub=read.table("~/Downloads/UCI HAR Dataset/test/subject_test.txt")
trainSub=read.table("~/Downloads/UCI HAR Dataset/train/subject_train.txt")
s=rbind(testSub,trainSub)
#garbage collection
rm(testSub)
rm(trainSub)
#read in data labels and combine
testLabel=read.table("~/Downloads/UCI HAR Dataset/test/y_test.txt")
trainLabel=read.table("~/Downloads/UCI HAR Dataset/train/y_train.txt")
y=rbind(testLabel,trainLabel)
#garbage collection
rm(testLabel)
rm(trainLabel)
#read features list
featureslist=read.table("~/Downloads/UCI HAR Dataset/features.txt",stringsAsFactors=FALSE)
features=featureslist$V2
#Logical vector to keep only std and mean columns
keepColumns=grepl("(std|mean[^F])",features,perl=TRUE)
#keep only data we want, and name it human readable
x=x[,keepColumns]
names(x)=features[keepColumns]
names(x)=gsub("\\(|\\)",'',names(x))
names(x)=tolower(names(x))
#read activitiy list
activities=read.table("~/Downloads/UCI HAR Dataset/activity_labels.txt")
activities[,2]=gsub("_","",tolower(as.character(activities[,2])))
y[,1]=activities[y[,1],2]
names(y)="activity"
#add human readable labels to activitiy names
names(s)="subject"
tidyData=cbind(s,y,x)
write.table("tidyData","tidyData.txt")
#create second tidy data set with avg of each var for each act and each sub
us=unique(s)[,1]
ns=length(us)
nA=length(activities[,1])
nC=length(names(tidyData))
td=tidyData[1:(ns*nA),]
row=1
for (s in 1:ns){
	for (a in 1:nA){
		td[row,1]=us[s]
		td[row,2]=activities[a,2]
		tmp=tidyData[tidyData$subject==s &tidyData$activity==activities[a,2],]
		td[row, 3:nC]=colMeans(tmp[,3:nC])
		row=row+1
	}
}
write.table(td,"tidyData2.txt")
