run_analysis.R <- function(){
  ##code to read the output file back into R:
  ##Output<-read.table("GettingCleaningDataProjectOutput.txt", header=TRUE,stringsAsFactors = FALSE)
  
  ##Read subject_test.txt adn subject_train.txt using read.table. Name the variable "SubjectID". 
  subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt", 
                            col.names = "SubjectID")
  subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", 
                             col.names = "SubjectID")
  
  ##Read activity_labels.txt. 
  ActivityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", 
                               col.names=c("ID", "Description"), stringsAsFactors = FALSE)
  
  ##Read y_test.txt, label the variable "Activity" and convert the observations to descriptive names according to the 
  ##activity labels.
  yTest <- read.table("./UCI HAR Dataset/test/y_test.txt", 
                      col.names = "ActivityDescription")
  Activity <- vector(mode = "character")
  for(i in 1:length(row.names(yTest))){
    Activity[i] <- ActivityLabels[ActivityLabels$ID == yTest[i,1],2]
  }
  yTest$ActivityDescription<-Activity
  
  ##Read y_train.txt, label the variable "Activity" and convert the observations to descriptive names according to the 
  ##activity labels.
  yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt", 
                       col.names = "ActivityDescription")
  for(j in 1:length(row.names(yTrain))){
    Activity[j] <- ActivityLabels[ActivityLabels$ID == yTrain[j,1],2]
  }
  yTrain$ActivityDescription<-Activity
  
  ##Read features.txt, which will become the variable names for XTrain and XTest
  variableNames<-read.table("./UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)[,2]
  
  ##Read X_test.txt and X_train.txt, naming the columns accordingly
  XTest <- read.table("./UCI HAR Dataset/test/X_test.txt",col.names = variableNames)
  XTrain <- read.table("./UCI HAR Dataset/train/X_train.txt",col.names = variableNames)
  ##find the indices of VariableNames whose contents contain "mean" or "std". Create a vector of indices that will indicate which 
  ##columns to include in the final dataset.
  means<-grep("mean",variableNames,value=FALSE)
  stDevs<-grep("std",variableNames,value=FALSE)
  finalColumns<-sort(append(means,stDevs))
  XTest<-XTest[,finalColumns]
  XTrain<-XTrain[,finalColumns]
  
  ##clip the tables together into datasets
  Test<-cbind(subjectTest,yTest,XTest)
  Train<-cbind(subjectTrain,yTrain,XTrain)
  
  ##Merge the two datasets
  mergedData<-merge(Train,Test,all=TRUE)
  
  ##Create new tidy data set showing the average of each variable for each activity and each subject
  ##First, melt mergedData
  newSet <- melt(mergedData,id=c("SubjectID", "ActivityDescription"),measure.vars=names(XTest))
  ##Then, recast newSet
  newSet <- dcast(newSet, SubjectID + ActivityDescription ~ variable, mean)
  newColNames <- c("SubjectID","Activity Description")
  for (x in 1:length(names(XTest))){
    newColNames[x+2] <- paste("Mean_Of_", names(XTest[x]), sep = "")
  }
  names(newSet)<-newColNames
  ##if(!file.exists("GettingCleaningDataProjectOutput.txt")){
    ##file.create("GettingCleaningDataProjectOutput.txt")
  ##}
  ##write.table(newSet,fileConn,sep="\t", col.names=TRUE)
  newSet
  
  }