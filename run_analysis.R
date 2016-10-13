library(plyr)
file <- "data.zip"
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if(!file.exists(file)){
   #Downloads input data set 
    download.file(url,file, mode = "wb")
    
}
#create the destination folder if result_Data_Set doesnot exists
if(!file.exists("result_Data_Set")){
    dir.create("result_Data_Set")
} 

##reads a table from the zip data file

getTable <- function (filename,cols = NULL){
    data <- data.frame()
   
    unzippedFile <- unz(file, paste("UCI HAR Dataset",filename,sep="/"))
    
    
    if(is.null(cols)){
        data <- read.table(unzippedFile,sep="",stringsAsFactors=F)
    } else {
        data <- read.table(unzippedFile,sep="",stringsAsFactors=F, col.names= cols)
    }
    
    
    data
    
}

##Reads and creates a complete data set
getData <- function(type, features){
    
   
    subject_data <- getTable(paste(type,"/","subject_",type,".txt",sep=""),"id")
    y_data <- getTable(paste(type,"/","y_",type,".txt",sep=""),"activity")    
    x_data <- getTable(paste(type,"/","X_",type,".txt",sep=""),features$V2) 
    
    return (cbind(subject_data,y_data,x_data)) 
}

##saves the data into the result folder
saveResult <- function (data,name){
    
    
    file <- paste("result_Data_Set", "/", name,".csv" ,sep="")
    write.csv(data,file,row.names=FALSE)
}



##get common data tables

#features used for col names when creating train and test data sets
features <- getTable("features.txt")

## Load the data sets
train <- getData("train",features)
test <- getData("test",features)

# Merges the training and the test sets to create one data set. < DONE
data <- rbind(train, test)

# rearrange the data using id
data <- arrange(data, id)



# Uses descriptive activity names to name the activities in the data set
activity_labels <- getTable("activity_labels.txt")

data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

# Extracts only the measurements on the mean and standard deviation for each measurement. 
firstDataset <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject
secondDataset <- ddply(firstDataset, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })

# Adds "_mean" to colnames
colnames(secondDataset)[-c(1:2)] <- paste(colnames(secondDataset)[-c(1:2)], "_mean", sep="")

saveResult(firstDataset,"firstDataset")
saveResult(secondDataset,"secondDataset")
print("***Completed***")