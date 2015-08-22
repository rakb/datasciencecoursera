#############run_analysis.R##############################
##########Getting and Cleaning Data:Course Project#######
##Step 0: Read all the files in data tables
##Assuming the data files are under directory UCI HAR Dataset

xtest <- read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE,sep="",stringsAsFactors=FALSE)
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE,sep="",stringsAsFactors=FALSE)
subtest <- read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE,sep="",stringsAsFactors=FALSE)
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt",header=FALSE,sep="",stringsAsFactors=FALSE)
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE,sep="",stringsAsFactors=FALSE)
subtrain <- read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE,sep="",stringsAsFactors=FALSE)
actlbl <- read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE,sep="",stringsAsFactors=FALSE)
features <- read.table("./UCI HAR Dataset/features.txt",header=FALSE,sep="",stringsAsFactors=FALSE)


#Step 1: Merges the training and the test sets to create one data set

#we need to add column names to ytest as otherwise default column names 
#of xtest and ytest would overlap. 
#If this happens bind_rows does not give proper results

colnames(ytest) <- c("activity")
colnames(subtest) <- c("subject")
colnames(ytrain) <- c("activity")
colnames(subtrain) <- c("subject")

test_df <- bind_cols(subtest,bind_cols(ytest,xtest))
train_df <- bind_cols(subtrain,bind_cols(ytrain,xtrain))
xall <- bind_rows(test_df,train_df)
#xall <- bind_rows(xtest,xtrain)

#Step 2: Extracts only the measurements on the mean and standard deviation
#for each measurement. 
#first get the required column numbers
#selcols <- filter(features,grepl('-mean|-std',V2))[,1]
selcols <- grep("-mean|-std",features[,2])


#get mean and std. dev
#offsetting by 2 since two additional columns 
#i.e. activity and subject got added on left
x_mean_std <- xall[,c(1,2,selcols+2)]

#Step 3: Uses descriptive activity names to name the activities in the data set
## TODO: check if tapply or sapply can be used here !!
for(i in 1:nrow(x_mean_std)) x_mean_std$activity[i] = actlbl[x_mean_std$activity[i],2]


#Step 4: Appropriately labels the data set with descriptive variable names. 
cnames <- features[selcols,2]
cnames <- c("Subject","Activity",cnames)
colnames(x_mean_std) <- cnames

#Step 5: From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

xtidy <- gather(x_mean_std,Measure, Reading, -Subject,-Activity)
xtidy_grp <- group_by(xtidy,Subject,Activity,Measure)
tidy_mean_data <- summarize(xtidy_grp,Mean=mean(Reading))
write.table(tidy_mean_data,file="tidy_mean_data.txt",quote=FALSE,row.names=FALSE)

##########END###################
