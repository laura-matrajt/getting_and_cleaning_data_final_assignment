## Final assignment: Getting and cleaning Data course

library(dplyr)
library(reshape2)
########### Step 1: reading the data sets into dataframes ############

####Data sets for the test set:
mydatatest = read.table('UCI HAR Dataset/test/X_test.txt')
subjectsIDstest = read.table('UCI HAR Dataset/test/subject_test.txt')
testLabels = read.table('UCI HAR Dataset/test/y_test.txt')




####Data sets for the training set:
mydatatrain = read.table('UCI HAR Dataset/train/X_train.txt')
subjectsIDstrain = read.table('UCI HAR Dataset/train/subject_train.txt')
trainingLabels = read.table('UCI HAR Dataset/train/y_train.txt')

#### Data set with the names of the variables: 
features = read.table('UCI HAR Dataset/features.txt')

#### Data set with the names of the labels
labelnames = read.table('UCI HAR Dataset/activity_labels.txt')


########### STEP 2: Renaming columns to have descriptive variable names ############:
#rename the columns that refer to the subjects IDs:
subjectsIDstest = rename(subjectsIDstest, subjectID = V1)
subjectsIDstrain = rename(subjectsIDstrain, subjectID = V1)
testLabels = rename(testLabels, activity = V1)
trainingLabels = rename(trainingLabels, activity = V1)

#rename the columns of the measurements data sets so that they are descriptive of what the measurement is:
names(mydatatest) = features$V2
names(mydatatrain) = features$V2


########### STEP 3: Combine the data frames ############
#merge the three data frames for the testing set into a single data frame that I call testdf:
testdf = cbind(subjectsIDstest, testLabels, mydatatest)

#merge the three data frames for the training set into a single data frame that I call traindf:
traindf = cbind(subjectsIDstrain, trainingLabels, mydatatrain)



#finally, bind these two data sets into a single one that I call mydf:
mydf = rbind(testdf, traindf)


########### STEP 4: Reclassify the activity labels so that they are descriptive as opposed to numeric variables.
#reclassify the activity labels column to be more descriptive: i.e to describe the activity as one of the following:
#WALKING WALKING_UPSTAIRS WALKING_DOWNSTAIRS SITTING STANDING LAYING instead of 1,2,3,4,5,or 6:
mydf$activity = factor(mydf$activity, levels = as.factor(unique(mydf$activity)), labels = labelnames$V2)


########### STEP 5: Extract from this data frame only the measurements on the mean and standard deviation for each measurement ############
#I will use the dplyr function select for this task. However, this function requires that the names of the variables are "syntactically valid names".
#This means that the names should not contain "(", ")" , "-" and ",".
#In order to achieve this, we need to do some "cleaning" in the names of the variables:

#Remove the "(" from the names and replace it with "".
names(mydf) = gsub("\\(","", names(mydf))
#Remove the ") from the names and replace it with "".
names(mydf) = gsub("\\)","", names(mydf))
#Remove the "," from the names and replace it with "_".
names(mydf) = gsub(",","_", names(mydf) )
#Remove the "-" from the names and replace it with "_".
names(mydf) = gsub("-","_", names(mydf) )
names(mydf)

#we use the function make.names so that the function select can be applied and recognizes that for example, the variable names 
#"fBodyAcc_bandsEnergy_1_8" and "fBodyAcc_bandsEnergy_9_16" are actually different:
valid_column_names <- make.names(names=names(mydf), unique=TRUE, allow_ = TRUE)
names(mydf) <- valid_column_names


### Use select to select the colunms that we care about:
#the instructions are unclear as of if we should include columns with the words mean and Mean (as in fBodyGyro-meanFreq()-X) or only
#columns with the word mean (all lower cases).
#so I am providing here both alternatives. Personally, I believe that the correct one is the first one since the
#features-info.txt file states that these columns are indeed the mean of some measurement. Hence, I commented out the second option.


#select from our big dataframe only the columns we care about, that is the ones that compute the mean or standard
#deviation of a measurement:

finalDataFrame = cbind(mydf[, 1:2], select(mydf,matches("[Mm]ean|std")))
#finalDataFrame = cbind(mydf[, 1:2], select(mydf,matches("mean|std")))

########### STEP 6: create the data set for the 5th point of the assignment  ############

#create a new data-frame where values are grouped by subjectID and activity
mygrouping <- group_by(finalDataFrame, subjectID, activity)

#create a tidy data set with the average of each variable for each activity and each subject. 
newdataset <- summarize_each(mygrouping, funs(mean(., na.rm=TRUE)))

#in this final step, I will melt the data for it to be "tidy". Please note that what "tidy" means is subjective
#one can consider the previous version of the data frame already in tidy form. Here, I assume that each type of measurement 
#is a "value" that the variable "measurement" can take on. More on this subject in the codeBook.
newdataset <- melt(newdataset, id=c("subjectID", "activity"), variable.name = "Measurement", value.name = "mean_value")

#write this data set as a .txt file using the function write.table():
write.table(newdataset, 'tidy_dataset.txt', row.names = FALSE, col.names = TRUE)                              


#extra: create a txt file with the names of the variables that were used in the last data set to include in the repository:
varused = data.frame('variable_names'=names(finalDataFrame))
write.table(varused, 'variables_used.txt', col.names = TRUE)
