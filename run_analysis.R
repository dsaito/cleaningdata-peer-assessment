install.packages('dplyr')

library(dplyr)

# STEP 0.Prepare data for required analysis

#Load test data into separate tables
testsubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")
testfeatures <- read.table("./UCI HAR Dataset/test/y_test.txt")
testvalues <- read.table("./UCI HAR Dataset/test/X_test.txt")
assay <- rep(c('test'), each=nrow(testsubjects))

#Assemble unified test table
testable <- cbind(testsubjects, assay, testfeatures, testvalues)

#Load training data into separate tables
trainsubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
trainfeatures <- read.table("./UCI HAR Dataset/train/y_train.txt")
trainvalues <- read.table("./UCI HAR Dataset/train/X_train.txt")
assay <- rep(c('train'), each=nrow(trainsubjects))

#Assemble unified training table
traintable <- cbind(trainsubjects, assay, trainfeatures, trainvalues)

# STEP 1.Merge the training and the test sets to create one data set
table <- rbind(testable, traintable)

#Create and add column names
columnames <- read.table("./UCI HAR Dataset/features.txt") #names are the same for training and test data
columnames2 <- c('subject', 'group', 'activity', as.vector(columnames[,2]))
colnames(table) <- columnames2

#Make dplyr table
tidytable <- tbl_df(table)
rm('table')

#Remove duplicate columns with duplicated names
uniquecolumns <- c(); uniquecolumnames <- c()
for (n in 1:length(columnames2)) {
  if (!is.element(columnames2[n], uniquecolumnames)) {
    uniquecolumns <- c(uniquecolumns, n)
    uniquecolumnames <- c(uniquecolumnames, columnames2[n])
  }
}

tidytableunique <- tidytable[,uniquecolumns] #Strangely, this does not work with select() due to duplicate columnames

# STEP 2.Extract only the measurements on the mean and standard deviation for each measurement.
tidytablefixed <- tidytableunique[,1:3] #fixed part of the table with subjects, activities and group
tidytablemean <- select(tidytableunique, contains("mean")) #I believe I could use tidytableunique[,4:-1]
tidytablestd <- select(tidytableunique, contains("std")) #same here
tidytablemeanstd <- cbind(tidytablefixed, tidytablemean, tidytablestd)

# STEP 3.Use descriptive activity names to name the activities in the data set
activities <- as.vector(tidytablemeanstd[,3])
ractivities = factor(activities,labels=c("walking","walking upstairs","walking downstairs", "sitting", "standing", "laying"))
tidytablemeanstd[,3] <- ractivities

# STEP 4.Appropriately label the data set with descriptive variable names.
#Already done when adding column names!!

# STEP 5.From step 4, create a second, independent tidy data set with the average of each variable for each 
#activity and each subject.
summary <- tidytablemeanstd %>%
  group_by(subject, activity, group) %>%
    summarise_each(funs(mean))

write.table(summary, file='PeerAssessment.txt', row.name=FALSE)
