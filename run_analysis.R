#Read feature and activity label - common for test and train data
feature <- read.table("features.txt" ,header=FALSE)
activityLabel <- read.table("activity_labels.txt" ,header=FALSE)

colnames(activityLabel) <- c("activityId","activityType")

#Read test data
subjectTest <- read.table("./test/subject_test.txt", header=FALSE)
XTest <- read.table("./test/X_test.txt", header=FALSE)
YTest <- read.table("./test/y_test.txt", header=FALSE)

#Add Column names to test data
colnames(subjectTest) <- "subjectId"
colnames(XTest) <- feature[,2]
colnames(YTest) <- "activityId"

#Merge test data
testData <- cbind(YTest, subjectTest, XTest)

#Read train data
subjectTrain <- read.table("./train/subject_train.txt", header=FALSE)
XTrain <- read.table("./train/X_train.txt", header=FALSE)
YTrain <- read.table("./train/y_train.txt", header=FALSE)

#Add Column names to train data
colnames(subjectTrain) <- "subjectId"
colnames(XTrain) <- feature[,2]
colnames(YTrain) <- "activityId"

#Merge train data
trainData <- cbind(YTrain, subjectTrain, XTrain)

#Merge test and train data
combineData <- rbind(testData, trainData)

str(combineData)

#Extract mean and std deviation for each measurement
extractData <- combineData[, grepl("Id|mean|std", colnames(combineData))]

#Use descriptive activity name
extractData <- merge(activityLabel, extractData, by = 'activityId')
#Drop Activity ID column
extractData <- extractData[-c(1) ]

#Make descriptive label
names(extractData) <- gsub('act', 'Act', names(extractData))
names(extractData) <- gsub('sub', 'Sub', names(extractData))
names(extractData) <- gsub('std', 'Std', names(extractData))
names(extractData) <- gsub('mean', 'Mean', names(extractData))
names(extractData) <- gsub('Acc', 'Accelerometer', names(extractData))
names(extractData) <- gsub('Gyro', 'Gyroscope', names(extractData))
names(extractData) <- gsub('Mag', 'Magnitude', names(extractData))
names(extractData) <- gsub('BodyBody', 'Body', names(extractData))

# Delete the () from label
names(extractData) <- gsub('\\(\\)', '', names(extractData))

#change t in the begining of label to Time
names(extractData) <- gsub('^t', 'Time', names(extractData))

#change f in the begining of label to Freq
names(extractData) <- gsub('^f', 'Freq', names(extractData))

#Meke tidy data set with average for each Activity+Subject
library(dplyr)
tidyData <- extractData %>% group_by(ActivityType, SubjectId) %>% summarise_all(mean)

#Write tidy dataset to txt file
write.table(tidyData, file = 'Tidydata.txt', row.names = FALSE)
