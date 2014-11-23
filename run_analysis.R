

## Read in the two data sets and combine them using rbind
testData <- read.table("test/X_test.txt")
trainData <- read.table("train/X_train.txt")
X <- rbind(testData, trainData)

## Tidying up by removing testData, trainData
rm(testData)
rm(trainData)

## Reading  test/subject_test.txt , train/subject_train.txt and combining them by rbind
testSub <- read.table("test/subject_test.txt")
trainSub <- read.table("train/subject_train.txt")
S <- rbind(testSub, trainSub)

## Tidying up by removing testSub, trainSub
rm(testSub)
rm(trainSub)

## Read in test/y_test.txt, train/y_train.txt and combine them by using rbind
testLabel <- read.table("test/y_test.txt")
trainLabel <- read.table("train/y_train.txt")
Y <- rbind(testLabel, trainLabel)

## Tidying up by removing testLabel, trainLabel
rm(testLabel)
rm(trainLabel)

## Read Features List. This will be used as column names for data)
featuresList <- read.table("features.txt", stringsAsFactors=FALSE)

## Use only names from features list
features <- featuresList$V2

## Keep only std and mean columns
keepColumns <- grepl("(std|mean[^F])", features, perl=TRUE)

## Keep only required data. Name it human readable
X <- X[, keepColumns]
names(X) <- features[keepColumns]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

## Read ActivityList. add descriptive names to data set
activities <- read.table("activity_labels.txt")
activities[,2] = gsub("_", "", tolower(as.character(activities[,2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity" ## Add activity label

## Add human readable labels to activity names
names(S) <- "subject"
tidyData <- cbind(S, Y, X)
write.table(tidyData, "tidyData.txt")

## Create second tiny data set with avg of each var for each act and each sub
uS = unique(S)[,1]
nS = length(uS)
nA = length(activities[,1])
nC = length(names(tidyData))
td = tidyData[ 1:(nS*nA), ]

row = 1
for (s in 1:nS) {
	for (a in 1:nA) {
		td[row,1] = uS[s]
		td[row,2] = activities[a, 2]
		tmp <- tidyData[tidyData$subject==s & tidyData$activity==activities[a,2],]
		td[row, 3:nC] <- colMeans(tmp[, 3:nC])
		row = row + 1
	}
}

write.table(td, "tidyData2.txt")
