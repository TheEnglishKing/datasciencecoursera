library(dplyr)
run_analysis <- function(){
##load the necessary packages

##import data into R
  xtrain = read.table("/Users/joshuastein/Desktop/hello-world/GettingandCleaningDataClass/UCIHARDataset/train/X_train.txt")
  ytrain = read.table("/Users/joshuastein/Desktop/hello-world/GettingandCleaningDataClass/UCIHARDataset/train/y_train.txt")
  IDtrain <- read.table("/Users/joshuastein/Desktop/hello-world/GettingandCleaningDataClass/UCIHARDataset/train/subject_train.txt")
  
  xtest <- read.table("/Users/joshuastein/Desktop/hello-world/GettingandCleaningDataClass/UCIHARDataset/test/X_test.txt")
  ytest <- read.table("/Users/joshuastein/Desktop/hello-world/GettingandCleaningDataClass/UCIHARDataset/test/y_test.txt")
  IDtest <- read.table("/Users/joshuastein/Desktop/hello-world/GettingandCleaningDataClass/UCIHARDataset/test/subject_test.txt")
  features<- read.table("/Users/joshuastein/Desktop/hello-world/GettingandCleaningDataClass/UCIHARDataset/features.txt")

#merge
xdata <- rbind(xtrain, xtest)
IDdata <- rbind(IDtest,IDtrain)
ydata <- rbind(ytrain, ytest)
onedata <- cbind(xdata, ydata, IDdata)

##extract the means and std
wheresthosemeanssat <- grep("mean", features$V2, ignore.case = TRUE)
wherethosestdsat <- grep("std", features$V2)
meansandstds <- c(wheresthosemeanssat, wherethosestdsat)
onlyavgandstd <- subset(onedata, select = meansandstds)

#rename the columns with descriptive statistics
thenames <- features$V2[wheresthosemeanssat] 
thestdnames <- features$V2[wherethosestdsat]
thenamescombined <- c(thenames, thenameslowercasestd)

thenamesclean <- lapply(thenamescombined, function(x) gsub("-", "", x))
thenamescleaner <- lapply(thenamesclean, function(x) gsub("\\(|\\)", "", x))
thenameswocomma <- lapply(thenamescleaner, function(x) gsub(",", "", x))
thenameslowercase <- lapply(thenameswocomma, tolower)

featuresnames <- setNames(onlyavgandstd, thenameslowercase)

#rename the activities in the data set
activities <- onedata[,562]
activities_1 <- sapply(activities, function(x) gsub("1", "walking", x)) 
activities_2 <- sapply(activities_1, function(x) gsub("2", "walkingupstairs", x))
activities_3 <- sapply(activities_2, function(x) gsub("3", "walkingdownstairs", x)) 
activities_4 <- sapply(activities_3, function(x) gsub("4", "sitting", x)) 
activities_5 <- sapply(activities_4, function(x) gsub("5", "standing", x))
activities_6 <- sapply(activities_5, function(x) gsub("6", "laying", x))

#create data set
featuresnames$activities <- activities_6
featuresnames$ID <- IDdata

grouping_df <- data.frame(
  Group1 = featuresnames[, 87],
  Group2 = featuresnames[, 88]
)

tidydataset <- aggregate(featuresnames[, 1:53], by = grouping_df, FUN = mean)

#write tidydataset to csv
write.csv(tidydataset, file = "tidydataset.csv", row.names = TRUE)
}

