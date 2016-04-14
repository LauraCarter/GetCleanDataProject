runanalysis <- function(x = c()) ## input is a character vector i.e. the filename
{
      ## check if the data is already loaded
      if(!file.exists("UCI HAR Dataset")) 
            { 
                  dir.create("UCI HAR Dataset")
                  ## use the 'downloader' package to download and unzip the files
                  library(downloader) 
                  download("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", dest="dataset.zip", mode="wb") 
                  ## download into the working directory
                  unzip ("dataset.zip", exdir = "./")
                  ## note the date downloaded
                  dateDownloaded <- date()
            }
      
      ## create a list with variable names
      features <- read.table("./UCI HAR Dataset/features.txt")
      featlist <- as.character(features[,2])
      
      ## setting up the test data set
      
      ## read in the data for test and rename the variables 
      subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
      names(subject_test) <- c("subjectid")
      
      y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
      names(y_test) <- c("activity")
      
      x_test <- read.table("./UCI HAR Dataset/test/x_test.txt")
      names(x_test) <- featlist
      
      testdata <- cbind(subject_test, y_test, x_test)
      
      ## setting up the train data set
      
      ## read in the data for train and rename the variables 
      subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
      names(subject_train) <- c("subjectid")
      
      y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
      names(y_train) <- c("activity")
      
      x_train <- read.table("./UCI HAR Dataset/train/x_train.txt")
      names(x_train) <- featlist
      
      traindata <- cbind(subject_train, y_train, x_train)
      
      ## merge the test and train datasets
      data <- rbind(testdata, traindata)

      ## extract mean and sd columns only
      
      ## logical vector identifying if a column name has mean or std
      meansd <- grepl( "mean\\(|std", names(data))
      
      ## adding first 2 columns back in
      meansd[1] <- TRUE
      meansd[2] <- TRUE
      
      ## extracting mean and st d for each row
      subdata <- data[,meansd]
      
      ## labelling activities
      
      ## read in list of activity names
      acts <- read.table("./UCI HAR Dataset/activity_labels.txt")
      
      ## sub for the coding
      actsub <- function(x) {sub(x, acts[x,"V2"], x)}
      act2 <- sapply(subdata$activity, actsub)
      subdata$activity <- act2
      
      ## return a tidy data set
      
      ## melt the data
      datamelt <- melt(subdata, id=c("subjectid", "activity"))
      ## cast with means
      datacast <- dcast(datamelt, subjectid + activity ~ variable, mean)
      ## return tidy data set
      melt2 <- melt(datacast, id=c("subjectid", "activity"))
      ## sort it neatly
      tidy <- arrange(melt2, subjectid, activity)
      write.table(tidy, "./tidy_set.txt")
}