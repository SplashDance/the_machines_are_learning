
require(lubridate)
require(knitr)
require(plyr)
require(dplyr)
require(caret)
require(randomForest)

full.path <- paste0("~/Desktop/MOOCs/Coursera/Data Science/",
                    "8. Practical Machine Learning/Project/setup.aux.temp.R")
setwd(full.path)


set.seed(123456)
## Downloading Training & Testing datasets
train.file <- "training.csv"
test.file <- "test.csv"

# download necessary files if they aren't already in your curr. wrk. dir.
if (!(train.file %in% list.files())){
    site.train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    site.test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

    download.file(url = site.train, method='curl', destfile = './training.csv')
    download.file(url = site.test, method='curl', destfile = './test.csv')
}

train <- read.table(train.file, sep=',', header=TRUE,
                    stringsAsFactors = FALSE,
                    na.strings = c("NA", "", "#DIV/0!"))
testing.df <- read.table(test.file, sep=',', header=TRUE,
                         stringsAsFactors = FALSE,
                         na.strings = c("NA", "", "#DIV/0!"))



# Change name of column to 'date.time' and parse via lubridate
names(train)[5] <- 'date.time'
train$date.time <- dmy_hm(train$date.time)

# converts this column from character to boolean
train$new_window <- train$new_window == 'yes'

# ensures predicted variable is a factor
train$classe <- factor(train$classe)

# select only those entries with 95% of data
mask95 <- colMeans(is.na(train)) <= 0.95
df <- train[, mask95]
df <- df[, -c(1, 2, 5)]

inTrain <- createDataPartition(y = df[, 57], p=0.70, list=FALSE)
df.train <- df[inTrain, ]
df.cv <- df[-inTrain, ]

my.forest <- randomForest(x=df.train[, -57], y=df.train[, 'classe'],
                          xtest=df.cv[, -57], ytest = df.cv[, 'classe'],
                          keep.forest = TRUE)

# ## Perhaps better:
# my.forest <- randomForest(x=df.train[, -c(1,4,  57)], y=df.train[, 'classe'],
#                           xtest=df.cv[, -c(1,4,  57)], ytest = df.cv[, 'classe'],
#                           keep.forest = TRUE)

# my.forest <- randomForest(x=df.train[, -c(1,4,  57)], y=df.train[, 'classe'],
#                           xtest=testing.df, ytest = testing.df[, 'classe'])

print(my.forest)
print(summary(my.forest))
plot(my.forest)



# Create a list from sensors for eventual PCA
col.names <- names(train)
sensors <- c('belt', 'dumbbell', 'arm', 'forearm')
sensor.pattern <- c('belt', 'dumbbell', '[^e]arm', 'forearm')
grep_list = list()


for (idx in seq_along(sensors)){
    mask <- grepl(pattern = sensor.pattern[idx], x = col.names)
    grep_list[[sensors[idx]]] <- train[, mask]
}


# just a minor helper function
sh <- function(df){
    print(head(df, 8))
    print(str(df))
}


dataset.load <- function(filepath){
    # A function with all the necessary specifications in place for loading
    # our data frame so that we can ensure that both the training and test
    # sets will be loaded exactly the same way
    df <- read.table(filepath,
                    sep=',',
                    header=TRUE,
                    stringsAsFactors = FALSE,
                    na.strings = c("NA", "", "#DIV/0!"))
    return(df)
}


# create a function for cleaning up the dataframes that can be used on both
df.cleaner <- function(dataframe, mask=mask95){
    # Takes 'dataframe' and cleans up entries
    #
    # --> This is created so it can be used for both training & test sets
    # Also, can be passed additional masks.
    require(lubridate)
    # Change name of column to 'date.time' and parse via lubridate
    names(dataframe)[5] <- 'date.time'
    dataframe$date.time <- dmy_hm(dataframe$date.time)

    # converts this column from character to boolean
    dataframe$new_window <- dataframe$new_window == 'yes'

    # ensures predicted variable is a factor
    if ('classe' %in% names(dataframe)){
        dataframe$classe <- factor(dataframe$classe)
    }

    df <- dataframe[, mask]
    # df <- df[, -c(1, 2, 5)]

    return(df)
}

