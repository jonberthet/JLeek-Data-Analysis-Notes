
Video 2 - STRUCTURE OF DATA ANALYSIS _ Part I

1. Define Question
2. Define ideal data set
Depending on goal:sam
  Descriptive: whole population
  Exploratory: random sample with many variables measured
  Inferential: right population, randomly sampled
  Predictive: training and test data set from same population
  Causua: data from a randomized study
  Mechanistic: data about all components of the system


3. Determine accessible data
4. Obtain data
5. Clean Data
  may need reformating, subsamling, know sources of data, and determine if data is good enough, know how it is pre-proccessedlibarr


                Video 3 - STRUCTURE OF DATA ANALYSIS _ PART II
6. Exploratory Data Analysis
  Since we're trying to perform a prediction, we need a training set and a test set. Did this by creating a random training set indicator'
  look at summaries of the data, check for missing data, create exploratory plots, peform exploratory analyses (eg: clustering)

7. Statistical prediciton/modeling
  should be informed by results from exploratory analysis
  exact methods depend on question of interest
  transformations/processing should be accounted for when necessary
  measures of uncertainty should be reported
8. Interpret Results
  Use Appropriate Language
    Describes
    Inferential analysis: Correlates with/associated with
    Causal analysis: leads to/causes
    Prediction analysis: predicts
  Give explanation, interpret coefficients, and report measures of uncertainty
9. Challenge Results
  challenge all steps, measures of uncertainty, choices of terms in models, and think of potential alternative analyses
10. Synthesize/write up results
  Lead with a question
  Summarize analyses into the story, starts with beginning, then explains how performed the analysis, and ends with a conclusion. Don't include ever analysis, Include it only if it's needed for the story or needed to address a challenge
  Order analyses according to the story, rather than chronologically.
  Make pretty
11. Create reproducible code

                        Video 4 - ORGANIZING DATA ANALYSIS

Data
-Have 2 folders: (raw (have source) and processed (must be nice, named, easy to see -should occur in the README file - report location where data was found) data)

Figures
-Exploratory Figures (used to mess around with data, not presentable)
-Final Figures (presentable)

Code Folders
-Folder just for R code
  -Raw Scripts
    -less commented, multiple versions
  -Final Scripts (reproduce all analyses I used in presentation)
    -put small comments liberally, bigger commented blocks for whole sections. Put final process)
  -R Markdown Files (integrated bits of code and writing allow me to comment in code)
    -addition or substitute to distributed final scripts. very structured
Text 
  -Readme files (not necessarily in R markdown)
  -Text of Analysis
    -Include: title, introduction (motivation), methods (stats I used), results (include measures of uncertainty), and conclusions (or potential problems)
    -Tell a story
    -Don't include every analysis I did'
    -References should be included for statistical methods

                          Video 5 - GETTING DATA (PART I)

Roger and Andrew Jaffe's online lectures on setting directory '

getwd()    //get working directory

--Relative Paths (get/set your working directory)--
setwd("../")   //moves wd one spot up in list

--Absolute Paths--
setwd("/Users/Desktop/R Files/R")     //just write out whole directory

--Types of data--
fileUrl <- "https://..."  //direct to URL
download.file(fileUrl,destfile = "./data/cameras.csv",method="curl")
//need 'curl' method Mac doesn't support download.file when there's an https (secured) connection. Windows is Ok     //downloads files from internet. Great for txt files Always give time took file
list.files("./data")    //Takes .json, .rda, .xlsx, .csv

dateDownloaded <- date()    //access and stores data of time and day when I took the data
              
//NOTE: http - you can download.file() no prob. HTTPS, set method="curl"
              
read.table()      //main fn for reading data in R. Flexible, robust, but requires parameters. Reads data in RAM - big data can cause problems. 
                  //Important parameters (file, header, sep, row.names, nrows).
                  //related: read.csv(), read.csv2() <- versions of read.table() where parameters are already set

cameraData <- read.table("./data/cameras.csv", sep=",", header = TRUE)    //Must define parameters. values are separated by comma and there's a header line
head(cameraData)      //shows table with headers

read.csv        //already sets sep="," and header=TRUE

Reads .xlsx files, but it's slow and MUST download xlsx package to read them.   
//** Parameters = file, sheetIndex, rowIndex, collIndex, header
read.xlsx2() relies on low level Java functions so may be a bit faster

library(xlsx)
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/camera.xlsx",method="curl")
cameraData <- read.xlsx2("./data/camera.xlsx",sheetIndex=1) //******
head(cameraData)

cameraData <- read.csv(file.choose())     //finds file without having to rewrite location. Problem: less reproducible

                            Video 6 - GETTING DATA PART II
              
Interacting more directly with files
file - open a connection to a text file
url - open a url
gzfile - open a connection to a .gz file
bzfile - open a connection to a .bz2 file
?connections for more info
              REMEMBER TO CLOSE CONNECTIONS 
      
readLines()       //a fn to read lines of text from a connection
              //** parameters : con, n, encoding
              
conn <- file("./data/cameras.csv", "r")      //open up connection to camera.csv, which is downloaded into 'data' directory. pass through with 'r'
cameraData <- read.csv(con)
close(con)
head(cameraData)

//Read JSON
library(RJSONIO)
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.json?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/camera.json",method="curl")
con = file("./data/camera.json")
jsonCamera = fromJSON(con)
close(con)
head(jsonCamera)




Writing Data - write.table()
Writing Data - save(), save.image()
Reading saved data - load()
Remove everything - rm(list=ls())
Pasting character strings together - paste() and past0() <- paste0() is the same as paste but with sep=""
Looping

Getting Data off webpages
library(XML)
or
html3 <- htmlTreeParse("http://...")
xpathSApply
              
Other Packages
httr
RMySQL
bigmemory - handling data larger than RAM
RHadoop
foreign - getting data into R from SAS, SPSS, Octave

DATA RESOURCES SLIDES - LESSON 6!!! GREAT USE!!!

                        **** SUMMARIZING DATA - LESSON 7  ****

//Once I have the whole dataset, which is too large to view, run the following queries on my dataset called, 'eData':

//Tells me how many rows and columns that way I know how many variables I have. First gives me # of rows, then columns
dim(eDatat)

//look at names of variables in the dataframe
  names(eData)
              
//Gives me number of rows
  nrow(eData)
              
//Gives me the quantiles of the variable, Lat, in the eData set
  quantile(eData$Lat)

//Gives me quantile info for quantitative variables but also tells me frequency of certain infos like the Src
  summary(eData)
              
//tells me the class of the data frame
  class(eData)
    
//Look at class of each individual column. Says, I'm selecting the first row of the eData dataset and apply the class fn to every element to that row'
    read(eData[1,], class)
              
//Looks at unique values (all the values under a column or in a row for instance), certain variables will have a certain number of unique values. Way of knowing if certain info should be there or not. In example, looking at unique variables under column $Src
  unique(eData$Src)

// Tells me how many unique variables there are
length(unique(eData$Src))              
              
//gives me how many of those unique variables there are
table(eData$Src)

//Look at relationship between 'Src' variable and 'Version' variable
table(eData$Src, eData$Version)

//any() and all() great for finding missing data, or know particular characteristic of variable
eData$Lat[1:10]

//If I wanted to know value above 40, then I can do a formula
eData$Lat[1:10] > 40

//If I want to know if any (or all) of the data is above a 40, then,
any(eData$Lat[1:10] > 40)
or 
all(eData$Lat[1:10] > 40)

//Taking columns with latitude and longitude. Before the comma, i'm describing the rows'. After the comma, I'm describing the columns'. Returns rows where both long and lat are both > 0.
eData[eData$Lat > 0 & eData$Lon > 0, c("Lat", "Lon")]

//Returns Lat and Long. where either Lat or Long has to be greater than 0.
eData[eData$Lat > 0 | eData$Lon > 0, c("Lat", "Lon")]

//NOTE: WITH NEW DATASET
//If I'm missing values, use'
is.na(variable)

//Calculate total number of NA values
sum(is.na(variable))

//Gives me a table of when there is an NA
table(is.na(variable))

//shows a table and shows where the NA value is
table(c(0,1,2,3,4,NA,3,3,2,2,3), useNA = "ifany")

//Summarizing columns/rows and finding sums or means in a row or column. If there are any NA values in the sum or mean, it will return NA
rowSums()
rowMeans()
colSums()
colMeans()

//in analysis, skips all NA values in calculations
colMeans(reviews,na.rm=TRUE)





*********W2-VIDEO 8 - DATA MUNGING BASICS************
//Variables = columns
//Observations = rows

Munging Operations must be recorded and 90% of effort will be spent here. //Data Munging is turning raw to workable data
1. fix variable names
2. create new variables
3. merge data sets
4. reshape data sets
5. deal with missing data
6. take transforms of variables
7. check on and remove inconsistent values

//Fixing Character vectors - tolower(), toupper()

//First, get data
cameraData <- read.csv("./cameras.csv")

//2nd, list all the names of the variables
names(cameraData)

//3rd, make all letters in the variables lowercase
tolower(names(cameraData))
//Or make all uppercase
toupper(names(cameraData))

//4th, split variable names that are confusing (often when there's a .1 or .2 after the name'). Split the strings at the value of a period. Escape the special character with '\\' then whatever you want to split, like a '.' Splits character element into a character 2 elements. Now first element is before the dot and second element is after the dot
splitNames = strsplit(names(cameraData),"\\.")
splitNames[[5]]

//5th, splitting names is useful because we can use the sapply() to leap over all the names of the data frame and remove the trailing dot for every element of that character vector
//split names was the list that we got from the string split function and so the sixth element was the sixth element of that list and that list had, the sixth element of that list was a character vector that had location as the first element and one as the second element and the dot. Had been removed. So if we select just the first element of that character vector, we can see that it's the location.' From splitNames location.1
splitNames[[6]][1]   <- gets me "location"
splitNames[[6]][2]   <- gets me "1"

//6th, so we replace every variable with a '.' in its name and only take the first element. This is a long way to take out just a 1 in a variable, but it's good if we want to do this to many many variables'
firstElement <- function(x){x[1]}
sapply(splitNames, firstElement)



**//NOW NEW DATA SET//**
downloaded dropbox files on Peer Review Data
//GOAL: Put 2 data sets into 1 data set

//1st,
//look at first two rows of data in the 'reviews' data set
head(reviews,2)
//look at first two rows of data in the 'solutions' data set
head(solutions,2)

//2nd,
//Character Fixing vectors. Getting rid of underscores, periods, etc... (ex: time_left)
//substitute a character in a variable and replace it with something else. Use the 'substitute' command, but it only replaces the first underscore
  sub("_", "", names(review),)

//to remove ALL underscores in a made up variable called 'testName <- "this_is_a_test', use:
  gsub("_","", testName)

//take some quantitative info and turn it into ranges with data set 'review' and variable 'time'
reviews$time_left[1:10]

//Quantitative variables turn into ranges -- cut() - look at variables in a glance. Variables are replaced as being within a range
timeRanges <- cut(reviews$time_left, seq(0,3600, byt = 600))
timeRanges[1:10]

//Quantitative Variable in ranges by factor and shows me how many times values ran into those ranges
table(timeRanges, useNA="ifany")

//Quant variables in ranges - cut2() {Hmisc}    <- this cuts up variables into, in this example, 6, quantiles. Or 6 ranges of equal ranges
timeRanges <- cut2(reviews$time_left, g=6)
table(timeRanges,useNA = "ifany")

//ADDING AN EXTRA VARIABLE TO DATAFRAME
//1st, create the values, defined by the variable, 'timeRanges'
timeRanges <- cut2(reviews$time_left, g=6)

//2nd, put the 'timeRanges' data into the 'reviews' Data Frame
reviews$timeRanges <- timeRanges

//3rd, check to see if your new varaible is there
head(reviews,2)

//MERGE DATA SETS TOGETHER - merge()
//1st, merge variables together based on common variable names, all = TRUE because there may be values in 'solutions' that don't exist in 'reviews'
mergeData <- merge(reviews,solutions,all=TRUE)
head(mergedData)

//2nd, there's messed up variable titles, so going to merge them on the basis of, in the reviews
data set, the solution<u>ID variable. And in the solutions data set, the ID variable.</u>
mergedData2 <- merge(reviews, solutions, by.x = "solution_id", by.y = "id", all=TRUE)
head(mergedData2[,1:6],3)


//SORTING VALUES - sort()
//by default, sorts by increasing order
sort(mergedData2$reviewer_id[1:10])

//ORDERING VALUES - order()
//sort data frame by a particular variable
mergedData2$reviewer_id[1:10]

//it puts data from query directly above in order. It tells me what order the previous info must be placed to be in order
order(mergedData2$reviewer_id)[1:10]

//subset that info into a new data set
//mergedData2$reviewer_id[order(mergedData2$reviewer_id)]

//REORDERING DATA FRAME
head(mergeData[,1:6],3)

//reviewer_id is in increasing order and reordered all other variables to correspond with right reviewer_id
sortedData <- mergeData2[order(mergeData2$reviewer_id),]
head(sortedData[,1:6],3)

//REORDERING BY MULTIPLE VARIABLES

//want id increasing, then within id, the reviewer_id must be increasing
sortedData <- mergeData2[order(mergedData2$reviewer_id, mergedData2$id),]
head(sortedData[,1:6],3)

Reshaping data - example
//sometimes  need to change the format of datasets or shapes. Gives me all individual values, not the numbers per 
melt(misShaped,id.vars="people",variable.name="treatment",value.name="value")

MORE DATA & TIDY TOOLS
Andrew jaffe's Data Cleaning
