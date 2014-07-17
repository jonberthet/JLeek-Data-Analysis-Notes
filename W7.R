V1 - SMOOTHING

For non-linear trends, overfitting risk

//EXAMPLE: CD4 Data
//Get Data
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/cd4.data", destfile="data/cd4.data",method="curl")
//Identify data in table
cd4Data <- read.table("./cd4.data", col.names=c("time", "cd4", "age", "packs", "drugs", "sex", "cesd", "id"))
//order data
cd4Data <- cd4Data[order(cd4Data$time),]
//Look at data
head(cd4Data)
//Plot data
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
NOTE: linear model won't represent' info.

//So, take a running average of the points. Put dot of average of point 1 and 2.
points(mean(cd4Data$time[1:2]),mean(cd4Data$cd4[1:2]),col="blue",pch=19)

//Put dot of average of point 2 and 3.
points(mean(cd4Data$time[2:3]),mean(cd4Data$cd4[2:3]),col="blue",pch=19)

//Do this for all the points
aveTime <- aveCd4 <- rep(NA,length(3:(dim(cd4Data)[1]-2)))
for(i in 3:(dim(cd4Data)[1]-2)){
  aveTime[i] <- mean(cd4Data$time[(i-2):(i+2)])     //Here, average 2 points before and 2 points after time point in question
  aveCd4[i] <- mean(cd4Data$cd4[(i-2):(i+2)])       //Here, same thing  except for Cd4 count
}
lines(aveTime,aveCd4,col="blue",lwd=3)

//Here, it's a bit' like overfitting.So let's average more' points, like 10!!!
aveTime <- aveCd4 <- rep(NA,length(11:(dim(cd4Data)[1]-10)))
for(i in 11:(dim(cd4Data)[1]-2)){
  aveTime[i] <- mean(cd4Data$time[(i-10):(i+10)])     //Now averaging 20 time points around 1 point
  aveCd4[i] <- mean(cd4Data$cd4[(i-10):(i+10)])       
}
lines(aveTime,aveCd4,col="blue",lwd=3)

//Or do 400 points around a point
aveTime <- aveCd4 <- rep(NA,length(201:(dim(cd4Data)[1]-10)))
for(i in 201:(dim(cd4Data)[1]-2)){
  aveTime[i] <- mean(cd4Data$time[(i-200):(i+200)])
  aveCd4[i] <- mean(cd4Data$cd4[(i-20):(i+200)]) 

//Can do same thing as the AveTime, for fn with the 'filter function'
//vector equal to 1/200 is repeated 200 times. It will take 200 time points centered on the time point we care about, and multipy them each by 1/200, then add them up.
//which is equivalent to averaging 200 time points, 100 to each side.
//Then do that for the CD4 data
  filtTime <- as.vector(filter(cd4Data$time,filter=rep(1,200))/200)   
  filtCd4 <- as.vector(filter(cd4Data$cd4,filter=rep(1,200))/200)

NOTE: This process of averaging more points is 'Smoothing' - shows trend over time

Averaging = weighted sums
//look at 2nd value, which is the avg. of the 2 previous tie points with the 2 following time points, then divide them by 4.
//This is a calculation of moving avg of 4 points immediatly around time point of interest
filtCd4 <- as.vector(filter(cd4Data$cd4,filter=rep(1,4))/4)
filtCd4[2]

//Similarly, we can sum up first 4 CD4 counts, then multiply them each by 1/4, then add them up
sum(cd4Data$cd4[1:4] * rep(1/4,4))
  
////Disadvantage to this: values close to point have more effect on weighted avg. than points farther away. But that's' just the way it is.
//Here, values farther away from points impact trend less than in previous fns.  
  ws = 100; tukey = function(x) pmax(1 - x^2,0)^2
  filt= tukey(seq(-ws,ws)/(ws+1));filt=filt/sum(filt)
  filtTime <- as.vector(filter(cd4Data$time,filter=filt))
  filtCd4 <- as.vector(filter(cd4Data$cd4,filter=filt))
  plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1); lines(filtTime,filtCd4,col="blue",lwd=3)
  
//Lowess (loess) (aka - locally weighted scatterplot smoothing)
//Use loess()
//CD4 count on left-hand side, then tilde (~), then covariant to fit a smooth relationship with
//Define loess fn
  lw1 <- loess(cd4 ~ time,data=cd4Data)
//plot data
  plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
//plot time variable vs Lowess smooth fitted CD4 values
  lines(cd4Data$time,lw1$fitted,col="blue",lwd=3)
  
//Set 'span' of Lowess - Span is the number of points used when smoothing. So, if span = 0.1, then 1/10 of all data is used to calculate local weighted value for the data point.
  plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1,ylim=c(500,1500))
  lines(cd4Data$time,loess(cd4 ~ time,data=cd4Data,span=0.1)$fitted,col="blue",lwd=3)
//1/4 data used
  lines(cd4Data$time,loess(cd4 ~ time,data=cd4Data,span=0.25)$fitted,col="red",lwd=3)
//3/4 of data used
  lines(cd4Data$time,loess(cd4 ~ time,data=cd4Data,span=0.76)$fitted,col="green",lwd=3)
  
NOTE: As span increases, line gets smoother. May be more biased, but it will be less variable (more stable fit to data)
  
////***Loess fn can PREDICT new values
//Here, generate new variable, TME (sequence from -2 to 5), and I can predict values of that sequence using loess object (lm1) calcualted in previous lines. Standard Error = True, so it calculates SE in predictions
  tme <- seq(-2,5,length=100); pred1 = predict(lw1,newdata=data.frame(time=tme),se=TRUE)
  
//plot tme vs fitted values
  plot(tme,pred1$fit,col="blue",lwd=3,type="l",ylim=c(0,2500))
//plot tm values vs. predicted values, plus and minus 1.96 times standard error of the fit
  lines(tme,pred1$fit + 1.96*pred1$se.fit,col="red",lwd=3)
  lines(tme,pred1$fit - 1.96*pred1$se.fit,col="red",lwd=3)

NOTE: red lines are wider than blue line cuz red line uses less data near the right side and makes less accurate predictions
  
//plot points that lines are referencing
  points(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)

SPLINES
  //easier to use when wanting to adjust for more than one co-variant in model
  //fit 1 smooth variable and relate it to an outcome. Great for smoothing selected variables out of many
  //Equation: example in slides
    -apply multiple functions to each data point, weight them by coefficients BK

//Popular fn is splines()
library(splines)
//calculate natureal cubic splines for time data. # of degrees of freedom equals # of functions that will be applied to time variable
ns1 <- ns(cd4Data$time, df = 3)
par(mfrow = c(1,3))
//plot each matrix where each column of that matrix is equal to one of the functions. Plot each fn one by one
plot(cd4Data$time, ns1[,1]); plot(cd4Data$time, ns1[,2]); plot(cd4Data;time,ns1[,3])
  
//Can include splines in linear model
//On right side of tilde is covariants, we include sply matrix. Sply matrix is the set of 3 functions
lm1 <- lm(cd4Data$cd4 ~ ns1)
summary(lm1)

RESULT: estimate of intercept, then coefficient of each of the 3 spline fns. Not easy to interpret.

//But if I plot the data and fit it, it looks nice! - looks like a 3rd order polynomial
plot(cd4Data$time, cd4Data$cd4, pch = 19, cex = 0.1)
points(cd4Data$time, lm1$fitted, col = "blue", pch = 19, cex = 0.5)  

V2 - THE BOOTSTRAP
  //Used to estimate standard errors for improving predictions and for performing
  other types of analyses difficult to perform analatycally
  
  //Treat Sample as if it were the population
  What it's' good for:
    1. cacluating standard errors
  2. forming confidence intervals
  3. Performing hypothesis tests
  4. Improving predictors
  
V2 - BOOTSTRAPPING -
***GREAT FOR : treating sample as if it were the population. I can:
  calculating SE, 
  forming confidence intervals,
  performing hypothesis tests,
  improving predictors by averaging them
  
//Central Dogma of Stats
//FINDING MEAN
  If I want to know the mean about a population,
  get x # of samples from a sample of the pop
  and get the means from those sub samples.
//Now we can recalculate the statistic (mean of each sample)
//and get an idea on how variable our estimate of the mean would be if we took new samples from the entire population

//Example:
1. //Set Seed
set.seed(333);
2. //Generate 30 normal random variables. Each time I make samples, I make only 30 values.
x <- rnorm(30)
3. //Save each mean to a vector, bootMean
bootMean <- rep(NA, 1000)
for(i in 1:1000){bootMean[i] <- mean(sample(x,replace = TRUE))}
4. //Set 30 normal rv in each 1000 iterations and take its mean. this is repeated sampling of a sample
sampledMean <- rep(NA,1000)
for(i in 1:1000){sampledMean[i] <- mean(rnorm(30))}
5. //plot distribution of means
plot(density(bootMean))
lines(density(sampledMean), col= "red")


//Can do these previous 5 steps with boot package
//boot package uses 3 pieces of info:
    1. original data, x
    2. the fn we are going to calculate in each subsample
    3. # of times we're going to recalculate the function
//Fn we will calculate many times should have 2 arguments: 1) data x 2) data i as set of indices
//More samples used, more distribution looks like population

//FINDING CONFIDENCE INTERVALS
//Bootstrap package used to calculate confidence intervals w/out having to make assumptions about
normality or any other strong parametric modeling assumptions
example:
//get package and data on nuclear data
library(boot)
data(nuclear)
//create linear model, relationg log of costs to data to the date reactor was built. Use nuclear data
nuke.lm <- lm(log(cost) ~ date, data = nuclear)
//plot nuclear data vs. log costs
plot(nuclear$date, log(nuclear$cost), pch = 19)
abline(nuke.lm, col = "red", lwd = 3)

RESULT: got regression line. Now, I will calculate bootstrap samples and refit line
//EXPLICIT METHOD
//generate new data frame (nuclear0) in each of 3 simulated examples. I take sample w/ replacement from rows of this data frame.
par(mfrow = c(1,3))
for(i in 1:3){
  nuclear0 < nuclear[sample(1:dim(nuclear)[1], replace = TRUE),]
//THEN, fit linear model, using subsample data
nuke.lm0 <- lm(log(cost) ~ date, data=nuclear0)
//plot the fit that I get from each diff. random sampling of the data
plot(nuclear0$date, log(nuclear0$cost), pch = 19)
//RESULT: Slightly diff. line w/ diff slope and diff. intercept
  
//NOW, try to calculate distribution of estimated values if we used the POPULATION and recalculate linear model coefficients. Do this with boot fn.
1. Use boot fn on nuclear data
2. Do 1000 replications
3. Calculate statistic 'bs'
4. Send it one additional parameter 'formula parameter', which is formula used in model
5. bs fn subsample data according to random indices
6. Fits linear model using formula in boot fn.
7. Returns coefficients from fitting this linear model 
8. Now, plot density or distribution of the estimated slope terms across all 1000 sample data sets
9. Plot estimated (curve) and actual (line in the middle) observed slope term
  bs < function(data, indices, formula) {
    d <- data[indices,]
    fit <- lm(formula, data=d); return(coef(fit))
  }
  results <- boot(data=nuclear, statistic=bs, R=1000, formula = log(cost) ~ date)
  plot(density(results$t[,2]), col="red", lwd=3)
  lines(rep(nuke.lm$coeff[2], 10), seq(0,8, length = 10), col="blue", lwd = 3)
  
10a. Finally, w/ bootstrap, I have idea of estimated value and estimated distribution across repeated samplings from the population.
10b. Use those estimated values and distribution to calculate confidence intervals for linear model by using boot.ci
    -(ie - get confidence intervals for regression terms using fit from bootstrap model)
boot.ci(results)
  
NOTE: These CI's' are less parametric than assumptions from 'confant' fn.

  //IN MORE DETAIL IN VIDEO AND SLIDES: BOOTSTRAPPING FOR:
//HYPOTHESIS TESTING
//Find CI for sample median (very tough)
  
V3 - BOOTSTRAPPING FOR PREDICTION
//if we make prediction, how confident are we in prediction
  generate data
  that have the same trend observed in the
  original data set.
  Plus noise that is about the same order of
  magnitude of the original data set but is
  randomized using this inds index.
//get SE, prediction interval. Near center (where more data points), there's' less error
  
//BOOTSTRAP AGGREGATING (BAGGING) - improves prediction acuracy
  basic idea:
    1. resample cases and recalculate predictions
    2. Avg. or majority vote
  NOTE: similar bias, reduced variance (lower mean sq error and lower error ratess), more useful for non-linear fns
      -great for predictions for non-linear fns
  
  
//Bagged Loess curve
  -has lower error rate. Looks cool :)

//Bagged Trees
1. resample data
2. recalculate tree
3. average/mode of predictors

More stable, but not good with forests
//get bagged tree use 'ipred' package 


//Random Forests - more common than bagged trees and uses 1 less step
1. bootstrap samples
2. at each split, bootstrap variables
3. grow multiple trees and vote

Pros:
  1. Accuracy
Cons:
1. Speed
2. Interpretability
3. Overfitting

//predicting new values
//has prediction boundaries
//good for plotting data poorly made with linen 

V4 - COMBINING PREDICTORS
//Gets high accuracy and reduces interpretability by combining classifiers by
1. averaging or
2. voting

Big tradeoff: accuracy vs. interpretability

Diff. models given diff. weights.

//Basic Intuition - Majority vote - combining diff. models
1. Suppose there's' 5 completely independent classifiers
2. If accuracy is 70% for each test, then combining tests will give me higher accuracy:
  ex: if 70% accuracy for each, then: 10 x (0.7)^3(0.3)^2 + 5(0.7)^4(0.3)^1 + 1(0.7)^5 = 83.7% majority vote accuracy
With 101 independent classifiers, 99.9% majority vote accuracy

Approaches for combining classifiers
1. Bagging
2. Boosting (improvement over standard prediction models) - not discussed in this class
3. Combining diff. classifiers

//EXAMPLE TO COMBINE MODELS TO GET BETTER ACCURACY
//Get devtools package
library(devtools)
//Install mdeley and mewo2 package from github - allows me to calculate RMSE values
install_github("medley","mewo2")
//Set Seed
set.seed(453234)
//generate y values
y <- rnorm(1000)
//Then, generate 6 types of 'x' values that have different relationships to 'y'
x1 <- (y > 0)
x2 <- y*rnorm(1000)
x3 <- rnorm(1000,mean=y,sd=1); x4 <- (y > 0) & (y < 3)
x5 <- rbinom(1000,size=4,prob=exp(y)/(1+exp(y)))
x6 <- (y < -2) | (y > 2)
//Create dataset with with 'y' values and all the diff. 'x' values that can be used to predict 'y'
data <- data.frame(y=y,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6)
//Create subsample of this data set of size 500 - call it a training set - so I can build values on the training data & compare diff. prediction models
train <- sample(1:1000,size=500)
trainData <- data[train,]
//Remaining data is the test set
testData <- data[-train,]

//So, first thing to do is I can fit a linear model that relates 'y' to remaining variables 'x'.
library(tree)
//use tilda to compare 'y' to remaining variables in the data frame trainData. Use '.' to do this.
lm1 <- lm(y ~.,data=trainData)
//Then, calculate predicted values using predict fn() on the test dataset. I then calculate the root mean squared error (RMSE)
//in order to compare values and predict values that actually occur TEST dataset
rmse(predict(lm1,data=testData),testData$y)

NOTE: The smaller RMSE (the less variance), the better predictor I have

//Another way to do this is use the tree fn()
tree1 <- tree(y ~.,data=trainData)
rmse(predict(tree1,data=testData),testData$y)
tree2 <- tree(y~.,data=trainData[sample(1:dim(trainData)[1]),])
//Now lets add in 2 models
combine1 <- predict(lm1,data=testData)/2 + predict(tree1,data=testData)/2
rmse(combine1,testData$y)

//Now lets add in 3 models
combine2 <- (predict(lm1,data=testData)/3 + predict(tree1,data=testData)/3
             + predict(tree2,data=testData)/3)
rmse(combine2,testData$y)

//NOTE: Medley Package written by guy who won lots of Kaggle competitions
//Another prediction model - not talked about in this class
library(e1071)

example:
#library(devtools)
#install_github("medley","mewo2")
library(medley)
library(e1071)
library(randomForest)
x <- trainData[,-1]
y <- trainData$y
newx <- testData[,-1]
  
//create a matrix of predictors with defined variables
m <- create.medley(x, y, errfunc=rmse);
for (g in 1:10) {
  m <- add.medley(m, svm, list(gamma=1e-3 * g));
}

//can add diff. models to our combined model
for (mt in 1:2) {
  m <- add.medley(m, randomForest, list(mtry=mt));
}

//can take to x% of prediction models
m <- prune.medley(m, 0.8);
rmse(predict(m,newx),testData$y)

- [Bayesian model averaging](http://www.research.att.com/~volinsky/bma.html)
- [Heritage health prize](https://www.heritagehealthprize.com/c/hhp/details/milestone-winners)
- [Netflix model blending](http://www2.research.att.com/~volinsky/papers/chance.pdf)