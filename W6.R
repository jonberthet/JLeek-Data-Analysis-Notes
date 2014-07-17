V1 - Prediction Study Design
Motivation - prizes!
1. Fun prizes Kaggle
2. heritage health prize ($3m)
3. ww.oncotypedx.com - save lives

Steps to build prediction: (this video abou steps 1-3)
  1. Find the right data (lots of data doesn't help' necessarily)
-more data > better models
-know bench mark (know if my model is better than the others)
-with binary results, probability of perfect classification is approximately (1/2)^test set sample size. key point is I need a big enough test set to truly distinguish a good model from just chance
-kaggle gives you benchmarks of where you stand
-heritagehealthprize.com tells me base prediction ability
-need to start with raw data for predictions - processing is often cross-sample
2. Define error rate (often skipped over)
//Common Error Measures
1. Mean squared error (root mean sq. error)
-used for continuous data, sensitive to outliers
2. Median absolute deviation
-used for continous data, often more robust
3. Sensitivity (sometimes known as 'recall')
-If you want few missed positives
4. Specificity
-If you want few negatives called positives
5. Accuracy
-Weights false positives/negatives equally
6. Concordance
-how well everything coordinates to make prediction. ex: http://en.wikipedia.org/wiki/Cohen%27s_kappa

Study Design
-Common study design used in Kaggle (first created in Netflix competition)
http://www2.research.att.com/~volinsky/papers/ASAStatComp.pdf
1. Start off w/ 100million user data set. Divided this into 2 sets
1. Training Data (used to build predictive function)
2. Holding Out data set (not provided to ppl building the predictive fn). Sometimes just split this 
set into test and validation set, but here Split this one too!:
  1. Test set
2. Quiz set
3. Probe set
Used training data to create fn for Probe set, then tested on the quiz set 
(then returned score on predictive ability on the quiz set. Not until the very end when the competition was nearly over, did
they apply the predictive models to the test set. Test set was held until the very end so ppl could 
overfit their models to any particular characteristics of this test set. Test was completely independent
set of data.)

Key Issues:
  1. Accuracy
2. Overfitting (tune predictive fn too much to data set - that is why you have a test data set)
3. Interpretability by normal people
4. Computational speed

3. Split data so I do not overfit the data into:
  Training
Testing
Validation set (optional)
4. On the training set pick features
5. On the training set pick and prediction fn
6. On the training set cross-validate (compare diff. prediction models or estimate test set error if we don't have a' test set at hand)
7. If no validation, apply 1x to test set
8. If validation, apply to test set and refine
9. If validation, apply 1x to validation

//TABLE PROVIDED IN SLIDE IS AWESOME! USE IN ANALYSIS!!!
  Defining true/false positives
positive = identified
negative = rejected

True positive = correctly identified (person sick, ID sick)
False positive (Type 1 error) = incorrectly identified something (person sick, ID not sick)
Positive Predictive Value = ∑True Positive / ∑Test Oucome Positive = Total times you are sick and we call you sick / Total times we call you sick from tests

True negative = correctly rejected (person healthy, ID healthy)
False negative (Type 2 error) = incorrectly rejected (person healthy, ID sick)
Negative Predictive Value = ∑True Negative / ∑Test Outcome Negative = Total times you are not sick and we call you not sick / Total times we call you not sick from tests

//How to measure avg. quality of a particular test
Sensitivity (where condition is positive) = ∑True Positive / ∑Condition Positive = # of times I'm a true positive / total # of times I'm sick. = avg. # of times I'm true positive = avg. # of times ppl are sick
  Specificity (where condition is negative) = ∑True Negative / ∑Condition Negative = # of times I'm a True Negative / total # of times I'm not sick = avg. # of times I'm true negative = avg. # of times ppl are healthy
  
  //AWESOME RESOURCES at end of lecture




V2 - CROSS VALIDATION
//A way to estimate out-of-sample error rate for predictive functions.

Key Ideas
Focusing on training data
Avoid overfitting
make predictions generalizable

Steps in building a prediction (steps 1-3 in video 1)
4. On the training set pick features
5. On the training set pick prediction fn
6. On the training set cross-validation

Overfitting
//Set sample
set.seed(12345)

//Set 2 variables (x & y) that I want to predict
x <- rnorm(10); y <- rnorm(10)

//z is outcome, which equals 0 or 1
z <- rbinom(10, size = 1, prob = 0.5)

//plot & color by z values
plot(x,y, pch = 19, col = (z+3))

//NOTICE: blue dots near middle

//Use Classifier to say that all dots from -0.2 to 0.6 are sectioned off as blue dots
par(mfrow = c(1,2))
zhat <- (-0.2 < y) & (y < 0.6)
plot(x,y, pch = 19, col = (z+3));
plot(x,y, pch = 19, col = (zhat+3))

//NOTE: Perfect prediction

//New data with more scattered dots. If I use same classification,
very bad at predicting. Classifier good on training set is bad on test set

Key Idea:
  1. Accuracy on training set is optimistic
  2. A better estimate comes from an independent set (test set accuracy)
  3. Can't use test' set when building model or it becomes part of training set
  4. So we estimate test set accuracy with the training set

//Basic Approach for Cross Validation
pproach:
1. Use the training set
2. Split it into training/test sets
3. Build a model on the training set
4. Evaluate on the test set
5. Repeat and average the estimated errors
Used for:
1. Picking variables to include in a model
2. Picking the type of prediction function to use
3. Picking the parameters in the prediction function 
4. Comparing different predictors

//2 Different ways to pick training/test sets

1. Random SubSampling
-Test sets are little subsets within the greater training set
-Advantage: balance size of testing & training set
-Disadvantage: may get same elements in testing or might get elements that don't appear' among random samples

2. K-fold 
-Split sample into 3 (can be any 4,5,6...infinite parts. Extremes of breaking up sample into all but 1 sample point or exactly 1/2 is sample & test set) parts and test set is 1st, 2nd, or 3rd portion.
  Sample set is remaining 2/3 of sample
  -Build training set, apply to test set, calculate eror rate (do this for all 3 tests)

NOTES: training & test sets must be from same population
-sampling should be designed to mimic real patterns
-Cross validation estimates have variance
--Good Notes

V3 - Predicting with Regression
Key Ideas:
  1. Use Standard REgression model with lm() and glm()
      -***When I get a new sample that I want to predict an outcome for, just:
      1. subsitute in the values for the co-variates
      2. multiply the values by the coefficients
      3. THen I get a prediction for new value of the outcome
    NOTE: Useful when linear model is more correct
          Pros:
            Easy to implement & implement
          Cons:
            Often poor performance in nonlinear settings

EXAMPLE: 
//with Old Faithful dataset
data(faithful)
//see how much data is in dataset
dim(faithful)
//choose 1/2 of dataset for sample set. More common to do 1/3 sample set
set.seed(333)
trainSamples <- sample(1:272, size = (272/2), replace = F)
trainFaith <- faithful[trainSamples,]
testFaith <- faithful[-trainSamples,]
head(trainFaith)
//Plot datafd
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col = "blue", xlab = "Waiting", ylab = "Duration")

//Fit a linear model
lm1 <- lm(eruptions ~ waiting, data = trainFaith)
summary(lm1)
//NOTE: standard error is very small, so association b/w eruption duration and waiting time is statistically significant

//Then, plot the line
lines(trainFaith$waiting,lm1$fitted,lwd=3)


//Now, relate eruption duration to a baseline level + change in eruption duration given a 1 unit change in waiting time + error term we haven't measured'
//get intercept coefficients - predicted value of duration = estimated intercept + estimated slope term times the waiting time

//Now, predict a new value. Use equation below
//Expected duration = bo + b1(WT)        //WT = Waiting Time
coef(lm1)[1] + coef(lm1)[2]*80

//Or predict a new value using: predict()
newdata <-data.frame(waiting=80)      //data frame with values I want to predict, with waiting variable set to 80. 80 can be a vector too to give me results of many #s
predict(lm1, newdata)

//Calculate the Root Mean Square Errors (difference b/w sample test and real test regression line)
//below is fitted model minus actual eruption duration
//RESULT: 5.713 on training set
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))

//Do same thing on test set. Use predicted values from LM on test set and subtract actual values. 
//RESULT: 5.827   = normal that test set has higher error than training
sqrt(sum((predict(lm1,newdata=testFaith)- testFaith$eruptions)^2))

//Do prediction intervals - likely region for data to fall
//In example below, we do a 95% liklihood interval for data to fall inside (assumes normal distribution)
pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting) 
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue") 
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

Using Binary Data (with Raven win data)
//Best to use a logistic regression. Not much data to work with, so didn't split' b/w sample and test set.
//Here, it just predicts values for actual observations in training set
glm1 <- glm(ravenWinNum ~ ravenScore,family="binomial",data=ravensData) 
par(mfrow=c(1,2))
boxplot(predict(glm1) ~ ravensData$ravenWinNum,col="blue")

//By saying type = "response", I ask for the predicted probabilty that the Ravens are going to win each game
//Here, values are b/w 0 and 1
boxplot(predict(glm1,type="response") ~ ravensData$ravenWinNum,col="blue")

//Choosing a cutoff(resubstitution) to see when errors are lowest with using certain # of wins
xx <- seq(0,1,length=10); err <- rep(NA,10) for(i in 1:length(xx)){
  err[i] <- sum((predict(glm1,type="response") > xx[i]) != ravensData$ravenWinNum) }
plot(xx,err,pch=19,xlab="Cutoff",ylab="Error")

Comparing models with cross validation
library(boot)
cost <- function(win, pred = 0) mean(abs(win-pred) > 0.5)
//1st model
glm1 <- glm(ravenWinNum ~ ravenScore,family="binomial",data=ravensData) 
//2nd model
glm2 <- glm(ravenWinNum ~ ravenScore,family="gaussian",data=ravensData) 
//define error measure, which is if prediction is greater than 0.5 away 
//from the actual value of whether I want or not, I'm going' to call an error!
  //Then avg. those values those errors over all possible values that I'm going to' predict. So win = 1, lost = 0. So if prediction is >1.5 or <0.5, then it's an error'

//Use cv() (in the boot package) - make it do 3 fold cross validation. Do this for each of the 2 models
cv1 <- cv.glm(ravensData,glm1,cost,K=3)
cv2 <- cv.glm(ravensData,glm2,cost,K=3)

//Get results - look at delta term to see cost i get per model
cv1$delta
cv2$delta

RESULT: glm1 (logistic regression model) has slightly lower costs for both different folds than glm2 (gaussian model)
//So, using binary outcome works a lil. better



V6 - PREDICTING WITH TREES - predicting with functions that capture non-linearities better than linear relationships
Iteratively split variables into groups
split with max predicitiviness
evaluate 'homogeneity' within each branch
fitting multiple trees often works best (forests)

Pros:
  easy to implement & interpret
  Better performance in nonlinear settings
Cons:
  W/out pruning or cross validation, very prone to overfitting
  Hard to estimate certainty
  results can be very variable

Basic Algorithm
1. Start with all variables in 1 group
2. Find the variable & split that best separates the outcomes
3. Divide the data into two groups ('leaves') on that split ('nodes')
4. Within each split, find the best variable/split that separates the outcomes
5. Continue these steps until the groups are too small or sufficiently 'pure'

MATH MEASURES OF IMPURITY
P-hat (mk) = fraction of observations put to group 'm' that have outcome 'k'
Misclassification error = 1 - P-hat(mk)
Gini Index = measure of imbalance = result maximized if I had only 2 groups = ∑P-hat(mk) (1 - P-hat(mk))
Cross-entropy or Deviance = measures homogeneity so if all members of a group have the exact same outcome, the entropy will be very small.
                            But if all members of a group get randomly diff. outcomes, then impurity will be very large
                            Goal is to find splits that make impurity smaller

Example: Iris Data
//view data
data(iris)
names(iris)
table(iris$Species)

plot pedal width & sepal width
plot(iris$Petal.Width, iris$Sepal.Width, pch=19, col=as.numeric(iris$Species))
legend(1,4.5, legend=unique(iris$Species), col = unique(as.numeric(iris$Species)), pch = 19)

NOTE: I can find differences in trends

//An alternative to finding diff. trends in # terms is library(rpart)
library(tree)
tree1 <- tree(Species ~ Sepal.Width + Petal.Width, data=iris)
summary(tree1)

RESULT: 
Classification tree:
tree(formula = Species ~ Sepal.Width + Petal.Width, data = iris)
Number of terminal nodes (Or # of leaves that end up occuring in this classification tree): 5 
Residual mean deviance (Or measure of impurity):  0.204 = 29.57 / 145 
Misclassification error rate (Or how often out of these 150 samples are misclassified - here, it's pretty low', but this is a resubstitution, not performing cross-validation): 0.03333 = 5 / 150 

//Make a tree plot
plot(tree1)
text(tree1)
                          
//Another COOLER way to look at a CART model (partitions chart in sections) 
//  - only works with 1 or 2 continous variables. More than that doesn't' work here
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species)) 
partition.tree(tree1,label="Species",add=TRUE) 
legend(1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)

//NEW EXAMPLE: Predicting New Values
set.seed(32313)
//Generating new pedal and sepal widths in uniform distribution
newdata <- data.frame(Petal.Width = runif(20,0,2.5),Sepal.Width = runif(20,2,4.5)) 
//Apply prediction function
pred1 <- predict(tree1,newdata)
pred1
                          
RESULT:
  setosa versicolor virginica 
1 0        0.02174    0.97826   //Reports probabilities of being in a particular species
2 0        0.02174    0.97826 
3 1        0.00000    0.00000

//Calculate prediction value for each data point & plot them = more accurate when boxing it
pred1 <- predict(tree1,newdata,type="class") 
plot(newdata$Petal.Width,newdata$Sepal.Width,col=as.numeric(pred1),pch=19) 
partition.tree(tree1,"Species",add=TRUE)
                          
EXAMPLE: Pruning trees: Cars
try and predict the drive train (whether it's front' wheel drive, 4 wheel drive) based on other variables in dataset). Done here:
  
//Get Data
data(Cars93,package="MASS")
head(Cars93)
//Build tree
treeCars <- tree(DriveTrain ~ MPG.city + MPG.highway + AirBags +
                   EngineSize + Width + Length + Weight + Price + Cylinders +
                   Horsepower + Wheelbase, data = Cars93)
plot(treeCars) 
text(treeCars)

//Plot errors
par(mfrow = c(1,2))
//Use fn cv.tree(). method is important. If set method to 'misclass', will tell me # of mesclassification error. Default is deviance 

//Tells me that size 6-9 has least amount of misclassifications. This is not resubstitution, cuz resubstitution error rate gets smaller as I include more splits. 
//If doing cross-validation, I need to hit a sweet split size to have least amount of errors. Otherwise, too many splits is too complicated
plot(cv.tree(treeCars, FUN=prune.tree, method = "misclass"))

//Here, I get deviance scale for prediction model
plot(cv.tree(treeCars))

//Apply pruneTree() that has exactly 4 terminal leaves/nodes. So instead of getting the complicated tree
//I had previously, it gives me the 4 best splits
pruneTree <- prune.tree(treeCars, best = 4)
plot(pruneTree)
text(pruneTree)

//Show resubstitution error with pruneTree. See that it has a few more mistakes than when I included more leaves. But this is the resubstitution error
table(Cars93$DriveTrain,predict(pruneTree,type="class"))
//fit to all data
table(Cars93$DriveTrain,predict(treeCars,type="class"))

NOTE: simpler model less perfectly tuned to data can predict better with new data set******
  
