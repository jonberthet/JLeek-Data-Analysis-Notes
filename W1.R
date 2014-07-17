class()  //Says that it's character, numeric, logical (TRUE/FALSE)
Vectors = set of values with same class
    c(1,2,3,4)
    c("Jon", "Bob")
Lists = a vector of values of possibly different classes
        v1 = c(1,2,3,4)
        v2 = c("jon", "bob")
        mylist = list(number = v1, ppl = v2)
Matrices = vectors with multiple dimensions
        myMatrix = matrix(1,2,3,4), byrow = T, nrow = 2

Data Frames
        matrices with multiple vectors put into a graph

Factors = qualitative variables that can be included in models
    smoker = c("yes", "no", "yes", "no")
    smokeFactor = as.factor(smoker)     //use function as.factor!!

Missing Values = usually coded NA
      is.na(vector1)                    // to determine which values are missing

Subsetting
      Use [1] to get the first value in the dataframe
      Use [6] to get the 6th value in the dataframe

Logical Subsetting
        Will return value if only if true
        ie - myDataFrame[firstNames = "jeff",]
            myDataFrame[height <190,]                //Returns rows less than 90. I could add anything after the ',', but that would deal with columns
r
Variable naming Conventions
  Camel Caps = myHeightCM = 188
  Underscore = my_height_cm = 188
  Period =    my.height.cm = 188

SIMULATION BASICS
Important Simulation Functions
Distributions - always starts with r (rbeta, rbinom)
Densities - always starts with d (dbeta, dbinom)
Sampling - with and w/out replacement

rfoo functions general data                   ***args(XXX) = tells me argument for fn***

rbinom's function is (n, size, prob) = (number of trials, number of coins flipped, probability of each coin) -> pumps out how many coins are heads

dfoo functions calculate the density
Normal
dnorm
dbinom
sample draws a random sample
    sample(x,size=10,replace=FALSE)   // x = sample data, size = # of random samples we'd like to take, prob = which probabilities per data

sample can draw according to a set of probabilities
    probs = c(0.4,0.3,0.2, 0.1,0,0,0,0,0)     // Must correspond with exactly same count of data ----  sample()

Setting a Seed = ensures reproducible results from random process in R
  set.seed(12345)   //has 1 argument, which is a single integer, then run that function that generates random variables (like rnorm), or samples from random sample (like sample) . As long as seed is set and generate variable in the same order, we'll get the same values out. MUST KEEP ORDER OF SAMPLING DATA THE SAME TO GET THE SAME RESULTS OUT

http://cran.r-project.org/web/views/Distributions.html

                          TYPES OF DATA ANALYSIS QUESTIONS
1. Descriptive: Describe a set of data
  First kind of data analysis performed
  Commonly applied to census data
  description and interpretation are diff. steps
  Descriptions can't usually be genearlized w/out additional stats modeling

2. Exploratory: Find relationships you didn't know about
  Good for discovering new connections
  useful for future studies
  usually not the final say
  this should not be used for generalizing/predicting alone

3. Inferential Analysis : use relatively small sample of data to say something of a bigger data
  -common goal of stats models
  -involves estimating both on quantity you care about and uncertainity about your estimate
  -depends heavily on both population and sampling scheme

4. Predictive Analysis : use data on some objects to predict values for another object
  If X predicts Y, it doesn't mean that X causes Y
  Accurate prediction depends heavily on measuring right variables
  Although there's better and worse prediction models, more data and a simple model works well

5. Causal Analysis:  to find out what happens to 1 variable when you make another variable change
  -need randomized studies to identify causation
  -there's approaches to inferring causation in non-randomized studies, but they are complicated and sensitive to assumptions
  -usually ID'd as average effects, may not apply to every individual
  -usually the 'gold standard' of data analysis

Mechanistic Models : understand the exact changes in varibales that lead to changes in other variables for individual objects
  -very hard to infer, except in simple situations
  -usually modeled by deterministic set of equations (physical/engineering)\
  -geerally random component of data is measurement error
  - if equations are known but parameters are not, they may be inferred w/ data analysis

            SOURCES OF DATA

CENSUS : measure each individual (no inferential problem)

OBSERVATIONAL :
set.seed(5)      <- sets seed to make results reproducible
sample(1:8, size=4, replace=FALSE)      <-takes a sample of ppl 1 thru 8, pick a sample of size 4,and don't replace any ppl, so each individual can only be sampled once)

CONVENIENCE :
probs = c(5,5,5,5,1,1,1,1)/16         <- in this situation, proximity makes it easier to pick ppl closer to us than ones farther away. So it's 5x more likely to pick the first 4 people, and only 1x more likely to pick the latter 4 ppl

sample(1:8, size = 4, replace=FALSE, prob=probs)    <-shows that it's easier to choose first 4 ppl than latter 4 ppl

Randomized Trial : if we want to do causal analysis, do this!

Prediction Study : need 2 sets of data: 
1st - a training set where we build a predictive model and 
2nd, a test set where we evaluate the predictive model

Cross-Sectional : picks a particular time point and studies them at that time

Longitudinal : follows same sample of ppl over time

Retrospective : take random sample at the end of time, measure their outcome (inferential, rather than predictive or causal)