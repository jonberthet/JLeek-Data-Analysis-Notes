//Lesson 1 - Clustering
widely used in medical to determine what people are doing with smartphones

//Create vector that is a numeric value to activities or per variable. Note double ==
par(mfrow = c(1,2))
numericActivity <- as.numeric(as.factor(cameras$Location.1))[cameras$subject == 1]

//Plot first variables
plot(samsungData[samsungData$subject==1,1],pch=19,col=numericActivity,ylab=names(samsungData)[1]) 
plot(samsungData[samsungData$subject==1,2],pch=19,col=numericActivity,ylab=names(samsungData)[2]) 
legend(150,-0.1,legend=unique(samsungData$activity),col=unique(numericActivity),pch=19)

//Try to cluster data based on average acceleration of object - but not very clear if lot of data is similar
source("http://dl.dropbox.com/u/7710864/courseraPublic/myplclust.R")
distanceMatrix <- dist(samsungData[samsungData$subject==1,1:3])
hclustering <- hclust(distanceMatrix) 
myplclust(hclustering,lab.col=numericActivity)

//Or I do a SVD - where orthogonal or 2 plots are uncorrelation with each other
par(mfrow=c(1,2)) 
plot(samsungData[samsungData$subject==1,10],pch=19,col=numericActivity,ylab=names(samsungData)[10]) 
plot(samsungData[samsungData$subject==1,11],pch=19,col=numericActivity,ylab=names(samsungData)[11])

//left singular vector actually represents
the average of potentially multiple
patterns that are observed in the dataset.
So, we want to actually go back and see if
we can discover, what are the variables
that contribute to this pattern that we are
observing here.

//'So, the way that we do that is that we
look at the right singular vector that
corresponds to the left singular vector
that gives that pattern.' So again, we, we are looking at the second
left singular vector.

//So now, we look at the second right
singular vector. this is the v component of the svd and
it's the second column of v and we plot
that.'
plot(svd1$v[,2], pch = 19)

//Next, pick out some of these variables
that have a very high level of the weight,
so that means they're contributing a lot
of the variation to that pattern
that we've observed in the left singular
vector. calculated what the max variable the max
weight was for the second right singular
vector.
So, this is the variable that contributed
the most to the second left singular
vector's pattern.
The one that distinguished walking up from
walking down.

//you see that they've kind of been
separated, and the reason why is that we
picked out a variable using sort of a
multivariate technique.
We identified a variable that was
contributing to a major pattern in the
dataset that separated these activities
out.
So, we're able to sort of start to
identify the activities that distinguish
the patterns of variation in activity
monitoring from these Samsung devices.
The cool thing about these discovery
techniques is that we can actually go back
and see what variable that we've picked to
include in our model and include in our
clustering.

//Also use K-means clustering to do the same thing. Tell kmeans to give you a certain amount of clusters. Do that trial 100 times, average out the area of all those clustering


//LESSON 2 - BASIC LEAST SQUARES
Statistical Modeling Goals:
  1.Describe distribution of variables
  2. Describe relationships b/w variables
  3. Make inferences

//Plot data of childs' heights' and parents' heights' from data set
library(UsingR); data(galton)
//set parameters (rows, columns)
par(mfrow=c(1,2))
//plot
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)

//Highlight mean of bar chart for 'child' chart
lines(rep(meanChild,100),seq(0,150,length=100),col="red",lwd=5)

//plot child vs avg parent heights
plot(galton$parent,galton$child,pch=19,col="blue")

//jitter data adds tiny bit of random noise to each variable to give more oomph to data points. shows how much data is in the middle vs on the ridges
plot(jitter(galton$parent,factor=2),jitter(galton$child,factor=2),pch=19,col="blue")

//we can ask question, 'supposing parent's height is 64 inches tall, what is the average child height. Do this query:
plot(galton$parent,galton$child,pch=19,col="blue")
near65 <- galton[abs(galton$parent - 65)<1, ]
points(near65$parent,near65$child,pch=19,col="red")
lines(seq(64,66,length=100),rep(mean(near65$child),100),col="red",lwd=4)

//Know average child height for all parent heights. linear regression
//Use lm function (linear model)
  
//plot data
plot(galton$parent,galton$child,pch=19,col="blue")
//define a linear relationship.
lm1 <- lm(galton$child ~ galton$parent)
//plot the linear relationship
lines(galton$parent,lm1$fitted,col="red",lwd=3)
or
abline(lm1, col = "red", lwd = 3)

//Remember, equation of a line, plus allowing for variation (or everything we didn't measure' like how they eat, where they live, do they stretch) with variable 'e'
C = b + bP + e
C = height of child
P = height of avg. parent
e = error factor (everything we didn't measure')

//Best line to make the distance b/w equation of line and child height the smallest. lm() does this equation:
928 
 ∑    (Ci !{b0 +b1Pi})^2
i=1

//Create a least sq line that makes the sum of residuals the smallest
par(mfrow=c(1,2))
plot(galton$parent,galton$child,pch=19,col="blue")
lines(galton$parent,lm1$fitted,col="red",lwd=3)

//create a cloud like graph that plots the residuals
plot(galton$parent,lm1$residuals,col="blue",pch=19)
abline(c(0,0),col="red",lwd=3)

//LECTURE 3 - INFERENCE BASICS
Inference is about creating a conclusion of a sample, and what that conclusion says about the population

Fit a line to the data
lm1
//Gives me what I called and teh coefficients (intercept )
lm(formula = galton$child ~ galton$parent)

**If intercept = 23.9 and galton$parent = 0.64, if I increase parent's height' by 1 unit (1 unit = 1 inch), i'll get' a corresponding increase in the child's height' of 0.64 inches

//Create scenario with a million families based on the fit from the original data set. Data set has 1 million rows
newGalton <- data.frame(parent=rep(NA,1e6),child=rep(NA,1e6))

//generate the parent's height from a distribution where the mean of that distribution is the average of the parent's heights in the Galton dataset and the standard deviation is the standard deviation of the parent's heights in the Galton dataset.
newGalton$parent <- rnorm(1e6,mean=mean(galton$parent),sd=sd(galton$parent))

//generate child's heights from model fit from Galton data. we're making up child's heights with intercept term as the dalton model (1st coeff), plus 2nd coeff (with slope) with parent value, plus some noise (error term) (rnorm)
newGalton$child <- lm1$coeff[1] + lm1$coeff[2]*newGalton$parent + rnorm(1e6,sd=sd(lm1$residuals))m that have the same averages, sd, and y intercept as the galton data

//Do smooth scatter of 1 million families
smoothScatter(newGalton$parent,newGalton$child)

//put a line through it
abline(lm1,col="red",lwd=3)

//Take a sample from the million sample data set
//red line is from original fit of Dalton data
//black line is line we get when we fit the linear model on the sub-sample of data
//Create sampleGalton2, 3, 4... to create new samples of the million family data set
  
set.seed(134325); sampleGalton1 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm1 <- lm(sampleGalton1$child ~ sampleGalton1$parent) 
plot(sampleGalton1$parent,sampleGalton1$child,pch=19,col="blue") 
lines(sampleGalton1$parent,sampleLm1$fitted,lwd=3,lty=2)
abline(lm1,col="red",lwd=3)

//Another sample example
  
sampleGalton2 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm2 <- lm(sampleGalton2$child ~ sampleGalton2$parent)
plot(sampleGalton2$parent,sampleGalton2$child,pch=19,col="blue")
lines(sampleGalton2$parent,sampleLm2$fitted,lwd=3,lty=2)
abline(lm1,col="red",lwd=3)
  
//Let's do these' samples 100 times

sampleLm <- vector(100,mode="list") 
for(i in 1:100){
  sampleGalton <- newGalton[sample(1:1e6,size=50,replace=F),]
  sampleLm[[i]] <- lm(sampleGalton$child ~ sampleGalton$parent) }

//Smooth scatter these 100 samples
smoothScatter(newGalton$parent,newGalton$child)
for(i in 1:100){abline(sampleLm[[i]],lwd=3,lty=2)} 
abline(lm1,col="red",lwd=3)

//Compare original data and sample data:
//set parameters
par(mfrow=c(1,2))

//histogram of intercept terms. Centered on the intercept that was used to generate the data
hist(sapply(sampleLm,function(x){coef(x)[1]}),col="blue",xlab="Intercept",main="") 

//histogram of slope terms from 100 models - generated from actual slope that generated the data
hist(sapply(sampleLm,function(x){coef(x)[2]}),col="blue",xlab="Slope",main="")

S. E. (b0) = standard error = sq.rt of sd dev.

//summary()
Tells me what I called, the residuals, the coefficients (intercept, which givs the estimate of the intercept of the specific dataset, estimate of slope, and st. error ), significance, and standard error

//Estimating Values
//In blue histogram, Gives me distribution of slopes if I ran the study 100 times
hist(sapply(sampleLm,function(x){coef(x)[2]}),col="blue",xlab="Slope",main="",freq=F)

//With red line, gives me normal distribution that I estimate using estimated slope and variance from 1 subsample dataset
lines(seq(0,5,length=100),dnorm(seq(0,5,length=100),mean=coef(sampleLm4)[2],
sd=summary(sampleLm4)$coeff[2,2]),lwd=3,col="red")

//notice slope value is slightly different b/w 100 samples and 1 sample, but the variance is about the same

//We standardize variables to make comparison across variables and make distributions simple
we have 2 coefficients - can create a t-distribution (formula provided)
(b0 - b0) / (standard error(b0)) ~ t n-2
degrees of freedom ~ number of samples - number of things you estimated
//plot normal curve, then 2 other w/ diff degrees of freedom
x <- seq(-5,5,length=100) 
plot(x,dnorm(x),type="l",lwd=3)
lines(x,dt(x,df=3),lwd=3,col="red")
lines(x,dt(x,df=10),lwd=3,col="blue")

//Confidence Intervals (CI)// <- calculated by using T-distributions and using estimates and SE
can get beta1 and SE using lm()
//to get CI
//first, get info
summary(sampleLm4)$coeff

//second, get CI, at say, a 95% CI
confint(sampleLm4, leve = 0.95)

//AWESOME - Can produce 100 CI from the the data, and see central point of all those CI, also showing which CIs don't touch the center (ie - outside CI)'
par(mar=c(4,4,0,2));plot(1:10,type="n",xlim=c(0,1.5),ylim=c(0,100), xlab="Coefficient Values",ylab="Replication")
for(i in 1:100){
  ci <- confint(sampleLm[[i]]); color="red";
  if((ci[2,1] < lm1$coeff[2]) & (lm1$coeff[2] < ci[2,2])){color = "grey"} segments(ci[2,1],i,ci[2,2],i,col=color,lwd=3)
} 
lines(rep(lm1$coeff[2],100),seq(0,100,length=100),lwd=3)

//IMPORTANT: ***How to report all this analysis
"A one inch increase in parental height is associated with a 0.77 inch increase in child's height (95% CI: 0.42 - 1.12 inches."

Video 4 - P-Values

P-value

Idea: suppose nothing is going on - it's tells me 'how unusual is it to see the estimate we got'?'

Approach:
1.  Define the hypothetical distribution of a data summary (statistic) when "nothing is going on" (null hypothesis)
2.  Calculate the summary/statistic w/ the data we have (test statistic)
3.  Compare what we calculated to our hypothetical distribution and see if the value is 'extreme' (p-value)

//Using Galton Data
library(UsingR); data(galton)

//plot galton data
plot(galton$parent,galton$child,pch=19,col="blue")

//do linear regression analysis on galton data
lm1 <- lm(galton$child ~ galton$parent) 

//plot linear regression
abline(lm1,col="red",lwd=3)

//define null distribution

x <- seq(-20,20,length=100)

//has 926 defgrees of freedom because we use 2 of those samples to estimate the parameters
plot(x,dt(x,df=(928-2)),col="blue",lwd=3,type="l")

//calculate (find) real value (t statistic), which is (b1 hat / its SE)
arrows(summary(lm1)$coeff[2,3],0.25,summary(lm1)$coeff[2,3],0,col="red",lwd=4)

//BIG CONCLUSION: Since my data is all the way in the tails (near 0), it is unlikely that if nothing was happening, we would assume that this stat that we calcuated would be drawn from this distribution and its pretty likely tit would come from the area near 0.

//So how do we calculate p-value?

//view this
summary(lm1)

//I get
Call:
  put variables on right side of the tilda

Estimate Std = intercept term (23.9415) and parent's height' slope term (0.646)
Standard Error
t-values = estimate / Standard Error
p-value = if very small, then has ***, which says, that's as' far as this computer is calculating

//Simulated example of graphing area under curve to calculate p-value given. P-value must be no more than 1. 

A small p-value (typically ≤ 0.05)  strong evidence against the null hypothesis, so you reject the null hypothesis.
A large p-value (> 0.05)  weak evidence against the null hypothesis, so you fail to reject the null hypothesis.
p-values very close to the cutoff (0.05) are considered to be marginal (could go either way). Always report the p-value so your readers can draw their own conclusions.
OR
P < 0.05 (significant)    <- arbitrary # a statistician said when some1 asked him what is a sig. value
P <0.01 (Stonrgly significant)
P < 0.001 (very significant)

People report both the confidence interval and P-values
report this info using
summary(lm(galton$child ~ galton$parent))$coeff
***(with info provided) "A one inch increase in parental height is associated with a 0.77 inch increase in child's height (95% CI: 0.42 - 1.12 inches). This diffence was statistically significant (P < 0.001)"

**VIDEO 5 - REGRESSION WITH FACTOR VARIABLES
-p-value is comparing quant to quant
-however, it's useful' when I have a factor (covariate) and I want to detect differences in a quant outcome with respect to that factor covariate
//outcome is still quantitative
//covariate(s) are factor variables
//fitting lines = fitting means
//want to evaluate contribution of all factor levels at once

//In this example, use movie info:
download.file("http://www.rossmanchance.com/iscam2/data/movies03RT.txt",destfile="./data/movies.txt 
movies <- read.table("./data/movies.txt",sep="\t",header=T,quote="")
head(movies)"

//Show rotten tomato score by rating
//use jitter command to spread out x values a little to notice densities. Plot out data
plot(movies$score ~ jitter(as.numeric(movies$rating)), col="blue", xaxt="n", pch = 19)

//put in axis
axis(side=1, at=unique(as.numeric(movies$rating)), labels=unique(movies$rating))
              
//calculate mean score by rating
//Take movie score and break it down by the movie rating, and within each rating I apply the mean function
meanRatings <- tapply(movies$score,movies$rating,mean)
points(1:4,meanRatings,col="red",pch="-",cex=5)
       
              Si = b0 +b1()(Rai =" PG ")+b2()(Rai =" PG"13 ")+b3()(Rai =" R ")+ei"

The notation ()(Rai =" PG ") is a logical value that is one if the movie rating is "PG" and zero otherwise.
b0 = average of the G movies
b0 + b1 = average of the PG movies
b0 + b2 = average of the PG-13 movies 
b0 + b3 = average of the R movies              
              
//I do a summary of these results I just did in R this way:
lm1 <- lm(movies$score ~ as.factor(movies$ratings))
summary(lm1)
              
//plot fitted values
plot(movies$score ~ jitter(as.numeric(movies$rating)),col="blue",xaxt="n",pch=19)
axis(side=1,at=unique(as.numeric(movies$rating)),labels=unique(movies$rating)) 
points(1:4,lm1$coeff[1] + c(0,lm1$coeff[2:4]),col="red",pch="-",cex=5)
              
//QUESTION 1 'What is the average diff. in rating b/w G and R movies?'
b0 + b3 - b0 = b3
or
(R rating) - (G rating) = b3
//NOTE: all b values are the estimate under the coefficients section in:
lm1 <- lm(movies$score ~ as.factor(movies$rating))

//NOTE: value of b3 term is 'Estimate'                  
summary(lm1)
                                                
//calculate confidence interval b/w G and R movies. NOTE: cuz the confidence interval includes 0 
//(which means there's no diff b/w score and movie w/ rating G or R'), then it is unlikely that 
//there's a strong difference b/w the 2 ratings in the data set.'
lm1 <- lm(movies$score ~ as.factor(movies$rating))
confint(lm1)
                                                
//QUESTION 2 'What is the avg. diff. in rating b/w PG-13 and R movies?'
(b0 + b2) - (b0 + b3) = b2 - b3

//It is simpler to rewrite the formula so that R rating is b0. Looks like this:
Si = b0 +b1(Rai =" G ")+b2(Rai =" PG ")+b3(Rai =" PG"13 ")+ei
"
b0 = average of the R movies
b0 + b1 = average of the G movies
b0 + b2 = average of the PG movies
b0 + b3 = average of the PG-13 movies

//That way, avg. diff in rating b/w PG-13 and R movies is:
b0 + b3 - b0 = b3

//NOW, I need to tell R that the intercept term is equal to the average
//score in the R movies. So I just tell R what the reference category is,
//which is the category used as the intercept term (ie - 'R' rated movies or ref = "R")
lm2 <- lm(movies$score ~ relevel(movies$rating, ref = "R"))

//Find the summary, and get b3 under Estimate
summary(lm2)
//ANSWER: b3 = 0.205
                                              
//Find the Confidence Interval
confint(lm2)
ANSWER: CI is b/w -5.146 and 5.557, since it includes 0 (null), so 'it's unlikely that there's a major diff. in score b/w R and PG-13 movies'
                                              
QUESTION 3: Is there any difference in score b/w any of the movie ratings?
//So, going back to old formula:
Si = b0 +b1()(Rai =" PG ")+b2()(Rai =" PG"13 ")+b3()(Rai =" R ")+ei
"//I do a nova analysis of variance by doing this:
//A linear model where the outcome is the movie score, And where the covariant is equal to a factor variable.
lm1 <- lm(movies$score ~ as.factor(movies$rating))
anova(lm1)

//ANSWER: Get an analysis of variance table.
//Degrees of Freedom (DF) tells me how many parameters I had to estimate for the variable.
//Residuals = # of movies - degrees of freedom
//Mean Sqs = sum of squares / respective DF
//F value is the ratio = (as.factor(movies$rating) Mean Sq) / (Residuals Mean Sq)   <- 
//P-value of 0.43 doesn't suggest a very strong diff. b/w diff. categories

//Tukey's (honestly significant difference test) // aov() 
//calculates differences b/w all possible means of pairs of factors and qualitiative variables
lm1 <- aov(movies$score ~ as.factor(movies$rating))
TukeyHSD(lm1)


VIDEO 6 - Multiple Variable Regression
//Regression with multiple covariates
//still using least squares/Central Limit Theorem
//interpretation depends on all variables

//Example: Use WHO Childhood hunger data
download.file("http://apps.who.int/gho/athena/data/GHO/WHOSIS_000008.csv?profile=text&filter=Country:;SEX:", destfile = "./WHODATA.csv")
hunger <- read.csv("./WHODATA.csv")

//Eliminate the rows that use the 'Both sex' data point
hunger <- hunger[WHODATA$Sex!="Both sexes",]

//ID and plot percent hungry vs. time
lm1 <- lm(hunger$Numeric ~ hunger$Year)
plot(hunger$Year, hunger$Numeric, pch = 19, col = "blue")

//Linear Model
Hu1 = b0 + b1Yi + ei
b0 = percent hungry at Year 0
b1 = decrease in percent hungry per year
ei = everything we didn not measure

//Add line (regression) to linear model
lines(hunger$Year, lm1$fitted, lwd=3, col = "darkgrey")

//Color dots by male & female
//All males are in black and all females are in red
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1)) 

//NOW, 2 LINES
//We can do a separate regression line to male and to female (2 lines)

                                  HuFi = bf0 + bf1(YFi) + efi
bf0 = percent of girls hungry at Year 0
bf1 = decrease in percent of girls hungry per year 
efi = everything we didn't measure'
&
                                  HuMi = bf0 + bf1(YMi) + emi
bm0 = percent of boys hungry at Year 0
bm1 = decrease in percent of boys hungry per year
emi = everything we didn't measure'

//Step 1:
//define regression for male
lmM <- lm(hunger$Numeric[hunger$Sex=="Male"] ~ hunger$Year[hunger$Sex=="Male"])

//define regression for female
lmF <- lm(hunger$Numeric[hunger$Sex=="Female"] ~ hunger$Year[hunger$Sex=="Female"])

//Step 2:
//plot graph
plot(hunger$Year,hunger$Numeric,pch=19)

//Step 3:
//plot lines
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1)) 
lines(hunger$Year[hunger$Sex=="Male"],lmM$fitted,col="black",lwd=3) 
lines(hunger$Year[hunger$Sex=="Female"],lmF$fitted,col="red",lwd=3)

ANSWER: "Looks like females tend to be less hungry than boys over a longer spectrum of time and hunger has been decreasing over time."

//NOW, 2 lines, same slope
                                  
                                  Hui = b0 + b1(Sexi = "Male") + b2Yi + emi
b0 - percent hungry at year zero for females
b0 + b1 - percent hungry at year zero for males
b2 - change in percent hungry (for either males or females) in one year
e∗ - everything we didn't measure'

//Define linear relationship
lmBoth <- lm(hunger$Numeric ~ hunger$Year + hunger$Sex)

//plot dots
plot(hunger$Year,hunger$Numeric,pch=19)

//color the points
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1))
                                  
//plot red Male line
abline(c(lmBoth$coeff[1],lmBoth$coeff[2]),col="red",lwd=3)

//plot female black line. Same slope lines, but a little off cuz it considers 
abline(c(lmBoth$coeff[1] + lmBoth$coeff[3],lmBoth$coeff[2] ),col="black",lwd=3)

//NOW, 2 lines, different slopes (interactions)
another new formula - new variables
                                  
//define linear relationship, plot dots, plot lines
lmBoth <- lm(hunger$Numeric ~ hunger$Year + hunger$Sex + hunger$Sex*hunger$Year)
plot(hunger$Year,hunger$Numeric,pch=19) 
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1)) 
abline(c(lmBoth$coeff[1],lmBoth$coeff[2]),col="red",lwd=3)
abline(c(lmBoth$coeff[1] + lmBoth$coeff[3],lmBoth$coeff[2] +lmBoth$coeff[4]),col="black",lwd=3)

//IMPORTANT: look @ summary of terms
//R indicates interaction b/w variables with a colon.
//because hunger$Year : hunger$SexMale estimate coeff. is -0.02821, there's lil. difference, that's why slopes are so similar
                                  
//Interactions for continuous variables needs lots of care
                                  
Video 7 - REGRESSION IN THE REAL WORLD
THINGS TO LOOK OUT FOR

1. Confounders  -  a variable correlated with both outcome and the covariates
                    -can change the regression line
                    -can change sign of line
                    -sometimes detected by careful exploration
                                  
//using same hunger data set from WHO
par(mfrow = c(1,2))
plot(hunger$Year, hunger$Numeric, col= as.numeric(hunger$WHO.region), pch = 19)
plot(1:10, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
legend(1,10, col = unique(as.numeric(hunger$WHO.region)), legend = unique(hunger$WHO.region), pch = 19)
                                  
//Region is correlated with year - since we have more
//data of certain regions in certain years, did linear regression analysis
//relating year of measurement to what region it's' coming from.
anova(lm(hunger$Year ~ hunger$WHO.region))

//ANSWER: significant p-value and moderate f-value. it's a strong' association with year
                                  
//Region is correlated with hunger. Do another anova:
anova(lm(hunger$Numeric ~ hunger$WHO.region))
                                  
ANSWER: Large f-value and small p-value - which suggests that there's' a strong correlation b/w region and hunger
                                  
Including region - a complicated interaction
plot(hunger$Year,hunger$Numeric,pch=19,col=as.numeric(hunger$WHO.region))

//left hand side of tilda = % of those hungry  and right hand side of tilda = year + region + interaction b/w yr and region
lmRegion <- lm(hunger$Numeric ~ hunger$Year + hunger$WHO.region + hunger$Year*hunger$WHO.region ) 

//If I look at interaction b/w yr and region, 
//then for every single region (color), I get a different regression line
//below, I plot regression line for Southeast Asia, one of the 2 coefficient
//terms must be added together to get an intercept term and 2 must be added 
//together to get a slope. Must be right coefficients. UGH! (didn't work' for me)
abline(c(lmRegion$coeff[1] + lmRegion$coeff[6],lmRegion$coeff[2]+ lmRegion$coeff[12]),col=5,lwd=3)
                                  
2. - Income Data
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", destfile = "./adult.data")
incomeData <- read.csv("./data/income",header=FALSE)
income <- incomeData[,3]
age <- incomeData[,1]

//plot the data
//put 4 graphs in 4 columns in the same row
par(mfrow = c(1,4))

//shows most data gathered on left bottom
smoothScatter(age,income)

//shows skewed right data - makes hard for normal regression line
hist(income,col = "blue", breaks = 100)
                                  
//So, if I have right skewed data, ppl commonly take 
//the log of income (makes more symmetric).
//NOTE: added 1 to log cuz if some of income values equaled 0,
  //then it would be NA, which would be ignored by R, but might be 
  //incredibly important, if I have a bunch of 0 values in data
hist(log(income+1), col = "blue", breaks = 100)
smoothScatter(age, log(income+1))

3.  Outliers  -  don't follow' pattern of other data points
-I can delete the outliers, or
- sensitivity analysis - is it a big diff if I leave it in/take it out?
- Logs - if the data are skewed
- Robust methods - Robust in R or rlm()
                                  
4.   Variance Changes
//Things I can do if variance changes over time or over data set
1. Box-Cox Transform
2. Variance Stabilizing Transform
3. Weighted least squares
4. Huber-white standard errors
                                  
Keep track of:
1. Variance over time
2. Variance in units - can make more sense to do standardize or relative units (ie - deaths per 1000)
      -but affects model fits, interpretation, and inference
                                  
5.  Overloading Regression
      -fewer variable comparison the better
                                  
6. Correlation/Causation

