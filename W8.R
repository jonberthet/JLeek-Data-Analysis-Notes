V1 - MULTIPLE TESTING
//Don't' fool myself!!
----taken from Stats book by Brad Efron, Stanford stats professor
1. hypothesis testing/signficance analysis is commonly overused. People just report on the lowest p-values (most significance). 
  -Try to reduce false positives (false discoveries). Correct for multiple testing
2. correcting for multiple testing avoids false positives or discoveries
3. Two key components to multiple testing corrections
  -define error measure
  -define correction (or stats method used to control error measure)

//3 eras of stats:
1. age of Quetelet and successors - huge census level data used to answer questions
2. Classical Period - develop theory of optimal inference capable of getting answers from data set (when data was expensive)
3. Era of Mass Production (now) - lots of analysis, but lots of error buildup - must account for errors 

Error Rates
1. False Positive Rate - rate at which false results (Beta = 0) are called significant = avg. of (False results are called significant / # of significant variables) //NOTE: small distinction b/w type 1 error and 'false results called sig'
2. Family wise error rate (FWER) - probability of at least 1 false positive 
3. False Discovery Rate (FDR) - rate at which claims of significance are false
                                                                                                  
//CONTROLLING FALSE POSITIVE RATE
//First, set benchmark. All normally calculated p-values below a sig. level is signficiant - control false positive rate at level 'alpha' on avg.
  ie - the expected rate of false positives is less than 'alpha'

1. First, I want to control the family-wise error rate (FWER) or the probability we're going to make 1' error.
  -use the Bonferroni correction (oldest multiple testing correction)
- Suppose you do 'm' tests
- You want to control FWER at level '\alpha' so probability rate (Pr(V \geq 1)) < 'alpha'
- Calculate P-values normally
- Take alpha level in single hyptohesis and divide it by number of hypoth tests performed: Set $\alpha_{fwer} = \alpha/m$
- Call all p-values less than \alpha_{fwer} significant //NOTE: got new alpha level and we call all p-values less than this new alph level significant - so we can control FWER
                                                                                                  
__Pros: Easy to calculate, conservative. Makes few errors
__Cons: May be very conservative. If doing many hypothesis tests, then I want to screen out more tests
                                                                                                  
2. CONTROLLING FDR
- most popular correction when performing lots of tests in genomics, imaging, astronomy, signal-processing discipline
- Suppose you do $m$ tests
- You want to control FDR at level $\alpha$ so $E\left[\frac{V}{R}\right]$
- Calculate P-values normally
- Order the P-values from smallest to largest $P_{(1)},...,P_{(m)}$
- Call any $P_{(i)} \leq \alpha \times \frac{i}{m}$ significant
                                                                                                  
__Pros__: Still pretty easy to calculate, less conservative (maybe much less)
__Cons__: Allows for more false positives, may behave strangely under dependence
                                                                                                  
Cutoff of points to determine right or low enough p-values (no correction, then FDR, then FWER) - good graph in video
            
Look in slides:
//Adjusted P-values (not p-values) can be used directly w/out adjusting alpha 
//Case Study I: No True Positives
        -Controls FWER
        -Controls FDR
//Case Study II: 50% True Positives
        -Controls FWER
        -Controls FDR
                                                                                                  
NOTES:
- Multiple testing is an entire subfield
- A basic Bonferroni/BH correction is usually enough
- If there is strong dependence between tests there may be problems
- Consider method="BY" in vdot adjust fn
                                                                                                  
#### Further resources:
- [Multiple testing procedures with applications to genomics](http://www.amazon.com/Multiple-Procedures-Applications-Genomics-Statistics/dp/0387493166/ref=sr_1_2/102-3292576-129059?ie=UTF8&s=books&qid=1187394873&sr=1-2)
- [Statistical significance for genome-wide studies](http://www.pnas.org/content/100/16/9440.full)
- [Introduction to multiple testing](http://ies.ed.gov/ncee/pubs/20084018/app_b.asp)
                                                                                                  

                                                                                                  
//V2 - SIMULATION FOR MODEL CHECKING
Way back in the first week we talked about simulating data from distributions
in R using the _rfoo_ functions.
- In general simulations are way more flexible/useful
- For bootstrapping as we saw in week 7
- For evaluating models
- For testing different hypotheses
- For sensitivity analysis
- "...or for prediction"
- At minimum it is useful to simulate
- A best case scenario
- A few examples where you know your approach won't' work
- helps you to understand the boundaries of your model(s)
- [The importance of simulating the extremes](http://simplystatistics.org/2013/03/06/the-importance-of-simulating-the-extremes/)
                                                                                                  
### Simulating data from a model
Suppose that you have a regression model:

 Y_i = b_0 + b_1 X_i + e_i
                                                                                                  
Here is an example of generating data from this model where X_i and e_i are normal rv using rnorm():                                                                                                  

set.seed(44333)
//generate 50 rv, with mean = 0, and variance =1 (default)
x <- rnorm(50)
e <- rnorm(50)
//intercept = 1
b0 <- 1
slope = 2
b1 <- 2
//see if regression model is a reasonable fit or not to the data
y <- b0 + b1*x + e
                                                                                               
                                                                                                  
### Violating assumptions can change analysis

set.seed(44333)
x <- rnorm(50)
e <- rnorm(50)
//for fatter tails
e2 <- rcauchy(50)
b0 <- 1
b1 <- 2
//set 2 sets of 'y' values
y <- b0 + b1*x + e
y2 <- b0 + b1*x + e2
//fit linear model 
par(mfrow=c(1,2))
//RESULT: see cloud of data
plot(lm(y ~ x)$fitted,lm(y~x)$residuals,pch=19,xlab="fitted",ylab="residuals")
//RESULT: Couple of residuals are very large - suggests poor fitting
plot(lm(y2 ~ x)$fitted,lm(y2~x)$residuals,pch=19,xlab="fitted",ylab="residuals")


### Repeated simulations

set.seed(44333)
betaNorm <- betaCauch <- rep(NA,1000)
//generate 1000 simulations with variables x, e and e2
for(i in 1:1000) {
  x <- rnorm(50)
  e <- rnorm(50)
  e2 <- rcauchy(50)
  b0 <- 1
  b1 <- 2
  
  y <- b0 + b1*x + e
  y2 <- b0 + b1*x + e2
  
  betaNorm[i] <- lm(y ~ x)$coeff[2]
  betaCauch[i] <- lm(y2 ~ x)$coeff[2]
}

quantile(betaNorm)
quantile(betaCauch)

//Cauchy has fat tails, more error - so it can show high variability
### Monte Carlo Error
//Show boxplot
boxplot(betaNorm,betaCauch,col="blue",ylim=c(-5,5))

Note the `ylim` param there -- the right-side boxplot's' outliers otherwise go
way off the screen.

### Simulation based on a data set
//generate some aspects of your model and try to reproduce it . Don't want' exact same product though.. Change a few aspects
library(UsingR)
data(galton)
nobs <- dim(galton)[1]

par(mfrow=c(1,2))
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)
```

### Calculating means,variances
//Create a simulation of heights
lm1 <- lm(galton$child ~ galton$parent)
parent0 <- rnorm(nobs,sd=sd(galton$parent),mean=mean(galton$parent))
child0 <- lm1$coeff[1] + lm1$coeff[2]*parent0 + rnorm(nobs,sd=summary(lm1)$sigma)
par(mfrow=c(1,2))
plot(galton$parent,galton$child,pch=19)
plot(parent0,child0,pch=19,col="blue")
```

### Simulating more complicated scenarios
//
library(bootstrap)
data(stamp)   //about thickness of stamps over the years
nobs <- dim(stamp)[1]

hist(stamp$Thickness,col="grey",breaks=100,freq=F)
dens <- density(stamp$Thickness)
lines(dens,col="blue",lwd=3)
```

### A simulation that is too simple
//plot density of original dataset. And generate new thickness values. Simulations of real data. but not very accurate
plot(density(stamp$Thickness),col="black",lwd=3)
for (i in 1:10) {
newThick <- rnorm(nobs,mean=mean(stamp$Thickness),sd=sd(stamp$Thickness))
lines(density(newThick),col="grey",lwd=3)
}


### How density estimation works
![How density estimation works](https://dl.dropbox.com/u/7710864/courseraPublic/week8/002simulationForModelChecking/assets/img/kde.png)

<http://en.wikipedia.org/wiki/File:Comparison_of_1D_histogram_and_KDE.png>

### Simulating from the density estimate
```{r, dependson="stamps",fig.height=4,fig.width=4}
plot(density(stamp$Thickness),col="black",lwd=3)
for (i in 1:10) {
newThick <- rnorm(nobs,mean=stamp$Thickness,sd=dens$bw)       //Here, I get more accurate simulations. It mimics origianl data much better
lines(density(newThick),col="grey",lwd=3)
}
```

### Increasing variability
//I add variability if I think simulations are too close to actual data. inc. standard deviation
plot(density(stamp$Thickness),col="black",lwd=3)
for (i in 1:10) {
newThick <- rnorm(nobs,mean=stamp$Thickness,sd=dens$bw*1.5)     
lines(density(newThick,bw=dens$bw),col="grey",lwd=3)
}

### Notes and further resources

#### Notes:
- Simulation can be applied to missing data problems - simulate what missing
data might be
- Simulation values are often drawn from standard distributions, but this may
not be appropriate
- Sensitivity analysis means trying different simulations with different
assumptions and seeing how estimates change

#### Further resources:

- [Advanced Data Analysis From An Elementary Point of View](http://www.stat.cmu.edu/~cshalizi/ADAfaEPoV/ADAfaEPoV.pdf)
- [The design of simulation studies in medical statistics](http://www.soph.uab.edu/ssg/files/Club_ssg/MPadilla_07.pdf)
- [Simulation studies in statistics](http://www4.stat.ncsu.edu/~davidian/st810a/simulation_handout.pdf)

How do we make better beer?
Data: Measures of beer quality
Statistics: the t-test
                                                                                                  
ANOVA made to answer field characthersitics, crop yields
                                                                                                  
Kapland-Meier Estimator - for survivla times of people
                                                                                                  
Recommender Systems for knowing what people like based on movie ratings
                                                                                                  
Great Scientistics
Daryle Morey
Hilary Mason
Daphne Koller
Nate Silver

Cool Resources
1. OpenIntro - basic grounding in stats
2. Elements of Statisticsal Learning - good for machine learning, more 
3. Advanced Data Analysis from an elementary point of view = dense math butgood and free

Andrew Gelman's Blog'
Larry Wasserman's Blog'
Statsblog
Flowing Data
junkcharts    //good data visualization
Hilary Mason's Blog'
Cosma Shalizi's Blog'