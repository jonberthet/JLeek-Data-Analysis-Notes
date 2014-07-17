W1 - ANOVA 
multiple factors and variables @ same time
//KEY IDEAS
1. Outcome is quantitative
2. Multiple explanatory variables
3. Goal = ID contributions of different variables


//Anova - aov()
//Left side of tilda is result/outcome, right side of tilda is the covariate variable (factor variable of left side variable)

//Show Analysis of Variance
aovObject <- aov(movies$score ~ movies$rating)
aovObject

//Get coefficients
//Intercept is the average of b0 (G rating movie)
//next variable - Intercept = avg. rating for PG movies
//3rd variable is - Intercept = avg. rating for PG-13 movies, etc...
aovObject$coeff

//Add a second factor to model and see what relative contribution of 
  2 factors is to the score.
//Here, I just add an extra set of variables (genre) to the first set (rating).
  It's really 2 variables with multiple levels.'

//Add a 2nd variable easily
aovObject2 <- aov(movies$score ~ movies$rating + movies$genre)
aovObject2

//Conclusion I reach is DFs for 2nd variable and residuals

//Look at summary 
summary(aovObject2)
//Conclusion: tells me DF, Sum of Sq, Mean Sq, F-value
F-value
  -1st F-value measures amount of variation explained by rating in the score varaible
  -If P-values < 0.05 generally considered significant
  -under Genre value, since it's the second' variable in the data set, it is the additional variation that rating did NOT explain already
//So, I should flip the right side tilda, switch the variables
aovObject 3 <- aov(movies$score ~ movies$genre + movies$rating)

//CONCLUSION: results are different. So ORDER MATTERS, especially if I have an unbalanced design
  (ie - genre variable is not distributed evenly b/w levels in the movie rating variable) - be careful.

//Adding quantitative variable
  - only include as 1 term
aovObject4 <-aov(movies$score ~ movies$genre + movies$rating + movies$box.office)
summary(aovObject4)
//NOTE: only 1 df for box office cuz everything is the same w/ 1 quant variable

//ANOVA language
Unit - 1 observation
Treatments - diff. conditions applied to diff. units in study
Factors - controlled units
Replicates - independent units w/ same factors and treatments

//Great references to ANOVA & A/B Testing

Video 2 - Binary Outcomes
//Linear Regression might not be best option

//When I put summary(lmRavens), 'Estimate' tells me % inc w/ 1 new point
//1 problem: have a probability more than 1 when using linear regression on binary outcomes

//Advantage of Log odds - can calculate -∞ to +∞ = more stable results, but not very useful for applied research
//Odds - calculate 0 to ∞ = prob / (1 - prob)
//probability - calculate (0 to 1)

Regression: Linear Vs Logistic
//Linear = use expected value, or Ravens win is equal to specific value, given by model provided
//Logistic = model probability of Ravens win itself. Instead of looking at Ravens wins itself, look at probability of Ravens wins
            -has advantages cuz limits from -∞ to +∞ , not limits of 0 to 1
//interpret Logistic Regression:
  log( Pr(RWi|RSi,b0,b1) / (1 - Pr(RWi|RSi,b0,b1) ) = b0 +b1RSi
b0 - Log odds of a Ravens win if they score zero points
b1 - Log odds ratio of win probability for each point scored (compared to zero points) 
exp(b1) - Odds ratio of win probability for each point scored (compared to zero points)
       
//Logistics Regression Line - glm()
//Always say family - "binomial" for logistic regressions
logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore, family = "binomial")
summary(logRegRavens)
//Interpreting results:
intercept term = log odds of Ravens win when they score 0. If # is negative, they basically 0 chance of winning
Estimate for Ravens score = log odds ratio for a 1 unit increase in Ravens score. So every time Ravens score 1 point, their log odds ratio increases by 0.1066. So, since this is a positive #, the more points they score, the more likely to win
//A sensible graph that shows prob of Ravens win vs their scorez-values and p-values analyzed similarly to a linear model

//show logistic regression
plot(ravensData$ravenScore,logRegRavens$fitted,pch=19,col="blue",xlab="Score",ylab="Prob Ravens Win"

//logs odds ratios of <0 means it's more likely for Ravens to lose. vice versa'
exp(logRegRavens$coeff)
     
//Get confidence intervals
exp(confint(logRegRavens))
     
//Apply Anova 
anova(logRegRavens, test="Chisq")
NOTE: deviance residuals is about log liklihood of the term - can calculate p-values
     
//Simpson's Paradox'
When doing regression, consider all important terms
     
//Interpreting Odds Ratios
     -not probabilities
     -Odds ratio of 1 = no diff. in odds = no effect
     -Log odds ratio of 0 = no difference in odds
     -Odds ratio < 0.5 or >2 commonly a 'moderate effect' - vague rule of thumb, but consider real issue
     -Relative risk = ratio of probabilities
     -Odds Ratio is NOT Relative Risk
     

     
V3 - COUNT OUTCOMES
//Many data take form of counts, like:
       -Calls to a call center
       -# of flu cases in an area
       -# of cars that cross a bridge
//Data may also be in form of rates
     -% of children passing tests
     -% of hits to a website from a country
//Linear regression w/ transformation is an option
     
Poisson Distribution
lambda parameter = rate (so, if lambda = 100, then avg. # of calls coming into call center in 1 hour is 100')
distributions (variance) are smaller when lambda is smaller (most times)
                         
Linear Regression for # of website views
download.file("https://dl.dropbox.com/u/7710864/data/gaData.rda",destfile="./data/gaData.rda",metho load("./data/gaData.rda")
gaData$julian <- julian(gaData$date)
head(gaData)
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
lm1 <- lm(gaData$visits ~ gaData$julian)
abline(lm1,col="red",lwd=3)
              
//Poisson/log-linear
//If I take the log of a Poisson distribution, I make a linear function
//I get Multiplicative differences 
//If JD increases by 1 day(unit), then take previous day expected value and multiply it by e(b1)
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits") 
lm1 <- lm(gaData$visits ~ gaData$julian)
              
//Mean-Variance Relationship
plot(glm1$fitted,glm1$residuals,pch=19,col="grey",ylab="Residuals",xlab="Date")
abline(lm1,col="red",lwd=3)
              
//REFERENCE LECTURE for more details: Poisson great for comparing relationships or dependencies between variables.               


              
V4 - MODEL CHECKING AND MODEL SELECTION
Sometimes model checking/selection not allowed
Often problems
-overfitting (model works for sample set but not for other samples)
-overtesting  (Confidence intervals don't' have the significance claimed)
-Biased inference

Linear Regression - basic assumptions
-variance constant
-summarizing linear trend
-all terms are right and enough
-no big outlier

//MODEL CHECKING - CONSTANT VARIANCE
//With dataset, see if another variable explains increased variance
//Use vcovHC{sandwich} variance estimators (if n is big) - changing variance situation
//Generated data where x-values are between 0 and 3, create linear model, and plot vcovHC. The result: variance-covariance matrix (variances on the upper left and lower right) + (covariance on lower left and upper right)
library(sandwich)
set.seed(3433); par(mfrow=c(1,2)); data <- rnorm(100,mean=seq(0,3,length=100),sd=seq(0.1,3,length=1)
lm1 <- lm(data ~ seq(0,3,length=100))
vcovHC(lm1)

//Then, just to see, get a linear model 
summary(lm1)$cov.unscaled
                                                 
//RESULT between linear and vcovHC is that I have more confidence than I should have with 
linear model relationship than otherwise. I underestimate variability in linear model. 
Also, there may not be a linear trend either
                                                 
//MODEL CHECKING: If I have a curvalinear trend, I can:
1. use Poisson regression
2. Use data transformation (take the log)
3. Smooth the data/fit a nonlinear trend
4. Use linear regression anyway (interpret line or use vcovHC{sandwich} variance estimators (if n is big))

                                                 
//MODEL CHECKING: Look for missing covariate (find difference b/w residuals of different variables)
set.seed(3433); par(mfrow=c(1,3)); z <- rep(c(-0.5,0.5),50)
data <- rnorm(100,mean=(seq(0,3,length=100) + z),sd=seq(0.1,3,length=100))
lm1 <- lm(data ~ seq(0,3,length=100))

//color the residuals to see which ones stand above and below the regression line                                                  
plot(seq(0,3,length=100),data,pch=19,col=((z>0)+3)); abline(lm1,col="red",lwd=3)
plot(seq(0,3,length=100),lm1$residuals,pch=19,col=((z>0)+3)); abline(c(0,0),col="red",lwd=3)

//Boxplot of the residuals and I notice 1 residuals is lower than the other
boxplot(lm1$residuals ~ z,col = ((z>0)+3) )
                                                 
//So, What to do?:
1. Use explanatory analysis to ID other variables to include
2. use vcovHC{sandwhich} variance estimators (if n is big)
3. Report unexplained patterns in the data - may be violations of assumptions in model
                                                 
//MODEL CHECKING - outliers
1. If outliers are experimental mistakes - remove & document
2. If they're real', consider reporting how sensitive estimate is to outlier
3. Consider using a robust linear model fit like rlm{MASS} - MASS package  = good if lots of outliers
                                                 
//MODEL CHECKING - default plots
                                                 
//These give me 3 sets of charts. - 
set.seed(343); par(mfrow = c(1,2))
                                                 
//MODEL CHECKING - deviance
commonly reported for GLM's'
usually compares the model where every point gets its own parameter to the model I'm using'
deviance doesn't tell me how' my model is wrong, just tells me it's' wrong

//MODEL SELECTION
-choose right variables by:
1. domain-specific knowledge
2. exploratory analysis (plots, color residuals to see variances b/w variables)
x <- seq(0,3, length = 100); y <- rnorm(100); lm1 <- lm(y ~ x)
plot(lm1)                                              
3. Statstical Selection (may bias selection)
-step-wise
-AIC
-BIC
-Modern approaches: Lass, Ridge-Regression

Error measures - R squared will always get bigger with more variables
-Adjusted R-squared is a lil better cuz takes into account # of estimated parameters
-AIC penalizes models w/ more parameters = measures quality of model
-BIC does the same with models w/ lots of parameters, but w/ bigger penalty
                                                 
//AWESOME MODEL SELECTION - STEP
  -I want to model scores but I don't know' what variables to include in the model
  -Solve this with step function in R
movies <- movies[,1]
//do linear analysis on movie data. consider all the terms in the data set
lm1 <- lm(score ~ .,data=movies)
                                                 
//aicFormula() = consider deleting one of the variables, then once it does, 
it will recompute the AIC score and see if it got better or worse. If it got better,
it will try again. It will try forward, backward, or both tests and give me back a 
result when the AIC isn't changing anymore'

//Give me best AIC result
aicFormula <- step(lm1)
                                                 
//Give me coefficients (estimates), may be biased
aicFormula
                                                 
MODEL SELECTION - regsubsets() = which looks at all possible subsets of variable and calculate the BIC score - goal is to minimize BIC
library(leaps);
regSub <- regsubsets(score ~ .,data = movies)
plot(regSub)

or use:

//Does model averaging - calculates posterior probabilities that each term be included in the model. If posterior rating is 0, then don't include'
variables with score closer or exactly 100 should be included
library(BMA)
bicglm1 <- bic.glm(score ~.,data=movies,glm.family="gaussian") 
                                                 
NOTE:
exploratory/visual analysis is key
automatic selection produces answers, but may bias inference
separate sample into 2 groups (use 1 group for the selection step and 1 group for inference)
not for causal relationships
print(bicglm1)