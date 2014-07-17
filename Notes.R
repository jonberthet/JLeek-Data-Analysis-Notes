					#R INTRO + ELEMENTARY STATS
#Read Excel File. Must be in .csv
Test1<-read.csv("Test1.csv", header=TRUE)

#get frequency of info
school.freq

#cbind changes data from rows to columns. Ex:
cbind(school.freq)
_______
#find the frequency of a variable. Example of composition column in the 'painter' chart.
> library(MASS)							# load the MASS package 
> composition = painters$Composition    # the painter compositions
> composition.freq = table(composition) #apply the table function
> composition.freq
_______
#Relative Frequency Distribution = Frequency / Sample Size

#reduce digits for numbers of dataset school
old = options(digits=1) 
> school.relfreq 

#increase digits
sprintf("%.10f",pbar)   #increases pbar fn to 10 digits

#restore old option
options(old)
_______
#barplot
> barplot(school.freq)
#add colors
> colors = c("red", "yellow", "green", "violet", 
+   "orange", "blue", "pink", "cyan") 
> bar(school.freq,         # apply the barplot function 
+   col=colors)                # set the color palette
_______
#pie chart
pie(school.freq)

_______
#Create logical index vector
> library(MASS)                 # load the MASS package 
> school = painters$School      # the painter schools 
> c_school = school == "C"      # the logical index vector

#ID the child data set
> c_painters = painters[c_school, ]

#Find mean composition score of school C
> mean(c_painters$Composition) 
_______
#Compute mean Composition score of all Schools
> tapply(painters$Composition, painters$School, mean)

_______
#Create half-integer sequences
> breaks = seq(1.5, 5.5, by=0.5)
> breaks

#classify all #s into duration frequencies (shows how many points are between 1.5 and 2, 2 and 2.5...). It's like, 'cut' the 'duration' into 'breaks' listed directly above - and do it with the Left column.
> duration.cut = cut(duration, breaks, right=FALSE)
> duration.freq = table(duration.cut)
cbind(duration.freq)

#add an x-axis title
xlab = "blah blah"
#add y-axis title
ylab = "blah blah"
#add title to chart
 main = "Title goes Here"
 
 #calculate cumulative frequency distribution with cumsum
 > duration.cumfreq = cumsum(duration.freq)
 
 #cumulative frequency graph or ogive
> duration = faithful$eruptions 
> breaks = seq(1.5, 5.5, by=0.5) 
> duration.cut = cut(duration, breaks, right=FALSE) 
> duration.freq = table(duration.cut)

#add a zero starting point
> cumfreq0 = c(0, cumsum(duration.freq)) 
> plot(breaks, cumfreq0,            # plot the data 
+   main="Old Faithful Eruptions",  # main title 
+   xlab="Duration minutes",        # x−axis label 
+   ylab="Cumulative eruptions")   # y−axis label 

#plot a cumulative relative frequency graph
> lines(breaks, cumfreq0)           # join the points

#or

Fn = ecdf(duration)
plot(Fn)
plot(Fn, main="main",xlab = "Duration min", ylab = "Cum eruption proportion")

#stem and leaf plot for Old Faithful's eruption durations (i.e - looks like a stem and its leaves)
> duration = faithful$eruptions 
> stem(duration) 

#scatter plot
duration = faithful$eruptions
waiting = faithful$waiting
head(cbind(duration,waiting))

#then

> duration = faithful$eruptions
> waiting = faithful$waiting
> plot(duration,waiting,xlab="Eruption duration", ylab = "Time waited")

#draw linear regression on scatter plot
> abline(lm(waiting ~ duration))

#Elementary Stats
mean()
median()
quartile()   #tells from 0% to 100% 
quantile(duration,c(.12, .53, .86))  #This is to list the 12%, 53%, and 86% percentiles
 max(faithful$eruptions) - min(faithful$eruptions)   #calc. range

IQR(duration)      #interquartile range = upeper quartile - lower quartile

#Boxplot
> boxplot(duration, horizontal=TRUE)
> boxplot(duration, horizontal=FALSE)

#Variance
var(duration)

#Standard Deviation
sd(duration)

#Covariance
cov(duration, waiting)

#correlation coefficient
P(xy) = Cov(xy)/(st.dev)x (st.dev)y
cor(x,y)

#Skewness Measure
> library(e1071)                    # load e1071 
> duration = faithful$eruptions     # eruption durations 
> skewness(duration)                # apply the skewness function 
[1] -0.41355

#Kurtosis fn
> library(e1071)                    # load e1071 
> duration = faithful$eruptions     # eruption durations 
> kurtosis(duration)                # apply the kurtosis function 

#Binomial Distribution
	#with 12 questions with 5 choices (1 correct answer), probability 		of 4 correct answers by random attempts.
dbinom(4, size=12, prob=0.2)

	#probability of 4 or less correct answers by random attempts
> dbinom(0, size=12, prob=0.2) + 
  dbinom(1, size=12, prob=0.2) + 
  dbinom(2, size=12, prob=0.2) + 
  dbinom(3, size=12, prob=0.2) + 
  dbinom(4, size=12, prob=0.2) 	
Answer = 0.9274

	#cumulative probability function for binomial distribution
	pbinom(4, size=12, prob=0.2)
Answer = 0.9274

#Poisson Distribution - prob of x occurrences in an interval 
				=((lambda^x)(e^-lambda))/x! where x = 0,1,2,3...
	#If 12 cars cross bridge per min on average, the prob. of having 16 or less cars crossing a bridge in 1 minute 
ppois(16, lambda = 12)   #describes lower tail. fyi -  lambda is average,    
Answer = 0.8987
ppois(16, lambda = 12, lower = FALSE)
Answer = 0.1013

#continuous uniform distribution = prob. distribution of random # selection from continuous interval between a and b. ie - area under a curve

#select 10 random numbers between 1 and 3
runif(10, min=1, max=3)

#Exponential Distribution 
	#= describes arrival time of randomly recurring independent event 	sequence. If μ is the mean waiting time for the next event recurrence, its probability density function is: http://www.r-tutor.com/elementary-statistics/probability-distributions/exponential-distribution)
#if mean checkout time of a supermarket is 3 minutes, find probability of a customer checkout being completed by the cashier in less than 2 minutes
pexp(2, rate= 1/3)
Answer: 0.4866 = 48.7% chance of cashier checkout in less than 2 minutes.

#Normal Distribution
#% of students scoring higher than 84, with norm distrib, mean = 72, st. dev = 15.2
pnorm(84, mean = 72, sd = 15.2, lower.tail = FALSE)
Answer: 21.5% of students will score higher than 84

#Chi-Squared Distribution -  test stat for population variance(norm. pop)
qchisq(.95,7) = the 95th percentile of chi-squared distribution with 7 degrees of freedom

#T-Distribution - small sample, as df inc, nears normal distribution
#Find 2.5th and 97.5th percentiles of t-distribution with 5 deg. freedom
qt(c(.25,.975),5,lower.tail = FALSE)

#F-Distribution - test stat for diffs b/w variances of 2 populations
#Find 95th percentile of F-distribution with (5,2) df
qf(.95,5,2)


#######################
#######################

#Point Estimate of Population Mean - the mean of a sample population, for height of 'survey' data

mean(survey.height,na.rm=TRUE)    #use na.rm = TRUE or when dealing with NA or NULL values. Or use height.response = na.omit(survey$Height)

#list population standard deviation as 9.3
sigma = 9.3

#standard error of the mean
sem = sigma/sqrt(n); sem

#margin of error, multiplied by standard error of mean - getting the distance to the right side of the confidence interval (or Zα∕2)
E = qnorm(.975)*sem; E

#adding up with sample mean to find confidence interval
xbar = mean(height)
xbar + c(-E,E)

#Or can download statistical program to get same info:
> library(TeachingDemos)         # load TeachingDemos package 
> z.test(height.response, sd=sigma) 

#######################
#######################

http://www.r-tutor.com/elementary-statistics/interval-estimation/interval-estimate-population-mean-unknown-variance
#Interval Estimate of Population Mean with Unknown Variance (t-test)
t.test(height)
or 
http://www.r-tutor.com/elementary-statistics/interval-estimation/interval-estimate-population-mean-unknown-variance

#######################
#######################

#Determining Sampling Size of Population Mean, after being given confidence level, sigma (ie - the unit being measured that will be used in the margin of error)
n = zstar^

> zstar = qnorm(.975) 
> sigma = 9.48 
> E = 1.2 				#describes margin of error in cm to have a 95% confidence interval
> zstar^2 ∗ sigma^2/ E^2 
Answer: 239.75			#must have sample size of 240 to achieve 1.2 cm 						margin of error at 95% confidence interval

#######################
#######################

#Point Estimate of Population Proportion
> k = sum(gender.response == "Female") 
> pbar = k/n; pbar 
Answer: 0.5    # point estimate of female student proportion in survey is 50%

#######################
#######################

#Interval Estimate of Population Proportion
# type is in standard error equation
prop.test(k,n)

#Sampling Size of Population Proportion