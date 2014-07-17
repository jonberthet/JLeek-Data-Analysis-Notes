//Video 1 - explains example assignment
//Video 2 - Graphs

BOXPLOTS
//compare quants on common scale. Include distribution of age, broken down by difficulty dressing variable (DDRS)
boxplot(pData$AGEP ~ as.factor(pData$DDRS), col = "blue")

//encode how many people have are in each sample. use varwideth=TRUE
boxplot(pData$AGEP ~ as.factor(pData$DDRS), col = c("blue","orange"), names = c("yes", "no"), varwideth = TRUE
        
BARPLOTS
//good for qualitative variables and to show amount
barplot(table(pData$CIT), col = "blue")

HISTOGRAM
//more detailed than barplots. gives me more of a shape of distribution or frequency of values than a barplot
hist(pData$AGEP)
        
//can set # of breaks
hist(pData$AGEP, breaks = 100, main = "AGE")
        
DENSITY PLOT
//A Historgram that's been smoothed out'. 'lwd' sets line thickness. The boundaries have a little error and extend out a little. beware, it's not super accurate'
dens <- desnity(pData$AGEP)
        
//Multiple distributions in same graph
dens <- desnity(pData$AGEP)
densMales <- density(pData$AGEP[which(pData#SEX == 1)])
plot(dens, lwd = 3)
lines(densMales, lwd = 3)       //adds the other line here
                         

                                      
                    VIDEO 3
plot(dens,lwd=3)

SCATTERPLOTS
//?par tells me all the parameters I can have on scatterplots
//pch = types of data point (circle, square, triangle...), cex = size of data points
plot(variable1, variable2, pch = 20, col = "blue", cex = 0.5)
                                      
                                      
//Using Color to distinguish variables
plot(pData$variable1, pData$variable2, pch=19,col=pData$Sex, cex=0.5)
                                      
//Use size to distinguish variables
percentMaxAge <- pData$AGEP/max(pData$AGEP)
plot(pData$variable1, pData$variable2, pch=19, col=pData$Sex, cex=percentMaxAge*0.5)
                                      
                                      
//overlaying lines/points
plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=0.5) 
lines(rep(100,dim(pData)[1]),pData$WAGP,col="grey",lwd=5) 
points(seq(0,200,length=100),seq(0,20e5,length=100),col="red",pch=19)
                                      
//Numeric Variables as factors - can break up a variable into multiple factors and assign those to chart
library(Hmisc)
ageGroups <- cut2(pData$AGEP,g=5)
plot(pData$JWMNP,pData$WAGP,pch=19,col=ageGroups,cex=0.5)
                                      
//WHEN LOTS OF POINTS HERE, you can take a sample of much smaller size of the the info, so it's not so crazy looking'
sampledValues <- sample(1:1e5, size = 1000, replace = FALSE)
plot(x[sampledValues],y[sampledValues],pch=19)
                                      
//Make lots of points look cool. Creates a smooth density plot 
smoothScatter(x,y)
                                      
//Another way to do a smooth Scatter - {hexbin}
library(hexbin)
x <- rnorm(1e5)
y <- rnorm(1e5)
hbo <- hexbin(x,y)
plot(hbo)

//QQ-plots - plots quantiles of X to quantiles of Y
x <- rnorm(20); y <-rnorm(20)
qqlot(x,y)
abline(c(0,1))
                                      
//plot obvservations over time or longitudinally - Matplot and spaghetti
//generate random matrix with 20 rows and 5 columns. just created 100 normal random variables
X <- matrix(rnorm(20*5),nrow =20)
                                      
//matplot (spaghetti plot cuz of how it looks) to plot diff. columns of that matrix. It takes 
//each column and plots it as one specific line. Common for longitudinally time series analysis
X <- matrix(rnorm(20*5), nrow = 20)
matplot(X, type = "b")
                                      
Heatmaps
//2d histogram
image(1:10, 161:236, as.matrix(pData[1:10,161:236]))     
//but this messes up the columns (161:236) and rows(1:10), 
//so to match columns and rows, do below:

newMatrix <- as.matrix(pData[1:10,161:236])
newMatrix <- t(newMatrx)[,nrow(newMatrix):1]
image(161:236, 1:10, newMatrix)
                                      
//MAPS!!!!
library(maps)
map("world")      //there's many maps of many states and countries in the maps package'
lat <- runif(40,-180,180); long <- runif(40,-90,90)
points(lat, long, col="blue", pch = 19)
                                      
//Missing Values and Plots
plot(x,y, pch=19, xlim=c(0,11),ylim=c(0,11))       //Find where there's gaps in the line'
or boxplot(x ~ is.na(y))        //compare values where y is na or not na. Then it shoes that when y = na, then y values are small. when y does not equal na, then y values are big

//VIDEO 4 - EXPOSITORY GRAPHS
graphs I'll show to people or used in final presentation'
Characteristics:
1. use position, common scales, info density is good - don't just report 1 point. Use graphs that are too tough to explain in words
2. should have ' large axises, title, etc

Never have an R output as the label of the axes. Label the axes with actual words, and always put in units.

//Change size of axes and labels
xlab = "Travel time (min)", ylab = "Last 12 month wages (dollars)", cex.lab =2, cex.axis =1.5)    // cex determines units of axes and fiddle with it to get the right sizes...

//Legends
//using same plot as before, use:
//legend uses x and y values for where it's located' for first input (100,2000..)
legend(100,200000), legend = "All surveyed", col = "blue", pch = 19, cex = 0.5)


//If using 2 variables in legend
legend(100,200000), legend = c("men", "women"), col=c("black", "red"), pch = c(19,19), cex = c(0.5,0.5))

//Title
plot(...., main = "Title Goes Here")

//Multiple Panels
//Show more than one type of info in graph in 1 story
//par command allows to set a lot of graphical parameters
//mfrow command tells me how I can orrient a set of graphs on the same figure
par(mfrow = c(1,2))     //create graphical device with 1 row and 2 columns. Fill in plots as I go into each slot

//gives me 2 graphs. Suggestion: don't do more than 2x2 panel plots'
hist(....)
plot(....)

//Adding text to plots or margins
mtext puts txt in the margins of figures. put new mtext() after each code per graph
mtext(text = "(a)", side = 3, line=1)   //3 corresponds to top of the plot. line tells me how far away from the top I put the title

//Figure Captions - explains what entire plots are about  *KEY*
example: Figure A shows that Males take less time to dress than females, etc...

www.vischeck.com      //shows how graph would look to some1 with colorblindness

//SAVING THE FILE
//Create PDF
pdf(file = "twoPanel.pdf", height = 4, width = 8)   //4 and 8 in inches
Then insert code to make graph
//Finish with this:
  dev.off()

//Or 
dev.copy2pdf(file = "title.pdf")

//Create png
png(file = "Title.png"), height = 480, width  = (2*480)    //png in pixels
Code, Code, Code
dev.off()

Video 5_Week 3_Hierarchal Graphs
Clustering - organizing things that are close into groups

Hierarchial clustering is agglomerative because it's bottom up analysis'
  -find closest two things, put them together, find next closest
  Requires:
    a defined distance
    a merging approach
  Produces:
    tree showing how close things are to each other

//FIRST, how do we define close:
  MOST IMPORTANT STEP
Distance or similarity
  Continuous - euclidean distance - like pythagorean theorom - can do this for continous variables in clustering and do this to multiple variables at once: Sq.Rt((A1 - A2)^2 + (B1 - B2)^2 + ... +(Z1 - Z2)^2)
  Continuous - correlation similarity
  Binary - manhattan distance - often used for binary variables = |A1 - A2| +|B1 - B2|+...+|Z1 - Z2| 
Pick a distance/similarity that makes sense for your problem

//Example Problem for Hierarchal Clustering:

//Set seed so always get same variable out
set.seed(1234); par(mar=c(0,0,0,0))

//Set 3 clusters, each with 4 points. Do this for x and y variables, then plot:
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd = 0.2)
plot(x,y, pch=19, cex=2)

//Use text formula to label points
text(x+0.5,y+0.05, labels = as.character(1:12))

//With this data set, hierarchical clustering works:
  //1st - calculates distance between all different points. - dist(dataFrame)
  //dist formula - method is automatically euclidian distance and tells of distance between 2 points
dist(dataFrame)    

//Hierarchical Cluster #1
First, find the 2 closest points out of all the points and puts them together

Hierarchical Cluster #2
Second, gets the 2 closest points' ' x values and y values, and creates a new point to get the merge value between closest values

Hierarchical Cluster #3
//Get a Dendogram, or a tree of closest points to eachother
//example using 'hclust' formula
dataFrame <- data.frame(x=x,y=y)
distxy <- dist(dataFrame)         
hClustering <- hclust(distxy)     //plot the distances between variables in the distxy dataframe in a dendogram
plot(hClustering)

//COLOR CODE DENDOGRAM NUMBERS:
myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,..
## modifiction of plclust for plotting hclust objects *in colour*!
## Copyright Eva KF Chan 2009
## Arguments:
## hclust:      hclust object
## lab:         a character vector of labels of the leaves of the tree
## lab.col:     colour for the labels; NA=default device foreground colour
## hang:        as in hclust & plclust
## Side effect: A display of hierarchical cluster with coloured leaf labels.       
y <- rep(hclust$height,2); x <- as.numeric(hclust$merge)
y <- y[which(x<0)]; x <- x[which(x<0)]; x <- abs(x)
y <- y[order(x)]; x <- x[order(x)]
plot( hclust, labels=FALSE, hang=hang, ... )
text( x=x, y=y[hclust$order]-(max(hclust$height)*hang),
      labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )
}

//EVEN COOLER DENDOGRAMS! (gallery.r-enthusiasts.com/RGraphGallery.php?graph=79)

//Merging Points or merging clusters
One way is 'complete linkage' or 'average linkage' by comparing points of the two farthest points between the 2 clusters

//Another merge type is by using a 'heatmap()'

dataFrame <- data.frame (x=x,y=y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)

//WEEK 3_Video 6_K-means clustering

//K-means clustering is a partitioning approach, not an agglomerative (hierarchal) approach
//Partitioning:
  //STEPS: First determines Fixed number of clusters, gets a 'centroid' of each cluster, assigns things to the closest centroid, then recalculated the centroid
  //Requires: A defined metric, number of clusters, and initial guess as to cluster centroid
  //Produces: final estimate of cluster centroids and an assignment of each point to clusters
  
kmeans()
//specify data I'm' using (NOTE: x and y variables taken from previous example)
dataFrame <- data.frame(x,y)

//Performs kmeans here. I tell it to create 3 centers. I tell it the maximum number of iterations to perform with the iter.max, which is default is set at 10. nstarts tells me the number of starts, which re-starts the calculations a number of times then gets the averages of these centers
kmeansObj <- kmeans(dataFrame, centers=3)

//Tells me the names of the variables for me to cluster
names(kmeansobj)

//tells me which points were attached to which cluster. So if answer is "3 3 3 3 1 1 1 2 2 2", then first data point is assigned to 3rd cluster..., 4th data point is assigned to cluster 1...
kmeansObj$cluster

//Make a plot of the values I made:
par(mar=rep(0.2,4))
plot(x,y, col=kmeansObj$cluster, pch = 19, cex =2)
points(kmeansObj$centers, col= 1:3, pch = 3, lwd = 3)

//HEAT MAP EXAMPLE GIVEN FOR KMEANS

//NOTES:
  I can choose clusters by eye/intuition, or, pick by cross validation/information theory, Deviance Information Criteron (DIC), Akaike Information Criterion (AIC), Deviance Information Criterion (DIC)
  K-means finds clusters in a Gaussian mixture model
  K-means is not deterministic (diff # of clusters & iterations)
  Good Youtube Video: http://www.youtube.com/watch?v=wQhVWUcXM0A
                                
                                
//Video 7 - Dimension Reduction - Principal components analysis and singular value decomposition
                                
Related Problems:
1. I have new set of variables (columns) with many values (rows) that are uncorrelated and explain as much variance as possible
2. If I put all the variables together in 1 matrix, find the best matrix created with fewer variables (lower rank) that explains the general data
                                
//First Goal is 'Statistical', Second Goal is 'data compression'
                                
1st approach is a 'Singular Value Decomposition'  (SVD) by taking data and creating a matrix decomposition. Decomposition has 3 parts:
1.  X = UDV^T
U = left singular vectors (orthogonal)
D = singular values (diagonal matrix)
V = right singular vectors (orthogonal)
                                
Principal Components (PCA) of a matrix are equal to V if you scale (subtract the mean, divide by the stand. deviation) the variables by the appropriate quantities before perfroming the SVD. 
                                
*** SVD and PCA give the same solutions, just different approach.
                                
//Calculate singular value decomposition - svd()
//plot SVD to scaled matrix using image command
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))

//Then, plot first singular vector, which is first column of the U component of the list that results when I apply SVD to a matrix in R.
plot(svd1$u[,1]40:1,,xlab = "Row", ylab = "First left singular vector", pch = 19)

//Then, plot the first column of the V matrix or the red singular matrix, which also gets created like U.
plot(svd1$v[,1],xlab = "Column", ylab = "First right singular vector", pch = 19)   
                                

//Now, look at the D component in the SVD
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,2))

                                
//Can do a SVD when # of rows is much smaller than # of columns, or # of columns is much smaller than # of rows - fast.svd fn
//Create 10k rows and 40 columns
bigMatrix <- Matrix(rnorm(1e4*40), nrow = 1e4)

//Perform SVD on bigMatrix
system.time(svd(scale(bigMatrix)))
                                
//Perform SVD faster, with tolerance set to 0, only calculates values above the tolerance. If set at 0, then will calculate all the singular values and vectors
system.time(fast.svd(scale(bigMatrix), tol=0))
                                
//SVD cannot take place with datasets with missing values.
//As a result, I can impute missing values

//plot D, and see that decreasing set of values, which start with the first right singular vector or left singular vector corresponds to this first d value. The 2nd right singular vector or left singular vector that correwsponds to this value and so forth.
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
                                
Big, cuz explains how many patterns actually appear to explain variations in the data set


//% variance explained by a particular column or row of the singular vectors
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Percent of variance explained", pch = 19)
                                
//OTHER WORD DATA ANALYSIS
Latent Semantic Analysis - uses SVD -  analyzes relationships b/w set of documents and terms they contain by producing a set of concepts related to the documents and terms,

