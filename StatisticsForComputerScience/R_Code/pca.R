
# Packages being used: ----------------------------------------------------
install.packages("psych")
install.packages(factoextra)
install.packages("stats")


# Libraries being used: ---------------------------------------------------
library(stats)
library("psych")
library(factoextra)


# Section1: Task a ---------------------------------------------------------

#Data cleaning
#loading original data into data frame for reference
originalData <- read.csv('C:/Users/Fran/Documents/Bsc/3BSC/3bsc_pendrive/FirstSem/StatisticsForCS/MonteCarlo_PCA_Home/data.csv')
names(originalData) <- c("SequenceName", "mcg", "gvh", "lip",
                  "chg", "aac",
                  "alm1", "alm2", "Category")

#actual dataset being tampered with
ecoli <- read.csv('C:/Users/Fran/Documents/Bsc/3BSC/3bsc_pendrive/FirstSem/StatisticsForCS/MonteCarlo_PCA_Home/data.csv')
names(ecoli) <- c("SequenceName", "mcg", "gvh", "lip",
                     "chg", "aac",
                     "alm1", "alm2", "Category")
head(ecoli[c(-1,-9)])
seq<-factor(c(ecoli$SequenceName))
ecoli.scaled <- ecoli[c(-1,-9)]
ecoli$SequenceName <- as.integer(seq)


#*


#Preforming PCA
PC <- prcomp(ecoli.scaled, center = TRUE,scale. = TRUE)
PC


# Section1: Task b -------------------------------------------------------

#Extracting eigenvalues from PCA
help(get_eigenvalue)
eig.val <- get_eigenvalue(PC)
eig.val[1:7,]

#Visualize eigenvals
fviz_screeplot(PC, 'eigenvalue', barfill='red', addlabels='true', main="Screen plot of PCA with eigenvalues")


# Section1: Task c --------------------------------------------------------

#Scree plots
fa.parallel(ecoli.scaled, fa="pc", show.legend=FALSE, main="Scree
            plot with parallel analysis")


# Section1: Task d --------------------------------------------------------

#Calculating variance 
biplot(PC)

#compute standard deviation of each principal component
std_dev <- PC$sdev

#compute variance
pr_var <- std_dev^2

#check variance of components
pr_var[1:7]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:7]
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b", main="Cumulative variance plot")

#first 10 rows after dimensionality reduction
ecoli_dr <- ecoli.scaled[c(-4,-5,-6,-7)]
head(ecoli_dr, n=10)
