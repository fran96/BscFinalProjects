
# Packages being used: ----------------------------------------------------
pkgs <- c("cluster",  "NbClust")
install.packages(pkgs)
install.packages('factoextra')
remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
install.packages('ClusterR') 
install.packages("C50")
install.packages("clue")
install.packages("caret")
install.packages("gmodels")
install.packages("gridExtra")

# Libraries being used: ---------------------------------------------------
library(gmodels)
library(caret) #used for confusion matrix and accuracy
library(rgl)
library("C50")
library("clue")
library(ClusterR)
library(ggplot2)
library(factoextra)
library(cluster)
library(NbClust)
library(gridExtra)
library(flexclust)


# Task 3 ------------------------------------------------------------------

#Data cleaning:
#actual dataset being tampered with
ecoli_kmeans <- read.csv('C:/Users/Fran/Documents/Bsc/3BSC/3bsc_pendrive/FirstSem/StatisticsForCS/MonteCarlo_PCA_Home/data.csv', stringsAsFactors = FALSE)

names(ecoli_kmeans) <- c("SequenceName", "mcg", "gvh", "lip","chg", "aac","alm1", "alm2", "Category")

head(ecoli_kmeans)
str(ecoli_kmeans)

#Visualization
ggplot(ecoli_kmeans, aes(PC$x[,1],PC$x[,2],PC$x[,3], color = Category)) + geom_point()



#Find optimal clusters
#Method 1 (Elbow method)
wss <- (nrow(ecoli_kmeans[c(-1,-9)]))*sum(apply(ecoli_kmeans[c(-1,-9)],2,var))
for (i in 2:8) wss[i] <- sum(kmeans(ecoli_kmeans[c(-1,-9)], 
                                    centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



#Method 2 (NB Clust using Euclidean distance)
res<-NbClust(ecoli_kmeans[c(-1,-9)], diss=NULL, distance = "euclidean", min.nc=2, max.nc=8, 
             method = "kmeans", index = "kl") 
res$All.index
res$Best.nc #3
res$Best.partition



#*


#Applying Kmeans algorithm
help(kmean)
set.seed(235)
ecoliCluster <- kmeans(PC$x[,1:3], 3, nstart = 25)

#view newly created clusters
ecoliCluster
str(ecoliCluster)
table(ecoliCluster$cluster, ecoli_kmeans$Category)

#Creating dataframe without the first and last elements
ecoliMapped.scaled <- ecoli_kmeans[c(-1,-9)]
ecoliMapped.scaled <- data.frame(ecoliMapped.scaled)

#Function to find best mappings for class
getMode <- function(v){
  distinct <- unique(v)
  
  distinct[which.max(tabulate(match(v,distinct)))]
}

#Preparing data for mapping
ecoliC <- as.data.frame(ecoliCluster$cluster) #creating data frame with kmeans clusters
names(ecoliC) <- c("Cluster")
ecoliC$Category <- ecoli_kmeans$Category #adding actual category in dataframe
ecoli_Mapped <- ecoli_kmeans #renaming it to avoid altering original data frame from now on

#Map the categories
ecoli_Mapped$Category[ecoli_Mapped$Category == "cp"] <- getMode(ecoliC$Cluster[ecoliC$Category == "cp"])
ecoli_Mapped$Category[ecoli_Mapped$Category == "im"] <- getMode(ecoliC$Cluster[ecoliC$Category == "im"])
ecoli_Mapped$Category[ecoli_Mapped$Category == "pp"] <- getMode(ecoliC$Cluster[ecoliC$Category == "pp"])

ecoli_Mapped$Category[ecoli_Mapped$Category == "imU"] <- getMode(ecoliC$Cluster[ecoliC$Category == "imU"])
ecoli_Mapped$Category[ecoli_Mapped$Category == "om"] <- getMode(ecoliC$Cluster[ecoliC$Category == "om"])
ecoli_Mapped$Category[ecoli_Mapped$Category == "omL"] <- getMode(ecoliC$Cluster[ecoliC$Category == "omL"])

ecoli_Mapped$Category[ecoli_Mapped$Category == "imL"] <- getMode(ecoliC$Cluster[ecoliC$Category == "imL"])
ecoli_Mapped$Category[ecoli_Mapped$Category == "imS"] <- getMode(ecoliC$Cluster[ecoliC$Category == "imS"])


#*







# Task 4 : Validation of clusters -----------------------------------------


#Validation of clusters (accuaracy)
seq<-factor(c(ecoli_Mapped$Category))
ecoli_Mapped["CategoryId"] <- as.integer(seq)
res = external_validation(ecoli_Mapped$CategoryId, ecoliCluster$cluster, 
                          
                          method = "adjusted_rand_index", summary_stats = T)

#Accuracy of kmeans clusters
#Crosstable
CrossTable(ecoli_Mapped$Category, ecoliC$Cluster,
           prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)


#Confusion matrix
confusionMatrix(as.factor(ecoli_Mapped$Category),as.factor(ecoliCluster$cluster)) #93% accuracy


#Checking if cross matrix result is correct
accuracyOfKmeans_table <- with(ecoliCluster, table(cluster, ecoli_Mapped$Category))
accuracyOfKmeans_table #22 outliers

sum(diag(accuracyOfKmeans_table)) #N cases correctly classified
sum(accuracyOfKmeans_table)-sum(diag(accuracyOfKmeans_table)) #N cases incorrectly classified
(accuracyOfKmeans_table[1,1]+accuracyOfKmeans_table[2,2]+accuracyOfKmeans_table[3,3])/nrow(ecoli_Mapped) #accuracy = 93%
#1-(22/336) therefore 93% accuracy, crossmatrix result is correct.


#*


#Visualisation of clusters 

#ggplot
ggplot(ecoliC, aes(PC$x[,1],PC$x[,2],PC$x[,3], color = Cluster)) + geom_point() #with clusters
ggplot(ecoliC, aes(PC$x[,1],PC$x[,2],PC$x[,3], color = ecoli_Mapped$Category)) + geom_point() #with label

#2D rep of cluster solution
clusplot(ecoliMapped.scaled, ecoliCluster$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)

#3D Plot
plot3d(PC$x[,1:3], col=ecoliCluster$cluster,main="k-means clusters")
plot3d(PC$x[,1:3], col=ecoli_Mapped$Category, main="Actual classes")

# Comparison of different k in 2D -----------------------------------------


ecoliCluster_k4 <- kmeans(ecoli_Mapped[c(-1,-9)], centers = 4, nstart = 25)
ecoliCluster_k6 <- kmeans(ecoli_Mapped[c(-1,-9)], centers = 6, nstart = 25)
ecoliCluster_k8 <- kmeans(ecoli_Mapped[c(-1,-9)], centers = 8, nstart = 25)

# plots to compare
p1 <- fviz_cluster(ecoliCluster, geom = "point", data = ecoli_Mapped[c(-1,-9)]) + ggtitle("k = 3")
p2 <- fviz_cluster(ecoliCluster_k4, geom = "point",  data = ecoli_Mapped[c(-1,-9)]) + ggtitle("k = 4")
p3 <- fviz_cluster(ecoliCluster_k6, geom = "point",  data = ecoli_Mapped[c(-1,-9)]) + ggtitle("k = 6")
p4 <- fviz_cluster(ecoliCluster_k8, geom = "point",  data = ecoli_Mapped[c(-1,-9)]) + ggtitle("k = 8")

grid.arrange(p1, p2, p3, p4, nrow = 2)


# Splitting data into train and test --------------------------------------
#Testing and training
#sampling: train 75, test 25
set.seed(235)
newEcoli <- ecoli_kmeans


newEcoli$Category[newEcoli$Category == "cp"] <- 1
newEcoli$Category[newEcoli$Category == "im"] <- 1
newEcoli$Category[newEcoli$Category == "pp"] <- 1

newEcoli$Category[newEcoli$Category == "imU"] <- 2
newEcoli$Category[newEcoli$Category == "om"] <- 2
newEcoli$Category[newEcoli$Category == "omL"] <- 2

newEcoli$Category[newEcoli$Category == "imL"] <- 3
newEcoli$Category[newEcoli$Category == "imS"] <- 3
newEcoli$Category

newEcoli.rows <- nrow(newEcoli)
newEcoli.sample <- sample(newEcoli.rows, newEcoli.rows*0.75)

newEcoli.train <- newEcoli[newEcoli.sample,]
newEcoli.test <- newEcoli[-newEcoli.sample,]

newEcoli.train <- kcca(newEcoli.train[,2:8], k=3, kccaFamily("kmeans"))
# newEcoli.train <- kcca(PCA$scores, k=3, kccaFamily("kmeans"))
summary(newEcoli.train)


newEcoli.test.pred <- predict(newEcoli.train, newEcoli.test[,2:8], k=3, kccaFamily("kmeans"))
newEcoli.test.pred

CrossTable(newEcoli.test$Category, newEcoli.test.pred,
           prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)

confusionMatrix(as.factor(newEcoli.test$Category), as.factor(newEcoli.test.pred))#52%


#Alternative method of evaluating clusters (silhouette method)
sil <- silhouette(ecoliCluster$cluster, dist(ecoli_kmeans[c(-1,-9)]))
plot(sil, main ="Silhouette plot - K-means")

fviz_nbclust(ecoli_kmeans[c(-1,-9)], kmeans, method = "silhouette")

#*






