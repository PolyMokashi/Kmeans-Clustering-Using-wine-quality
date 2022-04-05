library(dplyr)          # For data manipulation ,filtering etc
library(magrittr)       # For pipes
library(caret)          # For dummy variables, nearZeroVar(), findCorrleation()
library(ggplot2)        # For plotting
library(ggthemes)       # For a variety of plot-themes
library(gridExtra)      # Arranging ggplots in a grid
library(lattice)        # It is designed with an emphasis on multivariate data, and in particular allows easy conditioning to produce "small multiple" plots.
library(vegan)          #for multivariate analysis of community data.
library(NbClust)        #provides 30 indices for determining the number of clusters
library(cluster)        #objects in the same group (called a cluster) are more similar (in some sense or another) to each other than to those in other groups (clusters).# For silhoutte()
library(factoextra)     # get_clust_tendency() assesses hopkins stat
library(clustertend)    # Another package for hopkins() function
library(data.table)     #fast aggregation of large datasets, low latency add/update/remove of columns, quicker ordered joins, and a fast file reader.
library(GGally)         #extension of ggplot
library(ggcorrplot)     #to visualize easily a correlation matrix using ggplot2
library(mclust)         #clustering, classification, and density estimation based on finite normal mixture modelling.
library(fpc)            #Clustering by merging Gaussian mixture components

#Lets clear any previous stored variables and do garbage collection. This will remove unwanted variables and free up memory.
rm(list=ls());gc()

wineData <- read.csv("C:/Users/prade/OneDrive/Documents/Kmeans clustering and DBscan/winequality1.csv")

options(scipen = 999)   #it is used for decimal numbers
set.seed(321)       #get random samples from 321
#Let us start buy looking at the strucutre of data we are going to use.
str(wineData)

for(i in 1:11){
  print(paste("---Plot for---", colnames(wineData)[i]))
  
  #Overall distribution
  print(ggplot(wineData, aes_string("quality", colnames(wineData)[i]))+
          geom_count(col="tomato3", show.legend=F)+
          theme_solarized(light = TRUE))
  
  #Color wise scatter plot 
  print(ggplot(wineData, aes_string("quality", colnames(wineData)[i]))+
          geom_jitter(aes(col=as.factor(color))))
}

#Making changes for converting data types

#Converting 2 integer columns too numeric
wineData$quality <- as.numeric(wineData$quality)
wineData$good <- as.numeric(wineData$good)

#for ease creating a data set without color column
wineData_no_color <- wineData[1:13]
str(wineData_no_color)

View(wineData)

nzv <- nearZeroVar(wineData)      #removing non-informative points
print(paste("---Column number with----", nzv))
head(wineData)

#By looking at data we can say that Columns 1, 4, 6, 7, 9, 11, 12 can cause issues, we will try to normalize them so that there value are in 0 to 1 range.
print("---Normalizing Data----")

#takes a list, vector, or data frame as an argument and returns a vector or matrix
norm_data <- sapply(wineData[,c(1,4,6,7,9,11,12)], function(x) (x - min(x))/(max(x) - min(x)))
View(norm_data)
class(norm_data)

print("---Type of returned data")
class(norm_data)
str(norm_data)

print("---Converting data from matrix to data.frame---")
norm_data <- data.frame(norm_data)    # norm_data is a 'matrix' converting to data.frame
View(norm_data)
class(norm_data)
str(norm_data)
print("---Normalised data---")
head(norm_data)

#Binding the normalised data with other data
wineData_norm <- cbind(wineData[,c(2,3,5,8,10,13)],norm_data)
head(wineData_norm)
View(wineData_norm)
str(wineData_norm)

#Converting data.frame to matrix or array
wineData_scaled <- scale(wineData_no_color)

head(wineData_scaled)

class(wineData_scaled)

#CHECK whether data.frame
wineData_scaled_df <- as.data.frame(wineData_scaled)
class(wineData_scaled_df)

#Lets find correlation using cor()
corr_norm <- round(cor(wineData_norm),1)
corr_norm
View(corr_norm)
#Let us plot a Correlogram using above returned results.
ggcorrplot(corr_norm, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Wine Data", 
           ggtheme=theme_dark)

#round of values
corr_scaled <- round(cor(wineData_scaled_df),1)

View(corr_scaled)

##Lets plot same for Scaled data
ggcorrplot(corr_scaled, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Wine Data", 
           ggtheme=theme_dark)

#Let us calculate it thought data.
#Again we will make use of Caret Library.
a<-findCorrelation(corr_norm, cutoff = 0.7, verbose = T)
print("--- Columns number---")
a

print("---Column name we want to remove---")
colnames(wineData_norm)[a]

#Let us remove column "Good" from our clustering equation.
#Removing Good column as high quality wine will be good
wineData_scaled_df$good <- NULL
str(wineData_scaled_df)

# Initialize total within sum of squares error: wss
wss <- 0
View(wineData_norm)
# For 1 to 20 cluster centers
for (i in 1:10) {
  km.out <- kmeans(wineData_norm, centers = i)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}
wss

# Plot total within sum of squares vs. number of clusters
plot(1:10, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

#get 60% of data in wine_train_data
wine_train_data <- sample_frac(wineData_norm, 0.65)

View(wine_train_data)
head(wine_train_data)

str(wine_train_data)

nrow(wine_train_data)

#Let us plot Silhouette Plot using NbCluster
fviz_nbclust(wine_train_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

km <- kmeans(wineData_norm, 2, iter.max = 140 , algorithm="Lloyd", nstart=100)
km

#Structure of km
str(km)

# Centroid Plot against 1st 2 discriminant functions
clusplot(wineData, km$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

fviz_cluster(km, data = wineData_norm,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

fviz_cluster(list(data = wineData_norm, cluster = km$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

pam.res <- pam(wineData_norm, 2)
# Visualize
fviz_cluster(pam.res)

# Random data generated from the iris data set
random_df <- apply(wineData_norm, 2, 
                   function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)

head(random_df)

# Compute Hopkins statistic for Wine dataset
res <- get_clust_tendency(head(wine_train_data, 500), n = 499, graph = TRUE)
res$hopkins_stat
plot(res$plot)


# Compute Hopkins statistic for random dataset
res <- get_clust_tendency(head(random_df, 500), n = 499, graph = TRUE)
res$hopkins_stat
plot(res$plot)
