#Test1:
#Test whether K-means++ can create a good partition for 2-d spherical data

#create a data set that contains 3 spherical 2-d clusters
#cluster 1 
cluster.1.column.1 <- runif(100, 20, 30)
cluster.1.column.2 <- runif(100, 20, 30)
#cluster 2 
cluster.2.column.1 <- runif(100, 0, 10)
cluster.2.column.2 <- runif(100, 20, 30)
#cluster 3 
cluster.3.column.1 <- runif(100, 0, 10)
cluster.3.column.2 <- runif(100, 0, 10)

#plot data points to visualize the clusters
plot(cluster.1.column.1,cluster.1.column.2, col = "blue", xlim=c(0,30), 
     ylim=c(0,30))
points(cluster.2.column.1,cluster.2.column.2, col = "red")
points(cluster.3.column.1,cluster.3.column.2, col = "darkseagreen4")

#Combine clusters into one dataframe, randomize
column.1 <- c(cluster.1.column.1, cluster.2.column.1, cluster.3.column.1)
column.2 <- c(cluster.1.column.2, cluster.2.column.2, cluster.3.column.2)
test.df.1 <- cbind(column.1,column.2)
test.df.1.random <- test.df.1[sample(nrow(test.df.1)),] 

#run k-means function and display results
#centroids should be approximately (5,5), (5,25), and (25,25)
#because centroids are initialized randomly, results may not be as expected
test.1.results <- k.means.plus.plus(test.df.1.random, 3, .01)
test.1.results

#Test2:
#Test whether K-means can create a good partition for 5-d spherical data

#create a data set that contains 4 spherical 6-d clusters
#cluster 1 
cluster.1.column.1 <- runif(100, 20, 30)
cluster.1.column.2 <- runif(100, 20, 30)
cluster.1.column.3 <- runif(100, 20, 30)
cluster.1.column.4 <- runif(100, 20, 30)
cluster.1.column.5 <- runif(100, 20, 30)
cluster.1.column.6 <- runif(100, 20, 30)
#cluster 2 
cluster.2.column.1 <- runif(100, 0, 10)
cluster.2.column.2 <- runif(100, 0, 10)
cluster.2.column.3 <- runif(100, 0, 10)
cluster.2.column.4 <- runif(100, 20, 30)
cluster.2.column.5 <- runif(100, 20, 30)
cluster.2.column.6 <- runif(100, 20, 30)
#cluster 3 
cluster.3.column.1 <- runif(100, 0, 10)
cluster.3.column.2 <- runif(100, 0, 10)
cluster.3.column.3 <- runif(100, 0, 10)
cluster.3.column.4 <- runif(100, 0, 10)
cluster.3.column.5 <- runif(100, 0, 10)
cluster.3.column.6 <- runif(100, 0, 10)
#cluster 4 
cluster.4.column.1 <- runif(100, 20, 30)
cluster.4.column.2 <- runif(100, 0, 10)
cluster.4.column.3 <- runif(100, 20, 30)
cluster.4.column.4 <- runif(100, 0, 10)
cluster.4.column.5 <- runif(100, 20, 30)
cluster.4.column.6 <- runif(100, 0, 10)

#Combine clusters into one dataframe, randomize
column.1 <- c(cluster.1.column.1, cluster.2.column.1, cluster.3.column.1,
              cluster.4.column.1)
column.2 <- c(cluster.1.column.2, cluster.2.column.2, cluster.3.column.2,
              cluster.4.column.2)
column.3 <- c(cluster.1.column.3, cluster.2.column.3, cluster.3.column.3,
              cluster.4.column.3)
column.4 <- c(cluster.1.column.4, cluster.2.column.4, cluster.3.column.4,
              cluster.4.column.4)
column.5 <- c(cluster.1.column.5, cluster.2.column.5, cluster.3.column.5,
              cluster.4.column.5)
column.6 <- c(cluster.1.column.6, cluster.2.column.6, cluster.3.column.6,
              cluster.4.column.6)
test.df.2 <- cbind(column.1, column.2, column.3, column.4, column.5, column.6)
test.df.2.random <- test.df.2[sample(nrow(test.df.2)),] 

#run k-means function and display results
#centroids should be approximately (25,25,25,25,25,25), (5,5,5,25,25,25)
#(5,5,5,5,5,5) and (25,5,25,5,25,5)
test.2.results <- k.means.plus.plus(test.df.2.random, 4, .01)
test.2.results
