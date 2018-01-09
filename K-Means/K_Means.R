k.means.implementation <- function(input.data, num.clusters, threshold = 0.1){
  #Implementation of Lloyd's algorithm for k-means
  #
  #Args:
  #  input.data - Data that is to be partitioned with k-means algorithm
  #  num.clusters - Number of clusters(k) for data
  #  threshold - If the change(Euclidean distance) between sum of old centroids
  #    and sum of new centroids is below the threshold, k-means stops iterating.
  #
  #Returns:
  #  centroids - centroid(mean) of each cluster
  #  partition.vector - Each of the data points from input.data is assigned
  #    a label from 1 to k.  The label assigned is the nearest centroid to 
  #    the data point.
  #
  #error handling
  if (threshold < 0){
    print("threshold needs to be positive")
    return()
  }
  if(num.clusters < 1){
    print("numCluster needs to be greater than 1")
    return()
  }
  old.centroids <- centroid.initialization(input.data, num.clusters)
  new.centroids <- matrix(c(0), nrow = num.clusters, ncol = ncol(input.data))
  #For each centroid create a list to hold data points assigned to it
  centroid.list <- rep(list(list()), num.clusters)
  repeat{
    #assign data points to the nearest centroid
    assignment.distance <- rep(.Machine$double.xmax, nrow(input.data))
    assignment <- rep(-1, nrow(input.data))
    for(i in 1:num.clusters){
      #measure Euclidean distance between centroids and data point
      assignment.temp.distance <- euclidean.dist(matrix(rep(old.centroids[i, ],
                                                            nrow(input.data)), ncol = ncol(input.data), byrow=TRUE), input.data)
      assignment.distance <- ifelse(assignment.temp.distance <= assignment.distance, 
                                    assignment.temp.distance, assignment.distance)
      assignment <- ifelse(assignment.temp.distance <= assignment.distance,
                           i, assignment)
    }
    #check if one of the centroids is empty
    for(i in 1:num.clusters){
      if(length(centroid.list[[i]])==0)
      {
        print("Empty Centroid.  Retry or Pick fewer Clusters")
        return()
      }
    }
    #create list of data assigned to centroids
    for(i in 1:num.clusters){
      is.in.assignment <- ifelse(assignment == i, 1, 0)
      centroid.list[[i]] <- which(is.in.assignment == 1)
    }
    #set newCentroids to be the average of assigned centroids
    for(i in 1:num.clusters){
      cluster.size <- length(centroid.list[[i]])
      #cluster.sum holds the total sum of all data points assigned to the centroid
      cluster.sum <- colSums(input.data[centroid.list[[i]],])
      new.centroids[i, ] <- cluster.sum/cluster.size
    }
    #break the repeat loop if change between old.centroid and new.centroid is
      #less than threshold
    centroid.distance <- sum(euclidean.dist(old.centroids, new.centroids))
    if((centroid.distance/num.clusters) < threshold)
    {
      break
    }
    #set new.centroid as old.centroid for next iteration
    old.centroids <- new.centroids
  }
  return.list <-list("centroids" = new.centroids, "partiton.vector" = centroid.list)
  return(return.list)
}

centroid.initialization <- function(input.data, num.clusters){
  #Initializes centroids randomly.  A value is chosen at random between the 
  #minimum and maximum value of the data field.
  #
  #Args:
  #  input.data - Data that is to be partitioned with k-means algorithm
  #  num.clusters - Number of clusters(k) for data
  #
  #Returns:
  #  initial.centroids - starting centroid(mean) for each cluster
  #
  initial.centroids <- matrix(c(0), nrow = num.clusters, ncol = ncol(input.data))
  for(i in 1:ncol(input.data)){
    initial.centroids[, i] <- runif(num.clusters, min(input.data[, i]), 
                                    max(input.data[, i]))
  }
  return(initial.centroids)
}

euclidean.dist <- function(x,y){
  #Euclidean distance between corresponding indices for two vectors.
  #
  #Args:
  #  x - first data entry
  #  y - second data entry
  #
  #Returns:
  #  distance - Euclidean distance between x and y
  #
  distance <- sqrt(rowSums((x-y)^2))
  return(distance)
}
