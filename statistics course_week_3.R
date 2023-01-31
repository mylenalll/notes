library(ggplot2)



# ========= K-Means Clustering

# Suppose this is one of the clusters in our data, then what would be the within-cluster sum of squares?
cl <- kmeans(data.frame(x=c(-3,1,2,3,5,6,7), y = c(3,4,6,8,2,11,1)),1)
cl$tot.withinss




# Write a "smart_hclust" function that receives an input dataframe with an arbitrary numeric 
# number of clusters to use and the number of clusters needed to be allocated using hierarchical 
# clustering. The function should include in the original data set a new variable factorial 
# group - the number of the cluster to which the solution of the problem is assigned.
test_data1 <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")

smart_hclust <- function(test_data, cluster_number){
  dist_matrix <- dist(test_data)
  fit <- hclust(dist_matrix)
  test_data$cluster <- as.factor(cutree(fit, cluster_number))
  return(test_data)
}

smart_hclust(test_data, 3)




# If we are interested not only in the very fact that we were able to identify clusters 
# in our data, but we also want to understand how they differ, it is reasonable to compare 
# the clusters with each other according to the available variables. The function should 
# return the names of variables for which significant differences were found between 
# the selected clusters (p < 0.05). In other words, after we have identified a given number 
# of clusters, we add a new grouping variable to the initial data - the cluster number, 
# and compare the resulting groups with each other in terms of quantitative variables using 
# analysis of variance.

test_data1 <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
test_data2 <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")

get_difference <-  function(test_data, n_cluster){
  test_data_wclusters <- smart_hclust(test_data, n_cluster)
  p.results <- sapply(test_data_wclusters[, -ncol(test_data_wclusters)], function(x) summary(aov(x ~ cluster, test_data_wclusters))[[1]]$'Pr(>F)'[1])
  return(names(which(p.results < 0.05)))
}

get_difference(test_data1, 2)
get_difference(test_data2, 2)




# Write a "get_pc" function that receives a dataframe with an arbitrary number of quantitative 
# variables as input. The function should perform principal component analysis and add two new 
# columns with the values of the first and second principal components to the original data. 
# The new variables should be named "PC1" and "PC2".
test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")

get_pc <- function(d){
  return(cbind(d, prcomp(d)$x[, 1:2]))
}

get_pc(test_data)




# Write a get_pca2 function that takes an input on a data frame with an arbitrary 
# numeric number used. The function must calculate what is the minimum value of the main 
# components of the reducing agent that is greater than 90% of the variability in the original 
# data and includes the values of the components in the original dataframe in the new application 
# types.
get_pca2 <- function(data){
  test <- prcomp(data)
  importances <- summary(test)[[6]][3, 1:ncol(summary(test)[[6]])]
  return(cbind(data, test$x[, 1:min(which(importances >= 0.9))]))
}

result  <- get_pca2(swiss)
str(result)




# Write an is_multicol function that takes an arbitrary-sized dataframe with quantitative 
# variables as input. The function should check for the existence of strict multicollinearity,
# namely the presence of a linear combination between the predictors. A linear combination 
# is a situation where one variable can be expressed in terms of another variable using 
# the equation V1 = k*V2 + b. The function returns the names of variables between 
# which there is a linear relationship or the message "There is no collinearity in the data"
test_data1 <- read.csv("https://stepic.org/media/attachments/course/524/Norris_1.csv")
test_data2 <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
test_data3 <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
test_data4 <- as.data.frame(list(V1 = c(0, 6, 12, 5, 3), V2 = c(9, -4, -4, 18, 9), V3 = c(-6, -4, 12, 4, 10), V4 = c(0, 0, -2, 16, 16), V5 = c(5, 11, 17, 10, 8)))
test_data5 <- as.data.frame(list(V1 = c(11, 20, 18, 16, 16), V2 = c(-8, -7, -5, -13, 8), V3 = c(13, 12, 10, 18, -3), V4 = c(6, 15, 13, 11, 11), V5 = c(6, 13, 23, 10, 24), V6 = c(7, 1, 16, 13, 1)))

is_multicol <- function(d){
  correlations <- cor(d)
  diag(correlations) <- 0
  correlations <- abs(correlations)
  correlations <- round(correlations, digits = 3)
  indexes <- which(correlations == 1, arr.ind = TRUE)
  result <- dimnames(indexes)[[1]]
  if (!is.null(result)){
    return(result)
  } else {
    return("There is no collinearity in the data")
  }
}

is_multicol(test_data1)
is_multicol(test_data2)
is_multicol(test_data3)




# In the swiss data, using all variables, select two clusters using hierarchical 
# clustering and store the value of the clusters as a factor in the cluster variable.
# Then visualize the relationship between the Education and Catholic variables 
# in the two selected clusters
my_plot <- ggplot(smart_hclust(swiss, 2), aes(Education, Catholic, col = cluster)) +
  geom_point() +
  geom_smooth(method = "lm")
