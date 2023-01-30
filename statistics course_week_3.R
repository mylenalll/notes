
# ========= K-Means Clustering

# Suppose this is one of the clusters in our data, then what would be the within-cluster sum of squares?
cl <- kmeans(data.frame(x=c(-3,1,2,3,5,6,7), y = c(3,4,6,8,2,11,1)),1)
cl$tot.withinss




# Write a "smart_hclust" function that receives an input dataframe with an arbitrary numeric 
# number of clusters to use and the number of clusters needed to be allocated using hierarchical 
# clustering. The function should include in the original data set a new variable factorial 
# group - the number of the cluster to which the solution of the problem is assigned.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")

smart_hclust <- function(test_data, cluster_number){
  dist_matrix <- dist(test_data)
  fit <- hclust(dist_matrix)
  test_data$cluster <- as.factor(cutree(fit, cluster_number))
  return(test_data)
}

smart_hclust(test_data, 3)




# Если нас интересует не только сам факт того, что мы смогли выделить кластеры в наших 
# данных, но мы также хотим понять, чем же они различаются, разумно сравнить кластеры 
# между собой по имеющимся переменным. Функция должна вернуть названия переменных, 
# по которым были обнаружен значимые различия между выделенными кластерами (p < 0.05). 
# Иными словами, после того, как мы выделили заданное число кластеров, мы добавляем 
# в исходные данные новую группирующую переменную — номер кластера, и сравниваем 
# получившиеся группы между собой по количественным переменным при помощи 
# дисперсионного анализа.
