# Write a "smart_test" function that receives a dataframe with two nominative variables with an arbitrary number 
# of gradations as input. The function should test the hypothesis that the two variables are independent using 
# a chi-squared test or Fisher's exact test.
smart_test <-  function(x){
  x <- table(x)
  if (min(x) < 5){
    return(fisher.test(x)$p.value)
  }
  else {
    test <- chisq.test(x)
    return(unname(c(test$statistic, test$parameter, test$p.value)))
  }
}
smart_test(mtcars[,c("am", "vs")])
smart_test(mtcars[1:20,c("am", "vs")])




# Write a function "most_significant" that receives a dataframe with an arbitrary number of variables as input, 
# where each variable is a nucleotide sequence. For each variable, we can test the null hypothesis that all 
# nucleotides (A, T, G, C) occur with equal probability within that sequence.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
most_significant <-  function(x){
  names <- colnames(x)
  chi.results <- c()
  for (col in colnames(x)){
    as <- length(which(x[col] == "A"))
    ts <- length(which(x[col] == "T"))
    gs <- length(which(x[col] == "G"))
    cs <- length(which(x[col] == "C"))
    chi.results <- append(chi.results, chisq.test(c(as,ts,gs,cs))$p.value)
  }
  return(names[which(chi.results == min(chi.results))])
}
most_significant(test_data)



# Create a new variable important_cases - a factor with two gradations ("No" and "Yes").
# The variable should take the value Yes if the values of at least three quantitative variables for this flower 
# are above the average. Otherwise, the important_cases variable will be set to No.
iris
test <- apply(iris[,1:4], 2, mean)
for (i in 1:nrow(iris)){
  iris$important_cases[i] <- ifelse(sum(iris[i,1:4] > test) > 2, 1, 0)
}
iris$important_cases <- factor(iris$important_cases, levels = c(0, 1), labels = c('No', 'Yes'))


# Let's write a get_important_cases function that accepts a dataframe with an arbitrary number of quantitative variables as input 
# (at least two variables are guaranteed). The function must return a dataframe with a new variable - the important_cases factor.
# The important_cases variable is set to Yes if, for a given case, more than half of the scale variables have values above the mean.
# Otherwise, the important_cases variable is set to No.
test_data <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))


get_important_cases(test_data)

get_important_cases <- function(x){
  threshold <- length(x)%/%2 + 1
  meanss <- sapply(x, mean)
  for (i in 1:nrow(test_data)){
    x$important_cases[i] <- ifelse(sum(x[i,] > meanss) >= threshold, 1, 0)
  }
  x$important_cases <- factor(x$important_cases, levels = c(0, 1), labels = c('No', 'Yes'))
  return(x)
}




# Write a stat_mode function that takes a vector of numbers of arbitrary length as input and returns a numeric vector 
# with the most common value. If there is more than one most frequently occurring value, the function should return multiple 
# mode values as a numeric vector.
stat_mode <- function(x){
  conttable <- table(x)
  return(as.numeric(names(which(conttable == max(conttable)))))
}




# Let's write the max_resid function, which receives a dataframe with two variables as input: the type of drug and the result 
# of its use. The function should find the cell of the contingency table with the maximum value of the standardized residue 
# and return a vector of two elements: the name of the row and column of this cell.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
ctable <- table(test_data)
test <- chisq.test(ctable)
stresiduls <- test$stdres
abs(stresiduls)
ind <- which(stresiduls == max(stresiduls), arr.ind = TRUE)
row_names <- row.names(ind)
ind[2]
result <- ifelse(ind[2] == 2, "positive", "negative")

test_data <- as.data.frame(list(Drugs = c(2, 2, 2, 3, 2, 3, 3, 3, 2, 2, 1, 2, 1, 1, 3, 3, 2, 2, 1, 1, 1, 1, 2, 2, 2, 1, 3, 3, 2, 1, 2, 2, 2, 2, 2, 2, 3, 2, 2, 3, 2, 2, 2, 3, 3, 2, 2, 2, 1, 3, 1, 2, 2, 2, 2, 1, 3, 2, 2, 2, 1, 1, 2, 3, 2, 2, 1, 1, 2, 3, 2, 1, 2, 3, 2, 1, 2, 3, 2, 2, 2, 3, 1, 2, 3, 3, 2, 3, 2, 3, 2, 1, 2, 2, 1, 3, 2, 1, 2, 1, 2, 2, 1, 2, 2), Result = c(2, 2, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 1, 1, 2, 2, 1, 1, 2, 2, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2, 2, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 2, 
                                                                                                                                                                                                                                                                                                                                                                                 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 2, 2)))
max_resid <- function(x){
  ctable <- table(x)
  test <- chisq.test(ctable)
  stresiduls <<- test$stdres
  # stresiduls <<- abs(stresiduls)
  ind <<- which(stresiduls == max(stresiduls), arr.ind = TRUE)
  row_name <<- row.names(ind)
  result <<- ifelse(ind[2] == 2, "positive", "negative")
  return(c(row_name, result))
}

max_resid(test_data)




# Plot the frequency distribution of the color variable, on which the cut variable is responsible for the color of the bars. 
# Save the chart code to the obj variable.
library(ggplot2)
diamonds
obj <- ggplot(diamonds, aes (x = color, fill = cut))+  
  geom_bar(position = 'dodge')
