# =========== Logit regression. Intercept only model 

library(dplyr)
library(ggplot2)
library(vcd)


# read data 
titanic <- read.csv("https://stepic.org/media/attachments/course/524/train.csv")
titanic <- na.omit(titanic)
glimpse(titanic)
titanic <- mutate(titanic, 
                  Survived = factor(Survived, labels = c("No", "Yes")), 
                  Pclass = factor(Pclass, labels = c("First", "Second", "Third")), 
                  Sex = factor(Sex, labels = c("Female", "Male")))

# create model
simple_fit <- glm(Survived ~ 1, titanic, family = "binomial")
summary(simple_fit)
  #               Estimate Std. Error z value Pr(>|z|)    
  # (Intercept)  -0.3799     0.0762  -4.985  6.2e-07 ***
  
  # y = intercept
  # log (p/(1-p)) = intercept
  # intercept significantly less than 0 => log odds less than 0 =>
  # odds are less than 1 => p(Survive) < p(Not Survive)
  # coefficients = logaritms, if odd is needed:
exp(coef(simple_fit)) # 0.6839623 
  # chi-squared test would give us only the information that 
  # distribution of propabilities of "Survive" differs from uniform
  # but logit regression tells that p(Survive) ~= 0.68 
  # and p(Survive) < p(Not Survive)




# There were 50 students in our sample. Some of them passed 
# the exam (positive outcome), and some did not (negative outcome). 
# The intercept of the regression turned out to be -0.8472979.
# How many people did pass the exam?
# log(p/(1-p)) = -0.8472979
exp(-0.8472979) # odd(pass) = 0.4285714
0.4285714 / (1 + 0.4285714) # p(pass) = 0.3
people_passed <- 50 * 0.3




# =========== Logit regression. One categorical predictor model

table(titanic$Survived, titanic$Sex)

odds_male = 93 / 360
odds_female = 197 / 64

log(odds_female)

odds_ratio = odds_male / odds_female
log(odds_ratio)

fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
summary(fit1)
  #             Estimate Std. Error z value Pr(>|z|)    
  #(Intercept)   1.1243     0.1439   7.814 5.52e-15 ***
  #SexMale      -2.4778     0.1850 -13.392  < 2e-16 ***
  
  # Intercept = lod odds for Survival for female
  # SexMale = log(odds_ratio) = log(odds_male / odds_female)

exp(1.1243) + exp(-2.4778 )
1 / exp(-2.4778 )

exp(0.6451) # 1.906178 = survive / not survive,  p(survive) > p(not survive)


# how to determine if a predictor is statistically significant? does it better predict survival?
# -- compare a model without predictors with a model with some predictor

anova(simple_fit, fit1, test = "Chisq")
  # Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
  #1       713     964.52                          
  #2       712     750.70  1   213.82 < 2.2e-16 ***
  # the smaller "Resid. Dev" the better
anova(fit1, test = "Chisq")
  #     Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
  #NULL                   713     964.52              
  #Sex   1   213.82       712     750.70 < 2.2e-16 ***



# =========== Logit regression. Two categorical predictors 

fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")
summary(fit2)
  #                  Estimate Std. Error z value Pr(>|z|)    
  #(Intercept)            3.3081     0.5875   5.631 1.79e-08 ***
  #SexMale               -3.7301     0.6217  -6.000 1.98e-09 ***
  #PclassSecond          -0.8804     0.7256  -1.213  0.22503    
  #PclassThird           -3.4653     0.6202  -5.588 2.30e-08 ***
  #SexMale:PclassSecond  -0.4204     0.8040  -0.523  0.60106    
  #SexMale:PclassThird    2.1542     0.6760   3.187  0.00144 ** 

table(titanic$Survived, titanic$Pclass, titanic$Sex)

# Intercept = 3.3081 logarithm of odds of survival for women from 1st class
female_p1_odds = 82 / 3
log(female_p1_odds)

# SexMale = -3.7301 log(male_p1_odds / female_p1_odds)
female_p1_odds = 82 / 3
male_p1_odds = 40 / 61
log(male_p1_odds / female_p1_odds)

# the chances of survival for first-class passengers are statistically significantly reduced when moving from the gradation "woman" 
# to the gradation "man". in other words, in the first grade for women, the chances of survival are statistically higher than those for men

# PclassSecond = -0.8804 = log(female_p2_odds / female_p1_odds)
female_p1_odds = 82 / 3
female_p2_odds = 68 / 6
log(female_p2_odds / female_p1_odds)

# SexMale:PclassSecond = -0.4204
male_p2_odds = 15 / 84
log(male_p2_odds / female_p2_odds) - log(male_p1_odds / female_p1_odds)
# the proportion between survived males and females is approximately the same in the first and second grades,
# therefore this coefficient is small and not significant

# SexMale:PclassThird = 2.1542
male_p3_odds = 38 / 215
female_p3_odds = 47 / 55
log(male_p3_odds / female_p3_odds) - log(male_p1_odds / female_p1_odds)
# the relationship between sex and survival differs in first and third grade


# log(p/(1-p)) = 3.3 - 3.7*SexMale - 0.88*PclassSecond - 3.36*PclassSecond - 0.42*SexMale*PclassSecond + 2.15*SexMale*PcclassThird

# for male from 3nd class:
# log(p/(1-p)) = 3.3 - 3.7 - 3.46 + 2.15 = -1.71
exp(-1.71) # 0.18 = positive / negative



# model comparison
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")

anova(fit1, fit2, test = "Chisq")
  #    Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
  #1       712     750.70                          
  #2       708     642.28  4   108.42 < 2.2e-16 ***

anova(fit2, test = "Chisq")
  #          Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
  #NULL                         713     964.52              
  #Sex         1  213.816       712     750.70 < 2.2e-16 ***
  #Pclass      2   78.269       710     672.43 < 2.2e-16 ***
  #Sex:Pclass  2   30.156       708     642.28  2.83e-07 ***

# the interaction of variables tells us about the differences in the strength of the relationship of two variables 
# at different gradations of another variable


# model with both categorical and quantitative predictors 
fit3 <- glm(Survived ~ Sex + Pclass + Age, titanic, family = "binomial")
summary(fit3)
  #              Estimate Std. Error z value Pr(>|z|)    
  #(Intercept)   3.777013   0.401123   9.416  < 2e-16 ***
  #SexMale      -2.522781   0.207391 -12.164  < 2e-16 ***
  #PclassSecond -1.309799   0.278066  -4.710 2.47e-06 ***
  #PclassThird  -2.580625   0.281442  -9.169  < 2e-16 ***
  #Age          -0.036985   0.007656  -4.831 1.36e-06 ***



# =========== Problems from the course

# Write a "get_coefficients" function that receives a data frame with two variables:
# x (a factor with arbitrary numerical gradations);
# y (a factor with two gradations). 
# The function builds a logit model, where y is the dependent variable and x is the independent variable, 
# and returns a vector with the values of the model's coefficients.

test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
get_coefficients <- function(dataset){
  test <- glm(y ~ x, dataset, family = "binomial")
  return(coef(test))
}




# If there are quantitative predictors in our model, then in the intercept we will have a value corresponding to the base level 
# of categorical predictors and the zero level of quantitative ones. It doesn't always make sense. In such situations, 
# it makes sense to center the quantitative variable so that zero is the average value of the variable. The easiest way 
# to center a variable is to subtract from each observation the average of all observations.

#Your task will be to write a "centered" function that takes as input a dataframe and variable names that need to be centered 
#as described above.

centered <- function(test_data, var_names){
  test_data[var_names] <- sapply(test_data[var_names], function(x) x - mean(x))
  return(test_data)
}




# Write a function that takes a baggage data set as input. It builds a logit regression where the dependent variable is 
# whether baggage was forbidden and the predictors are the other variables, and returns a vector with the names of statistically 
# significant variables (p < 0.05). If there are no significant predictors in the data, the function returns a string with the message "Prediction makes no sense".

test_data1 <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
test_data2 <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")

get_features <- function(dataset){
  dataset <- transform(dataset, is_prohibited = factor(is_prohibited), type = factor(type))
  fit <- glm(is_prohibited ~ ., dataset, family = "binomial")
  result <- anova(fit, test = "Chisq")
  names <- rownames(subset(result,`Pr(>Chi)`<0.05))
  if (length(names) != 0) {
    return(names)
  } else {
    return("Prediction makes no sense")
  }
}

get_features(test_data1)
get_features(test_data2)
