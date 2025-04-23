#####################################################
#HW12
# Ben Horner
# MATH 240
#####################################################
##################################
#Libraries
##################################
library(pwr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(xtable)
library(e1071)
library(effectsize)
library(VGAM)

##################################
# Question 1
##################################
#values of t20 that provide support
alpha <- 0.05
n <- 20  # sample size
df <- n - 1  # degrees of freedom
t_crit_20 <- qt(1 - alpha, df = df)  # one-sided upper tail
t_crit_20

#Values of t30 that provide support
n <- 30  # sample size
df <- n - 1  # degrees of freedom
t_crit_30 <- qt(1 - alpha, df = df)  # one-sided upper tail
t_crit_30

#Simulation Study for Type I error
R = 10000 #number of sims
alpha = 0.05
set.seed = 151
a = 0
b = 4
n = 20 #t_20
rejects = 0 #will add to this for each time we reject H0

for (i in 1:R) {
  x <- rlaplace(n, location = a, scale = b)
  #take observation (x) from the Laplace dist. n times
  t = mean(x) / (sd(x) / sqrt(n)) #T-stat formula
  
  #Compare to critical point calculated in a) and b)
  if (t > t_crit_20){
    rejects = rejects + 1
  }
}
TypeI_20 = rejects/R
  
n = 30 #t_30
rejects = 0 #will add to this for each time we reject H0

for (i in 1:R) {
  x <- rlaplace(n, location = a, scale = b)
  #take observation (x) from the Laplace dist. n times
  t = mean(x) / (sd(x) / sqrt(n)) #T-stat formula
  
  #Compare to critical point calculated in a) and b)
  if (t > t_crit_30){
    rejects = rejects + 1
  }
}
TypeI_30 = rejects/R
print(c(TypeI_20, TypeI_30))


##################################
# Question 1
##################################
