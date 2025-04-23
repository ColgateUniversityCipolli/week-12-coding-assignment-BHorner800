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
# Question 2
##################################
R = 10000
n = 15
#critical t-point which is a = 0.05
alpha <- 0.05
df <- n - 1  # degrees of freedom

t15_rtail = qt(1 - alpha, df = df)  #right-tailed text
t15_ltail = qt(alpha, df = df) #left-tailed test
t15_2tail_low = qt(alpha/2, df = df) # left side of two tailed
t15_2tail_high = qt(1 - alpha/2, df = df) #right side of two tailed

##########
#Beta(10,2)
##########
a = 10
b = 2
mu = a/(a+b)

left_reject = 0
right_reject = 0
twotail_reject = 0

for (i in 1:R){
  x = rbeta(n = n, shape1 = a, shape2 = b)
  #left-tailed test
  t_left = t.test(x, alternative = "less", mu = mu)$statistic
  if (t_left < t15_ltail){
    left_reject = left_reject + 1
  }
  #right-tailed test
  t_right = t.test(x, alternative = "greater", mu = mu)$statistic
  if (t_right > t15_rtail){
    right_reject = right_reject + 1
  }
  #two-tail test
  t_twotail = t.test(x, alternative = "two.sided", mu = mu)$statistic
  if (t_twotail < t15_2tail_low | t_twotail > t15_2tail_high){
    twotail_reject = twotail_reject + 1
  }
}
    

#Calculating the Type I error
TypeI_left_102 = left_reject/R
TypeI_right_102 = right_reject/R
TypeI_2tail_102 = twotail_reject/R


##########
#Beta(2,10)
##########
a = 2
b = 10
mu = a/(a+b)

left_reject = 0
right_reject = 0
twotail_reject = 0

for (i in 1:R){
  x = rbeta(n = n, shape1 = a, shape2 = b)
  #left-tailed test
  t_left = t.test(x, alternative = "less", mu = mu)$statistic
  if (t_left < t15_ltail){
    left_reject = left_reject + 1
  }
  #right-tailed test
  t_right = t.test(x, alternative = "greater", mu = mu)$statistic
  if (t_right > t15_rtail){
    right_reject = right_reject + 1
  }
  #two-tail test
  t_twotail = t.test(x, alternative = "two.sided", mu = mu)$statistic
  if (t_twotail < t15_2tail_low | t_twotail > t15_2tail_high){
    twotail_reject = twotail_reject + 1
  }
}



TypeI_left_210 = left_reject/R
TypeI_right_210 = right_reject/R
TypeI_2tail_210 = twotail_reject/R

##########
#Beta(10,10)
##########
a = 10
b = 10
mu = a/(a+b)

left_reject = 0
right_reject = 0
twotail_reject = 0

for (i in 1:R){
  x = rbeta(n = n, shape1 = a, shape2 = b)
  #left-tailed test
  t_left = t.test(x, alternative = "less", mu = mu)$statistic
  if (t_left < t15_ltail){
    left_reject = left_reject + 1
  }
  #right-tailed test
  t_right = t.test(x, alternative = "greater", mu = mu)$statistic
  if (t_right > t15_rtail){
    right_reject = right_reject + 1
  }
  #two-tail test
  t_twotail = t.test(x, alternative = "two.sided", mu = mu)$statistic
  if (t_twotail < t15_2tail_low | t_twotail > t15_2tail_high){
    twotail_reject = twotail_reject + 1
  }
}



TypeI_left_1010 = left_reject/R
TypeI_right_1010 = right_reject/R
TypeI_2tail_1010 = twotail_reject/R


##########
# Results
##########
Betas = c("Beta(10,2)", "Beta(2,10)", "Beta(10,10)")
left_tests = c(TypeI_left_102, TypeI_left_210, TypeI_left_1010)
right_tests = c(TypeI_right_102, TypeI_right_210, TypeI_right_1010)
two_tests = c(TypeI_2tail_102, TypeI_2tail_210, TypeI_2tail_1010)
results = data.frame(Beta = Betas,
                     left_tailed = left_tests,
                     right_tailed = right_tests,
                     two_tailed = two_tests)
results
xtable(results)
