library(dslabs)
library(tidyverse)
library(dplyr)

## Preparatory code from the previous chapter

polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>% 
  mutate(start = avg - 1.96*se, end = avg + 1.96*se) 

# Posterior distribution
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

#Probability statement
posterior_mean + c(-1.96, 1.96)*posterior_se

#Posterior probability
1 - pnorm(0, posterior_mean, posterior_se) #this seems to be a little bit overconfident

# Simulating data points
set.seed(3)
J<- 6
N <- 2000
d <- .021
p <- (d + 1)/2
X <- d + rnorm(J,0,2*sqrt(p*(1-p)/N))

# i pollsters and j polls from pollsters
I <- 5
J <- 6
N <- 2000
X <- sapply(1:I, function(i){
  d + rnorm(J,0,2*sqrt(p*(1-p)/N))
})

# pollster effect model
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d + 1)/2
h <- rnorm(I, 0, 0.025)
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J,0,2*sqrt(p*(1-p)/N))
})

# general bias variability
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

## Electoral college ##

# aggregating results from a poll taken during the last week before the election
results <- polls_us_election_2016 %>%
  filter(state!="U.S." & 
           !grepl("CD", state) & 
           enddate >="2016-10-31" & 
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

results %>% arrange(abs(avg))

results <- left_join(results, results_us_election_2016, by = "state")
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns a standard deviation, the median of the rest, to states with just one poll
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm=TRUE), sd))

# posterior mean, posterior st. error
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n), 
                   B = sigma^2 / (sd^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

results %>% mutate(sigma = sd/sqrt(n), 
                   B = sigma^2 / (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2))) %>%
  ggplot(aes(avg, posterior_mean, size = n)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0)

# we keep the total number of electoral votes for Clinton

mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n), 
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>% 
    summarize(clinton = sum(clinton)) %>% 
    .$clinton + 7## 7 for Rhode Island and D.C.
})
mean(clinton_EV>269)

# model with general bias

tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/n  + bias_sd^2),  
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>% 
    summarize(clinton = sum(clinton) + 7) %>% .$clinton ## 7 for Rhode Island and D.C.
})
mean(clinton_EV_2>269)
