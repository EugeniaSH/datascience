### Association tests ###

library(tidyverse)
library(dslabs)

data("research_funding_rates")
research_funding_rates %>% 
  select(discipline, contains("total"))

names(research_funding_rates)

totals <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women)

totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                     percent_women = yes_women/(yes_women+no_women))

