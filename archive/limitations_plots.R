library(tidyverse)
library(ggplot2)

dat[dat$Country.Region == "Korea, South", ] %>% ggplot(aes(x=day, y=new_cases)) + 
                                                geom_line() + 
                                                geom_smooth( se = F) +
                                                labs(title = "South Korea - new cases")+ xlab("Day") + ylab("New cases")
  
dat[dat$Country.Region == "Italy", ] %>% ggplot(aes(x=day, y=new_cases)) + 
  geom_line() + 
  geom_smooth( se = F) +
  labs(title = "Italy - new cases")+ xlab("Day") + ylab("New cases")

dat[dat$Country.Region == "US", ] %>% ggplot(aes(x=day, y=new_cases)) + 
  geom_line() + 
  geom_smooth( se = F) +
  labs(title = "US - new cases")+ xlab("Day") + ylab("New cases")
