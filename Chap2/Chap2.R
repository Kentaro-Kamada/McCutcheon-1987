library(tidyverse)
library(janitor)
library(kamaken)

data <- 
  read_csv('Chap2/tab2.2.csv') %>% 
  mutate(across(A:C, factor)) 

data_person <- 
  pmap_dfr(list(data$A, data$B, data$C, data$n),
           ~tibble(A = rep(..1, ..4), B = rep(..2, ..4), C = rep(..3, ..4)))
  
  

data_person %>% 
  tabyl(A, B) %>% 
  chisq.test(correct = F)

data_person %>% 
  group_split(C) %>% 
  map(~tabyl(., A, B) %>% chisq.test)
  

data_person %>% 
  filter(C == 2) %>% 
  describe_d_(A, B)


