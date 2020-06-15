

jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")


str(jobs_gender)
library(dplyr)

#To add a column , total workers male female proportion
prop_male_female <- jobs_gender %>% 
  select(year,occupation,total_workers,workers_male,workers_female) %>% 
  mutate(prop = workers_male/workers_female)

prop_male_female

# To filter the minor_category:architecture and engineering
arch_eng <- jobs_gender %>%
  select(year,occupation,minor_category,major_category,workers_male,workers_female) %>% 
  filter(minor_category=="Architecture and Engineering")
arch_eng


#to arrange total earnings in ascending orders according to wages
 asc_wage <- jobs_gender %>% 
   select(year,occupation,major_category,minor_category,total_earnings) %>% 
   arrange(total_earnings)
 asc_wage
 
 #mean wage of whole major category
 mean_wage <- jobs_gender %>% 
   group_by(major_category) %>% 
   summarise(ave_wage = mean(total_earnings))
 mean_wage
 
 #
number <- jobs_gender %>% 
   filter((total_earnings_male<50000) & (total_earnings_female <30000) ) %>% 
   group_by(occupation) %>% 
   summarise(total = n())
number
 
 
#
average <- jobs_gender %>% 
  summarise(average=mean(total_earnings))
   
average
 
#female earning more than average=49762.09
high_income <- jobs_gender %>% 
  select(year,occupation,total_earnings_female) %>% 
  filter(total_earnings_female > 49762.09) %>% 
  group_by(occupation) 

high_income
library(ggplot2)

plot <- jobs_gender %>% 
  ggplot(aes(x=occupation, y= total_workers, fill=occupation))+
           geom_col()+
           guides(fill=F)
plot
html_document:
  keep_md: yes
df: paged
theme: readable
---
