library(rio)
library(here)
library(tidyverse)
library(janitor)
here()
here("data")
list.files(here("data"))

safe <- import(here("data", "2006_school_safety.sav"),
               setclass = "tbl_df") 

library(skimr)

skim(safe)

characterize(safe)
head(safe)

View(safe)

head(safe)

safe %>% 
  select(school_id, urban, level, minority, enroll, tt_cm, tt_dp, tt_s, tt_wsvb, tt_pbi, disc_rec, inc_rec, inc_rep_po, oss)

safe %>% 
  summary(mean(oss))

safe$level

safe %>% 
  filter(level == 3 |
         level == 4)

safe %>% 
  group_by(level = 3)

safe %>% 
  filter(urban == 2)


filter(safe, (urban ==1))

safe$urban
filter(safe, (urban == 4))


safe %>% 
  characterize() %>% 
  select(school_id:level) %>% 
  head()

safe %>% 
  mutate(urban = factor(urban,
                        levels = c("1",
                                   "2",
                                   "3",
                                   "4"),
                        labels = c("City", 
                                   "Urban fringe",
                                   "Town",
                                   "Rural"))) %>% 
  ggplot(aes(urban)) +
  geom_bar()
  

ggplot(safe, aes(x = urban)) +
  geom_histogram(fill = "cornflowerblue", alpha = 0.7) 

library(ggridges)
ggplot(safe, aes(oss, enroll)) +
  geom_density_ridges(fill = "cornflowerblue", alpha = 0.4)

  
str(safe, (enroll))

safe %>% 
  str(enroll)

safe$enroll

safe %>% 
  unite(teach_training, tt_cm, tt_dp, tt_pbi)


(table <- safe %>% 
    filter(urban == "City" |
           urban == "Urban fringe" |
           urban == "Town" |
           urban == "Rural") %>% 
    group_by(urban, oss))
  
#use full data set Pipe to select variables we need #pipe to select as data is loaded in
#geom_bar for plots (training to oss)
#keep data as is - plan to use regression model for 3-way interaction 
#figures: average out of school suspension 
#density plots, ridgeline plots 
#use means to make table or table can be regression output 
#add a variable to filter by to inlcude for project 
#add summarize 
#filter school id by number of suspensions 
#use unite() function

linmod1 <- lm(oss ~ tt_cm, data=safe) %>% 
print(linmod1)

linmod2 <- lm(oss ~ tt_cm + tt_dp, data=safe)
summary(linmod2)

ggplot(safe, aes(tt_cm, oss)) +
  geom_violin()

linmod3 <- lm(oss ~ tt_cm, tt_dp, tt_wsvb, data = safe)
summary(linmod3)

ggplot(safe, aes(tt_cm, oss)) +
  geom_density_ridges(alpha = 0.7) + 
  scale_fill_viridis_d()
