# Claire Zhang and Jim Wright

#Data input 

library(rio)
library(here)
library(tidyverse)
library(janitor)
here()
here("data")
list.files(here("data"))

safe <- import(here("data", "2006_school_safety.sav"),
               setclass = "tbl_df") %>% 
  characterize() %>% 
  janitor::clean_names() 

library(knitr)
library(kableExtra)

#selecting data we would like to further investigate 

safe_organized <- safe %>% 
  select(school_id, urban, level, minority, enroll, tt_cm, tt_dp, tt_s, tt_wsvb, tt_pbi, disc_rec, inc_rec, inc_rep_po, oss)

safe_organized <- safe_organized %>% 
  mutate(total_discipline_actions = disc_rec + inc_rec + inc_rep_po + oss)

head(safe_organized)
View(safe_organized)

tidy_disc <- safe_organized %>% 
  pivot_longer(
    cols = c(disc_rec, inc_rec, inc_rep_po, oss),
    names_to = "discipline_type",
    values_to = "frequency"
  ) 

head(tidy_disc)
##copied above new code to bottom of document where most recent updated are located 11/21/19

total_frequency <- tidy_disc %>% 
  summarize(sum(frequency)) %>% 
  group_by(school_id) ## didn't work

total_frequency <- tidy_disc %>% 
  mutate(total_frequency = frequency) %>% 
  group_by(school_id)

View(total_frequency)

tidy_disc %>% 
  mutate(discipline_type = factor(discipline_type, 
                                  levels = c(disc_rep,
                                             inc_rec, 
                                             inc_rep_po,
                                             oss),
                                  labels = c(disc_rep = "Disciplinary Actions Reported",
                                             inc_rec = "Incidents Reported", 
                                             inc_rep_po = "Incidents Reported to Police",
                                             oss = "Out of School Suspensions")))


head(tidy_disc)

tidy_disc %>% 
  filter(disc_rec > 50)


#plots 

ggplot(safe_organized) +
  geom_bar(aes(urban, fill = level),
           position = "dodge") +
  theme_minimal() +
  labs(x = "School Urbanness Category with Fill of School Level", 
       y = "Total",
       title = "Number of Schools per Urban Category and Level",
       caption = "Created by Jim Wright and Claire Zhang",
       tag = "(1)")

ggplot(safe_organized) +
  geom_bar(aes(urban, fill = level),
           position = "dodge") +
  facet_wrap(~enroll) +
  theme_minimal() +
  labs(x = "School Urbanness Category with Fill of School Level Faceted by School Enrollment", 
       y = "Total",
       title = "Number of Schools per Urban Category and Level",
       caption = "Created by Jim Wright and Claire Zhang",
       tag = "(2)")

ggplot(safe_organized, aes(oss, disc_rec)) +
  geom_point(aes(color = enroll), position = "jitter") +
  geom_smooth() +
  theme_minimal() +
  labs(x = "Total Number of Out of School Suspensions", 
       y = "Total Number Disciplinary Actions Reported to School Administration",
       title = "Relationship Between Out of School Suspensions and Reported Disciplinary Actions",
       caption = "Created by Jim Wright and Claire Zhang",
       tag = "(3)")


ggplot(safe_organized) +
  geom_bar(aes(x = urban, fill = tt_cm), 
           position = "dodge") +
  facet_wrap(~enroll) +
  theme_minimal() +
  labs(x = "School Urbanness Category", 
       y = "Number of Schools That Provide Teacher Training on Classroom Management",
       title = "The Provision of Teacher Training on Classroom Management by Urbanness and Enrollment of School",
       caption = "Created by Jim Wright and Claire Zhang",
       tag = "(4)")
#curious on how to change legend title on plot 4


#Model fit

mod1 <- lm(oss ~ urban + enroll, data = safe_organized)
summary(mod1)

# Schools coded as Urban with an enrollment over 1,000 or more students reported an average of 
# 36.03 out of school suspensions.  Schools coded as rural reported, on average, 6.26 fewer suspensions
# controlling for school enrollment size.  Schools coded as Town reported, on average, 4.34 fewer suspensions. 
# ^Continue results following same interpretation. The model accounted for approximately 6.3% of the variance. 

mod1_summary <- safe_organized %>% 
  group_by(urban) %>% 
  summarize(total_oss = sum(oss))

mod1_plot <- safe_organized %>% 
  mutate(pred_mod1 = predict(mod1))

ggplot(safe_organized, aes(urban, oss)) +
  geom_boxplot()
#Not sure if this plot makes any sense 

head(mod1_summary)

#mutate practice 

safe_organized <- safe_organized %>% 
  mutate(total_discipline_actions = disc_rec + inc_rec + inc_rep_po + oss)

head(safe_organized)  

View(safe_organized)

mean_discipline_actions <- safe_organized %>% 
  summarize(mean(total_discipline_actions))

mean_discipline_actions #we can filter this value into safe_organized after mutate 

#filtering in total discipline over the average and tt_pbi status no

filtered_schools <- safe_organized %>% 
  filter(total_discipline_actions >= 159 & tt_pbi == "no")

View(filtered_schools)

safe_organized <- safe_organized %>% 
  mutate(total_discipline_actions = disc_rec + inc_rec + inc_rep_po + oss)

head(safe_organized)
View(safe_organized)

mean_discipline_actions <- safe_organized %>% 
  summarize(mean(total_discipline_actions))

tidy_disc <- safe_organized %>% 
  pivot_longer(
    cols = c(disc_rec, inc_rec, inc_rep_po, oss),
    names_to = "discipline_type",
    values_to = "frequency"
  ) %>% 
  filter(total_discipline_actions >= 159 & tt_pbi == "no") ##I'm curious if we could incorporate pivot_wider in here

View(tidy_disc)

ggplot(tidy_disc) +
  geom_bar(aes(urban, fill = level),
           position = "dodge") +
  theme_minimal() +
  labs(x = "School Urbanness Category with Fill of School Level", 
       y = "Total",
       title = "Number of Schools per Urban Category and Level",
       caption = "Created by Jim Wright and Claire Zhang",
       tag = "(1)")

ggplot(tidy_disc) +
  geom_bar(aes(urban, fill = level),
           position = "dodge") +
  facet_wrap(~enroll) +
  theme_minimal() +
  labs(x = "School Urbanness Category with Fill of School Level Faceted by School Enrollment", 
       y = "Total",
       title = "Number of Schools per Urban Category and Level",
       caption = "Created by Jim Wright and Claire Zhang",
       tag = "(2)")

ggplot(tidy_disc, aes(total_discipline_actions, frequency)) +
  geom_point(aes(color = discipline_type), position = "jitter") +
  geom_smooth() +
  theme_minimal() +
  labs(x = "Total Number of Disciplinary Actions Reported to School Administration", 
       y = "Type of Disciplinary Action by Frequency",
       title = "Relationship Between Out of School Suspensions and Reported Disciplinary Actions",
       caption = "Created by Jim Wright and Claire Zhang",
       tag = "(3)") ##this plot needs more work

ggplot(tidy_disc, aes(total_discipline_actions)) +
  geom_histogram(aes(fill = discipline_type)) +
  theme_minimal() +
  labs(x = "Total Number of Disciplinary Actions Reported to School Administration", 
       y = "Type of Disciplinary Action by Frequency",
       title = "Relationship Between Out of School Suspensions and Reported Disciplinary Actions",
       caption = "Created by Jim Wright and Claire Zhang",
       tag = "(4)") ##this plot needs more work

tidy_disc %>% 
  mutate(discipline_type = factor(discipline_type, 
                                  levels = c(disc_rep,
                                             inc_rec, 
                                             inc_rep_po,
                                             oss),
                                  labels = c(disc_rep = "Disciplinary Actions Reported",
                                             inc_rec = "Incidents Reported", 
                                             inc_rep_po = "Incidents Reported to Police",
                                             oss = "Out of School Suspensions"))) #need code like this to re-title legend

head(tidy_disc)

tidy_new %>% tidy_disc
mutate(discipline_type = factor(discipline_type,
                                levels = c(disc_rec,
                                           inc_rec,
                                           inc_rep_po,
                                           oss),
                                labels = c(disc_rec = "Disciplinary Actions Reported",
                                           inc_rec = "Incidents Reported",
                                           inc_rep_po = "Incidents Reported to Police",
                                           oss = "Out of School Suspensions")))


ggplot(safe_organized) +
  geom_bar(aes(x = urban, fill = tt_cm), 
           position = "dodge") +
  facet_wrap(~enroll) +
  theme_minimal() +
  labs(x = "School Urbanness Category", 
       y = "Number of Schools That Provide Teacher Training on Classroom Management",
       title = "The Provision of Teacher Training on Classroom Management by Urbanness and Enrollment of School",
       caption = "Created by Jim Wright and Claire Zhang",
       tag = "(5)")
