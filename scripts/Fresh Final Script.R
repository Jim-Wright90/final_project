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

safe_organized <- safe %>% 
  select(school_id, urban, level, minority, enroll, tt_cm, tt_dp, tt_s, tt_wsvb, tt_pbi, disc_rec, inc_rec, inc_rep_po, oss)

safe_organized <- safe_organized %>% 
  mutate(total_discipline_actions = disc_rec + inc_rec + inc_rep_po + oss) %>% 
  rename(teacher_training_classroom_management = tt_cm,
         teacher_training_discipline_policies = tt_dp,
         teacher_training_safety_procedures = tt_s,
         teacher_training_early_warning_signs_for_violent_behavior = tt_wsvb,
         teacher_training_positive_behavioral_intervention = tt_pbi,
         disciplinary_actions_recorded = disc_rec, 
         incidents_recorded = inc_rec,
         incidents_reported_to_police = inc_rep_po,
         out_of_school_suspensions = oss,
         urbanicity = urban,
         minority_percentage = minority, 
         enrollment = enroll)

head(safe_organized)
View(safe_organized)

mean_discipline_actions <- safe_organized %>% 
  summarize(mean(total_discipline_actions))

mean_discipline_actions


tidy_disc1 <- safe_organized %>% 
  pivot_longer(
    cols = c(disciplinary_actions_recorded, incidents_recorded, incidents_reported_to_police, out_of_school_suspensions),
    names_to = "discipline_type",
    values_to = "total"
  ) %>% 
  filter(total_discipline_actions >= 159 & 
         teacher_training_classroom_management == "no" & 
         teacher_training_positive_behavioral_intervention == "no" &
         teacher_training_discipline_policies == "no" &
         teacher_training_early_warning_signs_for_violent_behavior == "no" &
         teacher_training_safety_procedures == "no")

#plot for tidy_disc1
ggplot(tidy_disc1, aes(urbanicity)) +
  geom_bar(stat = "count", position = "dodge", fill = "blue") +
  facet_wrap(~level) +
  theme_minimal() +
  scale_fill_discrete(name = "Discipline Type", labels = c("Disciplinary Actions Recorded", 
                                                           "Incidents Recorded",
                                                           "Incidents Reported to Police",
                                                           "Out of School Suspensions")) +
  labs(x = "School Urbanicity Level",
       y = "Total Disciplinary Actions",
       title = "Number of Disciplinary Actions by School Urbanicity and Level for Schools That Did Not Provide Any Form
                of Teacher Training and Reported More Than the Average Number of Disciplinary Actions",
       tage = "(1)")
  

ggplot(tidy_disc1, aes(minority_percentage)) +
  geom_bar(stat = "count", position = "dodge", fill = "blue") +
  facet_wrap(~enrollment) +
  theme_minimal() +
  scale_fill_discrete(name = "Discipline Type", labels = c("Disciplinary Actions Recorded", 
                                                           "Incidents Recorded",
                                                           "Incidents Reported to Police",
                                                           "Out of School Suspensions")) +
  labs(x = "Minority Percentage",
       y = "Total Disciplinary Actions",
       title = "Number of Disciplinary Actions by School Minority Percentage and Enrollment Size for Schools That Did Not Provide Any Form
       of Teacher Training and Reported More Than the Average Number of Disciplinary Actions",
       tage = "(2)")  

#descriptive statistics table tidydisc1
knitr::kable(tidy_disc1,
             caption = "Statistics for the Dataset, tidy_disc1") #try this knitr::kable code for table in RMD to check if it works

tidy_table1 <- tidy_disc1 %>% 
  group_by(school_id, urbanicity, level, minority_percentage, enrollment) %>% 
  summarize(sum(total_discipline_actions))

apa_table(tidy_table1, 
          caption = "Total Disciplinary Actions for Schools Not Providing Any Form of Teacher Training by School Category")


#tidy_disc2

tidy_disc2 <- tidy_disc1 %>% 
  pivot_wider(
    names_from = "discipline_type",
    values_from = "total"
  )

ggplot(tidy_disc2, aes(disciplinary_actions_recorded, incidents_recorded)) +
  geom_point(aes(color = minority_percentage), position = "jitter") +
  facet_wrap(~enrollment) +
  theme_minimal() +
  scale_fill_discrete(name = "Minority Percentage", labels = c("Less than 5 Percent",
                                                               "5 to 20 Percent",
                                                               "20 to 50 Percent",
                                                               "50 Percent or More")) +
  labs(x = "Total Disciplinary Actions Recorded",
       y = "Total Incidents Recorded",
       title = "Relationship of Total Disciplinary Actions Recorded and Total Incidents Recorded by School Enrollment and Minority Percentage",
       tage = "(3)")


ggplot(tidy_disc2, aes(incidents_reported_to_police, out_of_school_suspensions)) +
  geom_point(aes(color = urbanicity), position = "jitter") +
  facet_wrap(~level) +
  theme_minimal() +
  labs(x = "Total Disciplinary Actions Recorded",
       y = "Total Incidents Recorded",
       title = "Relationship of Total Incidents Reported to Police and Total Out of School Suspensions by School Level and Urbanicity",
       tage = "(4)")


knitr::kable(tidy_disc2,
             caption = "Statistics for the Dataset, tidy_disc2") #try this knitr::kable code for table in RMD to check if it works

tidy_table2 <- tidy_disc2 %>% 
  group_by(school_id, total_discipline_actions, incidents_recorded, incidents_reported_to_police, out_of_school_suspensions) %>% 
  summarize(sum(total_discipline_actions))

apa_table(tidy_table2, 
          caption = "Total Disciplinary Actions for Schools Not Providing Any Form of Teacher Training by Specific Form of Discipline")


#Model fit of entire selected data set

mod1 <- lm(total_discipline_actions ~ urbanicity + enrollment, data = safe_organized)
summary(mod1)

# Schools coded as Urban with an enrollment over 1,000 or more students reported an average of 
# 333.11 total disciplinary actions.  Schools coded as rural reported, on average, 52.18 total disciplinary actions
# controlling for school enrollment size.  Schools coded as Town reported, on average, 34.26 fewer total disciplinary. 
# ^Continue results following same interpretation. The model accounted for approximately 20.05% of the variance. 

mod1_summary <- safe_organized %>% 
  group_by(urbanicity, enrollment) %>% 
  summarize(total_discipline_actions = sum(total_discipline_actions))

apa_table(mod1_summary, 
          caption = "Linear Regression Results displaying Total Disciplinary Actions Predicted by School Urbanicity when Controlling for School Enrollment")



mod2 <- lm(total_discipline_actions ~ urbanicity + enrollment + level, data = safe_organized)
summary(mod2)

#accounts for 24% of variance 

mod2_summary <- safe_organized %>% 
  group_by(urbanicity, enrollment, level) %>% 
  summarize(total_discipline_actions = sum(total_discipline_actions))


apa_table(mod1_summary, 
          caption = "Linear Regression Results displaying Total Disciplinary Actions Predicted by School Urbanicity when Controlling for School Enrollment and Level")


mod3 <- lm(total_discipline_actions ~ urbanicity + enrollment + level + minority_percentage, data = safe_organized)
summary(mod2)

#accounts for 24% of variance. Not much dissimlar to model 2

mod3_summary <- safe_organized %>% 
  group_by(urbanicity, enrollment, level, minority_percentage) %>% 
  summarize(total_discipline_actions = sum(total_discipline_actions)) %>% 
  na.omit()


#total descriptive table 

total_table <- safe_organized %>% 
  group_by(urbanicity, 
           level, 
           enrollment, 
           minority_percentage, 
           total_discipline_actions, 
           disciplinary_actions_recorded, 
           incidents_recorded,
           incidents_reported_to_police,
           out_of_school_suspensions) %>% 
  na.omit()

apa_table(total_table)