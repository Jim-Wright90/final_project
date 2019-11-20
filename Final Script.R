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

tidy_disc <- safe_discipline %>% 
  pivot_longer(
    cols = c(disc_rec, inc_rec, inc_rep_po),
    names_to = "discipline_type",
    values_to = "frequency"
  ) 

head(tidy_disc)


tidy_disc %>% 
  mutate(discipline_type = factor(discipline_type, 
                                  levels = c(disc_rec,
                                             inc_rec, 
                                             inc_rep_po),
                                  labels = c(disc_rec = "Disciplinary Actions Reported",
                                             inc_rec = "Incidents Reported", 
                                             inc_rep_po = "Incidents Reported to Police")))


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
summary(mod)

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

