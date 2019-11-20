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

##online code for descriptive tables
descriptives <- mixed_data %>% group_by(Dosage) %>%
  summarize(
    Mean = mean(Recall)
    , Median = median(Recall)
    , SD = sd(Recall)
    , Min = min(Recall)
    , Max = max(Recall)
  )
descriptives[, -1] <- printnum(descriptives[, -1])

apa_table(
  descriptives
  , caption = "Descriptive statistics of correct recall by dosage."
  , note = "This table was created with apa_table()."
  , escape = TRUE
)

descriptives <- safe_organized %>% 
  group_by(urban) %>% 
  summarize(
    Mean = mean(oss)
    , Median = median(oss)
    , SD = sd(oss)
    , Min = min(oss)
    , Max = max(oss)
  )


apa_table(
  descriptives
  , caption = "Descriptive statistics of the total number of out of school suspensions by school urbanness."
  , note = "This table was created with apa_table()."
  , escape = TRUE
)

head(safe)

safe_organized <- safe %>% 
  select(school_id, urban, level, minority, enroll, tt_cm, tt_dp, tt_s, tt_wsvb, tt_pbi, disc_rec, inc_rec, inc_rep_po, oss)

#Research questions:
#1. How do four characteristics (urbaness, level, minority percentage, and enrollment) predict number of out of school suspensions and number of total disciplinary actions recorded?
#2. Do four characteristics precict what teacher trainings are provided? 
#3. Do teacher trainings have an impact on the total number of out of school suspensions (oss) and total disciplinary actions recorded (disc_rec)?

head(safe_organized)

safe_discipline <- safe_organized %>% 
  select(school_id, urban, level, minority, enroll, disc_rec, inc_rec, inc_rep_po, oss)

safe_train <- safe_organized %>% 
  select(school_id, urban, level, minority, enroll, tt_cm, tt_dp, tt_s, tt_wsvb, tt_pbi)

safe_organized$urban

safe_organized$urban
safe_organized$enroll
str$enroll

safe_discipline %>% 
  summarize(mean(oss))

safe_discipline %>% 
  count(oss)

tidy_disc <- safe_discipline %>% 
  pivot_longer(
    cols = c(disc_rec, inc_rec, inc_rep_po),
    names_to = "discipline_type",
    values_to = "frequency"
  ) 
  
head(tidy_disc)


head(safe_organized)

#below code using filter more
tidy_disc2 <- safe_discipline %>% 
  pivot_longer(
    cols = c(oss, disc_rec, inc_rec, inc_rep_po),
    names_to = "discipline type",
    values_to = "frequency"
  ) %>% 
  filter(frequency >= mean(frequency),
         urban == "City",
         urban == "Urban fringe",
         level == "High School",
         enroll == "1,000 or more") %>% 
  ggplot(tidy_disc, aes(urban, frequency)) +
  geom_point(aes(fill = "level", alpha = 0.7))

head(tidy_disc2)
View(tidy_disc2)
tidy_disc %>% 
  summarize(mean(frequency))

tidy_disc %>% 
  arrange(frequency)

tidy_disc %>% 
  arrange(desc(frequency))

head(tidy_disc)


#plot displaying number of oss by type of urban school setting and filled with level 
##can mix and match categorigal variables in code plots below to create different graphics 

safe_discipline %>% 
  arrange(oss)

ggplot(safe_discipline) +
  geom_bar(aes(x = urban, fill = level))

ggplot(safe_discipline) +
  geom_bar(aes(urban, fill = level),
           position = "dodge")

ggplot(safe_discipline) +
  geom_bar(aes(urban, fill = level),
           position = "dodge") +
  facet_wrap(~enroll)

install.packages("ggridges")
library("ggridges")

ggplot(safe_discipline, aes(urban, y = oss)) +
  geom_density_ridges(aes(fill = factor(level)),
                      alpha = 0.7) +
  scale_fill_viridis_d() ##this one didn't work 

ggplot(safe_discipline, aes(x = oss)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~level)

ggplot(safe_discipline, aes(level, oss)) +
  geom_boxplot()

ggplot(safe_discipline, aes(urban, oss)) +
  geom_violin()

#scatterplot displaying number of oss by number of total number of discplines reported
## can shift variables around for different plots
###graphing variables have to be quantifiable while fill variable can be categorical 

ggplot(safe_discipline, aes(oss, disc_rec)) +
  geom_point(aes(color = enroll), position = "jitter") +
  geom_smooth() +
  theme_minimal() +
  labs(x = "Total Number of Out of School Suspensions", 
       y = "Total Number Disciplinary Actions Reported to School Administration",
       title = "Relationship Between Out of School Suspensions and Reported Disciplinary Actions",
       caption = "Created by Jim Wright and Claire Zhang",
       tag = "(1)")



?geom_smooth()


#bargraph displaying yes/no status of teacher training by urban level
##categorical variable and teacher training variables can be swapped for different comparions
###one idea is to play with the data to combine the teacher trainings into one variable 

ggplot(safe_organized) +
  geom_bar(aes(x = urban, fill = tt_cm), 
           position = "dodge") 

ggplot(safe_organized) +
  geom_bar(aes(x = urban, fill = tt_cm), 
           position = "dodge") +
  facet_wrap(~enroll) +
  theme_minimal() +
  labs(x = "School Urbanness Category", 
       y = "Number of Schools That Provide Teacher Training on Classroom Management",
       title = "The Provision of Teacher Training on Classroom Management by Urbanness and Enrollment of School",
       caption = "Created by Jim Wright and Claire Zhang",
       tag = "(2)")


#Linear models 

linmod1 <- lm(oss ~ urban, data=safe_organized) %>% 
  summary(linmod1)

linmod2 <- lm(oss ~ tt_cm + tt_dp, data=safe_organized) %>% 
  summary(linmod2)

linmod3 <- lm(oss ~ tt_cm, tt_dp, tt_wsvb, data = safe_organized)
summary(linmod3)

linmod4 <- lm(oss ~ disc_rec + urban, data = safe_organized)
summary(linmod4) #this model appears to work



t.test(safe_organized$tt_cm, na.action = "always")
?t.test

## linear model from Joe 
mod <- lm(oss ~ urban + enroll, data = safe_organized)
summary(mod)

#descriptive summary stats
mean,median,25th and 75th quartiles,min,max
summary(safe_organized)

