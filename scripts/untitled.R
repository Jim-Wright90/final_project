```{r, relationship between teacher training and student suspension, include=TRUE}
tt_oss <- tidy_disc %>% 
  pivot_longer(cols = c("tt_cm", "tt_dp", "tt_s", "tt_wsvb", "tt_pbi"),
               names_to = "training",
               names_prefix = "tt",
               values_to = "status") 
ggplot(tt_oss, aes(x = training, y = frequency, fill = status), 
       position = "dodge") +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Teacher Training Category", 
       y = "Total Number of Outshool Suspension",
       title = "Relationship Between Teacher Training and Student Suspension",
       tag = "(5)",
       fill = "Training")
# ymax = 1000?
```