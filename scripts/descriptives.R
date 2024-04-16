


data_demo <- data_tidy %>%
  pivot_wider(
    names_from = "question",
    values_from = "value") %>%
  select(age:level_studies) %>%
  distinct()


data_demo %>%
  group_by(province) %>%
  summarize(
    mean_age = mean(age),
    median_age = median(age),
    min_age = min(age),
    max_age = max(age))

data_demo %>%
  ggplot() +
  aes(x = mother_tongue) +
  facet_wrap(.~ province, scales = "free_y") +
  geom_bar()

