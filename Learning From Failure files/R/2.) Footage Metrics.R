library(tidyverse)

#Let's start by looking at the footage data on its own by employee

Analysis_Footage <- read_csv("Data/Analysis Footage.csv")
HR_Relevant <- read_csv("Data/Relevant HR Data.csv")

Footage_With_Gaps <- Analysis_Footage %>%
  group_by(Employee_ID) %>% 
  mutate(Gap_Hours = as.numeric(Clip_End - lag(Clip_Start))/3600) %>% 
  ungroup() %>% 
  select(-created_date_record_start, -date_record_end)

Footage_Metrics_by_Employee <- Footage_With_Gaps %>% 
  group_by(Employee_ID) %>% 
  summarize(Clips = n(),
            Days_With_Footage = n_distinct(day(Clip_Start)),
            Footage_Hours = sum(Duration_Hours),
            Gaps = sum(Gap_Hours > .25 & Gap_Hours < 8, na.rm = TRUE),
            Average_Clips_per_Day = Clips/Days_With_Footage,
            Average_Footage_Hours_per_Day = Footage_Hours/Days_With_Footage,
            Average_Clip_Length = mean(Duration_Hours),
            Average_Gaps_per_Day = Gaps/Days_With_Footage)

write_csv(Footage_Metrics_by_Employee, "Output/Footage Metrics by Employee.csv")

Footage_Metrics_by_Employee %>% 
  select(Employee_ID, contains("Average")) %>% 
  pivot_longer(-Employee_ID, names_to = "Metric", values_to = "Value") %>% 
  ggplot(aes(Value)) +
  geom_histogram() +
  facet_wrap(~Metric, scales = "free")

#How about by facility?

Footage_Metrics_by_Facility <- Footage_With_Gaps %>% 
  inner_join(HR_Relevant) %>% 
  group_by(HR_Location) %>% 
  summarize(Staff = n_distinct(Employee_ID),
            Clips = n_distinct(Clip_ID),
            Footage_Hours = sum(Duration_Hours),
            Average_Clips_per_Staff = Clips/Staff,
            Average_Hours_per_Staff = Footage_Hours/Staff,
            Average_Clip_Length = mean(Duration_Hours),
            Average_Gaps_per_Staff = sum(Gap_Hours > .25 & Gap_Hours < 8, na.rm = TRUE)/Staff)

write_csv(Footage_Metrics_by_Facility, "Output/Footage Metrics by Facility.csv")