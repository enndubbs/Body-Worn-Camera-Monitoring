#Now we need to join the shift data to the camera data.
#Problem: no common unit of analysis besides employee id.
#So how are we to join clips of footage to hours worked?
#Need a common unit of analysis: Employee ID by hour of day

#This function takes any data frame with a Posix start & end column
#and creates an entry for every unit of a given interval

library(tidyverse)
source("R/Interval Convert.R")

Analysis_Footage <- read_csv("Data/Analysis Footage.csv")
HR_Relevant <- read_csv("Data/Relevant HR Data.csv")
Shift_Data <- read_csv("Data/Shift Data Formatted.csv")

Footage_by_Hour <- Analysis_Footage %>% 
  Interval_Convert(Clip_Start, Clip_End, "hour") %>% 
  filter(Seconds_Duration_Within_Interval > 1) %>% 
  group_by(Employee_ID, Camera_Serial_Number, Interval_Start, Interval_End) %>% 
  summarize(Footage_Duration_Within_Interval = sum(Seconds_Duration_Within_Interval)) %>% 
  ungroup()

#No camera is recording more than an hour of footage within a one hour interval
#Some staff appear to be using two cameras at the same time Some staff
#identified in the HR system aren't in the shift data let's append those to our
#follow-up list

Footage_by_Hour %>% 
  filter(Footage_Duration_Within_Interval > 3601)

#Looks like nobody recorded more than an hour of footage within a single hour of time.

Multiple_Cameras_Same_Time <- Footage_by_Hour %>% 
  count(Employee_ID, Interval_Start) %>% 
  filter(n>1) %>% 
  select(Employee_ID) %>% 
  unique() %>% 
  mutate(Reason = "Assigned to multiple cameras at the same time")

#22 Employee ID's were assigned to more than one camera at the same time.

Staff_Missing_in_Shift_Data <- HR_Relevant %>% 
  anti_join(Shift_Data, by = "Employee_ID") %>% 
  select(Employee_ID) %>% 
  mutate(Reason = "Missing in Shift Data")

#120 Employee ID's were missing in the shift data
#These 142 ID's will also need follow-up

Followup2 <- Multiple_Cameras_Same_Time %>% 
  bind_rows(Staff_Missing_in_Shift_Data)

write_csv(Followup2, "Output/ID's needing follow-up 2.csv")

#Now to format shift data and compare

Shifts_by_Hour <- Shift_Data %>% 
  Interval_Convert(Shift_Start, Shift_End, "hour") %>% 
  filter(Seconds_Duration_Within_Interval > 1) %>%
  group_by(Employee_ID, Interval_Start, Interval_End, Shift_Start, Shift_End) %>% 
  summarize(Shift_Duration_Within_Interval = sum(Seconds_Duration_Within_Interval)) %>% 
  ungroup()

Combined_by_Hour <- Footage_by_Hour %>% 
  full_join(Shifts_by_Hour) %>% 
  inner_join(HR_Relevant)

Combined_by_Shift <- Combined_by_Hour %>% 
  filter(!is.na(Shift_Start)) %>% 
  group_by(Employee_ID, HR_Location, Shift_Start) %>% 
  summarize(Shift_Duration = sum(Shift_Duration_Within_Interval),
            Footage_Within_Shift = sum(Footage_Duration_Within_Interval, na.rm = TRUE),
            Percent_of_Shift_With_Footage = Footage_Within_Shift / Shift_Duration) %>% 
  ungroup()

#How much footage got recorded with no corresponding shift timekeeping?

Footage_no_Shift <- Combined_by_Hour %>% 
  group_by(HR_Location) %>% 
  filter(is.na(Shift_Duration_Within_Interval)) %>% 
  summarize(Footage_Hours_No_Shift = floor(sum(Footage_Duration_Within_Interval)/3600),
            Employee_IDs_With_Missing_Shift = n_distinct(Employee_ID))

#Yowza

#How many shift hours got worked with no footage?

Shift_no_Footage <- Combined_by_Hour %>% 
  group_by(HR_Location) %>% 
  filter(is.na(Footage_Duration_Within_Interval)) %>% 
  summarize(Shift_Hours_No_Footage = floor(sum(Shift_Duration_Within_Interval)/3600),
            Employee_IDs_With_Missing_Footage = n_distinct(Employee_ID))

#Oh dear

write_csv(Footage_no_Shift %>% 
            inner_join(Shift_no_Footage),
          "Output/Footage-Shift Mismatch Summary Statistics.csv")

#How much overlap did we actually have?

Facility_Camera_Metrics <- Combined_by_Hour %>% 
  group_by(Employee_ID, Interval_Start) %>% 
  mutate(Has_Footage = !is.na(Footage_Duration_Within_Interval),
         Has_Shift = !is.na(Shift_Duration_Within_Interval)) %>% 
  group_by(HR_Location, Has_Footage, Has_Shift) %>% 
  summarize(Footage_Hours = sum(Footage_Duration_Within_Interval)/3600,
            Shift_Hours = sum(Shift_Duration_Within_Interval)/3600,
            Employee_IDs = n_distinct(Employee_ID)) %>% 
  group_by(HR_Location) %>% 
  mutate(Percent_of_Total_Footage_Hours = Footage_Hours/sum(Footage_Hours, na.rm = TRUE),
         Percent_of_Total_Shift_Hours = Shift_Hours/sum(Shift_Hours, na.rm = TRUE))

write_csv(Facility_Camera_Metrics, "Output/Facility-Camera Metrics.csv")

Facility_Camera_Metrics %>% 
  filter(Has_Footage) %>% 
  ggplot(aes(x = HR_Location, y = Percent_of_Total_Footage_Hours, fill = Has_Shift)) +
  geom_col()

#19% (Alpha, Delta) to 42% (Beta) of footage hours had no associated shift records

Facility_Camera_Metrics %>% 
  filter(Has_Shift) %>% 
  ggplot(aes(x = HR_Location, y = Percent_of_Total_Shift_Hours, fill = Has_Footage)) +
  geom_col()

#But 43% (Alpha, Epsilon) to 64% (Delta) of shift hours had no associated footage.
#Bear in mind this is only for staff that appeared in both data sets, the actual
#proportion missing is higher.

#There's a pretty serious mismatch of data here. Let's look at a different unit
#of analysis - employee-shifts. 

Combined_by_Shift %>% 
  ggplot(aes(x = Percent_of_Shift_With_Footage)) +
  geom_histogram()

#Looks like an enormous number of shifts had no
#camera footage whatsoever. What happens when we filter those out?

Combined_by_Shift %>% 
  filter(Percent_of_Shift_With_Footage > 0) %>% 
  ggplot(aes(x = Percent_of_Shift_With_Footage)) +
  geom_histogram()

#That certainly looks better!
#When we break it out by facility, we can see Delta is once again an outlier

Facility_Shift_Metrics <- Combined_by_Shift %>% 
  filter(Percent_of_Shift_With_Footage > 0) %>% 
  group_by(HR_Location) %>% 
  summarize(Footage_Hours = sum(Footage_Within_Shift)/3600,
            Shift_Hours = sum(Shift_Duration)/3600,
            Employee_IDs = n_distinct(Employee_ID),
            Percent_of_Total_Shift_Hours_Recorded = Footage_Hours/Shift_Hours)

write_csv(Facility_Shift_Metrics, "Output/Facility-Shift Metrics.csv")

Facility_Shift_Metrics %>% 
  ggplot(aes(x = HR_Location, y = Percent_of_Total_Shift_Hours_Recorded)) +
  geom_col() +
  ylim(0, 1)


#Seems like for hours that contained both shifts and clips, employees recorded
#the majority of the shift. The main problem appears to be the mismatch between
#the data sets.

Compare_Employee_by_Hour <- Combined_by_Hour %>% 
  group_by(Employee_ID, HR_Location) %>% 
  summarize(Hours_Footage_Total = sum(Footage_Duration_Within_Interval, na.rm = TRUE)/3600,
            Hours_Footage_No_Shift = sum(is.na(Footage_Duration_Within_Interval)),
            Hours_Shift_Total = sum(Shift_Duration_Within_Interval, na.rm = TRUE)/3600,
            Hours_Shift_No_Footage = sum(is.na(Shift_Duration_Within_Interval))) %>% 
  mutate(Missing_Shift_Data = Hours_Shift_Total == 0,
         Missing_Camera_Data = Hours_Footage_Total == 0)

Missing_IDs <- Compare_Employee_by_Hour %>% 
  filter(Missing_Shift_Data | Missing_Camera_Data) %>% 
  ungroup()

Missing_IDs %>% 
  summarize(Employees_Missing_Shift_Data = sum(Missing_Shift_Data),
            Employees_Missing_Camera_Data = sum(Missing_Camera_Data))

# On the level of individual staff, it looks like
# 58 had footage associated with their ID but no shift data
# 87 had shifts associated with their ID but no footage data

Compare_Employee_by_Shift <- Combined_by_Shift %>% 
  group_by(Employee_ID, HR_Location) %>% 
  summarize(Zero_Footage_Shifts = sum(Percent_of_Shift_With_Footage == 0),
            Average_Percent_of_Other_Shifts_Recorded = mean(Percent_of_Shift_With_Footage[Percent_of_Shift_With_Footage>0]))

Comparison_Metrics <- Compare_Employee_by_Hour %>% 
  full_join(Compare_Employee_by_Shift)

Comparison_Metrics %>% 
  anti_join(Missing_IDs) %>% 
  select(Employee_ID, Hours_Footage_No_Shift, Hours_Shift_No_Footage, Zero_Footage_Shifts, Average_Percent_of_Other_Shifts_Recorded) %>% 
  pivot_longer(-Employee_ID, names_to = "Metric", values_to = "Value") %>% 
  ggplot(aes(Value)) +
  geom_histogram() +
  facet_wrap(~Metric, scales = "free")

# Of those that appear in both data sets, it does look like
# a good faith effort to abide by the policy was largely followed
# For intervals with both shift data and footage data, most
# employees recorded 3/4 or more of the interval

Footage_Metrics_by_Employee <- read_csv("Output/Footage Metrics by Employee.csv")

All_Employee_Metrics <- Comparison_Metrics %>% 
  full_join(Footage_Metrics_by_Employee)

write_csv(All_Employee_Metrics, "Output/All Employee Metrics.csv")

#Serious systemic problems, not much to conclude about
#individual users (besides a handful of obvious outliers)
#Lesson: be aware of your assumptions!