#Import and Format Data, Establish Parameters

library(tidyverse)
library(readxl)

Footage_Data <- read_excel("Data/Original Data/Footage Data.xlsx", col_types = "text") %>% 
  mutate(Duration_Hours = as.numeric(duration_seconds)/3600) %>% 
  rename(Footage_Job_Type = `Employee Role`)

Shift_Data <- read_excel("Data/Original Data/Shift Data.xlsx") %>% 
  mutate(`Employee ID` = as.character(`Employee ID`)) %>% 
  rename(Shift_Job_Type = `Job Type`, Shift_Location = Facility)

HR_Data <- read_csv("Data/Original Data/HR Data.csv", col_types = "c") %>% 
  mutate(across(contains("Date"), ~as.Date(.x, "%m/%d/%Y"))) %>% 
  rename(HR_Job_Type = `Job Type`, HR_Location = Location)

Secure_Facilities <- c("Alpha", "Beta", "Gamma", "Delta", "Epsilon")

#Underscore all variable names to make R coding easier

colnames(Footage_Data) <- str_replace_all(colnames(Footage_Data), " ", "_")
colnames(Shift_Data) <- str_replace_all(colnames(Shift_Data), " ", "_")
colnames(HR_Data) <- str_replace_all(colnames(HR_Data), " ", "_")

#Now that we have the data, let's start with an exploratory skim.

library(skimr)

#If you don't use skim on new data sets I don't know what you're doing.

skim(Footage_Data)
skim(HR_Data)
skim(Shift_Data)

#Looks like we have a few missing Employee ID's in footage data. Also, time
#fields in shift data have been cursed by Excel date formatting, will need to be
#exorcised later on.

#All ID's should appear in HR data. Check for ID's in footage and shift data
#that do not appear in HR data.

Footage_ID_Missing_in_HR <- Footage_Data %>% 
  select(Employee_ID) %>% 
  unique() %>% 
  na.omit() %>% 
  anti_join(HR_Data, by = "Employee_ID")

Shift_ID_Missing_in_HR <- Shift_Data %>% 
  select(Employee_ID) %>% 
  unique() %>% 
  na.omit() %>% 
  anti_join(HR_Data, by = "Employee_ID")

Invalid_IDs <- Footage_ID_Missing_in_HR %>% 
  bind_rows(Shift_ID_Missing_in_HR) %>% 
  mutate(Reason = "ID not in HR system")

#HR system is authoritative source, so these 44 ID's are errors.

#Let's see how much the data labels match up
#In other words, are the roles and locations consistent from one data set to another?

QC_Labels <- HR_Data %>% 
  left_join(Footage_Data %>% 
              select(Employee_ID, Footage_Job_Type) %>% 
              unique()) %>% 
  left_join(Shift_Data %>% 
              select(Employee_ID, Shift_Job_Type, Shift_Location) %>% 
              unique())

Label_Mismatch <- QC_Labels %>% 
  filter(Footage_Job_Type != HR_Job_Type |
           Footage_Job_Type != Shift_Job_Type |
           HR_Job_Type != Shift_Job_Type|
           HR_Location != Shift_Location) %>% 
  mutate(Reason = "Job Type or Location Label Mismatch")

#Role and location mismatch (77 ID's) minimal.
#Not a huge deal, might have just been mislabeled in shift or camera system

#So far so good, but this is the easy stuff.
#We expect to see somewhere in the footage data all Employee ID's that
#
#-are correctional (only correctional staff are required to wear cameras at all times on the dorm)
#-work at a secure facility (roll out to halfway houses was still ongoing)
#-were employed during the time frame of the study, 2019-04-01 through 2019-04-21
#-not in their first 3 months of employment, a mandatory training period prior to interacting with youth

HR_Relevant <- HR_Data %>% 
  filter(HR_Job_Type %in% c("Correctional", "Correctional Supervisor"),
         HR_Location %in% Secure_Facilities,
         is.na(Termination_Date) | Termination_Date >= "2019-04-01",
         Hire_Date <= "2019-01-21")

write_csv(HR_Relevant, "Data/Relevant HR Data.csv")

#HR data indicates 863 staff that meet this criteria. 
#How many are missing in the footage and shift data?

Missing_From_Footage_Data <- HR_Relevant %>% 
  anti_join(Footage_Data, by = "Employee_ID") %>% 
  mutate(Reason = "Missing from footage data")

Missing_From_Shift_Data <- HR_Relevant %>% 
  anti_join(Shift_Data, by = "Employee_ID") %>% 
  mutate(Reason = "Missing from shift data")

Expected_IDs_Missing <- Missing_From_Footage_Data %>% 
  bind_rows(Missing_From_Shift_Data)

#149 expected HR IDs missing in footage and 120 expected HR IDs missing from HR system. 
#269 IDs missing in total. That could definitely be better.

#What about staff whose IDs exist but were not employed during the time frame of the study?

Footage_Not_Employed <- Footage_Data %>% 
  anti_join(HR_Relevant, by = "Employee_ID") %>% 
  anti_join(Invalid_IDs) %>% 
  count(Employee_ID) %>% 
  inner_join(HR_Data) %>% 
  filter(Hire_Date > "2019-04-21"|
           Termination_Date < "2019-04-01") %>% 
  select(Employee_ID) %>% 
  unique() %>% 
  mutate(Reason = "Not employed at time of footage")

Shift_Not_Employed <- Shift_Data %>% 
  anti_join(HR_Relevant, by = "Employee_ID") %>% 
  anti_join(Invalid_IDs) %>% 
  count(Employee_ID) %>% 
  inner_join(HR_Data) %>% 
  filter(Hire_Date > "2019-04-21"|
           Termination_Date < "2019-04-01") %>% 
  select(Employee_ID) %>% 
  unique() %>% 
  mutate(Reason = "Not employed at time of shift")

Not_Employed <- Footage_Not_Employed %>% 
  bind_rows(Shift_Not_Employed)

#27 footage ID's and 13 Shift ID's appear in data but were assigned to staff not
#employed during the time frame.

#All of the problem ID's will be excluded from analysis and handed off to
#facility staff for follow-up. Label mismatches will be followed up on but not
#excluded from analysis.

Excluded_ID <- Invalid_IDs %>% 
  bind_rows(Expected_IDs_Missing) %>% 
  bind_rows(Not_Employed)

Followup_ID <- Excluded_ID %>% 
  bind_rows(Label_Mismatch)

write_csv(Followup_ID, "Output/ID's needing follow-up 1.csv")

#How much of the total quantity of footage needs to be excluded from the
#analysis for reasons of data quality?

Excluded_ID_Footage <- Footage_Data %>% 
  semi_join(Excluded_ID, by = "Employee_ID")

nrow(Excluded_ID_Footage)/nrow(Footage_Data)
sum(Excluded_ID_Footage$Duration_Hours)/sum(Footage_Data$Duration_Hours)

#11.3% of clips representing 10.6% of total footage excluded from analysis
#for data quality issues

#How much of the total quantity of footage is being excluded for any reason?
#Some footage will not be included because it does not fall under the
#"always-on" policy. For example, oversight employees (such as Monitoring or
#Inspector General staff) use cameras when interacting with youth but do not
#work full-time on the dorm. We would not expect to see their camera usage line
#up with their recorded shifts.

library(lubridate)

Analysis_Footage <- Footage_Data %>% 
  semi_join(HR_Relevant, by = "Employee_ID") %>% 
  arrange(Employee_ID, Camera_Serial_Number, created_date_record_start) %>% 
  mutate(Clip_Start = ymd_hms(created_date_record_start, tz = "US/Central"),
         Clip_End = ymd_hms(date_record_end, tz = "US/Central"))

1-nrow(Analysis_Footage)/nrow(Footage_Data)
1-sum(Analysis_Footage$Duration_Hours)/sum(Footage_Data$Duration_Hours)

#12.8% of clips representing 14.0% of total footage excluded from analysis
#for either data quality issues or employee not subject to always-on policy

#Remove irrelevant staff from Shift data and store copies of formatted tables

Shift_Format <- Shift_Data %>% 
  semi_join(HR_Relevant) %>%
  mutate(Shift_Start = ymd_hms(str_c(Start_Date, " ", str_sub(Start_Time, -8, -1)), tz = "US/Central"),
         Shift_End = ymd_hms(str_c(End_Date, " ", str_sub(End_Time, -8, -1)), tz = "US/Central"))

write_csv(Analysis_Footage, "Data/Analysis Footage.csv")
write_csv(Shift_Format, "Data/Shift Data Formatted.csv")

#Samples for reference in Quarto

write_csv(Analysis_Footage %>% 
            filter(Employee_ID == 9001005,
                   Clip_Start <= "2019-04-03") %>% 
            select(Employee_ID, Clip_ID, Clip_Start, Clip_End),
          "Output/Footage Sample.csv")

write_csv(Shift_Format %>% 
            filter(Employee_ID == 9001005,
                   Shift_Start <= "2019-04-03") %>% 
            select(Employee_ID, Shift_ID, Shift_Start, Shift_End),
          "Output/Shift Sample.csv")
