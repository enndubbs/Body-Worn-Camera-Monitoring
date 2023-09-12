library(sqldf)
library(lubridate)

Interval_Convert <- function(DF, Start_Col, End_Col, Int_Unit, Int_Length = 1) {
    
    Start_Col2 <- enquo(Start_Col)
    End_Col2 <- enquo(End_Col)
    
    Start_End <- Sample_Shifts %>%
      ungroup() %>%
      summarize(Min_Start = min(!!Start_Col2),
                Max_End = max(!!End_Col2)) %>%
      mutate(Start = floor_date(Min_Start, Int_Unit),
             End = ceiling_date(Max_End, Int_Unit))
    
    Sample_Shifts <- Sample_Shifts %>%
      mutate(Single = !!Start_Col2 == !!End_Col2)
    
    Interval_Table <- data.frame(Interval_Start = seq.POSIXt(Start_End$Start[1], Start_End$End[1], by = str_c(Int_Length, " ", Int_Unit))) %>%
      mutate(Interval_End = lead(Interval_Start)) %>%
      filter(!is.na(Interval_End))
    
    by <- join_by(Interval_Start <= !!End_Col2, Interval_End >= !!Start_Col2)  
    
    Interval_Data_Table <- Interval_Table %>% 
      left_join(Sample_Shifts, by) %>% 
      mutate(Seconds_Duration_Within_Interval = if_else(!!End_Col2 > Interval_End, Interval_End, !!End_Col2) -
               if_else(!!Start_Col2 < Interval_Start, Interval_Start, !!Start_Col2)) %>%
      filter(!(Single & Interval_End == !!Start_Col2),
             as.numeric(Seconds_Duration_Within_Interval) > 0)
    
    return(Interval_Data_Table)
}
