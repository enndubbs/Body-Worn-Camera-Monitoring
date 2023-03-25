library(sqldf)
library(lubridate)

Interval_Convert <- function(DF, Start_Col, End_Col, Int_Unit, Int_Length = 1) {
  
  Start_Col2 <- enquo(Start_Col)
  End_Col2 <- enquo(End_Col)
  
  Start_Col_Text <- deparse(substitute(Start_Col))
  End_Col_Text <- deparse(substitute(End_Col))
  
  Start_End <- DF %>%
    ungroup() %>%
    summarize(Min_Start = min(!!Start_Col2),
              Max_End = max(!!End_Col2)) %>%
    mutate(Start = floor_date(Min_Start, Int_Unit),
           End = ceiling_date(Max_End, Int_Unit))
  
  DF <- DF %>%
    mutate(Start_Col_Numeric = as.numeric(!!Start_Col2),
           End_Col_Numeric = as.numeric(!!End_Col2),
           Single = Start_Col_Numeric == End_Col_Numeric)
  
  Interval_Table <- data.frame(Interval_Start = seq.POSIXt(Start_End$Start[1], Start_End$End[1], by = str_c(Int_Length, " ", Int_Unit))) %>%
    mutate(Interval_End = lead(Interval_Start),
           Interval_Start_Numeric = as.numeric(Interval_Start),
           Interval_End_Numeric = as.numeric(Interval_End)) %>%
    filter(is.na(Interval_End)==FALSE)
  
  Interval_Data_Table<-sqldf('SELECT * from Interval_Table
                            LEFT JOIN DF
                            ON (Start_Col_Numeric>=Interval_Start_Numeric AND End_Col_Numeric<=Interval_End_Numeric)
                            OR (Start_Col_Numeric<Interval_Start_Numeric AND End_Col_Numeric>Interval_Start_Numeric AND End_Col_Numeric<=Interval_End_Numeric)
                            OR (Start_Col_Numeric>=Interval_Start_Numeric AND Start_Col_Numeric<Interval_End_Numeric AND End_Col_Numeric>Interval_End_Numeric)
                            OR (Start_Col_Numeric<Interval_Start_Numeric AND End_Col_Numeric>Interval_End_Numeric)') %>%
    mutate(Seconds_Duration_Within_Interval = if_else(Start_Col_Numeric>=Interval_Start_Numeric & End_Col_Numeric<=Interval_End_Numeric, End_Col_Numeric - Start_Col_Numeric,
                                      if_else(Start_Col_Numeric<=Interval_Start_Numeric & End_Col_Numeric > Interval_Start_Numeric & End_Col_Numeric <= Interval_End_Numeric, End_Col_Numeric - Interval_Start_Numeric,
                                              if_else(Start_Col_Numeric>=Interval_Start_Numeric & Start_Col_Numeric<Interval_End_Numeric & End_Col_Numeric>Interval_End_Numeric, Interval_End_Numeric - Start_Col_Numeric,
                                                      if_else(Start_Col_Numeric<Interval_Start_Numeric & End_Col_Numeric>Interval_End_Numeric, Interval_End_Numeric - Interval_Start_Numeric, Interval_End_Numeric - Interval_Start_Numeric)
                                              )))
    ) %>%
    filter(!(Single & Interval_End_Numeric == Start_Col_Numeric)) %>%
    select(-contains("Numeric"))
  
  return(Interval_Data_Table)
}
