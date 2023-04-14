#Full Analysis
#Each code segment is self-contained and can be run independently

#Required_Packages <- c("tidyverse", "readxl", "sqldf", "lubridate", "skimr")
#install.packages(Required_Packages)

#Import and format data, establish parameters, and run first quality control checks
source("R/1.) Initial QC.R")

#Generate footage metrics used in Quarto presentation
source("R/2.) Footage Metrics.R")

#Additional metrics comparing footage data with shift data, plus necessary quality control checks
source("R/3.) Shift and Footage Comparison Metrics.R")