library(dplyr)

import <- function(path){
  file.names <- list.files(path = path, full.names = T)
  df.list <- lapply(file.names, read.csv, header = T, sep = ";", dec =",", stringsAsFactors = F, na.strings = "-")
  df <- dplyr::bind_rows(df.list)
  df <- rename(df,Time = Cas, Power_kW = P..1, Energy_kWh = A..1)
  df <- select(df, Time, Power_kW, Energy_kWh)
  df <- mutate(df, Time = as.POSIXct(Time, format = "%d.%m.%Y %H:%M", tz="Europe/Prague"))
  df <- arrange(df, Time)
  return (df)
}


#----------------Client Code-----------------------

pv1.1.df <- import("./data/FVE/FVE1.1")
pv1.2.df <- import("./data/FVE/FVE1.2")
pv2.df <- import("./data/FVE/FVE2")


