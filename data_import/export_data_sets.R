library(dplyr)
library(ggvis)
library(insol)

#----------------Client Code-----------------------

#nejako to este oddelit??????????????

weather1.df <- weather.import("../original_data/pocasie/oblast1")
weather2.df <- weather.import("../original_data/pocasie/oblast2")

pv1.1.df <- pv.data.import("../original_data/FVE/FVE1.1")
pv1.2.df <- pv.data.import("../original_data/FVE/FVE1.2")
pv2.df <- pv.data.import("../original_data/FVE/FVE2")

#------------------pv and weather merge (hour observations)------------------

pv1.1.hour.df <- pv.weather.merge.hour(pv1.1.df, weather1.df)
pv1.2.hour.df  <- pv.weather.merge.hour(pv1.2.df, weather1.df)
pv2.hour.df  <- pv.weather.merge.hour(pv2.df, weather2.df)

weather.parameters <- c("GHI", "Temperature")
pv1.1.hour.df <- add.next.prev.weather.parametres(pv1.1.hour.df, weather.parameters)
pv1.2.hour.df <- add.next.prev.weather.parametres(pv1.2.hour.df, weather.parameters)
pv2.hour.df <- add.next.prev.weather.parametres(pv2.hour.df, weather.parameters)

#position of pv must be annonymized
pv1.1.hour.df <- add.daynight.flag(pv1.1.hour.df,0, 0)
pv1.2.hour.df <- add.daynight.flag(pv1.2.hour.df,0, 0)
pv2.hour.df <- add.daynight.flag(pv2.hour.df,0, 0)

pv1.1.hour.df <- add.sun.position (pv1.1.hour.df,0, 0)
pv1.2.hour.df <- add.sun.position (pv1.2.hour.df,0, 0)
pv2.hour.df <- add.sun.position (pv2.hour.df,0, 0)

pv1.1.hour.df <- add.day.length(pv1.1.hour.df, 0, 0)
pv1.2.hour.df <- add.day.length(pv1.2.hour.df, 0, 0)
pv2.hour.df <- add.day.length(pv2.hour.df, 0, 0)

pv1.1.hour.df <- compute.average.3hour.weather(pv1.1.hour.df, weather.parameters)
pv1.2.hour.df <- compute.average.3hour.weather(pv1.2.hour.df, weather.parameters)
pv2.hour.df <- compute.average.3hour.weather(pv2.hour.df, weather.parameters)

pv1.1.hour.df <- add.yesterday.hour.generation(pv1.1.hour.df)
pv1.2.hour.df <- add.yesterday.hour.generation(pv1.2.hour.df)
pv2.hour.df <- add.yesterday.hour.generation(pv2.hour.df)

pv1.hour <- filter(pv1.1.hour.df, !is.na(Energy_kWh), !is.na(Energy_kWh_yesterday))
pv2.hour <- filter(pv1.2.hour.df, !is.na(Energy_kWh), !is.na(Energy_kWh_yesterday))
pv3.hour <- filter(pv2.hour.df, !is.na(Energy_kWh), !is.na(Energy_kWh_yesterday))

pv3.hour.no.night <- filter(pv3.hour, daynight != "night")
pv2.hour.no.night <- filter(pv2.hour, daynight != "night")
pv1.hour.no.night <- filter(pv1.hour, daynight != "night")

pv1.hour$daynight <- ifelse(pv1.hour$daynight == "night", F, T)
pv2.hour$daynight <- ifelse(pv2.hour$daynight == "night", F, T)
pv3.hour$daynight <- ifelse(pv3.hour$daynight == "night", F, T)

#------------------pv and weather merge (agregated day observations)------------------
pv1.1.day.df <- pv.weather.merge.day(pv1.1.df, weather1.df)
pv1.2.day.df  <- pv.weather.merge.day(pv1.2.df, weather1.df)
pv2.day.df  <- pv.weather.merge.day(pv2.df, weather2.df)

pv1.1.day.df <- add.yesterday.generation(pv1.1.day.df)
pv1.2.day.df <- add.yesterday.generation(pv1.2.day.df)
pv2.day.df <- add.yesterday.generation(pv2.day.df)

pv1.day <- filter(pv1.1.day.df, !is.na(Energy_kWh), !is.na(Energy_kWh_yesterday))
pv2.day <- filter(pv1.2.day.df, !is.na(Energy_kWh), !is.na(Energy_kWh_yesterday))
pv3.day <- filter(pv2.day.df, !is.na(Energy_kWh), !is.na(Energy_kWh_yesterday))

export.data.sets()


#----------------------------------------------------------------

pv.data.error.rate <- function(pv.df){
  error.rate <- dim(pv.df %>% filter(is.na(Energy_kWh)|is.na(Power_kW)))[1] / dim(pv.df)[1] * 100
  print(error.rate)
}

pv.data.error.rate(df1)
pv.data.error.rate(df2)
pv.data.error.rate(df3)