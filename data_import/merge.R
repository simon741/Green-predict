library(dplyr)
library(insol)
change.timezone <- function(df,tz){
  attributes(df$Time)$tzone <- tz
  df <- arrange(df, Time)
  df <- distinct(df, Time) #delete repeated timestamp values due to CET/CEST change
  return (df)
}

add.missing.timestamps <- function(pv.df){
  prev.time <- pv.df$Time[1]
  size <- dim(pv.df)[1]
  hour <- 3600
  for(i in 2:size){
    while(prev.time + hour != pv.df$Time[i]){
      new.row <- list(prev.time + hour , NA, NA)
      pv.df <- rbind(pv.df,new.row)
      prev.time <- prev.time + hour
    }
    prev.time <- pv.df$Time[i]
  }
  pv.df <- arrange(pv.df, Time)
  return (pv.df)
}

compute.hour.energy.generation <- function(pv.df){
  prev.row <- pv.df[1,]
  size <- dim(pv.df)[1]
  halfhour <- 1800
  new.pv.df <- pv.df[0,]
  for(i in 2:size){
      en.generated <- pv.df$Energy_kWh[i] - prev.row$Energy_kWh
      mean.power <- mean(c(pv.df$Power_kW[i],prev.row$Power_kW))
      new.row <- list(pv.df$Time[i] - halfhour, mean.power, en.generated)
      new.pv.df[i-1, ] <- new.row
      prev.row <- pv.df[i,]
  }
  return (new.pv.df)
}

pv.weather.merge <- function(pv.df,weather.df){
  pv.df <- change.timezone(pv.df,"GMT")
  pv.df <- filter(pv.df, format(Time, format = "%M", tz="GMT") == "30")
  pv.df <- add.missing.timestamps(pv.df)
  pv.df <- compute.hour.energy.generation(pv.df)
  pv.df$Energy_kWh[pv.df$Energy_kWh < 0] <- NA
  start.time <- if(pv.df$Time[1] > weather.df$Time[1]) pv.df$Time[1] else weather.df$Time[1]
  end.time <- if(tail(pv.df$Time,1) < tail(weather.df$Time,1)) tail(pv.df$Time,1) else tail(weather.df$Time, 1) 
  weather.df <- filter(weather.df, Time >= start.time, Time <= end.time )
  pv.df <- filter(pv.df, Time >= start.time, Time <= end.time)
  merged.df <- merge(pv.df, weather.df)
  return (merged.df)
}

add.next.prev.weather.parametres <- function(pv.df, weather.parameters){
  size <- dim(pv.df)[1]
  new.pv.df <- pv.df[-c(1,size-1),]
  for(weather.parameter in weather.parameters){
    next.values <- pv.df[-c(1,2), weather.parameter]
    prev.values <- pv.df[-c(size-2,size-1), weather.parameter]
    next.name <- paste0("next_", weather.parameter)
    prev.name <- paste0("prev_", weather.parameter)
    new.pv.df[,next.name] <- next.values
    new.pv.df[,prev.name] <- prev.values
  }
  return(new.pv.df)
}

mean.weather.parametres <- function(pv.df, weather.parameters){
  for(current.parameter in weather.parameters){
    next.parameter <- paste0("next_", current.parameter)
    prev.parameter <- paste0("prev_", current.parameter)
    pv.df[,current.parameter] <- (pv.df[,current.parameter] + pv.df[,next.parameter] + pv.df[,prev.parameter])/3
  }
  return(pv.df)
}

add.yesterday.hour.generation <- function(pv.df){
  day <- 24
  size <- dim(pv.df)[1]
  yesterday <- pv.df$Energy_kWh[-((size-day+1) : size)]
  pv.df <- pv.df[-(1:day),]
  pv.df <- mutate(pv.df, Energy_kWh_yesterday = yesterday)
  return(pv.df)
}

add.sun.position <- function(pv.df, latitude, longitude){
  sun.pos <- sunpos(sunvector(JD(pv.df$Time),latitude, longitude,0))
  pv.df <- mutate(pv.df, sun_azimuth = sun.pos[,1], sun_zenith = sun.pos[,2])
  return(pv.df)
}

add.daynight.flag <- function(pv.df, latitude, longitude){
  day <- daylength(latitude, longitude, JD(pv.df$Time) ,0)
  sunrise <- day[,1]
  sunset <- day[,2]
  hours <- as.numeric(format(pv.df$Time, format = "%H"))
  daynight <- ifelse(hours < sunrise | hours > sunset,"night","day")
  pv.df <- mutate(pv.df, daynight = as.factor(daynight))
  return(pv.df)
}

export.data.sets <-function(){
  save(list = c("pv1.hour", "pv1.day", "pv2.hour", "pv2.day", "pv3.hour", "pv3.day"),file = "./green_predict_app/data_sets.RData")
}

#----------------Client Code-----------------------
pv1.1.hour.df <- pv.weather.merge (pv1.1.df, weather1.df)
pv1.2.hour.df  <- pv.weather.merge (pv1.2.df, weather1.df)
pv2.hour.df  <- pv.weather.merge (pv2.df, weather2.df)

weather.parameters <- c("GHI", "Temperature", "Cloudiness", "Relative.humidity")
pv1.1.hour.df <- add.next.prev.weather.parametres(pv1.1.hour.df, weather.parameters)
pv1.2.hour.df <- add.next.prev.weather.parametres(pv1.2.hour.df, weather.parameters)
pv2.hour.df <- add.next.prev.weather.parametres(pv2.hour.df, weather.parameters)

pv1.1.hour.df <- mean.weather.parametres(pv1.1.hour.df, weather.parameters)
pv1.2.hour.df <- mean.weather.parametres(pv1.2.hour.df, weather.parameters)
pv2.hour.df <- mean.weather.parametres(pv2.hour.df, weather.parameters)

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


#-----------------------------------------------------------------------------

pv.data.error.rate <- function(pv.df){
  error.rate <- dim(pv.df %>% filter(is.na(Energy_kWh)|is.na(Power_kW)))[1] / dim(pv.df)[1] * 100
  print(error.rate)
}

pv.data.error.rate(df1)
pv.data.error.rate(df2)
pv.data.error.rate(df3)
