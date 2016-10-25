library(dplyr)
library(insol)

change.timezone <- function(df,tz){
  attributes(df$Time)$tzone <- tz
  df <- arrange(df, Time)
  df <- distinct(df, Time) #delete repeated timestamp values due to CET/CEST change
  return (df)
}

add.missing.timestamps.hour <- function(pv.df){
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

pv.weather.merge.hour <- function(pv.df,weather.df){
  pv.df <- change.timezone(pv.df,"GMT")
  pv.df <- filter(pv.df, format(Time, format = "%M", tz="GMT") == "30")
  pv.df <- add.missing.timestamps(pv.df)
  pv.df <- compute.hour.energy.generation(pv.df)
  pv.df$Energy_kWh[pv.df$Energy_kWh < 0] <- NA
  
  start.time <- if(pv.df$Time[1] > weather.df$Time[1]) 
    pv.df$Time[1] 
  else 
    weather.df$Time[1]
  
  end.time <- if(tail(pv.df$Time,1) < tail(weather.df$Time,1)) 
    tail(pv.df$Time,1) 
  else 
    tail(weather.df$Time, 1) 
  
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

compute.average.3hour.weather <- function(pv.df, weather.parameters){
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

add.day.length <- function(pv.df, latitude, longitude){
  day.length <- daylength(latitude,longitude,JD(pv.df$Time),0)[,3]
  pv.df <- mutate(pv.df, day_length = day.length)
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

add.missing.timestamps.day <- function(pv.df){
  prev.Time <- pv.df$Time[1]
  size <- dim(pv.df)[1]
  one.day <- 1
  for(i in 2:size){
    while(prev.Time + one.day != pv.df$Time[i]){
      new.row <- list(prev.Time + one.day , NA, NA)
      pv.df <- rbind(pv.df,new.row)
      prev.Time <- prev.Time + one.day
    }
    prev.Time <- pv.df$Time[i]
  }
  pv.df <- arrange(pv.df, Time)
  return (pv.df)
}

compute.day.energy.generation <- function(pv.df){
  pv.df <- filter(pv.df, format(Time, format = "%H:%M", tz="GMT") == "00:00")
  pv.df <- mutate(pv.df, Time = as.Date(Time))
  pv.df <- add.missing.timestamps.day(pv.df)
  
  prev.row <- pv.df[1,]
  size <- dim(pv.df)[1]
  one.day <- 1
  new.pv.df <- select(pv.df, Time, Energy_kWh)[0,]
  
  for(i in 2:size){
    en.generated <- pv.df$Energy_kWh[i] - prev.row$Energy_kWh
    new.row <- list(pv.df$Time[i-1],en.generated)
    new.pv.df[i-1, ] <- new.row
    prev.row <- pv.df[i,]
  }
  return (new.pv.df)
}

compute.average.day.weather <- function(weather.df){
  weather.df <- mutate(weather.df, Time = as.Date(Time))
  weather.df <- rename(weather.df, Time = Time)
  weather.df <- group_by(weather.df, Time) %>%
    summarise(Temperature = mean(Temperature), Wind.speed = mean(Wind.speed),
              Wind.direction = mean(Wind.direction), Cloudiness = mean(Cloudiness),
              Relative.humidity = mean(Relative.humidity), 
              Atmospheric.pressure = mean(Atmospheric.pressure), GHI = mean(GHI))
  return (weather.df)
}

pv.weather.merge.day <- function(pv.df,weather.df){
  pv.df <- change.timezone(pv.df,"GMT")
  pv.df <- compute.day.energy.generation(pv.df)
  
  weather.df <- compute.average.day.weather(weather.df)
  pv.df$Energy_kWh[pv.df$Energy_kWh < 0] <- NA
  
  start.Time <- if(pv.df$Time[1] > weather.df$Time[1]) 
    pv.df$Time[1] 
  else 
    weather.df$Time[1]
  
  end.Time <- if(tail(pv.df$Time,1) < tail(weather.df$Time,1)) 
    tail(pv.df$Time,1) 
  else 
    tail(weather.df$Time, 1) 
  
  weather.df <- filter(weather.df, Time >= start.Time, Time <= end.Time )
  pv.df <- filter(pv.df, Time >= start.Time, Time <= end.Time)
  merged.df <- merge(pv.df, weather.df)
  return (merged.df)
}

add.yesterday.generation <- function(pv.df){
  size <- dim(pv.df)[1]
  yesterday <- pv.df$Energy_kWh[-size]
  pv.df <- pv.df[-1,]
  pv.df <- mutate(pv.df, Energy_kWh_yesterday = yesterday)
  return(pv.df)
}

export.data.sets <-function(){
  save(list = c("pv1.hour", "pv1.day", "pv2.hour", "pv2.day", "pv3.hour", "pv3.day", "pv3.hour.no.night", "pv2.hour.no.night", "pv1.hour.no.night"),
       file = "../data_sets.RData")
}