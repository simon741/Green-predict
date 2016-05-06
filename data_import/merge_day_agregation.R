library(dplyr)
change.timezone <- function(df,tz){
  attributes(df$Time)$tzone <- tz
  df <- arrange(df, Time)
  df <- distinct(df, Time) #delete repeated timestamp values due to CET/CEST change
  return (df)
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
  
  start.Time <- if(pv.df$Time[1] > weather.df$Time[1]) pv.df$Time[1] else weather.df$Time[1]
  end.Time <- if(tail(pv.df$Time,1) < tail(weather.df$Time,1)) tail(pv.df$Time,1) else tail(weather.df$Time, 1) 
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