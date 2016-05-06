library(dplyr)

weather.import <- function(path){
  file.names <- list.files(path = path, full.names = T)
  df.list <- lapply(file.names, read.csv, header = F, sep = ",", dec =".", stringsAsFactors = F)
  df <- bind_rows(df.list)
  df <- select(df, 1:9) #odstrani nepotrebny stlpec
  colnames(df) <- c("Date","Time","Temperature","Wind.speed", "Wind.direction", "Cloudiness", "Relative.humidity", "Atmospheric.pressure", "GHI")
  df <- mutate(df, Time = paste(df$Date, df$Time))
  df <- select(df, -Date)
  df <- mutate(df, Time = as.POSIXct(Time, format = "%d-%m-%Y %H:%M", tz="GMT"))
  df <- arrange(df, Time)
  return (df)
}