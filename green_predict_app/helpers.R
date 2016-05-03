get.title <- function(parameter){
  titles <- list('Temperature' = 'Teplota (stupne celzia)',
                 'GHI' = 'GHO (W/m2)',
                 'Cloudiness' = 'Oblacnost (%)',
                 'Wind.direction' = 'Smer vetra (stupne)',
                 'Relative.humidity' = 'Relativna vlhkost (%)',
                 'Atmospheric.pressure' = 'Atmosfericky tlak (hPa)',
                 'Time' = 'Cas')
  return(titles[[parameter]])
}

