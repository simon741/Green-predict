library(dplyr)
library(ggvis)

pv.vis <- function(pv, parametres){
  for(parameter in parametres){
      formula <- as.formula(paste0("~",parameter))
      vis <- pv %>% ggvis(formula, ~Energy_kWh)%>% 
        layer_points( opacity := 0.4) %>% 
        layer_smooths(stroke := "red")
      print(vis)
  }
}

pv.vis(pv3.hour, weather.parameters)