library(shiny)
library(ggvis)

shinyUI(navbarPage(
  title = span("Green Predict", style = "color: #2ecc71;
                                          font-size: 30px;
                                          margin-left: -15px;"),
  tabPanel("O projekte",
           fluidRow(
             column(8,offset = 2,
               h1("O projekte"),
               span(
                 p("Táto webová aplikácia slúži ako prezentácia výsledkov mojej bakalárskej práce.Úlohou práce
                   bolo vytvorenie predikčného modelu pre výrobu elektriny z fotovoltaickej elektrárne. Vstupom
                   pre model sú parametre predpovede počasia."),
                 
                 p("Aplikácia v súčasnosti ponúka vizualizáciu dát a predikciu na historických dátach. 
                   Predikciu je možné porovnať aj so skutčnými hodnotami. Uvedená funkcionalita beží na troch
                   data setoch (3 elektrárne), pričom každý data set má vlastný predikčný model."),
                 
                 style = "font-size: 17px;"
               )
             )
           )
  ),
  tabPanel("Vizualizácia dát",
    sidebarLayout(
      sidebarPanel(
        selectInput("vis.select.pv", label = h3("Dátová množina"), 
                    choices = list("FVE 1 (850 kW)" = "pv1", "FVE 2 (850 kW)" = "pv2", "FVE 3 (948 kW)" = "pv3")),
        
        radioButtons("time.range", label = h3("Časový interval"),
                     choices = list("1 hodina" = "hour", "1 deň (agregované)" = "day"), 
                     selected = "hour"),

        selectInput("select.parameter", label = h3("Parameter počasia"), 
                    choices = c('Teplota (stupne celzia)' = 'Temperature',
                                'GHO* (W/m2)' = 'GHI',
                                'Oblacnost (%)' = 'Cloudiness',
                                'Smer vetra (stupne)' = 'Wind.direction',
                                'Relativna vlhkost (%)' = 'Relative.humidity',
                                'Atmosfericky tlak (hPa)' = 'Atmospheric.pressure',
                                'Cas' = 'Time')),
        
        p("*GHO - globálne horizontálne ožiarenie"),
               
        checkboxGroupInput("layers", label = h3("Vrstvy grafu"),
                           choices = list("Body" = "points","LOESS model" = "loess"),
                            selected = c("points","loess"))
        ),
      
        mainPanel(
          ggvisOutput("data_ggvis")
        )
      )
  ),
  tabPanel("Predikcia",
    sidebarLayout(
      sidebarPanel(
        selectInput("prediction.select.pv", label = h3("Elektráreň"), 
                    choices = list("FVE 1 (850 kW)" = "pv1", "FVE 2 (850 kW)" = "pv2", "FVE 3 (948 kW)" = "pv3")),
        dateInput("prediction.date", label = h3("Predikovaný deň"),format = "dd.mm.yyyy",
                  language = "sk", weekstart = 1,
                  value = "2014-12-01"),
        checkboxGroupInput("prediction.layers", label = h3("Vrstvy grafu"),
                           choices = list("Predikcia" = "prediction","Skutočnosť" = "real"),
                           selected = "prediction")
      ),
      mainPanel(
        actionButton("refresh", "Obnoviť"),
        h1("Priebeh výroby elektriny počas dňa", align = "center"),
        ggvisOutput("day_hour_prediction_ggvis"),
        htmlOutput("predicted.day.generation")
      )
    )
  )
))
