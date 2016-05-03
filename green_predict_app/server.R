library(shiny)
library(ggvis)
library(RSNNS)

load("model_pv1.RData")
pv1.model <- model
pv1.targets.norm.params <- targets.norm.params
pv1.inputs.hour <- inputs.hour

load("model_pv2.RData")
pv2.model <- model 
pv2.targets.norm.params <- targets.norm.params
pv2.inputs.hour <- inputs.hour

load("model_pv3.RData")
pv3.model <- model 
pv3.targets.norm.params <- targets.norm.params
pv3.inputs.hour <- inputs.hour

load("data_sets.RData")

source("helpers.R")

shinyServer(function(input, output, session) {
  
  pv <- reactive({     
    selection <- eval(as.name(paste0(input$vis.select.pv, ".", input$time.range)))
    selection <- selection[,c(input$select.parameter, "Energy_kWh")]
    names(selection) <- c("x","y")
    return (selection)
  })
  
  plot.data <- reactive({
    layers <- input$layers
    if(length(layers) != 0){
      progress.part <- 1/(1 + length(layers))
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Vytváram graf", value = 0)
      progress$inc(progress.part, detail = "Načítavam dáta")
      plot <- ggvis(pv(), ~x, ~y) %>%
      add_axis("x", title = get.title(input$select.parameter),
               properties=axis_props(title=list(fontSize = 15))) %>%
      add_axis("y", title = "Energia (kWh)", title_offset = 45,
               properties=axis_props(title=list(fontSize = 15)))
      for(i in layers){
        if(i == "loess"){
          progress$inc(progress.part, detail = "Vytváram model LOESS")
          plot <- plot%>% layer_smooths(stroke := "red")
        } 
        if(i == "points"){
          progress$inc(progress.part, detail = "Vytváram body")
          plot <- plot %>% layer_points(opacity := 0.4)
        } 
      }
      plot <- set_options(plot, width = "auto")
    }
    else{
      NULL
    }
  })
  
  observe({
    plot <- plot.data()
    if(!is.null(plot)) bind_shiny(plot, "data_ggvis")
  })
  
  current.pv.day.set <- reactive({
    eval(as.name(paste0(input$prediction.select.pv, ".day")))
  })
  
  observe({
    updateDateInput(session, "prediction.date",
                    min = min(current.pv.day.set()$Time),
                    max = max(current.pv.day.set()$Time),
                    value = min(current.pv.day.set()$Time))
  })
  
  predicted.day.generation <- function(hour.predictions){   
    day.prediction <- sum(hour.predictions$Prediction)
    day.prediction <- round(day.prediction, 2)
    output$predicted.day.generation <- renderText({ 
      paste(h1("Elektráreň ", names(input$prediction.select.pv), "vyrobí ", strong(day.prediction),
               "kWh", align = "center"))
    })
  }
  
  predicted.hour.generation <- reactive({  
    pv.hour <- eval(as.name(paste0(input$prediction.select.pv,".hour")))
    model <- eval(as.name(paste0(input$prediction.select.pv,".model")))
    inputs.hour <- eval(as.name(paste0(input$prediction.select.pv,".inputs.hour")))
    targets.norm.params <- eval(as.name(paste0(input$prediction.select.pv,".targets.norm.params")))
    
    date <- input$prediction.date
    if(date < as.Date(min(inputs.hour$Time)))date <- as.Date(min(inputs.hour$Time))
    if(date > as.Date(max(inputs.hour$Time)))date <- as.Date(max(inputs.hour$iTime))

    inputs.hour <- inputs.hour[as.Date(inputs.hour$Time) == date,]
    
    real <- pv.hour[as.Date(pv.hour$Time) == date,]$Energy_kWh
    predictions <- predict(model, inputs.hour[,predictors])
    predictions <- denormalizeData(predictions, targets.norm.params)
    predictions[predictions < 0] <- 0
    times <- inputs.hour[as.Date(inputs.hour$Time) == date,]$Time
    
    result.df <- data.frame(id = 1:length(times), Time = times, Prediction = predictions, Real = real)

    predicted.day.generation(result.df)
    result.df
  })

  all_values <- function(x) {
    if(is.null(x$id)) return(NULL)
    predictions <- predicted.hour.generation()
    row = predictions[predictions$id == x$id, ]
    
    half.hour <- 1800
    from <- row$Time - half.hour
    to <- row$Time + half.hour
    text <- paste0("<strong>",format(from, format = "%H:%M",tz="GMT")," - ",format(to, format = "%H:%M",tz="GMT"),"<br />")
    if(!is.null(row$Prediction)){
      prediction <- round(row$Prediction, 2)
      text <- paste0(text,"Predikcia: ", prediction, " kWh <br />")
    }
    if(!is.null(row$Real)){
      real <- round(row$Real, 2)
      text <- paste0(text,"Skutočnosť: ", real, " kWh <br />")
    }
    text <- paste0(text,"</strong>")
  }
  
  plot.prediction <- reactive({  
    layers <- input$prediction.layers
    if(length(layers) != 0){
        predictions <- predicted.hour.generation()
        predictions$Time <- predictions$Time - 60*60 #odcita jednu hodinu, ggvis pridava hodinu, bug, nedalo sa inak
        plot <- predictions %>% 
        ggvis( ~Time, ~Prediction, key := ~id) %>% 
        scale_nominal("fill", range = c("black","red"))  %>%
        scale_nominal("stroke", range = c("black","red")) %>% #,domain[length(domain)])
        add_axis("x", title = "Čas(UTC)",
                 properties=axis_props(title=list(fontSize = 15))) %>%
        add_axis("y", title = "Energia (kWh)", title_offset = 45,
                 properties=axis_props(title=list(fontSize = 15))) %>%
        hide_legend(c("stroke", "fill"))
        if("prediction" %in% layers) plot <- plot %>% 
            layer_points(fill = "Predikcia" ) %>% 
            layer_lines(stroke = "Predikcia" )
        if("real" %in% layers) plot <- plot %>%
            layer_points( ~Time, ~Real, fill = "Skutočnosť") %>%
            layer_lines(~Time, ~Real, stroke = "Skutočnosť")
      plot <- add_legend(plot, "fill", properties = legend_props(labels = list(fontSize = 15))) %>%
              set_options(width = "auto") %>%
              add_tooltip(all_values, "hover")
    }
    else{
      NULL
    }
   })
  
  observe({
    plot <- plot.prediction()
    if(!is.null(plot)) bind_shiny(plot, "day_hour_prediction_ggvis")
  })
  
  observeEvent(input$refresh, {
    selected <- input$prediction.layers
    updateCheckboxGroupInput(session, "prediction.layers", 
                             selected = "prediction")
    updateCheckboxGroupInput(session, "prediction.layers",
                            selected = selected)
    plot <- plot.prediction()
    if(!is.null(plot)) bind_shiny(plot, "day_hour_prediction_ggvis")
  })
})
