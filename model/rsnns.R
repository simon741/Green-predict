library(RSNNS)
library(dplyr)
library(sirad)
library(cvTools)
library(ggvis)

day.error.vis <- function(result){
  result <- data.frame(result, Dif = abs(result$Target - result$Prediction))
  result <- mutate(result, Time = as.numeric(format(Time, format = "%m", tz="GMT")))
  
  result$Time[result$Time %in% c(12,1,2)] <- "Zima"
  result$Time[result$Time %in% c(3,4,5)] <- "Jar"
  result$Time[result$Time %in% c(6,7,8)] <- "Leto"
  result$Time[result$Time %in% c(9,10,11)] <- "Jesen"
  
  result$Time <- factor(result$Time, levels = c("Zima","Jesen","Jar","Leto"))
  
  print(result)
  vis <- result %>% ggvis(x = ~Prediction, y= ~Target) %>% 
    layer_points(size = ~Dif, fill = ~Time) %>% 
    layer_lines(~Target,~Target) %>%
    add_axis("x", title = "Predikovana hodnota energie (kWh)",
             title_offset = 45,
             properties=axis_props(title=list(fontSize = 15))) %>%
    add_axis("y", title = "Cielova hodnota energie (kWh)",
             title_offset = 45,
             properties=axis_props(title=list(fontSize = 15))) %>%
    add_legend("fill", title = "Rocne obdobie") %>%
    add_legend("size", title = "Chyba (kWh)", properties = legend_props(legend = list(y = 100)))
  print(vis)
}

rsns.crossval <- function(conf, inputs.hour, targets.hour, folds){
    
    print("***************Hour predictions*****************")
    validation.errors <- data.frame()
    train.errors <- data.frame()
    for(i in 1:folds$K){
      
    inputs.train <- inputs.hour[folds$which != i,conf$predictors]
    inputs.validation <- inputs.hour[folds$which == i,conf$predictors]
    targets.train <- targets.hour[folds$which != i,"Energy_kWh"]
    targets.validation <- targets.hour[folds$which == i,"Energy_kWh"]
    
    model <- mlp(inputs.train, targets.train, size = conf$size, maxit = conf$maxit,
                 initFunc = "Randomize_Weights",
                 learnFunc = conf$learnFunc,
                 updateFunc = "Topological_Order", updateFuncParams = c(0),
                 hiddenActFunc = "Act_Logistic", shufflePatterns = T,
                 linOut = T)
    
    train.predictions <- predict(model,inputs.train[,conf$predictors])
    train.predictions[train.predictions < 0] <- 0
    train.error <- modeval(train.predictions,targets.train,
                           stat=c("MAE","RMAE","RMSE","RRMSE"))
    train.errors <- rbind(train.errors, train.error)
    
    validation.predictions <- predict(model,inputs.validation[,conf$predictors])
    validation.predictions[validation.predictions < 0] <- 0
    validation.error <- modeval(validation.predictions,targets.validation,
                                stat=c("MAE","RMAE","RMSE","RRMSE"))
    validation.errors <- rbind(validation.errors, validation.error)
    
     print(paste("Crossvalidation iteration: ", i))
     print("Train set errors:")
     print(paste("MAE: ",train.error$MAE))
     print(paste("RMAE: ",train.error$RMAE,"%"))
     print(paste("RMSE: ",train.error$RMSE))
     print(paste("RRMSE: ",train.error$RRMSE,"%"))
     print("Validation set errors:")
     print(paste("MAE: ",validation.error$MAE))
     print(paste("RMAE: ",validation.error$RMAE,"%"))
     print(paste("RMSE: ",validation.error$RMSE))
     print(paste("RRMSE: ",validation.error$RRMSE,"%"))
     cat("\n")
  }
  print("**************************************")
  print("Final result:")
  print("Train set errors:")
  print(paste("MAE: ",mean(train.errors$MAE)))
  print(paste("RMAE: ",mean(train.errors$RMAE),"%"))
  print(paste("RMSE: ",mean(train.errors$RMSE)))
  print(paste("RRMSE: ",mean(train.errors$RRMSE),"%"))
  print("Validation set errors:")
  print(paste("MAE: ",mean(validation.errors$MAE)))
  print(paste("RMAE: ",mean(validation.errors$RMAE),"%"))
  print(paste("RMSE: ",mean(validation.errors$RMSE)))
  print(paste("RRMSE: ",mean(validation.errors$RRMSE),"%"))
  cat("\n")
}

rsns.crossval.sum.hours <- function(conf, inputs.hour, targets.hour, targets.day, folds, targets.norm.params, vis.errors){
  
  print("***************Day predictions*****************")
  
  validation.errors <- data.frame()
  train.errors <- data.frame()
  results <- data.frame()
  for(i in 1:folds$K){
    
    inputs.train <- inputs.hour[as.Date(inputs.hour$Time) %in% targets.day$Time[folds$which != i],conf$predictors]
    inputs.validation <- inputs.hour[as.Date(inputs.hour$Time) %in% targets.day$Time[folds$which == i],]
    targets.train <- targets.hour[as.Date(targets.hour$Time) %in% targets.day$Time[folds$which != i],"Energy_kWh"]
    targets.validation <- targets.day[folds$which == i,]
    
    model <- mlp(inputs.train, targets.train, size = conf$size, maxit = conf$maxit,
                 initFunc = "Randomize_Weights",
                 learnFunc = conf$learnFunc,
                 updateFunc = "Topological_Order", updateFuncParams = c(0),
                 hiddenActFunc = "Act_Logistic", shufflePatterns = T,
                 linOut = T)

    validation.predictions <- apply(targets.validation,1,sum.hour.predictions, model = model,inputs.test = inputs.hour, predictors = conf$predictors, targets.norm.params)
    validation.error <- modeval(validation.predictions,targets.validation$Energy_kWh,
                          stat=c("MAE","RMAE","RMSE","RRMSE"))
    validation.errors <- rbind(validation.errors, validation.error)
    result <- data.frame(Time = targets.validation$Time, Target = targets.validation$Energy_kWh, Prediction = validation.predictions)
    results <- rbind(result,results)
    
    if(vis.errors == T) day.error.vis(result)
    
    print(paste("Crossvalidation iteration: ", i))
    print("Validation set errors:")
    print(paste("MAE: ",validation.error$MAE))
    print(paste("RMAE: ",validation.error$RMAE,"%"))
    print(paste("RMSE: ",validation.error$RMSE))
    print(paste("RRMSE: ",validation.error$RRMSE,"%"))
    cat("\n")
    
  }
  print("**********************************************")
  print("Final result:")
  print("Validation set errors:")
  print(paste("MAE: ",mean(validation.errors$MAE)))
  print(paste("RMAE: ",mean(validation.errors$RMAE),"%"))
  print(paste("RMSE: ",mean(validation.errors$RMSE)))
  print(paste("RRMSE: ",mean(validation.errors$RRMSE),"%"))
  cat("\n")
  if(vis.errors == T) day.error.vis(results)
}

sum.hour.predictions <- function(target, model, inputs.test, predictors, targets.norm.params){
  day <- target[1]
  prediction <- predict(model,inputs.test[as.Date(inputs.test$Time) == day, predictors])
  prediction <- denormalizeData(prediction, targets.norm.params)
  prediction[prediction < 0] <- 0
  prediction <- sum(prediction)
  return (prediction)
}

normalize <- function(pv, predictors){
  inputs <- data.frame(pv[,predictors] %>%
                         normalizeData(type = "0_1"))
  colnames(inputs) <- predictors
  
  targets <- dplyr::select(pv, Energy_kWh)
  targets.norm.params <- getNormParameters(normalizeData(targets, type = "0_1"))
  targets <- data.frame(normalizeData(targets, type = "0_1"))
  colnames(targets) <- c("Energy_kWh")
  
  normalized.data <- list(inputs = inputs,
                          targets = targets,
                          targets.norm.params = targets.norm.params)
  return (normalized.data)
}

test.confs <- function(pv.hour, pv.day, num.folds, confs, output.to.file, day.error.vis){
  if(output.to.file == T){
    sink(file = "test_results.txt", append = TRUE, type = "output",
         split = FALSE)
  }
  
  for(conf in confs){
    normalized.data <- normalize(pv.hour, conf$predictors)
    targets.norm.params <- normalized.data$targets.norm.params
    inputs.hour <- normalized.data$inputs
    inputs.hour <- cbind(inputs.hour, Time = pv.hour$Time)
    targets.hour <- normalized.data$targets
    targets.hour <- cbind(targets.hour, Time = pv.hour$Time)
    targets.day <- pv.day[,c("Time", "Energy_kWh")]
    day.folds <- cvFolds(dim(targets.day)[1], K = num.folds, type = "random")
    hour.folds <- cvFolds(dim(inputs.hour)[1], K = num.folds, type = "random")
    
    
    print(deparse(substitute(pv.hour)))
    print(conf)
    
    rsns.crossval.sum.hours(conf, inputs.hour, targets.hour, targets.day, day.folds, targets.norm.params, day.error.vis)
    
    rsns.crossval(conf,inputs.hour, targets.hour, hour.folds)
  } 
  
  closeAllConnections()
}

test.day.mean.parametres <- function(pv.day, num.folds, conf, output.to.file, day.error.vis){
  if(output.to.file == T){
    sink(file = "test_results.txt", append = TRUE, type = "output",
         split = FALSE)
  }
  
  normalized.data <- normalize(pv.day, conf$predictors)
  targets.norm.params <- normalized.data$targets.norm.params
  inputs.day <- normalized.data$inputs
  targets.day <- normalized.data$targets
  folds <- cvFolds(dim(inputs.day)[1], K = num.folds, type = "random")
  
  print(deparse(substitute(pv.day)))
  print(conf)
  print("***************Day predictions*****************")
  model <- rsns.crossval(conf,inputs.day, targets.day, folds)
  
  closeAllConnections()
}

export.model <- function(pv.hour, pv.day, conf, pv.name){
  predictors <- conf$predictors
  print(conf)
  normalized.data <- normalize(pv.hour, predictors)
  targets.norm.params <- normalized.data$targets.norm.params
  inputs.hour <- normalized.data$inputs
  inputs.hour <- cbind(inputs.hour, Time = pv.hour$Time)
  targets.hour <- normalized.data$targets
  targets.day <- pv.day[,c("Time", "Energy_kWh")]
    
  model <- mlp(inputs.hour[,predictors], targets.hour$Energy_kWh, size = conf$size, maxit = conf$maxit,
                initFunc = "Randomize_Weights",
                learnFunc = conf$learnFunc,
                updateFunc = "Topological_Order", updateFuncParams = c(0),
                hiddenActFunc = "Act_Logistic", shufflePatterns = T,
                linOut = T)
  
  train.predictions.day <- apply(targets.day,1,sum.hour.predictions, model,inputs.test = inputs.hour, predictors = conf$predictors, targets.norm.params)
  train.error.day <- modeval(train.predictions.day,targets.day$Energy_kWh,
                              stat=c("MAE","RMAE","RMSE","RRMSE"))

  train.predictions.hour <- predict(model,inputs.hour[,predictors])
  train.predictions.hour[train.predictions.hour < 0] <- 0
  
  train.error.hour <- modeval(train.predictions.hour,targets.hour$Energy_kWh,
                           stat=c("MAE","RMAE","RMSE","RRMSE"))
    
  print("Exported model hour error: ")
  print(paste("MAE: ",train.error.hour$MAE))
  print(paste("RMAE: ",train.error.hour$RMAE,"%"))
  print(paste("RMSE: ",train.error.hour$RMSE))
  print(paste("RRMSE: ",train.error.hour$RRMSE,"%"))
  
  print("Exported model day error: ")
  print(paste("MAE: ",train.error.day$MAE))
  print(paste("RMAE: ",train.error.day$RMAE,"%"))
  print(paste("RMSE: ",train.error.day$RMSE))
  print(paste("RRMSE: ",train.error.day$RRMSE,"%"))
  
  
  save(list = c("model", "predictors", "targets.norm.params", "inputs.hour"),file = paste0("../model_", pv.name,".RData"))
}