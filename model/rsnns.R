library(RSNNS)
library(dplyr)
library(sirad)
library(cvTools)
library(ggvis)

day.error.vis <- function(result){
  result <- data.frame(result, Dif = abs(result$Target - result$Prediction))
  result <- mutate(result, order = seq(1,dim(result)[1],1))
  
  vis <- result %>% ggvis(x = ~Prediction, y= ~Target) %>% 
    layer_points(size = ~Dif, fill = ~Time) %>% 
    layer_lines(~Target,~Target) %>%
    add_axis("x", title = "Predikovana hodnota") %>%
    add_axis("y", title = "Cielova hodnota") %>%
    add_legend("fill", title = "Chyba")
  print(vis)
}


rsns.crossval <- function(conf, inputs.hour, targets.hour, folds){
  
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
                 hiddenActFunc = "Act_Logistic", shufflePatterns = conf$shufflePatterns,
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
     
  }
  print("**********************************************")
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
  return (model)
}

rsns.crossval.sum.hours <- function(conf, inputs.hour, targets.hour, targets.day, folds, targets.norm.params, vis.errors){
  
  validation.errors <- data.frame()
  train.errors <- data.frame()
  for(i in 1:folds$K){
    
    inputs.train <- inputs.hour[as.Date(inputs.hour$Time) %in% targets.day$Time[folds$which != i],conf$predictors]
    inputs.validation <- inputs.hour[as.Date(inputs.hour$Time) %in% targets.day$Time[folds$which == i],]
    targets.train <- targets.hour[as.Date(targets.hour$Time) %in% targets.day$Time[folds$which != i],"Energy_kWh"]
    targets.validation <- targets.day[folds$which == i,]
    
    model <- mlp(inputs.train, targets.train, size = conf$size, maxit = conf$maxit,
                 initFunc = "Randomize_Weights",
                 learnFunc = conf$learnFunc,
                 updateFunc = "Topological_Order", updateFuncParams = c(0),
                 hiddenActFunc = "Act_Logistic", shufflePatterns = conf$shufflePatterns,
                 linOut = T)

    validation.predictions <- apply(targets.validation,1,sum.hour.predictions, model = model,inputs.test = inputs.hour, predictors = conf$predictors, targets.norm.params)
    validation.error <- modeval(validation.predictions,targets.validation$Energy_kWh,
                          stat=c("MAE","RMAE","RMSE","RRMSE"))
    validation.errors <- rbind(validation.errors, validation.error)
    result <- data.frame(Time = targets.validation$Time, Target = targets.validation$Energy_kWh, Prediction = validation.predictions)
    
    if(vis.errors == T) error.vis(result)
    
    print(paste("Crossvalidation iteration: ", i))
    print("Validation set errors:")
    print(paste("MAE: ",validation.error$MAE))
    print(paste("RMAE: ",validation.error$RMAE,"%"))
    print(paste("RMSE: ",validation.error$RMSE))
    print(paste("RRMSE: ",validation.error$RRMSE,"%"))
    
  }
  print("**********************************************")
  print("Final day agregation result:")
  print(paste("MAE: ",mean(validation.errors$MAE)))
  print(paste("RMAE: ",mean(validation.errors$RMAE),"%"))
  print(paste("RMSE: ",mean(validation.errors$RMSE)))
  print(paste("RRMSE: ",mean(validation.errors$RRMSE),"%"))
  
  return (model)
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

rsns.crossval2 <- function(inputs.day, inputs.hour, targets.day, targets.hour, conf, predictors, num.folds){
  
  test.errors <- data.frame()
  train.errors <- data.frame()
  for(i in 1:num.folds){
    print(paste("Crossvalidation iteration: ", i))
    inputs.train <- inputs.hour[as.Date(inputs.hour$Time) %in% inputs.day$Time[folds$which != i],]
    inputs.test <- inputs.hour[as.Date(inputs.hour$Time) %in% inputs.day$Time[folds$which == i],]
    targets.train <- targets.hour[as.Date(inputs.hour$Time) %in% inputs.day$Time[folds$which != i],]
    targets.test.hour <- targets.day[as.Date(inputs.hour$Time) %in% inputs.day$Time[folds$which == i],]
    targets.test.day <- targets.day[folds$which == i,]

    
    inputs.train <- inputs.train[,predictors]
    targets.train <- targets.train[,"Energy_kWh"]
    targets.test.hour <- targets.test.hour[,"Energy_kWh"]
    
    model <- SnnsRObjectFactory()
    model$setLearnFunc('Rprop')
    model$setUpdateFunc('Topological_Order')
    model$setUnitDefaults(1,0,1,0,1,'Act_Logistic','Out_Identity')
    model$createNet(c(ncol(inputs.train),conf$size,1), TRUE)
    train.patset <- model$createPatSet(inputs.train, targets.train)
    validation.patset <- model$createPatSet(inputs.test[,predictors], targets.test.hour)
    
  
    model$shufflePatterns(conf$shufflePatterns)
    model$initializeNet(conf$initFuncParams,"Randomize_Weights")
    
    model$saveNet(paste(basePath,"neuronka.net",sep=""),
                       "neuronka.net")
    for(j in 1:50){
      model$setCurrPatSet(train.patset$set_no)
      model$DefTrainSubPat()
      for(k in 1:300) {
        res <- model$learnAllPatterns(conf$learnFuncParams)
      }
      print(paste("Epoch number: ",j*300))
      print("Train set errors:")
      print(res[[2]])
      
      model$setCurrPatSet(validation.patset$set_no)
      model$DefTrainSubPat()
      res <- model$testAllPatterns(c(0))
      #validation.predictions <- model$predictCurrPatSet("output", c(0))
     # validation.error <- modeval(validation.predictions,targets.test.hour,
      #                           stat=c("MAE","RMAE","RMSE","RRMSE"))
    
      print("Validation set errors:")
      print(res[[2]])
     # print(paste("RMSE: ",validation.error$RMSE))

      print("------------------------------------------------")
    }


#     
#     test.predictions <- apply(targets.test.day,1,sum.hour.predictions, model = model,inputs.test = inputs.test, predictors = predictors)
#     test.predictions <- normalizeData(test.predictions, type = "0_1")
#     test.error <- modeval(test.predictions,targets.test.day$Energy_kWh,
#                           stat=c("MAE","RMAE","RMSE","RRMSE"))
#     test.errors <- rbind(test.errors, test.error)
    


    
    #toto pojde asi prec az nakoniec sa to bude pocitat na testovacej mnozine
#     print("Day agregation test set errors:")
#     print(paste("MAE: ",test.error$MAE))
#     print(paste("RMSE: ",test.error$RMSE))
#     print(paste("RMAE: ",test.error$RMAE,"%"))
#     print(paste("RRMSE: ",test.error$RRMSE,"%"))
#     print("------------------------------------------------")
  }
  print("Final result:")
  print(paste("MAE: ",mean(test.errors$MAE)))
  print(paste("RMAE: ",mean(test.errors$RMAE),"%"))
  print(paste("RMSE: ",mean(test.errors$RMSE)))
  print(paste("RRMSE: ",mean(test.errors$RRMSE),"%"))
}


test.confs <- function(pv.hour, pv.day, num.folds, confs, output.to.file, day.error.vis){
  if(output.to.file == T){
    sink(file = "final_test.txt", append = TRUE, type = "output",
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
    print("***************DAY*****************")
    
    model <- rsns.crossval.sum.hours(conf, inputs.hour, targets.hour, targets.day, day.folds, targets.norm.params, day.error.vis)
    
    print("***************HOUR*****************")
    
    model <- rsns.crossval(conf,inputs.hour, targets.hour, hour.folds)
  } 
  
  closeAllConnections()
}

test.day.mean.parametres <- function(pv.day, num.folds, conf, output.to.file, day.error.vis){
  if(output.to.file == T){
    sink(file = "final_test.txt", append = TRUE, type = "output",
         split = FALSE)
  }
  
  normalized.data <- normalize(pv.day, conf$predictors)
  targets.norm.params <- normalized.data$targets.norm.params
  inputs.day <- normalized.data$inputs
  targets.day <- normalized.data$targets
  folds <- cvFolds(dim(inputs.day)[1], K = num.folds, type = "random")
  
  print(deparse(substitute(pv.day)))
  print(conf)
  print("***************DAY*****************")
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
                hiddenActFunc = "Act_Logistic", shufflePatterns = conf$shufflePatterns,
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
  
  
  save(list = c("model", "predictors", "targets.norm.params", "inputs.hour"),file = paste0("./green_predict_app/model_", pv.name,".RData"))
}

  vis <- result %>% ggvis(x = ~Prediction, y= ~Target) %>% 
    layer_points(size = ~dif) %>% 
    layer_lines(~Target,~Target) %>%
    add_axis("x", title = "Predikovana hodnota") %>%
    add_axis("y", title = "Cielova hodnota") %>%
    add_legend("fill", title = "Chyba")