library(MASS)

lm.crossval <- function(inputs.hour, targets.hour, folds, predictors){
  validation.errors <- data.frame()
  train.errors <- data.frame()
  for(i in 1:folds$K){
    inputs.train <- inputs.hour[folds$which != i,predictors]
    inputs.validation <- inputs.hour[folds$which == i,predictors]
    targets.train <- targets.hour[folds$which != i,"Energy_kWh"]
    targets.validation <- targets.hour[folds$which == i,"Energy_kWh"]
    
    train.set <- as.data.frame(cbind(targets.train, inputs.train))
    validation.set <- as.data.frame(cbind(targets.validation, inputs.validation))
    colnames(train.set) <- c("Energy_kWh", predictors)
    colnames(validation.set) <- c("Energy_kWh", predictors)
    
    model <- rlm(Energy_kWh ~ poly(GHI,2) + poly(Temperature,2) + Energy_kWh_yesterday + poly(Relative.humidity,2) + poly(Cloudiness,2), data=pv3.hour)
    
    validation.predictions <- predict(model,validation.set)
    validation.predictions[validation.predictions < 0] <- 0
    validation.error <- modeval(validation.predictions,targets.validation,
                                stat=c("MAE","RMAE","RMSE","RRMSE"))
    validation.errors <- rbind(validation.errors, validation.error)
    
    print(paste("Crossvalidation iteration: ", i))
    print("Validation set errors:")
    print(paste("MAE: ",validation.error$MAE))
    print(paste("RMAE: ",validation.error$RMAE,"%"))
    print(paste("RMSE: ",validation.error$RMSE))
    print(paste("RRMSE: ",validation.error$RRMSE,"%"))
    
  }
  print("**********************************************")
  print("Final result:")
  print("Validation set errors:")
  print(paste("MAE: ",mean(validation.errors$MAE)))
  print(paste("RMAE: ",mean(validation.errors$RMAE),"%"))
  print(paste("RMSE: ",mean(validation.errors$RMSE)))
  print(paste("RRMSE: ",mean(validation.errors$RRMSE),"%"))
  print(summary(model))
  return (model)
}


test.lin.model <- function(pv.hour, num.folds, output.to.file){
  
  if(output.to.file == T){
    sink(file = "final_test.txt", append = TRUE, type = "output",
         split = FALSE)
  }
  predictors <- c("GHI","Temperature", "Energy_kWh_yesterday", "Relative.humidity", "Cloudiness")
  normalized.data <- normalize(pv.hour, predictors)
  targets.norm.params <- normalized.data$targets.norm.params
  inputs.hour <- normalized.data$inputs
  targets.hour <- normalized.data$targets
  folds <- cvFolds(dim(inputs.hour)[1], K = num.folds, type = "random")
  
  print(deparse(substitute(pv.hour)))
  print("linear model")
  print("***************HOUR*****************")
  lm.crossval(inputs.hour, targets.hour, folds, predictors)
  
  closeAllConnections()
}


