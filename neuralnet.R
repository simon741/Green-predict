library(RSNNS)
library(dplyr)
library(sirad)
library(neuralnet)
library(cvTools)

set.seed(100)

sum.hour.predictions <- function(target, nn, inputs.test, predictors){
  day <- target[2]
  prediction <- neuralnet::compute(nn,inputs.test[as.Date(inputs.test$Time) == day, predictors])
  prediction <- sum(prediction$net.result)
  return (prediction)
}

nn.crossval2 <- function(inputs.day, inputs.hour, targets.day, targets.hour, threshold, predictors, hidden.layers, num.folds){
  
  formula <- paste("Energy_kWh ~",paste(predictors, collapse = " + "))
  
  folds <- cvFolds(dim(targets.day)[1], K = num.folds, type = "random")
  
  test.errors <- data.frame()
  train.errors <- data.frame()
  
  for(i in 1:num.folds){
    inputs.train <- inputs.hour[as.Date(inputs.hour$Time) %in% inputs.day$Time[folds$which != i],]
    inputs.test <- inputs.hour[as.Date(inputs.hour$Time) %in% inputs.day$Time[folds$which == i],]
    targets.train <- targets.hour[as.Date(inputs.hour$Time) %in% inputs.day$Time[folds$which != i],]
    targets.test <- targets.day[folds$which == i,]
    
    inputs.train <- inputs.train[,predictors]
    targets.train <- targets.train[,"Energy_kWh"]

    train.set <- as.data.frame(cbind(targets.train, inputs.train))
    #test.set <- as.data.frame(cbind(targets.test, inputs.test))
    colnames(train.set) <- c("Energy_kWh", predictors)
    #colnames(test.set) <- predictors
    
    nn <- neuralnet(formula,
                    data = train.set,
                    hidden = hidden.layers,
                    linear.output = T,
                    #learningrate.limit = 0.5,
                    learningrate.factor = list(minus = 0.5, plus = 1.2),
                    #algorithm = 'backprop',
                    #learningrate = 0.01,
                    act.fct = 'logistic',
                    lifesign = 'full',
                    lifesign.step = 500,
                    threshold = threshold
                    #stepmax = 3000
    )
    test.predictions <- apply(targets.test,1,sum.hour.predictions, nn = nn,inputs.test = inputs.test, predictors = predictors)
    test.predictions <- normalizeData(test.predictions, type = "0_1")
    test.error <- modeval(test.predictions,targets.test$Energy_kWh,
                          stat=c("MAE","RMAE","RMSE","RRMSE"))
    test.errors <- rbind(test.errors, test.error)
    
    train.predictions <-neuralnet::compute(nn,inputs.train[,predictors])
    train.error <- modeval(train.predictions$net.result,targets.train,
                          stat=c("MAE","RMSE"))
    #train.errors <- rbind(train.errors,train.error)
    
    print(paste("Crossvalidation iteration: ", i))
    print("Train set errors:")
    print(paste("MAE: ",train.error$MAE))
    print(paste("RMSE: ",train.error$RMSE))
    print("Test set errors:")
    print(paste("MAE: ",test.error$MAE))
    print(paste("RMSE: ",test.error$RMSE))
    print(paste("RMAE: ",test.error$RMAE,"%"))
    print(paste("RRMSE: ",test.error$RRMSE,"%"))
    print("------------------------------------------------")
  }
  print("Final result:")
  print(paste("MAE: ",mean(test.errors$MAE)))
  print(paste("RMAE: ",mean(test.errors$RMAE),"%"))
  print(paste("RMSE: ",mean(test.errors$RMSE)))
  print(paste("RRMSE: ",mean(test.errors$RRMSE),"%"))
}

nn.crossval <- function(inputs, targets, threshold, predictors, hidden.layers, num.folds){
  formula <- paste("Energy_kWh ~",paste(predictors, collapse = " + "))
  
  folds <- cvFolds(dim(targets.day)[1], K = num.folds, type = "random")
  
  test.errors <- data.frame()
  train.errors <- data.frame()
  
  for(i in 1:num.folds){
    
    inputs.train <- inputs[folds$which != i,]
    inputs.test <- inputs[folds$which == i,]
    targets.train <- targets[folds$which != i,]
    targets.test <- targets[folds$which == i,]
    
    inputs.train <- inputs.train[,predictors]
    targets.train <- targets.train[,"Energy_kWh"]
    inputs.test <- inputs.test[,predictors]
    targets.test <- targets.test[,"Energy_kWh"]
    
    train.set <- as.data.frame(cbind(targets.train, inputs.train))
    #test.set <- as.data.frame(cbind(targets.test, inputs.test))
    colnames(train.set) <- c("Energy_kWh", predictors)
    #colnames(test.set) <- predictors
    
    
    nn <- neuralnet(formula,
                    data = train.set,
                    hidden = hidden.layers,
                    linear.output = T,
                    #learningrate.limit = 0.5,
                    #learningrate.factor = list(minus = 0.5, plus = 1.2),
                    #algorithm = 'backprop',
                    #learningrate = 0.01,
                    act.fct = 'logistic',
                    lifesign = 'full',
                    lifesign.step = 500,
                    threshold = threshold
                    #stepmax = 3000
    )
    test.predictions <-neuralnet::compute(nn,inputs.test)
    test.error <- modeval(test.predictions$net.result,targets.test,
                          stat=c("MAE","RMAE","RMSE","RRMSE"))
    test.errors <- rbind(test.errors,test.error)
    
    train.predictions <-neuralnet::compute(nn,inputs.train)
    train.error <- modeval(train.predictions$net.result,targets.train,
                           stat=c("MAE","RMSE"))
    
    print(paste("Crossvalidation iteration: ", i))
    print("Train set errors:")
    print(paste("MAE: ",train.error$MAE))
    print(paste("RMSE: ",train.error$RMSE))
    print("Test set errors:")
    print(paste("MAE: ",test.error$MAE))
    print(paste("RMSE: ",test.error$RMSE))
    print(paste("RMAE: ",test.error$RMAE,"%"))
    print(paste("RRMSE: ",test.error$RRMSE,"%"))
    print("------------------------------------------------")
  }
  print("Final result:")
  print(paste("MAE: ",mean(test.errors$MAE)))
  print(paste("RMAE: ",mean(test.errors$RMAE),"%"))
  print(paste("RMSE: ",mean(test.errors$RMSE)))
  print(paste("RRMSE: ",mean(test.errors$RRMSE),"%"))
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
#predictors <- c("GHI", "Temperature","next_GHI", "prev_GHI", "next_temperature", "prev_temperature")
predictors <- c("GHI", "Temperature", "Energy_kWh_yesterday")

#predictors <- c("GHI", "Temperature")

normalized.data <- normalize(pv3.day, c("GHI","Temperature", "Energy_kWh_yesterday"))
inputs.day <- normalized.data$inputs
inputs.day <- cbind(inputs.day, Time = pv3.day$Time)
targets.day <- normalized.data$targets
targets.day <- cbind(targets.day, Time = pv3.day$Time)
targets.norm.params <- normalized.data$targets.norm.params

normalized.data <- normalize(pv3.hour, predictors)
inputs.hour <- normalized.data$inputs
inputs.hour <- cbind(inputs.hour, Time = pv3.hour$Time)
targets.hour <- normalized.data$targets
targets.hour <- cbind(targets.hour, Time = pv3.hour$Time)

hidden.layers <- c(5,8,5)

nn.crossval2(inputs.day, inputs.hour, targets.day, targets.hour, 0.03, predictors, hidden.layers,5)
nn.crossval(inputs.day, targets.day, 0.03, predictors, hidden.layers, 10)

rsns.crossval(inputs.day, inputs.hour, targets.day, targets.hour, conf, predictors, 5)

#--------------------------------------------

denormalizeData(errors$MAE, targets.norm.params)

denormalizeData(neuralnet::compute(nn, test.set[8,c(2,3,4)])$net.result, targets.norm.params)
as.POSIXct()

