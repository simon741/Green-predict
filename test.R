set.seed(100)





conf <-list(size = c(10,10), maxit = 5000,
            initFuncParams = c(-0.5, 0.5),
            learnFuncParams = c(0.5, 1.2, 0, 0.5, 1.2),
            predictors = c("GHI", "Temperature", "Energy_kWh_yesterday"),
            shufflePatterns = T)

conf1 <-list(size = c(13,13), maxit = 5000,
             predictors = c("GHI","Temperature", "next_GHI", "prev_GHI", "next_temperature", "prev_temperature",  "Energy_kWh_yesterday"),
             shufflePatterns = T)

conf1 <-list(size = c(13,13), maxit = 5000,
             predictors = c("GHI","Temperature", "next_GHI", "prev_GHI", "next_temperature", "prev_temperature",  "Energy_kWh_yesterday"),
             shufflePatterns = T)


conf1 <-list(size = c(10,10), maxit = 5000,
             predictors = c("GHI", "Temperature"),
             shufflePatterns = T)

conf2 <-list(size = c(10,10), maxit = 5000,
             predictors = c("GHI", "Temperature", "Wind.direction"),
             shufflePatterns = T)

conf3 <-list(size = c(10,10), maxit = 5000,
             predictors = c("GHI", "Temperature", "Wind.speed"),
             shufflePatterns = T)

conf4 <-list(size = c(10,10), maxit = 5000,
             predictors = c("GHI", "Temperature", "Atmospheric.pressure"),
             shufflePatterns = T)

conf5 <-list(size = c(10,10), maxit = 5000,
             predictors = c("GHI", "Temperature", "Cloudiness"),
             shufflePatterns = T)

conf6 <-list(size = c(10,10), maxit = 5000,
             predictors = c("GHI", "Temperature", "Relative.humidity"),
             shufflePatterns = T)

conf7 <-list(size = c(10,10), maxit = 5000,
             predictors = c("GHI", "Temperature", "Relative.humidity", "Cloudiness"),
             shufflePatterns = T)

conf8 <-list(size = c(10,10), maxit = 5000,
             predictors = c("GHI", "Temperature", "Energy_kWh_yesterday"),
             shufflePatterns = T)




confs <- list(conf1,conf2,conf3,conf4,conf5,conf6,conf7,conf8)


test.conf(num.folds = 10 ,confs, output.to.file = T, day.error.vis = T)
