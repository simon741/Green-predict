library(RSNNS)
library(dplyr)
library(sirad)
library(cvTools)
library(ggvis)

#------------Client Code----------------------------------

load("../data_sets.RData")
best.conf.FVE3 <-list(size = c(15,15), maxit =5000,
                      learnFunc = "Rprop",
                      predictors = c("GHI", 
                                     "next_GHI", 
                                     "prev_GHI",
                                     "Temperature", 
                                     "Energy_kWh_yesterday", 
                                     "Relative.humidity", 
                                     "Cloudiness", 
                                     "sun_azimuth"))

conf.for.mean.day.weather.parametres.FVE3 <-list(size = c(15,15), maxit =500,
                      learnFunc = "Rprop",
                      predictors = c("GHI",
                                     "Temperature",
                                     "Energy_kWh_yesterday", 
                                     "Relative.humidity", 
                                     "Cloudiness"))

best.conf.FVE2 <-list(size = c(10,10), maxit =3000,
                      learnFunc = "Rprop",
                      predictors = c("GHI", 
                                     "next_GHI", 
                                     "prev_GHI",
                                     "Temperature", 
                                     "Energy_kWh_yesterday", 
                                     "Relative.humidity", 
                                     "Cloudiness", 
                                     "sun_azimuth"))

best.conf.FVE1 <-list(size = c(10,10), maxit = 3000,
                      learnFunc = "Rprop",
                      predictors = c("GHI", 
                                     "next_GHI", 
                                     "prev_GHI",
                                     "Temperature", 
                                     "Energy_kWh_yesterday", 
                                     "Relative.humidity", 
                                     "Cloudiness", 
                                     "sun_azimuth"))

set.seed(100) 

confs <- list(best.conf.FVE3)
test.confs(pv3.hour, pv3.day, num.folds = 10 ,confs, output.to.file = T, day.error.vis = T)

confs <- list(best.conf.FVE2)
test.confs(pv2.hour, pv2.day, num.folds = 10 ,confs, output.to.file = T, day.error.vis = F)

confs <- list(best.conf.FVE1)
test.confs(pv1.hour, pv1.day, num.folds = 10 ,confs, output.to.file = T, day.error.vis = F)

confs <- list(conf.for.mean.day.weather.parametres.FVE3)
test.day.mean.parametres(pv3.day, num.folds = 10 ,conf9, output.to.file = T, day.error.vis = F)

test.lin.model(pv3.hour, num.folds = 10, output.to.file = F)

export.model(pv3.hour,pv3.day, best.conf.FVE3,"pv3")
export.model(pv2.hour,pv2.day, best.conf.FVE2,"pv2")
export.model(pv1.hour,pv1.day, best.conf.FVE1,"pv1")