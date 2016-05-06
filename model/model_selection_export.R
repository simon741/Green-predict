#------------Client Code----------------------------------

load("../data_sets.RData")

conf <-list(size = c(15,15), maxit = 1000,
            learnFunc = "Rprop",
            predictors = c("GHI", "next_GHI", "prev_GHI","Temperature", "Energy_kWh_yesterday", "Relative.humidity", "Cloudiness", "sun_azimuth"),
            shufflePatterns = T)

conf26 <-list(size = c(15,15), maxit = 6000,
              predictors = c("GHI", "next_GHI", "prev_GHI","Temperature", "Energy_kWh_yesterday", "Relative.humidity", "Cloudiness", "sun_azimuth"),
              shufflePatterns = T)

conf27 <-list(size = c(15,15), maxit = 7000,
              predictors = c("GHI", "next_GHI", "prev_GHI","Temperature", "Energy_kWh_yesterday", "Relative.humidity", "Cloudiness", "sun_azimuth"),
              shufflePatterns = T)

conf28 <-list(size = c(15,15), maxit = 8000,
              predictors = c("GHI", "next_GHI", "prev_GHI","Temperature", "Energy_kWh_yesterday", "Relative.humidity", "Cloudiness", "sun_azimuth"),
              shufflePatterns = T)

conf29 <-list(size = c(15,15), maxit = 5000,
              predictors = c("GHI","Temperature", "prev_Temperature", "next_Temperature", "Energy_kWh_yesterday", "Relative.humidity", "Cloudiness", "sun_azimuth"),
              shufflePatterns = T)

conf30 <-list(size = c(15,15), maxit = 5000,
              learnFunc = "Rprop",
              predictors = c("Temperature", "prev_Temperature", "next_Temperature", "Energy_kWh_yesterday", "Relative.humidity", "Cloudiness", "sun_azimuth"),
              shufflePatterns = T)



conf27 <-list(size = c(15,15), maxit =4000,
              learnFunc = "Rprop",
              predictors = c("GHI", "next_GHI", "prev_GHI","Temperature", "Energy_kWh_yesterday", "Relative.humidity", "Cloudiness", "sun_azimuth"),
              shufflePatterns = T)

conf28 <-list(size = c(13,13), maxit =3000,
              learnFunc = "Rprop",
              predictors = c("GHI", "next_GHI", "prev_GHI","Temperature", "Energy_kWh_yesterday", "Relative.humidity", "Cloudiness", "sun_azimuth"),
              shufflePatterns = T)
conf29 <-list(size = c(10,10), maxit =5000,
              learnFunc = "Rprop",
              predictors = c("GHI", "next_GHI", "prev_GHI","Temperature", "Energy_kWh_yesterday", "Relative.humidity", "Cloudiness", "sun_azimuth"),
              shufflePatterns = T)

conf29 <-list(size = c(10,10), maxit =5000,
              learnFunc = "Rprop",
              predictors = c("GHI","Temperature", "Energy_kWh_yesterday", "Relative.humidity", "Cloudiness", "sun_azimuth", "daynight"),
              shufflePatterns = T)

confs <- list(conf29,conf30)

set.seed(100) #zaruci ze nahodne generovane hodnoty budu vzdy rovnake

test.confs(pv3.hour, pv3.day, num.folds = 10 ,confs, output.to.file = T, day.error.vis = F)

confs <- list(conf29)

test.confs(pv1.hour, pv1.day, num.folds = 10 ,confs, output.to.file = T, day.error.vis = F)

confs <- list(conf29)

test.confs(pv2.hour, pv2.day, num.folds = 10 ,confs, output.to.file = T, day.error.vis = F)

test.day.mean.parametres(pv3.day, num.folds = 10 ,conf9, output.to.file = T, day.error.vis = F)

test.lin.model(pv3.day, num.folds = 10, output.to.file = F)

export.model(pv3.hour,pv3.day, conf,"pv3")
export.model(pv2.hour,pv2.day, conf28,"pv2")
export.model(pv1.hour,pv1.day, conf28,"pv1")
