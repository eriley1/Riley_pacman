# subject 14

setwd("~/simpsonlab/PACMAN/Riley_pacman")
source("Pacman_functions.R")
library(animation)
cona.dir <-"~/simpsonlab/PACMAN/Complete CONA/PACMAN"

# Unit c2, subject 14
unit.c2 <- read.csv("pacman_unitc2_30s.csv", head = T)
unit.c2 <- as.xts(unit.c2[,-1], order.by = as.POSIXct(unit.c2[,1], tz = "GMT"))
unit.c2.smooth <- pacmanSmooth(unit.c2, bin = 3)
unit.c2.bg <- pacmanBG(unit.c2.smooth,window = 480)

pacmanTempSummary( pacman.smooth = unit.c2.smooth, pacman.bg = unit.c2.bg)


myfun <- function(data){
  
  data <- as.data.frame(data)
  coef(rlm(PM_mV ~ Temperature_IN_C, data = data, init = "ls", maxit = 10))
  #coef(lm(PM_mV ~ Temperature_IN_C, data = data))
}

system.time({
  test.it.out <- rollapply(unit.c2.bg, width = 5760, by = 30, FUN = myfun, by.column = FALSE)
})
test.it.out<- test.it.out[!is.na(test.it.out$Temperature_IN_C),]
plot(test.it.out$Temperature_IN_C)
plot(test.it.out$X.Intercept.)


test.merge <- window(na.locf(merge(unit.c2.smooth, test.it.out), fromLast = TRUE), index(unit.c2.bg))
test.merge$PM_predicted <- as.numeric(test.merge$Temperature_IN_C) * as.numeric(test.merge$Temperature_IN_C.1) + as.numeric(test.merge$X.Intercept.)

plot(unit.c2.smooth$PM_mV, xlim = c(index(unit.c2.smooth)[5760*8], index(unit.c2.smooth)[5760*9]))
points(test.merge$PM_predicted, pch = 19, col = colorByDate(index(test.merge)), cex = .25)

# residuals of the fit. structure in baseline retained
plot(index(unit.c2.smooth), as.numeric(unit.c2.smooth$PM_mV) - as.numeric(test.merge$PM_predicted), type = "l")
abline(h = 0, col = "red")

#what happens if we just subtract the 4 hour 5%.... no structure in baseline, several small peaks wiped out.
plot(index(unit.c2.smooth), as.numeric(unit.c2.smooth$PM_mV) - as.numeric(unit.c2.bg$PM_mV), type = "l", ylim = c(-20,100))
abline(h = 0, col = "red")

test.merge$PM_Tadj <- as.numeric(test.merge$PM_mV) - as.numeric(test.merge$PM_predicted)

plot(test.merge$PM_Tadj)

PM.bg <- pacmanBGv2(test.merge, 120 , nind = 'PM_Tadj', pmThresh = 40)

oneminData<- read.csv(paste(cona.dir,"/1 min CSV/subject_14.csv", sep = ""), colClasses = "character") 
PM_reg <- oneminData[!is.na(oneminData$PM10.FDMS), c('date', 'PM10.FDMS')]
PM_reg$date<-as.POSIXct(PM_reg$date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT") 
class(PM_reg$PM10.FDMS) <- "numeric"
PM_reg <- as.xts(PM_reg[,-1], as.POSIXct(PM_reg$date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))

plot(PM_reg$date, PM_reg$PM10.FDMS)

plot(PM.bg + 7, xlim = c(index(PM.bg)[5760*1], index(PM.bg)[5760*8]) )
plot(PM.bg + 9)

lines(PM_reg, col = "blue" )

merge3<- window(na.locf(merge(PM.bg, PM_reg), fromLast = TRUE), index(PM.bg))

a <-as.numeric(merge3$PM_reg)
b <- as.numeric(merge3$PM_Tadj)

ccf(a, b, lag.max = 240, type = "correlation", na.action = na.omit)

plot(as.numeric(merge3$PM_reg), as.numeric(merge3$PM_Tadj))

cor(as.numeric(merge3$PM_reg), as.numeric(merge3$PM_Tadj), use = "complete.obs")

# Make a video showing the temperature correction... #
tempColors <- colorByDate(index(test.merge))

oopts = ani.options(ffmpeg = 'C:/ffmpeg/bin/ffmpeg.exe')
if (.Platform$OS.type != "windows") ani.options(ffmpeg = 'C:/ffmpeg/bin/ffmpeg.exe')
saveVideo({
  for(i in seq(1,(nrow(unit.c2.smooth) - 5760), 240) ){
    plot(unit.c2.smooth$PM_mV, xlim = c(index(unit.c2.smooth)[i], index(unit.c2.smooth)[i+5760]))
    points(test.merge$PM_predicted, pch = 19, col = tempColors, cex = .25)
  }
  
}, video.name = "temp_correction_robust.asf",interval =0.33 ,ani.height = 777, 
ani.width = 1128,   img.name = "myvid5", clean = TRUE, other.opts = "-b 350k") # higher bitrate, better quality
ani.options(oopts)

