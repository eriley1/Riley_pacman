# this program is for analyzing the pacman data collected
# At Amor Hirao's house.

# set paths to data files here #
setwd("~/simpsonlab/PACMAN/Riley_pacman")
source("Pacman_functions.R")
kitchen0704_path<- "~/simpsonlab/PACMAN/10_July_Amor_Hirao/10_July_Amor_Hirao/kitchen/Kitchen 17_7/Kitchen 17_7/20130704.TXT"
kitchen07path <- "~/simpsonlab/PACMAN/10_July_Amor_Hirao/10_July_Amor_Hirao/kitchen/Kitchen 17_7/Kitchen 17_7/201307.TXT"
heater07path <- "~/simpsonlab/PACMAN/10_July_Amor_Hirao/10_July_Amor_Hirao/heater/Heater 17_7/Heater 17_7/201307.TXT"
kitchen <- readPACMAN(kitchen07path)
heater <- readPACMAN(heater07path)
########## remove troughs from 1s data ######
kitchen.c <- pacmanClean1s(kitchen)
heater.c <- pacmanClean1s(heater)

heater.bin <- pacmanReduce(heater.c, bin = 30)
heater.smooth <- pacmanSmooth(heater.bin, bin = 3)
heater.bg <- pacmanBG(heater.smooth,window = 480)

plot(kitchen$CO_mV)


##### plot the PM sensor for kitchen 1 second data####
plot(kitchen$date,kitchen$PM_mV, type = 'l',
     main = "PM from kitchen, Amor Hirao", ylab = "PM mV", xlab = "Date")

# zoom in on one of the peaks with a trough
par(mfcol = c(2,1))
par(mar = c(4,4,.5,.5))
plot(kitchen$date,kitchen$PM_mV, pch = 19, xlim = c(kitchen$date[517230], kitchen$date[517800] ),
     cex = .5, xlab = "Time", ylab = "PM mV")
axis.POSIXct(1, at = kitchen$date[seq(517230,517800,114)], format = "%H:%M:%S" )
plot(kitchen$date,kitchen$PM_mV, pch = 19, xlim = c(kitchen$date[517230], kitchen$date[517300] ), 
     xaxt = "n",  cex = .5, xlab = "Time", ylab = "PM mV")
axis.POSIXct(1, at = kitchen$date[seq(517230,517300,14)], format = "%H:%M:%S" )

# plot the result of the trough censoring algorithem for the kitchen sensor #
plot(kitchen$date,kitchen$PM_mV, pch = 19, xlim = c(kitchen$date[517230], kitchen$date[517700] ),
     col = "green", cex = .75, xlab = "Time", ylab = "PM mV",xaxt = "n")
axis.POSIXct(1, at = kitchen$date[seq(517230,517800,114)], format = "%H:%M:%S" )
points(kitchen.c$date,kitchen.c$PM_mV, pch = 19, col= "black", cex = 0.75)


###### rebin the pacman to 30 seconds and compare to 1 second data #####
test <- pacmanReduce(kitchen.c, bin = 30)

par(mfcol = c(4,1))
par(mar = c(5,4,.5,.5))
par(las = 2)
plot(kitchen$date,kitchen$PM_mV, type = 'l', ylab = "PM mV", xlab = "Date")
lines(test$PM_mV, col = "green")

plot(kitchen$date, kitchen$PM_mV, xlim = c(kitchen$date[81000], kitchen$date[120000]),
     type = "l", xaxt = "n", ylab = "PM mV", xlab = "Time, July 11", ylim = c(600,1000) )
axis.POSIXct(1, at = kitchen$date[seq(81000,120000,8000)], format =  "%H:%M")
lines(test$PM_mV,xlim = c(index(test)[2700], index(test)[4000]), col = "green")

plot(kitchen$date, kitchen$PM_mV, xlim = c(kitchen$date[84000], kitchen$date[93000]),
     type = "l", xaxt = "n", ylab = "PM mV", xlab = "Time, July 11", ylim = c(600,1000))
axis.POSIXct(1, at = kitchen$date[seq(84000,93000,1800)], format = "%H:%M")
lines(test$PM_mV, xlim = c(index(test)[2800], index(test)[3100]), col = "green", lwd = 2)

plot(kitchen$date, kitchen$PM_mV, xlim = c(kitchen$date[84000], kitchen$date[87000]),
     type = "l", xaxt = "n", ylab = "PM mV", xlab = "Time, July 11", ylim = c(600,1000), lwd = 0.25)
axis.POSIXct(1, at = kitchen$date[seq(84000,87000,600)], format = "%H:%M")
lines(test$PM_mV, xlim = c(index(test)[2800], index(test)[2900]), ylim = c(600, 1000), col = "green", lwd = 2)


##### smooooooooth the data to 3 minutes ########

test.smooth <- pacmanSmooth(test, bin = 3)
test.smooth2 <- pacmanSmooth(test, bin = 5)

par(mfcol = c(4,1))
par(mar = c(5,4,.5,.5))
par(las = 2)
plot(test$PM_mV, type = 'l', ylab = "PM mV", xlab = "Date", main = "")
lines(test.smooth$PM_mV, col = "green", lwd = 2)
lines(test.smooth2$PM_mV, col = "blue")

plot(test$PM_mV,xlim = c(index(test)[2700], index(test)[4000]), , xaxt = "n", 
     ylab = "PM mV", xlab = "Time, July 11", ylim = c(600,1000), main = "")
axis.POSIXct(1, at = index(test)[seq(2700,4000,260)], format =  "%H:%M")
lines(test.smooth$PM_mV,xlim = c(index(test.smooth)[2700], index(test.smooth)[4000]), col = "green", lwd = 2)
lines(test.smooth2$PM_mV,xlim = c(index(test.smooth2)[2700], index(test.smooth2)[4000]), col = "blue")


plot(test$PM_mV,xlim = c(index(test)[2800], index(test)[3100]), , xaxt = "n", 
     ylab = "PM mV", xlab = "Time, July 11", ylim = c(600,1000), main = "")
axis.POSIXct(1, at = index(test)[seq(2800,3100,60)], format =  "%H:%M")
lines(test.smooth$PM_mV,xlim = c(index(test.smooth)[2800], index(test.smooth)[3100]), col = "green", lwd = 2)
lines(test.smooth2$PM_mV,xlim = c(index(test.smooth2)[2800], index(test.smooth2)[3100]), col = "blue")


plot(test$PM_mV,xlim = c(index(test)[2800], index(test)[2900]), , xaxt = "n", 
     ylab = "PM mV", xlab = "Time, July 11", ylim = c(600,1000), main = "")
axis.POSIXct(1, at = index(test)[seq(2800,2900,20)], format =  "%H:%M")
lines(test.smooth$PM_mV,xlim = c(index(test.smooth)[2800], index(test.smooth)[2900]), col = "green", lwd = 2)
lines(test.smooth2$PM_mV,xlim = c(index(test.smooth2)[2800], index(test.smooth2)[2900]), col = "blue")

##### Estimate background ######
my.color <- c(rep("orange",2880),rep("green",2880), rep("yellow",2880), rep("blue",2880), rep("pink",2880), rep("red",2880), rep("purple",2744) )
# corresponds to 4 hours 
test.bg <- pacmanBG(test.smooth,window = 480)
plot(index(test.smooth),test.smooth$PM_mV, main = "", xlab = "date", ylab = "PM mV", type = "l", ylim = c(600, 800))
points(test.bg$PM_mV, col = my.color, pch = 19, cex = .25)




plot(test.smooth$CO_mV)
lines(test.bg$CO_mV, col = "green")

plot(test.smooth$CO2_mV)
lines(test.bg$CO2_mV, col = "green")

plot(index(test.smooth), test.smooth$Temperature_IN_C, type = "l")
points(test.bg$Temperature_IN_C,, pch = 19, cex = .25, col = my.color)


plot(test.smooth$Movement)
lines(test.bg$Movement, col = "green")

cor(as.numeric(test.bg$Temperature_IN_C), as.numeric(test.bg$PM_mV), use = "pairwise.complete.obs")
plot(as.numeric(test.bg$Temperature_IN_C), as.numeric(test.bg$PM_mV), 
     col = my.color, xlim = c(12, 26), pch = 19, cex =.5)
abline(lm(as.numeric(test.bg$PM_mV)~as.numeric(test.bg$Temperature_IN_C)), col = "black", lwd = 2)
lm(as.numeric(test.bg$PM_mV)~as.numeric(test.bg$Temperature_IN_C))
abline(rlm(as.numeric(test.bg$PM_mV)~as.numeric(test.bg$Temperature_IN_C), init = "ls"), col = "blue", lwd = 2)
rlm(as.numeric(test.bg$PM_mV)~as.numeric(test.bg$Temperature_IN_C), init = "ls")

cor(as.numeric(test.smooth$Temperature_IN_C), as.numeric(test.smooth$PM_mV), use = "pairwise.complete.obs")
plot(as.numeric(test.smooth$Temperature_IN_C), as.numeric(test.smooth$PM_mV), 
     col = my.color, xlim = c(12, 26), pch = 19, cex =.5)
abline(lm(as.numeric(test.smooth$PM_mV)~as.numeric(test.smooth$Temperature_IN_C)), col = "black", lwd = 2)
lm(as.numeric(test.smooth$PM_mV)~as.numeric(test.smooth$Temperature_IN_C))
abline(rlm(as.numeric(test.smooth$PM_mV)~as.numeric(test.smooth$Temperature_IN_C), init = "ls"), col = "blue", lwd = 2)
summary(rlm(as.numeric(test.smooth$PM_mV)~as.numeric(test.smooth$Temperature_IN_C), init = "ls"))

test.temp <- tempPM(test, test.bg)

plot(test.temp$PM_mV, ylim = c(500, 1000), auto.grid = F)
plot(test.temp$Dust, ylim = c(-100, 400), auto.grid = F)
abline(h = 0, col = "red")


cor(as.numeric(test.bg$PM_mV), as.numeric(test.smooth$Temperature_IN_C), use = "pairwise.complete.obs")
plot(as.numeric(test.smooth$Temperature_IN_C), as.numeric(test.bg$PM_mV),col = my.color,
     xlim = c(12, 26), cex = 0.5, pch = 19)
abline(lm(as.numeric(test.bg$PM_mV)~as.numeric(test.smooth$Temperature_IN_C)), col = "black", lwd = 2)
lm(as.numeric(test.smooth$Temperature_IN_C) ~ as.numeric(test.bg$PM_mV))

cor(as.numeric(test.bg$CO_mV), as.numeric(test.smooth$Temperature_IN_C), use = "pairwise.complete.obs")
plot(as.numeric(test.smooth$Temperature_IN_C), as.numeric(test.bg$CO_mV))
abline(lm(as.numeric(test.bg$CO_mV)~as.numeric(test.smooth$Temperature_IN_C)), col = "green", lwd = 2)
lm(as.numeric(test.smooth$Temperature_IN_C) ~ as.numeric(test.bg$CO_mV))

cor(as.numeric(test.bg$CO2_mV), as.numeric(test.smooth$Temperature_IN_C), use = "pairwise.complete.obs")
plot(as.numeric(test.smooth$Temperature_IN_C), as.numeric(test.bg$CO2_mV))
abline(lm(as.numeric(test.bg$CO2_mV)~as.numeric(test.smooth$Temperature_IN_C)), col = "green", lwd = 2)
lm(as.numeric(test.smooth$Temperature_IN_C) ~ as.numeric(test.bg$CO2_mV))

test3 <- as.data.frame(test.smooth)

test3$t.bin <- round(test3$Temperature_IN_C, digits = 0)

a <- summaryBy(PM_mV ~ t.bin, data = test3, FUN = quantile, probs = 0.05, na.rm = T )

points(a[,1], a[,2], col = "green", pch = 19 )

plot(seq(1,nrow(test.bg)), as.numeric(test.bg$PM_mV))


#------------------------------------------------------------------- #

#heater.color <- c(rep("orange",2880),rep("green",2880), rep("yellow",2880), rep("blue",2880), rep("pink",2880), rep("red",2880), rep("purple",2747) )
heat.color <-rainbow(7)[as.Date(index(heater.bin$date))]
plot(heater.bin$PM_mV)
points(heater.bg$PM_mV, col =my.colors, pch = 19, cex =0.25)

plot(heater.bin$Temperature_IN_C)
points(heater.bg$Temperature_IN_C, col =colorByDate(index(heater.bg)), pch = 19, cex =0.25)

cor(as.numeric(heater.bg$PM_mV), as.numeric(heater.bg$Temperature_IN_C), use = "pairwise.complete.obs")
plot(as.numeric(heater.bg$Temperature_IN_C), as.numeric(heater.bg$PM_mV), pch = 19, cex =.5, col = heater.color, xlim = c(14, 29), ylim = c(250, 500))
abline(lm(as.numeric(heater.bg$PM_mV)~ as.numeric(heater.bg$Temperature_IN_C)))
abline(rlm(as.numeric(heater.bg$PM_mV)~ as.numeric(heater.bg$Temperature_IN_C), init = "ls"), col = "blue")
lm(as.numeric(heater.bg$PM_mV)~ as.numeric(heater.bg$Temperature_IN_C))
summary(rlm(as.numeric(heater.bg$PM_mV)~ as.numeric(heater.bg$Temperature_IN_C)))


lm(PM_mV~ Temperature_IN_C, data = heater.bg)

cor(as.numeric(heater.smooth$PM_mV), as.numeric(heater.smooth$Temperature_IN_C), use = "pairwise.complete.obs")
plot(as.numeric(heater.smooth$Temperature_IN_C), as.numeric(heater.smooth$PM_mV), pch = 19, cex =.5, col = heater.color, xlim = c(14, 29), ylim = c(250, 500))
abline(lm(as.numeric(heater.smooth$PM_mV)~ as.numeric(heater.smooth$Temperature_IN_C)))
lm(as.numeric(heater.smooth$PM_mV)~ as.numeric(heater.smooth$Temperature_IN_C))
abline(rlm(as.numeric(heater.smooth$PM_mV)~ as.numeric(heater.smooth$Temperature_IN_C), init = "ls"), col = "blue")
plot(rlm(as.numeric(heater.smooth$PM_mV)~ as.numeric(heater.smooth$Temperature_IN_C), init = "ls"))

#########################################################

byday <- summaryBy(PM_mV ~ Day + Temperature_IN_C, data = heater.c, FUN = quantile, probs = 0.1, na.rm = T)
byday <- byday[complete.cases(byday),]
plot(byday[,2], byday[,3], col =byday[,1], cex = .75)
cor(byday[,2], byday[,3])
abline(lm(byday[,3]~ byday[,2]))
lm(byday[,3]~ byday[,2])
abline(rlm(byday[,3]~ byday[,2], init = "ls" ), col = "blue")

kitchen.c$justDate <- as.Date(kitchen.c$date)
byday.k <- summaryBy(PM_mV ~ justDate + Temperature_IN_C, data = kitchen.c, FUN = quantile, probs = 0.1, na.rm = T)
byday.k <- byday.k[complete.cases(byday.k),]
plot(byday.k[,2], byday.k[,3], col =colorByDate(byday.k$justDate))
cor(byday.k[,2], byday.k[,3])
abline(lm(byday.k[,3]~ byday.k[,2]))
summary(lm(byday.k[,3]~ byday.k[,2]))
abline(rlm(byday.k[,3]~ byday.k[,2], init = "ls" ), col = "blue")
summary(rlm(byday.k[,3]~ byday.k[,2], init = "ls" ))


pacmanTempSummary(pacman.c = heater.c, pacman.smooth =heater.smooth , pacman.bg = heater.bg)
