# CONA

setwd("~/simpsonlab/PACMAN/Riley_pacman")
source("Pacman_functions.R")

# # Unit 10, subject 1
# #!!! PM data seems to be inverted !!!! #
# unit.10 <- read.csv("pacman_unit10_30s.csv", head = T)
# unit.10$PM_mV[unit.10$PM_mV > 1e4] <- NA
# unit.10 <- as.xts(unit.10[,-1], order.by = as.POSIXct(unit.10[,1], tz = "GMT"))
# unit.10.smooth <- pacmanSmooth(unit.10, bin = 3)
# unit.10.bg <- pacmanBG(unit.10.smooth,window = 480)
#pacmanTempSummary(pacman.bg = unit.10.bg)

# Unit c1, subject 3
unit.c1 <- read.csv("pacman_unitc1_30s.csv", head = T)
unit.c1 <- as.xts(unit.c1[,-1], order.by = as.POSIXct(unit.c1[,1], tz = "GMT"))
unit.c1.smooth <- pacmanSmooth(unit.c1, bin = 3)
unit.c1.smooth2 <- tempJumps(unit.c1.smooth)
rm(unit.c1)
rm(unit.c1.smooth)
unit.c1.bg <- pacmanBG(unit.c1.smooth2,window = 480)
rm(unit.c1.smooth2)

pacmanTempSummary(pacman.bg = unit.c1.bg, myname = "Unit c1, subject 3")

# Unit 16, subject 4
unit.16 <- read.csv("pacman_unit16_30s.csv", head = T)
unit.16 <- as.xts(unit.16[,-1], order.by = as.POSIXct(unit.16[,1], tz = "GMT"))
unit.16.smooth <- pacmanSmooth(unit.16, bin = 3)
unit.16.smooth2 <- tempJumps(unit.16.smooth)
rm(unit.16)
rm(unit.16.smooth)
unit.16.bg <- pacmanBG(unit.16.smooth2,window = 480)
rm(unit.16.smooth2)

pacmanTempSummary(pacman.bg = unit.16.bg, myname = "Unit 16, subject 4")


# Unit 13, subject 5
unit.13 <- read.csv("pacman_unit13_30s.csv", head = T)
unit.13 <- as.xts(unit.13[,-1], order.by = as.POSIXct(unit.13[,1], tz = "GMT"))
unit.13.smooth <- pacmanSmooth(unit.13, bin = 3)
unit.13.smooth2 <- tempJumps(unit.13.smooth)
rm(unit.13)
rm(unit.13.smooth)
unit.13.bg <- pacmanBG(unit.13.smooth2,window = 480)
rm(unit.13.smooth2)

pacmanTempSummary(pacman.bg = unit.13.bg, myname = "Unit 13, subject 5")


# Unit 17, subject 6
unit.17 <- read.csv("pacman_unit17_30s.csv", head = T)
unit.17 <- as.xts(unit.17[,-1], order.by = as.POSIXct(unit.17[,1], tz = "GMT"))
unit.17.smooth <- pacmanSmooth(unit.17, bin = 3)
unit.17.smooth2 <- tempJumps(unit.17.smooth)
rm(unit.17)

unit.17.smooth2<-tempJumps(unit.17.smooth)

par(mfcol = c(2,1))
par(mar = c(3,3,1,.5))

plot(unit.17.smooth$Temperature_IN_C, type = "p", pch = 19, cex = .5, main = "Temperature Unit 17")
points(unit.17.smooth2$Temperature_IN_C, col = "blue", pch = 19, cex = .5)
plot(unit.17.smooth$PM_mV, type = "p", pch = 19, cex = .5, main = "PM Unit 17")
points(unit.17.smooth2$PM_mV, col = "blue", pch = 19, cex = .5)

rm(unit.17.smooth)
unit.17.bg <- pacmanBG(unit.17.smooth2,window = 480)
rm(unit.17.smooth2)

pacmanTempSummary(pacman.bg = unit.17.bg, myname = "Unit 17, subject 6")

# Unit 18, subject 7
unit.18 <- read.csv("pacman_unitC18_30s.csv", head = T)
unit.18 <- as.xts(unit.18[,-1], order.by = as.POSIXct(unit.18[,1], tz = "GMT"))
unit.18.smooth <- pacmanSmooth(unit.18, bin = 3)
unit.18.smooth2 <- tempJumps(unit.18.smooth)
rm(unit.18)
rm(unit.18.smooth)
unit.18.bg <- pacmanBG(unit.18.smooth2,window = 480)
rm(unit.18.smooth2)

pacmanTempSummary(pacman.bg = unit.18.bg, myname = "Unit 18, subject 7")


# Unit 11, subject 8
unit.11 <- read.csv("pacman_unit11_30s.csv", head = T)
unit.11 <- as.xts(unit.11[,-1], order.by = as.POSIXct(unit.11[,1], tz = "GMT"))
unit.11.smooth <- pacmanSmooth(unit.11, bin = 3)
unit.11.smooth2 <- tempJumps(unit.11.smooth)
rm(unit.11)
rm(unit.11.smooth)
unit.11.bg <- pacmanBG(unit.11.smooth2,window = 480)
rm(unit.11.smooth2)

pacmanTempSummary(pacman.bg = unit.11.bg, myname = "Unit 11, subject 8")

# Unit 12, subject 10
unit.12 <- read.csv("pacman_unit12_30s.csv", head = T)
unit.12 <- as.xts(unit.12[,-1], order.by = as.POSIXct(unit.12[,1], tz = "GMT"))
unit.12.smooth <- pacmanSmooth(unit.12, bin = 3)
unit.12.smooth2 <- tempJumps(unit.12.smooth)
rm(unit.12)
rm(unit.12.smooth)
unit.12.bg <- pacmanBG(unit.12.smooth2,window = 480)
rm(unit.12.smooth2)

pacmanTempSummary(pacman.bg = unit.12.bg, myname = "Unit 12, subject 10")


# Unit c2, subject 14
unit.c2 <- read.csv("pacman_unitc2_30s.csv", head = T)
unit.c2 <- as.xts(unit.c2[,-1], order.by = as.POSIXct(unit.c2[,1], tz = "GMT"))
unit.c2.smooth <- pacmanSmooth(unit.c2, bin = 3)
unit.c2.smooth2 <- tempJumps(unit.c2.smooth)
rm(unit.c2)
rm(unit.c2.smooth)
unit.c2.bg <- pacmanBG(unit.c2.smooth2,window = 480)
rm(unit.c2.smooth2)

pacmanTempSummary(pacman.bg = unit.c2.bg, "Unit C2, Subject 14")

###############################

# Regressions on Temperature and save files #

unit.c1.reg <- segReg(unit.c1.bg, unit.name = "Unit c1")
plotRegress(unit.c1.reg,unit.name = "Unit c1")


unit.16.reg <- segReg(unit.16.bg, unit.name = "Unit 16")
plotRegress(unit.16.reg,unit.name = "Unit 16")


#no jumps but probably should be....
unit.17.reg <- segReg(unit.17.bg, unit.name = "Unit 17")
plotRegress(unit.17.reg,unit.name = "Unit 17")

#no jumps
unit.18.reg <- segReg(unit.18.bg, unit.name = "Unit 18")
plotRegress(unit.18.reg,unit.name = "Unit 18")

# jumps
unit.13.reg <- segReg(unit.13.bg, unit.name = "Unit 13")
plotRegress(unit.13.reg,unit.name = "Unit 13")

# no jumps
unit.11.reg <- segReg(unit.11.bg, unit.name = "Unit 11")
plotRegress(unit.11.reg,unit.name = "Unit 11")

#no jumps
unit.12.reg <- segReg(unit.12.bg, unit.name = "Unit 12")
plotRegress(unit.12.reg,unit.name = "Unit 12")

#no jumps
C2.regress <- segReg(unit.c2.bg, unit.name = "Unit C2")
plotRegress(C2.regress,unit.name = "Unit C2")


#plot all the pacman backgrounds together #

allBG <- cbind(unit.c1.reg$PM_mV_bg,unit.16.reg$PM_mV_bg, unit.17.reg$PM_mV_bg,
               unit.18.reg$PM_mV_bg, unit.13.reg$PM_mV_bg, unit.11.reg$PM_mV_bg, 
               unit.12.reg$PM_mV_bg, C2.regress$PM_mV_bg)
names(allBG)<- c("bg_c1", "bg_16","bg_17", "bg_18", "bg_13","bg_11",
                "bg_12", "bg_c2" )

plot(allBG$bg_c1 - min(allBG$bg_c1, na.rm = T), type = "p", pch = 19, cex = .25,
     main = "All CONA PM 4-hour 5%")
points(allBG$bg_16 - min(allBG$bg_16, na.rm = T), pch = 19, cex = .25, col = "red")
points(allBG$bg_17 - min(allBG$bg_17, na.rm = T), pch = 19, cex = .25, col = "blue")
points(allBG$bg_18 - min(allBG$bg_18, na.rm = T), pch = 19, cex = .25, col = "yellow")
points(allBG$bg_13 - min(allBG$bg_13, na.rm = T), pch = 19, cex = .25, col = "green")
points(allBG$bg_11 - min(allBG$bg_11, na.rm = T), pch = 19, cex = .25, col = "purple")
points(allBG$bg_12 - min(allBG$bg_12, na.rm = T), pch = 19, cex = .25, col = "hotpink")
points(allBG$bg_c2 - min(allBG$bg_c2, na.rm = T), pch = 19, cex = .25, col = "orange")
