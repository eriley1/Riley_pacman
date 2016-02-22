
library(zoo)
library(xts)
library(doBy)
library(MASS)
# readPACMAN opens a pacman txt file, scales the Temperature_mV, and generates a date column
readPACMAN<- function(filepath){
  # read in data as character.... error in file someplace that is creating factors! #
  pacman.data <- read.delim(filepath, colClasses = "character", header = TRUE)
  
  pacman.data$Temperature_mV <- as.numeric(pacman.data$Temperature_mV)/1000
  # create date column#
  pacman.data$date<-ISOdatetime(year=pacman.data$Year,
                                month=pacman.data$Month,
                                day=pacman.data$Day,
                                hour=pacman.data$Hour,
                                min=pacman.data$Minute,
                                sec=pacman.data$Second,
                                tz="GMT")
  #classify as numeric as needed 
  pacman.data$Count <- as.numeric(pacman.data$Count)
  pacman.data$Distance <- as.numeric(pacman.data$Distance)
  pacman.data$Temperature_IN_C <- as.numeric(pacman.data$Temperature_IN_C)
  pacman.data$PM_mV <- as.numeric(pacman.data$PM_mV)
  pacman.data$CO2_mV <- as.numeric(pacman.data$CO2_mV)
  pacman.data$CO_mV <- as.numeric(pacman.data$CO_mV)
  pacman.data$Movement <- as.numeric(pacman.data$Movement)
  pacman.data$COstatus <- as.numeric(pacman.data$COstatus)
  
  # remove bad lines 
  pacman.data <- pacman.data[is.finite(pacman.data$Count),]
  
  #####apply flags and signal conversions####
  
  # Inverting CO2
  pacman.data$CO2_mV <- -1 * pacman.data$CO2_mV
  
  # censor flagged CO data 
  pacman.data <- coValid(pacman.data)
  
  return(pacman.data)
}

# I'm confused by the two temperature channels. They appear to be labeled backwards. For now
# I've changed what Gus has to swap Temperature in C with Temperature_mV... I was expecting
# Temperature in mV to be a voltage, but it appears to get changed to celcius. not sure???
readConaPACMAN<- function(filepath){
  # read in data as character.... error in file someplace that is creating factors! #
  pacman.data <- read.delim(filepath,colClasses = "character", header = TRUE)
  
  names(pacman.data)<-c('Count','Year','Month','Day','Hour','Minute','Second',
                        'Distance','Temperature_mV','Temperature_IN_C','PM_mV','CO2_mV','CO_mV','Movement','COstatus')
  pacman.data$Temperature_IN_C <- (as.numeric(as.character(pacman.data$Temperature_IN_C))/100)-273.15
  pacman.data$date<-ISOdatetime(year=pacman.data$Year,
                                month=pacman.data$Month,
                                day=pacman.data$Day,
                                hour=pacman.data$Hour,
                                min=pacman.data$Minute,
                                sec=pacman.data$Second,
                                tz="GMT")
  
  pacman.data$Count <- as.numeric(pacman.data$Count)
  pacman.data$Distance <- as.numeric(pacman.data$Distance)
  pacman.data$Temperature_IN_C <- as.numeric(pacman.data$Temperature_IN_C)
  pacman.data$PM_mV <- as.numeric(pacman.data$PM_mV)
  pacman.data$CO2_mV <- as.numeric(pacman.data$CO2_mV)
  pacman.data$CO_mV <- as.numeric(pacman.data$CO_mV)
  pacman.data$Movement <- as.numeric(pacman.data$Movement)
  pacman.data$COstatus <- as.numeric(pacman.data$COstatus)
  
  #remove rows where date column is NA
  pacman.data<- pacman.data[is.finite(pacman.data$date),]
  
  #CO label
  nrecs=length(pacman.data$COstatus)
  pacman.data$CO_ok <- FALSE
  pacman.data$CO_ok[1:nrecs-1] <- (pacman.data$COstatus[-1]==1)&(pacman.data$COstatus[1:nrecs-1]==2)
  pacman.data$CO_raw <- pacman.data$CO_mV
  pacman.data$CO_mV[!pacman.data$CO_ok] <- NA
  
  # Inverting CO2
  pacman.data$CO2_mV <- -1 * pacman.data$CO2_mV
  
  
  return(pacman.data)
}



#tempPACMAN corrects the PM signal for the effects of temperature by detrending on 1/Temperature_IN_C #
tempPM <- function(pacman.xts, pacman.bg){
  tmodel <- lm(PM_mV ~ Temperature_IN_C, data = pacman.bg)
  pacman.xts$Dust<- pacman.xts$PM_mV - predict(tmodel, newdata = pacman.xts)
  return(pacman.xts)
}

#The sensor used by PACMAN operates in cycles and only one data point per cycle is valid.
#PACMAN reports the CO_status flag that indicates the position in the cycle that the sensor is at.
#The valid data point correspond to the end of the CO_status labelled 2
coValid <- function(pacman.data){
  nrecs=length(pacman.data$COstatus)
  pacman.data$CO_ok <- FALSE
  pacman.data$CO_ok[1:nrecs-1] <- (pacman.data$COstatus[-1]==1)&(pacman.data$COstatus[1:nrecs-1]==2)
  pacman.data$CO_raw <- pacman.data$CO_mV
  pacman.data$CO_mV[!pacman.data$CO_ok] <- NA
  return(pacman.data)
}

# censor data corresponding to fast changes in 1 s sensor response 
# that lead the data to be "spikey"
pacmanClean1s <- function(pacman.data){
  # clean PM using 1 second differences to remove troughs #
  #my.floor <- .1*mean(pacman.data$PM_mV, na.rm = T)
  
  diff <- as.numeric(pacman.data$PM_mV[-nrow(pacman.data)]) - as.numeric(pacman.data$PM_mV[-1])
  # typical response of sensor #
  noise <- 2*sd(diff, na.rm = T)
  diff.subset <- diff[complete.cases(diff)]
  diff.subset <- diff.subset[abs(diff.subset) > noise] 
  thresh <- sd(diff.subset)
  if(thresh > 5*noise){
    troughs <- which(abs(diff) > thresh)
    for(i in troughs){
      if(diff[i] > 0){
        pacman.data$PM_mV[i+1] <- NA
      } else if (diff[i] < 0){
        pacman.data$PM_mV[i] <- NA
      }
    }
  }
  my.floor <- mean(pacman.data$PM_mV, na.rm = T) * 0.1
  pacman.data$PM_mV[which(pacman.data$PM_mV < my.floor)] <- NA
  
  # censor spurious temperature readings #
  pacman.data$Temperature_IN_C[pacman.data$Temperature_IN_C > 40] <- NA
  
  return(pacman.data)
}

# pacmanReduce re-bins the 1s data by bin-entries of data #

pacmanReduce <- function(pacman.data, bin){
  #re-order so that date column is first # 
  data.shape <- pacman.data[, c('date',setdiff(names(pacman.data), 'date' )) ]
  #make sure no NAs in the index
  include <- c('date','Temperature_IN_C', 'PM_mV', 'CO2_mV', 'CO_mV', 'Movement')
  data.shape <- data.shape[is.finite(data.shape$date),include]
  #create xts object to use handy time series operations
  pacman.xts <- as.xts(data.shape[,-1], order.by = as.POSIXct(data.shape[,1]))
  # Just rebin sensor responses
  
  data.binned <- rollapply(pacman.xts, width =  bin, FUN = mean, by = bin, 
                           na.rm = T, by.column = T, align = "center")
  #data binned consists of a real valued entry every bin-entry, the remaining 1 s entries are NA
  # remove entries corresponding to NAs
  data.binned<- data.binned[!is.na(data.binned$PM_mV),]
  return(data.binned)
}



#pacmanSmooth applys a moving average smooth of bin-entries to the data #
# expects data to be class xts or zoo then censors data before the first temperature max on day 1 #
pacmanSmooth <- function(pacman.xts, bin){
  include <- c('Temperature_IN_C', 'PM_mV', 'CO2_mV', 'CO_mV', 'Movement')
  data.smoothed <- rollapply(pacman.xts[,include], width =  bin, FUN = mean, na.rm = T,
                             by.column = T, align = "center")
  return(data.smoothed)
}

# this function removes rows with missing days of data in the PM signal
# it requires 2 hours of data to be complete
# on start-up of the pacman it censors the data up to the maximum 
# temperature during the day that it was turned on
tempJumps<- function(pacman.smooth){
  #censor days with large number of missing PM observations > 90%
  # these correspond to times the box was restarted. If restarted then
  # censor to T-max of that day.
  
  PM_mV <- as.numeric(pacman.smooth$PM_mV)
  Temperature_IN_C <- as.numeric(pacman.smooth$Temperature_IN_C)
  data.sub <- data.frame(cbind(PM_mV, Temperature_IN_C))
  data.sub$justDate <- as.Date(index(pacman.smooth))
  
  my.fun <- function(x){
    return(sum(is.finite(x)))
  }
  
  checkDays <- summaryBy(PM_mV  ~ justDate, data = data.sub, FUN =  my.fun)
  
  # require that there be 2 hours of data in a day, 240 observations @30s bin .... #
  
  ind <- which(checkDays[,2] < 240)
  if(length(ind > 0 )){
    days <- checkDays$justDate[ind]
    pacman.smooth <- pacman.smooth[-which(as.Date(index(pacman.smooth)) == days), ]
  }
  
  # check for non-sequential days 
  
  gap <- which(diff(checkDays$justDate) > 1) + 1
  day1 <- as.Date(min(index(pacman.smooth), na.rm = T))
  
  restart <- c(day1, checkDays$justDate[gap])
  for(i in seq(1,length(restart))){
    day.ind <- which(as.Date(index(pacman.smooth)) == restart[i])
    new.start<- day.ind[1]+which.max(pacman.smooth$Temperature_IN_C[day.ind])
    pacman.smooth<- pacman.smooth[-seq(day.ind[1],new.start,1),]
  }
  return(pacman.smooth)
}

#pacmanBG estimates the background as a rolling percentile over window number
# of observations.
pacmanBG <- function(pacman.xts, window, nind = NULL){
  if(is.null(nind)){
  include <- c('Temperature_IN_C', 'PM_mV', 'CO2_mV', 'CO_mV', 'Movement')
  } else {
    include <- nind
  }
  
  BG <- rollapply(pacman.xts[,include], width = window, FUN = quantile, probs = 0.05,
                  na.rm = T, by.column = T, align = "center") 
  names_bg <- paste(names(BG),"_bg", sep = "")
  names(BG)<- names_bg
  output <- cbind(pacman.xts, BG)
  return(output)
}

# myfun2 is just a little function used by SegReg in the call to rollapply for
# computing the rolling regression. 
myfun2 <- function(data){
  
  data <- as.data.frame(data)
  #coef(rlm(PM_bg_clean ~ Temperature_IN_C, data = data, init = "ls", maxit = 10))
  coef(lm(PM_bg_clean ~ Temperature_IN_C_bg, data = data))
}

# this function performs a segmented rolling regression of the PM background signal
# on the background Temperature signals. It detects jumps in the background signal that
# exceed 5% of its mean in 30 s. Then it performs a rolling regression of 1 day on
# the segments every 15 minutes. It then returns the coefficients and the predicted values based
# upon the original 30s temperature data.
# it generates new variables: PM_bg_clean (background with jumps censored), 
# diff.bg (30 s first differences), PM_predicted (predicted PM signal from regression),
# PM_mV.c (30 s PM data with jumps censored), Temperature_IN_C_bg.1 (slope), X.Intercept. (intercept)
segReg <- function(pacman.bg, unit.name = "name me!!"){
  par(mfcol = c(1,1))
  #remove mV offset
  pacman.bg$PM_mV <- pacman.bg$PM_mV - min(pacman.bg$PM_mV_bg, na.rm = T)
  pacman.bg$PM_mV_bg <- pacman.bg$PM_mV_bg - min(pacman.bg$PM_mV_bg, na.rm = T)
  pacman.bg$PM_bg_clean <-pacman.bg$PM_mV_bg
    
  pacman.bg$diff.bg <-  c(diff(as.numeric(pacman.bg$PM_mV_bg), lag = 1),0)  
  offset <- mean(pacman.bg$PM_mV_bg, na.rm = T)
  #offset <- runmed(as.numeric(pacman.bg$PM_mV_bg), k = 2881*7, endrule = "constant",algorithm = "Stuetzle" )
  jumpThresh <- 10 * 54 / sd(pacman.bg$PM_mV_bg, na.rm = T)
  
  # Visually Check if we've found any jumps #
  plot(pacman.bg$PM_mV_bg, main =  unit.name, ylab = "PM, four hour 5%")
  lines(index(pacman.bg), pacman.bg$diff.bg + offset, col = "red", lwd = 2)
  sdAll <- sd(pacman.bg$diff.bg, na.rm = T)
  mtext(paste ("mean: ", round(mean(pacman.bg$diff.bg, na.rm = T), digits = 5), "; ", "sd: ", round(sdAll, digits = 2 )), side = 3, col = "red", line = -1, adj = 1)
  abline(h = jumpThresh + offset, col = "blue")
  abline(h = offset -jumpThresh, col = "blue")
  legend("topleft", c("4-hour 5%", "30 s first difference", "jump threshold"), col = c("black", "red", "blue"), lwd = 2 ) 
  
  # Checking for threshold crossings
  breaks <-which(abs(pacman.bg$diff.bg) > jumpThresh)
  
  # if we didn't have a jump, then the trace segments = 1 and life is good.
  if (length(breaks) == 0){
    starts<- 1
    stops <- nrow(pacman.bg)
  }else if (length(breaks) > 0){
    
    new.breaks <- breaks[1]
    #clean up breaks
    if(length(breaks) > 1){
      j = 1
      for(i in seq(2,length(breaks),1)){
        if(breaks[i] - new.breaks[j] > 60){
          new.breaks <- c(new.breaks, breaks[i])
          j <- j+1
        }    
      }
    }
    
    #clip 15 minutes before and after a break #  
    for(i in seq(1, length(new.breaks))){
      pacman.bg$PM_bg_clean[ (new.breaks[i]-30) : (new.breaks[i]+30 )] <- NA
    }
    
    new.breaks <- c(1, new.breaks, nrow(pacman.bg))
    points(index(pacman.bg)[new.breaks], rep(offset*1.1, length(new.breaks)), col = "blue", pch = "+", cex = 1.5)
    
    # Find the endpoints of the real data that is left
    whats.left2 <- which(is.finite(pacman.bg$PM_bg_clean))
    starts2 <- c(whats.left2[1],whats.left2[which(diff(whats.left2) != 1) +1] )
    stops2 <- c(whats.left2[which(diff(whats.left2) != 1)], whats.left2[length(whats.left2)] )
    
    
    #time difference in minutes #
    time_diff <- difftime(index(pacman.bg)[stops2],index(pacman.bg)[starts2], units = "mins")
    
    # censor periods less than 48 hours, units here are minutes #
    for(i in seq(1,length(time_diff),1)){
      if(time_diff[i] < 2880){
        pacman.bg$PM_bg_clean[starts2[i]:stops2[i]] <- NA
      }
    }
    
    # Find the endpoints of the real data that is left... again
    whats.left <- which(is.finite(pacman.bg$PM_bg_clean))
    starts <- c(whats.left[1],whats.left[which(diff(whats.left) != 1) +1] )
    stops <- c(whats.left[which(diff(whats.left) != 1)], whats.left[length(whats.left)] )
    
    # check that the censoring makes sense #
    plot(pacman.bg$PM_mV_bg, main =  unit.name, ylab = "PM, four hour 5%")
    lines(pacman.bg$PM_bg_clean, col = "blue", lwd = 2)
    legend("topleft", c("4-hour 5%", "jumps removed"), col = c("black", "blue"), lwd = 2 ) 
    points(index(pacman.bg)[starts], pacman.bg$PM_bg_clean[starts], col = "darkgreen", pch = "+", cex = 1.5)
    points(index(pacman.bg)[stops],pacman.bg$PM_bg_clean[stops], col = "red", pch = "+", cex = 1.5)
  }  
  
  #now do the regression #
  for(i in seq(1, length(starts))){
    print(i)
    peice <- pacman.bg[starts[i]: stops[i]]
    one.regression <- rollapply(peice, width =5760, by = 30, FUN = myfun2, by.column = FALSE )
    one.regression<- one.regression[!is.na(one.regression$Temperature_IN_C_bg),]
    if(i == 1){
      regress.done <- one.regression
    } else {
      regress.done<- rbind(regress.done, one.regression)
    }
  }
  
  
  # for some reason, you can't merge a vector of just time... probably not finding the right function
  # make a dummy variable. We need to fill in the original time series with the regression
  # coefficients that we found!
  pacman.bg$stupid <- seq(1,nrow(pacman.bg))
  pac.time <- pacman.bg[, 'stupid']
  regress.orig <- window(na.locf(merge(pac.time, regress.done), fromLast = TRUE), index(pac.time))
  
  # now that the time series filled in, we can merge it with the other data set
  # ... removing 'stupid' variable too.
  merge.data<- cbind(pacman.bg, regress.orig[,-1])
  # here we predict the PM signal using the 30 s Temperature data data
  merge.data$PM_predicted <- as.numeric(merge.data$Temperature_IN_C) * as.numeric(merge.data$Temperature_IN_C_bg.1) + as.numeric(merge.data$X.Intercept.)
  
  # now we need to censor all the times corresponding to the jumps we detected in PM_bg_clean
  merge.data$PM_predicted[is.na(as.numeric(merge.data$PM_bg_clean))]<- NA
  merge.data$PM_mV.c <- merge.data$PM_mV
  merge.data$PM_mV.c[is.na(as.numeric(merge.data$PM_bg_clean))]<- NA
  merge.data$Temperature_IN_C_bg.1[is.na(as.numeric(merge.data$PM_bg_clean))]<- NA
  merge.data$X.Intercept.[is.na(as.numeric(merge.data$PM_bg_clean))]<- NA
  
  return(merge.data)
}


pacmanBGv2 <- function(pacman.xts, window, nind = NULL, pmThresh){
  if(is.null(nind)){
    include <- c('Count', 'Distance', 'Temperature_mV','Temperature_IN_C', 'PM_mV', 'CO2_mV', 'CO_mV', 'Movement')
  } else {
    include <- nind
  }
  
  filtered <- as.numeric(pacman.xts[,nind])
  
  filtered[filtered > pmThresh] <- NA
  pacman.xts[,nind] <- filtered
  
  BG <- rollapply(pacman.xts[,include], width = window, FUN = quantile, probs = 0.05,
                  na.rm = T, by.column = T, align = "center") 
  return(BG)
}

#function to plot the output of the regressions #
plotRegress <- function(pacman.reg, unit.name = "name me!" ){
  # plot the regression coefficients 
  par(mfcol = c(2, 1))
  par(mar = c(4,5,1,.5))
  plot(pacman.reg$Temperature_IN_C_bg.1,  main = paste(unit.name, "Slope"), ylab =  "Slope, m * T")
  plot(pacman.reg$X.Intercept., main = paste(unit.name, "Intercept"), ylab = "Intercept" )
  
  #Plot the fit 
  
  par(mfcol = c(1, 1))
  plot(pacman.reg$PM_mV.c, type = "p", pch = 19, cex = .25, main = unit.name, ylab = "PM/ mV (30 s binned + 90 s smooth)" )
  points(pacman.reg$PM_predicted, pch = 19, col = "blue", cex = .25)
  
  # plot the residuals, complete and zoomed 
  par(mfcol = c(2, 1))
  plot(index(pacman.reg$PM_mV), as.numeric(pacman.reg$PM_mV.c) - as.numeric(pacman.reg$PM_predicted),
       type = "l", main = paste(unit.name, "detrended (full)"), ylab = "PM mV, 30s bin + 90s smooth",
       xlab = "")
  abline(h = 0, col = "red")
  
  plot(index(pacman.reg$PM_mV), as.numeric(pacman.reg$PM_mV.c) - as.numeric(pacman.reg$PM_predicted), 
       type = "l", main = paste(unit.name, "detrended (zoom)"), xlim = c(index(pacman.reg$PM_mV)[5800*1],index(pacman.reg$PM_mV)[5800*3] ),
       ylab = "PM mV, 30s bin + 90s smooth", xlab = "")
  abline(h = 0, col = "red")
  par(mfcol = c(1, 1))
}


# expects several data sets cleaned data #
pacmanTempSummary <- function(pacman.bg,myname){
  par(mar = c(3,3,1,.5))
  par(oma = c(1,1,2,1) )
  par(mgp = c(1.5,.5,0))
  split.screen(figs = c(2,1))
  split.screen(figs = c(2,1), screen = 1)
  split.screen(figs = c(2,2), screen = 2)
  
 
  # time series plots of smoothed data overlaid with background estimation colored by date for PM and Temperature.
  screen(3)
  plot(index(pacman.bg), as.numeric(pacman.bg$PM_mV), type = "l", ylab = "PM mV ", xlab = "", main = "") 
  points(pacman.bg$PM_mV_bg, col =colorByDate(index(pacman.bg)), pch = 19, cex =0.1, cex.axis = .75)
  mtext(myname, side = 3)
  
  screen(4)
  plot(index(pacman.bg), as.numeric(pacman.bg$Temperature_IN_C),type = "l", ylab = "Temp C", xlab = "", main = "")
  points(pacman.bg$Temperature_IN_C_bg, col =colorByDate(index(pacman.bg)), pch = 19, cex =0.1, cex.axis = .75)
  
 
  PM_mV <- as.numeric(pacman.bg$PM_mV)
  Temperature_IN_C <- as.numeric(pacman.bg$Temperature_IN_C)
  pacman.bin1 <- data.frame(cbind(PM_mV, Temperature_IN_C))
  pacman.bin1$justDate <- as.Date(index(pacman.bg))
  byday <- summaryBy(PM_mV ~ justDate + Temperature_IN_C, data = pacman.bin1, FUN = quantile, probs = 0.1, na.rm = T)
  byday <- byday[complete.cases(byday),]
  
  # regression models, each models has a least-squares and a robust regression 
  # where points with more influence over the coeffiecients are down-weighted
  
  # Model 1: Use a quantile (prob = 0.1) of the PM signal for each temperature (in .25 c increments)
  by.dayls <-lm(byday[,3]~ byday[,2])
  #by.dayrlm <- rlm(byday[,3]~ byday[,2], init = "ls" )
  by.day.summ <- summary(by.dayls)
  # model 2: Use the background estimated PM signal against the background estimated Temperature
  
  bg.ls <- lm(PM_mV_bg~ Temperature_IN_C_bg, data = pacman.bg)
  #bg.rlm <- rlm(PM_mV_bg~ Temperature_IN_C_bg, data = pacman.bg, init = "ls" )
  bg.summ <- summary(bg.ls)
  
  #model 3: Use the background estimated PM signal and the smoothed Temp signal
  smT.ls <- lm(PM_mV_bg~ Temperature_IN_C, data = pacman.bg)
  smT.summ <- summary(smT.ls)
  
  # model 4: Use the smoothed PM and smoothed Temperature signals
  sm.ls <- lm(PM_mV~ Temperature_IN_C, data = pacman.bg)
  #sm.rlm <- rlm(PM_mV~ Temperature_IN_C, data = pacman.bg, init = "ls" )
  sm.ls <- lm(PM_mV~ Temperature_IN_C, data = pacman.bg)
  sm.summ <-  summary(sm.ls)
  # scatter plots and regressions for various approaches #
  # MODEL 1
  screen(5)
  plot(byday[,2], byday[,3], col = colorByDate(byday$justDate), xlab = "Temp, C", ylab = "PM mV, 10% T-bin",
       main = "", cex = .7, cex.lab = .75)
  
  by.dayrlm.text <- paste("slope =",round(by.day.summ$coefficients[2,1], digits = 2),
                      " std err =", round(by.day.summ$coefficients[2,2], digits = 2), sep = " " )
  by.daycor <- paste("pearson r:", round(cor(as.numeric(byday[,3]),as.numeric(byday[,2]), use = "pairwise.complete.obs"), digits = 2))
  mtext(paste(by.daycor,", ", by.dayrlm.text, sep = ""), side = 3, cex = .75)
  abline(coef(by.dayls))
  
  
  # MODEL 2
  screen(6)
  plot(as.numeric(pacman.bg$Temperature_IN_C_bg), as.numeric(pacman.bg$PM_mV_bg), 
       col = colorByDate(index(pacman.bg)), xlab = "Temp, C (4 hour 5%)", ylab = "PM mV,(4 hour 5%) ", main = "", cex = .25, cex.lab = .75)
  
  bg.rlm.text <- paste("slope =",round(bg.summ$coefficients[2,1], digits = 2),
                          " std err =", round(bg.summ$coefficients[2,2], digits = 2), sep = " " )
  bg.cor <- paste("pearson r:", round(cor(as.numeric(pacman.bg$Temperature_IN_C_bg),
                                             as.numeric(pacman.bg$PM_mV_bg),
                                             use = "pairwise.complete.obs"), digits = 2))
  mtext(paste(bg.cor,", ", bg.rlm.text, sep = ""), side = 3, cex = .75)
  abline(coef(bg.ls), lwd = 2)
  
  
  # MODEL 3
  screen(7)
  plot(as.numeric(pacman.bg$Temperature_IN_C), as.numeric(pacman.bg$PM_mV_bg), col = colorByDate(index(pacman.bg)),
       xlab = "Temp, C (30 s bin)", ylab = "PM mV (4 hour 5%)", main = "", cex = .25, cex.lab = .75)
  
  smT.rlm.text <- paste("slope =",round(sm.summ$coefficients[2,1], digits = 2),
                       " std err =", round(sm.summ$coefficients[2,2], digits = 2), sep = " " )
  smT.cor <- paste("pearson r:", round(cor(as.numeric(pacman.bg$Temperature_IN_C),
                                          as.numeric(pacman.bg$PM_mV_bg),
                                          use = "pairwise.complete.obs"), digits = 2))
  mtext(paste(smT.cor,", ", smT.rlm.text, sep = ""), side = 3, cex = .75)
  abline(coef(smT.ls), lwd = 2)
  
  
  # MODEL 4
  screen(8)
  plot(as.numeric(pacman.bg$Temperature_IN_C), as.numeric(pacman.bg$PM_mV), col = colorByDate(index(pacman.bg)),
       xlab = "Temp, C (30 s bin)", ylab = "PM mV (30 s bin)", main = "", cex = .25, cex.lab = .75)
  
  sm.rlm.text <- paste("slope =",round(sm.summ$coefficients[2,1], digits = 2),
                       " std err =", round(sm.summ$coefficients[2,2], digits = 2), sep = " " )
  sm.cor <- paste("pearson r:", round(cor(as.numeric(pacman.bg$Temperature_IN_C),
                                          as.numeric(pacman.bg$PM_mV),
                                          use = "pairwise.complete.obs"), digits = 2))
  mtext(paste(sm.cor,", ", sm.rlm.text, sep = ""), side = 3, cex = .75)
  abline(coef(sm.ls), lwd = 2)
  
  
  close.screen(all = T)  
}

#color plot points by date 
colorByDate <- function(time_vector){
  my.dates <- as.Date(time_vector)
  n <- max(as.numeric(my.dates), na.rm = T) - min(as.numeric(my.dates), na.rm = T) + 1
  color.date<- as.numeric(my.dates) - min(as.numeric(my.dates), na.rm = T) + 1
  my.colors <- rainbow(n)[color.date]
}

