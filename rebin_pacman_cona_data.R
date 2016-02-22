# This R-script opens the cona pacman data, applies some cleaning proceedures and then re-bins
# the data to 30s and saves the 30 s data files.

setwd("~/simpsonlab/PACMAN/Riley_pacman")
source("Pacman_functions.R")

cona.dir <-"~/simpsonlab/PACMAN/Complete CONA/PACMAN"

openCleanBinWrite <- function(cona.dir, file.in, file.out){
  # open pacman file apply basic data adjustments on CO2 and CO
  unit.data<- readConaPACMAN(paste(cona.dir, "/", file.in, sep = ""))
  #clean 1 s data
  unit.c <- pacmanClean1s(unit.data)
  # bin to 30 seconds. Keeps only new  date/time, Temperature, PM, CO2, CO,and movement data 
  unit.bin <- pacmanReduce(unit.c, bin = 30)
  #writes file to disc.
  write.zoo(unit.bin, file = file.out, index.name = "date", sep = ",")  
}

system.time({
openCleanBinWrite(cona.dir, file.in = "pacman_11.txt", file.out = "pacman_unit11_30s.csv")
})

system.time({
  openCleanBinWrite(cona.dir, file.in = "pacman_10.txt", file.out = "pacman_unit10_30s.csv")
})

system.time({
  openCleanBinWrite(cona.dir, file.in = "pacman_12.txt", file.out = "pacman_unit12_30s.csv")
})

system.time({
  openCleanBinWrite(cona.dir, file.in = "pacman_13.txt", file.out = "pacman_unit13_30s.csv")
})

system.time({
  openCleanBinWrite(cona.dir, file.in = "pacman_16.txt", file.out = "pacman_unit16_30s.csv")
})

system.time({
  openCleanBinWrite(cona.dir, file.in = "pacman_17.txt", file.out = "pacman_unit17_30s.csv")
})

system.time({
  openCleanBinWrite(cona.dir, file.in = "pacman_C1.txt", file.out = "pacman_unitC1_30s.csv")
})

system.time({
  openCleanBinWrite(cona.dir, file.in = "pacman_C2.txt", file.out = "pacman_unitC2_30s.csv")
})

system.time({
  openCleanBinWrite(cona.dir, file.in = "pacman_18.txt", file.out = "pacman_unitC18_30s.csv")
})