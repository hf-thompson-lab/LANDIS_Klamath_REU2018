start_time <- Sys.time()
print(start_time)

linesLog <- c(paste("run at", start_time, "log:"))

library(raster)
library(rgdal)
library(rgeos)

#set working directory
workingDirectoryPath <- "C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty"
setwd(workingDirectoryPath)
print(getwd())
linesLog <- c(linesLog, getwd())

#get the current timestep
timestep <- readLines( "lockfile",n = 1)
timestep <- as.numeric(timestep)


#set up paths 
luOutputPath <- paste0("land-use-maps/land-use-", timestep, ".img")
landUseMasterPath <- "land-use-maps/combindLandUseRaster-master.img"
slopePath<- paste0("clippedRaster/slopeClip16S.img")
developPath <- paste0("clippedRaster/land-use/develop16S.img") #https://gis1.usgs.gov/csas/gap/viewer/land_cover/Map.aspx 
severityPath <- file.path("fire", paste0("severity-",timestep,".img"))
fuelPath <- file.path("fire", "fuels", paste0("FuelType-", timestep, ".img"))
timePath <- file.path("DFFS-output", paste0("TimeOfLastFire-",timestep, ".img"))
cutHistoryPath <- paste0("land-use-maps/cutHistory.img")

#read in rasters 
luMaster <- raster(landUseMasterPath)
slopeRaster <- raster(slopePath)
developRaster <- raster(developPath)
severityRaster <- raster(severityPath)
fuelRaster <- raster(fuelPath)
timeRaster <- raster(timePath)


#read in history of cuts 
if (timestep==1)# generate new cutHistory 
{
  cutHistory <- luMaster
  values(cutHistory)[!(is.na(values(luMaster) ))] <- -1
}else #read in cutHistory  
{
  cutHistory <- raster(cutHistoryPath)
}


print("Rasters Read!")


# set up limits for cutting on private and forest service
#total cells active cells -- 300*312 - length(inSquareInactiveCells) - length(outOfSquareCells) = 54271
# 54271*.05 = 2714 
privateLimit =  1114 # 1 cell = 7.29 ha = acre
fsLimit = 1600




# Section set up ----

# we can grab the unforested areas from the fuels and delete them

#unforested areas are 1 , out of bounds areas are 0 
setupRaster <- fuelRaster
suR <- values(setupRaster)
# suR[suR==0] <- NA 
# suR[suR==1] <- NA
# values(setupRaster) <- suR
# plot(setupRaster)

outOfSquareCells <- which(is.na(values(luMaster)))  # out of square cells 
outOfBoundsCells <- which(suR ==0) # inactive sites and out of square cells 
inSquareInactiveCells <- (setdiff(outOfBoundsCells, outOfSquareCells )) # insquare cells that are inactive 
unforestedCells <- which(suR == 1) # un forested cells 
wildernessCells <- which(values(luMaster)== 1111) # | values == 111 # wilderness area cells 


#delete cuts that are older than 15 years 
recentCuts <-  which(!(values(cutHistory) < timestep-14) & values(cutHistory)> 0) # cuts that happened recently and dont need to be cut again 
#hist(values(cutHistory)[recentCuts])

dontCuts <- c( unforestedCells, wildernessCells, recentCuts)
length(dontCuts)
dontCuts <- unique(dontCuts)
length(unique(dontCuts))

DestroyCellsThatShouldBeNA <- outOfBoundsCells

length(recentCuts)
length(outOfBoundsCells)
length(wildernessCells)
length(unforestedCells) # this could change depending on fires/cutting
linesLog <- c(linesLog, paste("OoB #:", length(outOfBoundsCells)), paste("WA #:", length(wildernessCells)),paste("UF #:", length(unforestedCells)))


#30 percent slope is the max for mechanical thinning - uplands
#mechanical treatments would be used to prevent fire from entering land near developed area. 


# if the developed areas have not been cut in 5-15 years depending on severities of fires 
#developed areas # 580 -584 
#attrTablePath <- "clippedRaster/land-use/GAP_LANDFIRE_National_Terrestrial_Ecosystems_2011_Attributes.csv"
#attrTable <- read.table(attrTablePath, header = T, sep=",")

#freqs<- freq(developRaster)
#vals <-sort(freqs[,1])
#vals

#clippedTable <- attrTable[which(attrTable$Value %in% vals),]
#clippedTable[c(1,19)]

# get severity of fire with in proximity to development 
#ideally we can order the severities here and then figure out where fire is closest to which developed area (or fire management area)
cells <- which(values(developRaster) > 580) #581 is developed open space 582-584 are low to high intensity of development
neighborhoodMatrix <- matrix(1, ncol=9, nrow = 9) # not sure on the sizing here 
neighborhoodMatrix[5,5] <- 0




sumTable <- data.frame(cell=numeric(length(cells)), mean=numeric(length(cells)), median=numeric(length(cells)), count=numeric(length(cells)) )
sV <- values(severityRaster)
for (i in 1:length(cells))
{
  adj <- adjacent(developRaster, cells[i], directions=neighborhoodMatrix, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)
  sumTable[i,]<-c(cells[i], mean(sV[adj], na.rm=T), median(sV[adj], na.rm = T), sum(sV[adj] > 2))
}
sumTable <- sumTable[order(-sumTable$mean),]

stR <- severityRaster
#where 4 is the mean and median threshold for action. 
proximityToDevelopmentCells <- adjacent(developRaster, sumTable$cell[sumTable$mean>2 | sumTable$median>2 | sumTable$count > 20], directions=neighborhoodMatrix, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)
print(paste(length(proximityToDevelopmentCells), "cells identified because of fire that is near developed cells"))

linesLog <- c(linesLog, paste(length(proximityToDevelopmentCells), "cells identified because of fire that is near developed cells"))
#values(stR)[proximityToDevelopmentCells] <- 10

## TODO -- #exclude cells that are also unforest areas !!! ====
#plot(stR)



#this is cutting in areas surrounding development ----
dR <- developRaster
values(dR)[which(values(developRaster) < 580)] <- 1
#values(dR)[which(values(developRaster) == 581)] <- 10

neighM <- matrix(1, ncol=3, nrow = 3) # not sure on the sizing here  #this is the sizing for cutting? 
neighM[2,2] <- 0

#(dR)[which(values(developRaster) > 581)]
# cells that have development values 
# cells <- which(values(developRaster) > 581) ## changed for proximity checking 

cells <- proximityToDevelopmentCells

#cells that are surronding the developed zones (cut more around medium and high and less around low and open)
#values(dR)[adjacent(dR, cells, directions=16, pairs=F, target=NULL, sorted=T,  include=FALSE, id=FALSE)] #<- 589

df <- data.frame(cellNum=adjacent(dR, cells, directions=neighM, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T), slopes=values(slopeRaster)[adjacent(dR, cells, directions=neighM, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)]) 
#which( values(slopeRaster)[adjacent(dR, cells, directions=16, pairs=F, target=NULL, sorted=T,  include=FALSE, id=FALSE)] > 30 )
df <- rbind(df,data.frame(cellNum=cells, slopes=values(slopeRaster)[cells]))
values(dR)[subset(df, df$slopes <30)$cellNum] <- 1020
values(dR)[subset(df, df$slopes >30)$cellNum] <- 1021 
# plot(dR)
#take into account slope. 
#values(dR)[ values(slopeRaster)[adjacent(dR, cells, directions=16, pairs=F, target=NULL, sorted=T,  include=FALSE, id=FALSE)] > 30 ] <- 1020 
#values(dR)[which( values(slopeRaster)[adjacent(dR, cells, directions=16, pairs=F, target=NULL, sorted=T,  include=FALSE, id=FALSE)] < 30 )] <- 1021

values(dR)[which(values(dR) < 585)] <-0 # set the development areas to be cut??? 
values(dR)[which(values(dR) == 1020)] <-20 
values(dR)[which(values(dR) == 1021)] <-21 
values(dR)[which(values(dR) == 0)] <-1





# might need addition qualifier of "if fire was super bad"
# clear cutting (and planting?) burnt forests ----
rm(cells)
cells <- which(values(severityRaster) > 2 & values(severityRaster)<6)
# hist(values(severityRaster)[cells])

#df
rm(df)
df <- data.frame(cellNum=adjacent(dR, cells, directions=8, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T), slopes=values(slopeRaster)[adjacent(dR, cells, directions=8, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)]) 
df <- rbind(df,data.frame(cellNum=cells, slopes=values(slopeRaster)[cells]))
values(dR)[subset(df, df$slopes <30)$cellNum] <- 2030
values(dR)[subset(df, df$slopes >30)$cellNum] <- 2031
#plot(dR)
#plot(severityRaster)

values(dR)[which(values(dR) == 2030)] <-30 
values(dR)[which(values(dR) == 2031)] <-31 

values(dR)[which(values(dR) == 581)] <-1 # set the open space to grow 

#freq(dR)
developedCells <- which(values(dR) >581 &values(dR) <1000 )# developed cells that werent hit by fire... 
#(developedCells %in% unforestedCells)
values(dR)[developedCells] <- 1

# plot(dR)

# freq(dR)
# Section - change navalue, output plots, write land-use raster ---- 

values(dR)[DestroyCellsThatShouldBeNA] <- 0 
values(dR)[dontCuts] <-1 

# values(dR)[is.na(values(dR))] <-0
# values(cutHistory)[is.na(values(dR))] <-0


#TODO # need to do getting rid of bad cuts better ====

# values(dR)[which(values(dR)>1) %in% c(wildernessCells ,unforestedCells)] <- 1 
# values(dR)[which(values(dR)>1) %in% recentCuts] <-
# values(dR)[which(values(dR)>0) %in% outOfBoundsCells] <-0
# plot(dR)

values(cutHistory)[which(values(dR)>1)] <- timestep

cutsMade<-sum(values(dR)>1)




print(paste0("cells cut: ", cutsMade))
linesLog <- c(linesLog, paste0("cells cut: ", cutsMade))
print(paste0("ha cut: ", cutsMade*7.29))
linesLog <- c(linesLog, paste0("ha cut: ", cutsMade*7.29))
totalCutsPossible <- 310*312 - length(outOfBoundsCells) - length(unforestedCells) - length(wildernessCells)
print(paste0("percent cut (total cells -OoB-UF-WA): ", round(cutsMade/(totalCutsPossible)*100,2), "%" ))
linesLog <- c(linesLog, paste0("percent cut (total cells -OoB-UF-WA): ", round(cutsMade/(totalCutsPossible)*100,2), "%" ))

print(freq(dR))
if (TRUE){ # printing out graphs at each time step 
  somePDFPath = paste0("C:/Users/hfintern/Desktop/Results/plot-", timestep,".pdf")
  pdf(file=somePDFPath)  
  plot(severityRaster, main=paste0("Severity-", timestep))
  plot(fuelRaster, main=paste0("FuelTypes-", timestep))
  plot(dR, main=paste0("landUse-", timestep))
  dev.off() 
}

#luOutputPath<- "land-use-maps/land-use-0.img" 

writeRaster(dR, luOutputPath, overwrite = T,format="HFA", datatype="INT2S", NAvalue=0)
writeRaster(cutHistory, cutHistoryPath, overwrite =T, format="HFA", datatype="INT2S", NAvalue=0); 

# Section Extra code (not run) ----
if (FALSE){
  
  #   nm <- neighborhoodMatrix 
  # corners <- c(1:4, 10:13)
  # for (r in corners)
  # {
  #   for (c in corners)
  #   {
  #     if (!( r+c ==6 | (r+c==7) | (r+c ==8) | r-c ==6 | (r-c==7) | (r-c ==8)|r+c ==20 | (r+c==21) | (r+c ==22) |-r+c ==6 | (-r+c==7) | (-r+c ==8)) )
  #       nm[r,c] <- 0 
  #   }
  # }
  # neighborhoodMatrix <- nm
  
  
  # end cutting burnt forests 
  
  # this is probably a smaller percentage of cutting forest that are outside of the national forest and owned by private owners 
  # cutting private owners ----
  #category of 1 
  
  # end cutting private owners 
  
  plot(luMaster)
  freq(luMaster)
  
  #no cutting in the the national wilderness area 
  #1111,111
  
  
  
  
  # write out raster 
  # record what was cut
  
  # 
  
  # 
  
  
  # connected components with images 
  library(raster)
  library(spatstat)
  
  image <- im(matrix(values(severityRaster), 310, 312))
  ci <- connected(image)
  ci <- connected(image, method= "C", background = 0)
  plot(ci)
  plot(severityRaster)
  tR <- severityRaster
  values(tR) <- as.integer(ci$v)
  plot(tR)
  freq(tR)
  writeRaster(tR, filename ="test.img", format="HFA", dataType="INT2S")
  
  
  imageF <- im(matrix(values(fuelRaster), 310, 312))
  plot(imageF)
  plot(fuelRaster)
  ci <- connected(imageF, method= "C")
  plot(ci)
  
  tR <- fuelRaster
  values(tR) <- as.integer(ci$v)
  plot(tR)
  freq(tR)
  writeRaster(tR, filename ="test.img", format="HFA", dataType="INT2S")
  
  values(tR)[values(tR) ==1] <- 30
  plot(tR)
  
  #0 for nonactive sites 
  #1 for active and not disturbed sites 
  #2 for burned sites where no cohorts were damaged 
  #[fire severity +2] for all disturbed sites 
  
  
  
  #fuel types?? and response to specific fuels 
  # 
  freq(fuelRaster)
  #still unsure about this what these fuel types are
  # value count
  # [1,]     0 39329   areas that are NA/out of map
  # [2,]     1 16382   unforested areas
  # the rest are the fuels plus one 
  # [3,]     2 28763  
  # [4,]     4 10171
  # [5,]     5   419
  # [6,]     6   177
  # [7,]     7   373
  # [8,]    11   784
  # [9,]    12   322
  
  
  
  minValue<- (min(values(timeRaster)))
  minValue
  values(timeRaster) <- values(timeRaster) / minValue   # reverse the time so that the highest values are the time since last fire
  # 0 are the more recent fires and 1 are the the place that havent had fire in a long time. 
  
  # values(timeRaster)[values(timeRaster)<0] <- 5 # these values are pretty much where the most recent fires where
  
  plot(timeRaster)
  
  timeRaster
  #try to change the map values for severity and fuels to see if the 
  #dynamic fire and fuels systems looks at the output maps after it wrote them.
  
}

# end timer ----
end_time <- Sys.time()
total_time <- end_time - start_time
linesLog <- c(linesLog, paste("total time taken",total_time))
linesLog<- c(linesLog," ","END","********************************************")
write(linesLog, paste0("logs/logfile",".txt"), append=T)
print(total_time)