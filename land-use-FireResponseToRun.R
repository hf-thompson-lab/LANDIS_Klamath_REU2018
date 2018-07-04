# Evan Waldmann 

## Section - hard coded value definitions & rules that are used to generate cuts ----
# raster temp values: 
# 0  -- NA values 
# 1  -- forest growth = no cuts 
# 20 -- development proximity cuts
# 21 -- steep development proximity cut
# 30 -- fire salvage cut
# 31 -- steep slope fire salvage cut
# 155 -- clear cut for fuel break on private land
# 156 -- clear cut for fuel break on FS land 

# rules:  
# 30 percent slope is the max for mechanical thinning - uplands
# right now greater than 54 degree slope will not be cut. 

# mechanical treatments would be used to prevent fire from entering land near developed area. 
# cuts are assumed to last 15 years and after 15 years the area is eligible to cut again
# cut limit per year is 4% of total active cells - this is then divided between FS and private cut limits 
    # in a bad year the FS be able to cut an extra 50 to 200 cells #not implemented yet 

# land around developed area will be cut if there is enough fire within 415.53ha circle of the development (not sure on the size)
#                               (9X9 cell search for areas around development that >=40.35% [23/57] of the cells have fire on them) 

# post fire salvage logging in areas that were not completely destroyed (I am unsure of the quantity of this cutting - might need to turn it down)

# fuel breaks implemented to clear cut in order to divide large groups of connect fuel types. 

# Section - start setup ----
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
severityHistoryPath <- paste0("logs/severityHistory.img")
dataPerTimeStepPath <- paste0("logs/trendsPerTimeStep.csv")

#read in rasters 
luMaster <- raster(landUseMasterPath)
slopeRaster <- raster(slopePath)
developRaster <- raster(developPath)
severityRaster <- raster(severityPath)
fuelRaster <- raster(fuelPath)
timeRaster <- raster(timePath)

#create or read in history rasters  
if (timestep==1)# generate new cutHistory 
{
  cutHistory <- luMaster
  values(cutHistory)[!(is.na(values(luMaster) ))] <- -1
  
  severityHistory <- luMaster
  values(severityHistory)[!(is.na(values(luMaster) ))] <- -1
  
  #overwrite log file and trendsperTimeStep file 
  write(" ", paste0("logs/logfile",".txt"), append=F)
  write.table( data.frame(TimeStep= numeric() , CellsCut = numeric(),FSCuts= numeric(), PrivateCuts= numeric(), CutsNearDevelopment = numeric()  , CutsForPostFireSalvage = numeric(), rejectedProximityCuts=numeric(), rejectedSalvageCuts= numeric(), CellsWithFire=numeric()) ,  file=dataPerTimeStepPath ,  append = F,  sep=',',  row.names=F,  col.names=T )
}else #read in cutHistory  
{
  cutHistory <- raster(cutHistoryPath)
  severityHistory <- raster(severityHistoryPath)
}

print("Rasters Read!")

ONLYFIREBREAK <-F # if TRUE it skips over the proximity to development and salvage cutting. 
PERMANENTFSFIREBREAKS <- F # if TRUE this forces FS land to use the same  fire break cuts
# end of setup. 


# Section - cut limits and determining inactive cells and prohibited cells from cut history ---- 

# set up limits for cutting on private and forest service
#total cells active cells -- 300*312 - length(inSquareInactiveCells) - length(outOfSquareCells) = 54271  # this might be wrong it might be  57391
# 54271*.05 = 2714 
maxPercentCut <- .04 #max percent of cells that will be cut each year 
maxCutCells<- 54271 * maxPercentCut 
fsLimit<- floor(maxCutCells * 0.6766776) #(30414+12080)/(14464+5836+4 +30414+12080) #these numbers come from freq(luMaster)
privateLimit <- floor(maxCutCells* 0.3233224) # (14464+5836+4)/(14464+5836+4 +30414+12080)

# we can grab the unforested areas from the fuels and delete them later
#unforested areas are 1 , out of bounds areas are 0 
setupRaster <- fuelRaster
suR <- values(setupRaster)

outOfSquareCells <- which(is.na(values(luMaster)))  # out of square cells 
outOfBoundsCells <- which(suR ==0) # inactive sites and out of square cells 
inSquareInactiveCells <- (setdiff(outOfBoundsCells, outOfSquareCells )) # insquare cells that are inactive 
unforestedCells <- which(suR == 1) # un forested cells 
wildernessCells <- which(values(luMaster)== 1111) # | values == 111 # both values are wilderness area cells (111 is only 4 cells though) 


#delete cuts that are older than 15 years 
recentCuts <-  which( !(values(cutHistory) < timestep-14) & values(cutHistory)> 0) # cuts that happened recently and dont need to be cut again 

dontCuts <-unique( c( unforestedCells, wildernessCells, recentCuts)) #set to 1 [forest] later (in land use map) 
DestroyCellsThatShouldBeNA <- outOfBoundsCells #set to 0 [NA] later (in land use map)

linesLog <- c(linesLog, paste("OoB #:", length(outOfBoundsCells)), paste("WA #:", length(wildernessCells)),paste("UF #:", length(unforestedCells)))


if (!ONLYFIREBREAK){
# Section - identifying cells with fire that is within a ciclre of 9 cells (415.53 ha) from developed areas ----

# metadata for developRaster if you want it #developed areas # 580 -584 
#attrTablePath <- "clippedRaster/land-use/GAP_LANDFIRE_National_Terrestrial_Ecosystems_2011_Attributes.csv"
#attrTable <- read.table(attrTablePath, header = T, sep=",");clippedTable <- attrTable[which(attrTable$Value %in% vals),];clippedTable[c(1,19)]

# get severity of fire with in proximity to development 
#ideally we can order the severities here and then figure out where fire is closest to which developed area (or fire management area)

cells <- which(values(developRaster) > 580) #581 is developed open space 582-584 are low to high intensity of development
neighborhoodMatrix <- matrix(1, ncol=9, nrow = 9) # not sure on the sizing here 415.53ha circle 
neighborhoodMatrix[5,5] <- 0

# this gets rid of the boxy fire response cuts  
nm <- neighborhoodMatrix
corners <- c(1:3, 6:9)
for (r in corners)
{
  for (c in corners)
  {
    if (( r+c ==2 | (r+c==3) | (r+c ==4) |abs(r-c) ==8 | (abs(r-c)==7) | (abs(r-c) ==6) |r+c ==16 | (r+c==18) | (r+c ==17)) )
      nm[r,c] <- NA
  }
}
neighborhoodMatrix <- nm

sumTable <- data.frame(cell=numeric(length(cells)), mean=numeric(length(cells)), median=numeric(length(cells)), count=numeric(length(cells)) )
sV <- values(severityRaster)
for (i in 1:length(cells)) # this could be sped up 
{
  adj <- adjacent(developRaster, cells[i], directions=neighborhoodMatrix, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)
  sumTable[i,]<-c(cells[i], mean(sV[adj], na.rm=T), median(sV[adj], na.rm = T), sum(sV[adj] > 2))
}
sumTable <- sumTable[order(-sumTable$mean),]

#where 2 is the mean and median and 23/57 cells need fire as thresholds for cutting.  
proximityToDevelopmentCells <- adjacent(developRaster, sumTable$cell[sumTable$mean>2 | sumTable$median>2 | sumTable$count >= 23], directions=neighborhoodMatrix, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)
print(paste(length(proximityToDevelopmentCells), "cells identified because of fire that is near developed cells"))

linesLog <- c(linesLog, paste(length(proximityToDevelopmentCells), "cells identified because of fire that is near developed cells"))


#Section - cutting in areas surrounding development ----
dR <- developRaster
values(dR)[which(values(developRaster) < 580)] <- 1  #we only care about the space labeled as developed which is 580-584

neighM <- matrix(1, ncol=3, nrow = 3) # clear within a quarter of a mile of important structures -- from SISKIYOU COUNTY WILDFIRE PROTECTION PLAN APRIL 23, 2008
neighM[2,2] <- 0
cells <- proximityToDevelopmentCells

#take into account slope. 
rm(adj)
adj<- adjacent(dR, cells, directions=neighM, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)
df <- data.frame(cellNum=adj, slopes=values(slopeRaster)[adj]) 
df <- rbind(df,data.frame(cellNum=cells, slopes=values(slopeRaster)[cells]))

values(dR)[which(values(dR) < 585)] <- 1 # set the development areas to be forest growth
values(dR)[subset(df, df$slopes <31)$cellNum] <- 20 # regular cuts 
values(dR)[subset(df, df$slopes >30)$cellNum] <- 21 # steep cuts 
values(dR)[subset(df, df$slopes >60)$cellNum] <- 1  # if slope is too steep dont cut


# Section - Salavage logging - clear cutting (and planting?) burnt forests ----
# might need addition qualifier of "if fire was super bad" (probably best to be implemented as if fire has reach XX% of active cells)
rm(cells)
cells <- which(values(severityRaster) > 2 & values(severityRaster)<6)

rm(df)
df <- data.frame(cellNum=adjacent(dR, cells, directions=8, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T), slopes=values(slopeRaster)[adjacent(dR, cells, directions=8, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)]) 
df <- rbind(df,data.frame(cellNum=cells, slopes=values(slopeRaster)[cells]))
values(dR)[subset(df, df$slopes <31)$cellNum] <- 30 #regular cuts
values(dR)[subset(df, df$slopes >30)$cellNum] <- 31 #step cuts 
values(dR)[subset(df, df$slopes >60)$cellNum] <- 1    #too steep to cut 
values(dR)[which(values(dR) == 581)] <-1 # set the open space to grow 

developedCells <- which(values(dR) >581 &values(dR) <1000 )# developed cells that werent hit by fire... 
values(dR)[developedCells] <- 1 #set for forest to growth 


# Section - get fuel break cuts ---- 
source("FindFuelBreaks.R")
fuelBreaks <- findFuelBreaks(fuelRaster, fuelValsToConsider= (c(1:6,10,11)+1), minCellsConnect=35, numFuelsToSplit=8, numOfLargestFuelsToPickFrom=15)

#try to keep the FS cutting the same fire breaks 


# if there is a fuel break in FS land then get rid of it. 
# read in this historic fuel breaks and cut it (the recent cuts destoryer should take care of the currency problem)


#let private owners cut whatever they want so they can keep 


values(dR)[which(!is.na(values(fuelBreaks)))] <- 155
#plot(dR)
}else{#ONLYFIREBREAKS 
  source("FindFuelBreaks.R")
  fuelBreaks <- findFuelBreaks(fuelRaster, fuelValsToConsider= (c(1:6,10,11)+1), minCellsConnect=35, numFuelsToSplit=8, numOfLargestFuelsToPickFrom=15)
  values(fuelBreaks)[which(!is.na(values(fuelBreaks)) )] <- 155 
  values(fuelBreaks)[which(is.na(values(fuelBreaks)) )]<-0
  dR <- fuelBreaks
}


# Section - change navalue, exclude prohibited cuts, 

#curious about how many cuts were proposed in inactive and prohibited sites 
prepostFireClearing <- sum(values(dR)==30, na.rm = T)+sum(values(dR)==31, na.rm = T)
preproximityClearing <- sum(values(dR)==20, na.rm = T)+sum(values(dR)==21, na.rm = T)

values(dR)[DestroyCellsThatShouldBeNA] <- 0  # get rid of prohibited cuts  
values(dR)[dontCuts] <-1 # exclude cuts in cells that are unforested, wilderness areas, or that have been cut recently 

# get ownership data for permenant fire breaks and applying cut limit to remaining cuts 
cellsCut <- which(values(dR)>1)
ownerDF <- data.frame(cell= cellsCut,owner= as.integer(values(luMaster)[cellsCut]/1000)) #forest service owned land is over 1000. 
FS <- subset(ownerDF, owner==1)$cell
pri <- subset(ownerDF, owner==0)$cell

#change FS fuel break cutting code 
values(dR)[intersect(which(values(dR)==155), FS)] <- 156 

#TODO- Section - clear fire breaks on federal land and put in the permanant ones. 
if (PERMANENTFSFIREBREAKS)
{
  if (timestep == 1 || sample(1:100, 1) == timestep){ # store the cuts on the first time step or if there was a stange incident (1% chance). 
    permanentFuelBeaks <- which(values(dR) == 156)
    
    #this would be used to store the cuts (that would be in FS)
    cat(permanentFuelBeaks, file="land-use-maps/permanentFSCuts.txt", sep=", ", append=T) # file =""
    cat("", file="land-use-maps/permanentFSCuts.txt", sep="\n", append=T) # file =""
  }
  
  values(dR)[which(values(dR) == 156)] <- 1 # Clear fire breaks (set to forest growth)
  
  if (timestep %% 15 == 1){ # FS only cuts every 15 years 
    
    #read in the permanent cuts 
    rawdat <- readLines("land-use-maps/permanentFSCuts.txt")
    permanentCutCells <- as.numeric(unlist(strsplit(rawdat, ", ")))
    
    values(dR)[permanentCutCells] <- 156 # cut the permanent cuts
    
    # Finally make sure that none of the added cuts were on the wilderness area 
    values(dR)[wildernessCells] <-1  # this is probably extraneous depending on the permanent FS cuts are determined
  }
}


# Section - Apply cut limit. 
#if over either of the limits then random sample to delete proposed cuts. 
if(length(pri) > privateLimit)
{
  numToRemove<- length(pri) - privateLimit
  values(dR)[pri[sample(1:length(pri), numToRemove)]] <- 1
  
  print(paste("Reached private cut limit. Removing",numToRemove,"proposed cuts."))
  linesLog <- c(linesLog, paste("Reached private cut limit. Removing",numToRemove,"proposed cuts."))
}

if(length(FS) > fsLimit) 
{
  numToRemove<- length(FS) - fsLimit
  values(dR)[FS[sample(1:length(FS), numToRemove)]] <- 1
  
  print(paste("Reached Forest Service cut limit. Removing",numToRemove,"proposed cuts."))
  linesLog <- c(linesLog, paste("Reached Forest Service cut limit. Removing",numToRemove,"proposed cuts."))
}



# Section - output extra logs, output plots, write land-use raster ---- 
#stores cut history 
values(cutHistory)[cellsCut] <- timestep
values(severityHistory)[which(values(severityRaster)>2)] <- timestep  #might want to only record higher severity fires 

CellsWithFire =sum(values(severityHistory) ==timestep, na.rm = T)

cutsMade<-sum(values(dR)>1, na.rm = T)

# logging cuts made to file 
print(paste0("cells cut: ", cutsMade))
linesLog <- c(linesLog, paste0("cells cut: ", cutsMade))
print(paste0("ha cut: ", cutsMade*7.29))
linesLog <- c(linesLog, paste0("ha cut: ", cutsMade*7.29))
totalCutsPossible <- 310*312 - length(outOfBoundsCells) - length(unforestedCells) - length(wildernessCells)
print(paste0("percent cut (total cells -OoB-UF-WA): ", round(cutsMade/(totalCutsPossible)*100,2), "%" ))
linesLog <- c(linesLog, paste0("percent cut (total cells -OoB-UF-WA): ", round(cutsMade/(totalCutsPossible)*100,2), "%" ))


postFireClearing <- sum(values(dR)==30, na.rm = T)+sum(values(dR)==31, na.rm = T)
proximityClearing <- sum(values(dR)==20, na.rm = T)+sum(values(dR)==21, na.rm = T)
# TODO - fix logging data -- this isnt very good right now. ====
timestepData <- data.frame(TimeStep= timestep, CellsCut = cutsMade,FSCuts=length(FS), PrivateCuts=length(pri) , CutsNearDevelopment = proximityClearing , CutsForPostFireSalvage = postFireClearing, rejectedProximityCuts= preproximityClearing- proximityClearing, rejectedSalvageCuts= prepostFireClearing- postFireClearing, CellsWithFire=CellsWithFire )

print(freq(dR))

if (TRUE){ # printing out graphs at each time step 
  somePDFPath = paste0("C:/Users/hfintern/Desktop/Results/plot-", timestep,".pdf")
  pdf(file=somePDFPath)  
  plot(severityRaster, main=paste0("Severity-", timestep))
  plot(fuelRaster, main=paste0("FuelTypes-", timestep))
  plot(dR, main=paste0("landUse-", timestep))
  dev.off() 
}

writeRaster(dR, luOutputPath, overwrite = T,format="HFA", datatype="INT2S", NAvalue=0)
writeRaster(cutHistory, cutHistoryPath, overwrite =T, format="HFA", datatype="INT2S", NAvalue=0)
writeRaster(severityHistory, severityHistoryPath, overwrite=T, format="HFA", datatype="INT2S", NAvalue=0)
write.table( timestepData,  file=dataPerTimeStepPath ,  append = T,  sep=',',  row.names=F,  col.names=F )


# end timer ----
end_time <- Sys.time()
total_time <- end_time - start_time
linesLog <- c(linesLog, paste("total time taken",total_time))
linesLog<- c(linesLog," ",paste("END OF TIMESTEP",timestep),"********************************************")
write(linesLog, paste0("logs/logfile",".txt"), append=T)
print(total_time)