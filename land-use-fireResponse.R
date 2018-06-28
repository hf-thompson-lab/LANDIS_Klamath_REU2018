pal <- colorRampPalette(c("white", "red", "yellow","green", "blue", "purple"))

start_time <- Sys.time()
print(start_time)

library(raster)
library(rgdal)
library(rgeos)

#set working directory
workingDirectoryPath <- "C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty"
setwd(workingDirectoryPath)
print(getwd())

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

#read in rasters 
luMaster <- raster(landUseMasterPath)
slopeRaster <- raster(slopePath)
developRaster <- raster(developPath)
severityRaster <- raster(severityPath)
fuelRaster <- raster(fuelPath)
timeRaster <- raster(timePath)

print("Rasters Read!")





# Section set up ----

# we can grab the unforested areas from the fuels and delete them

#unforested areas are 1 , out of bounds areas are 0 
setupRaster <- fuelRaster
suR <- values(setupRaster)
suR[suR==0] <- NA 
suR[suR==1] <- NA

values(setupRaster) <- suR
plot(setupRaster)



library(spatstat)
imageF <- im(matrix(values(setupRaster), 310, 312))
plot(imageF)
ci <- connected(imageF, method= "C")
plot(ci)

tR1 <- fuelRaster
values(tR) <- as.integer(ci1$v)
plot(tR)
tempFreq <- freq(tR)
largeCCs <- tempFreq[which(tempFreq[,2]>20),1]
"%nin%" <- function(x, table) match(x, table, nomatch = 0L) == 0L #opposite of %in% 
values(tR)[values(tR) %nin% largeCCs] <- -1

largeCCs
values(tR)==largeCCs[4]
temp<- fuelRaster
values(temp)[values(tR)==largeCCs[4]] <- (values(fuelRaster)[values(tR)==largeCCs[4]])

largeCCs
unique(values(fuelRaster)[values(tR)==largeCCs[8]])
tempFreq

cellsTest <- which(values(tR) == largeCCs[3])
values(tR)[cellsTest] <- 100
possibleCuts <- numeric(length(cellsTest))
for (i in 1:length(cellsTest))
{

  adjCells <-adjacent(tR, cellsTest[i], directions=8, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)
  sct = sum(adjCells %in% cellsTest) 
  if (sct>1 & sct<5)
  {
    possibleCuts[i] = cellsTest[i]
  }
}
pc <- possibleCuts[which(possibleCuts >0)]

pc

setupRaster <- fuelRaster
suR <- values(setupRaster)
suR[suR==0] <- NA 
suR[suR==1] <- NA
suR[pc] <- NA
values(setupRaster) <- suR
plot(setupRaster)

library(spatstat)
imageF2 <- im(matrix(values(setupRaster), 310, 312))
plot(imageF2)
ci2 <- connected(imageF2, method= "C")
plot(ci2)

tR2 <- fuelRaster
values(tR2) <- as.integer(ci2$v)
plot(tR2)
tempFreq2 <- freq(tR2)
largeCCs2 <- tempFreq2[which(tempFreq2[,2]>20),1]
"%nin%" <- function(x, table) match(x, table, nomatch = 0L) == 0L #opposite of %in% 
values(tR)[values(tR) %nin% largeCCs] <- -1




plot(tR)
## ToDo # rank the values in this tR rasker down somehow... ---- 


writeRaster(tR, filename ="test.img", format="HFA", dataType="INT2S")

values(tR)[values(tR) ==1] <- 400
plot(tR)
freq(tR)

# plot(luMaster)
# plot(slopeRaster)
# plot(severityRaster)
# plot(fuelRaster)
# plot(developRaster)

# max cells to be cut 

#looking at biomass to see if it is dropping. 
#path<- "C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/output/biomass/biomasseco.txt"
#dat <- read.table(path, header=T )
#library(reshape2)
#mdat <- melt(dat, id="Time")
#plot(mdat)
#library(ggplot2)
#ggplot(mdat, aes(x=Time, y=value, color=variable)) + geom_point()
#head(mdat)

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

# variable that gets severity of fire with in proximity to development 
#ideally we can order the severities here and then figure out where fire is closest to which developed area (or fire management area)
cells <- which(values(developRaster) > 581)
neighborhoodMatrix <- matrix(1, ncol=13, nrow = 13) # not sure on the sizing here 
neighborhoodMatrix[7,7] <- 0

rm(sumTable)
sumTable <- data.frame(cell=numeric(length(cells)), mean=numeric(length(cells)), median=numeric(length(cells)))
sV <- values(severityRaster)
for (i in 1:length(cells))
{
  adj <- adjacent(developRaster, cells[i], directions=neighborhoodMatrix, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)
  sumTable[i,]<-c(cells[i], mean(sV[adj], na.rm=T), median(sV[adj], na.rm = T))
}
sumTable <- sumTable[order(-sumTable$mean),]

stR <- severityRaster
#where 4 is the mean and median threshold for action. 
values(stR)[adjacent(developRaster, sumTable$cell[sumTable$mean>4 | sumTable$median>4], directions=neighborhoodMatrix, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)] <- 10
## TODO -- #exclude cells that are also unforest areas !!! ====
plot(stR)

#this is cutting in areas surrounding development ----
dR <- developRaster
values(dR)[which(values(developRaster) < 580)] <- 1
#values(dR)[which(values(developRaster) == 581)] <- 10
plot(dR)

freq(dR)

neighM <- matrix(1, ncol=5, nrow = 5) # not sure on the sizing here 
neighM[3,3] <- 0

#(dR)[which(values(developRaster) > 581)]
# cells that have development values 
cells <- which(values(developRaster) > 581)

#cells that are surronding the developed zones (cut more around medium and high and less around low and open)
#values(dR)[adjacent(dR, cells, directions=16, pairs=F, target=NULL, sorted=T,  include=FALSE, id=FALSE)] #<- 589

df <- data.frame(cellNum=adjacent(dR, cells, directions=neighM, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T), slopes=values(slopeRaster)[adjacent(dR, cells, directions=neighM, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)]) 
#which( values(slopeRaster)[adjacent(dR, cells, directions=16, pairs=F, target=NULL, sorted=T,  include=FALSE, id=FALSE)] > 30 )

values(dR)[subset(df, df$slopes <30)$cellNum] <- 1020
values(dR)[subset(df, df$slopes >30)$cellNum] <- 1021 

#take into account slope. 
#values(dR)[ values(slopeRaster)[adjacent(dR, cells, directions=16, pairs=F, target=NULL, sorted=T,  include=FALSE, id=FALSE)] > 30 ] <- 1020 
#values(dR)[which( values(slopeRaster)[adjacent(dR, cells, directions=16, pairs=F, target=NULL, sorted=T,  include=FALSE, id=FALSE)] < 30 )] <- 1021

values(dR)[which(values(dR) < 585)] <-0 # set the development areas to be cut??? 
values(dR)[which(values(dR) == 1020)] <-20 
values(dR)[which(values(dR) == 1021)] <-21 
values(dR)[which(values(dR) == 0)] <-1



# plot(dR)
# 
# freq(dR)
# 
# typeof(values(dR))
# 
# values(dR) <- as.integer(values(dR))




dataType(dR)
typeof(values(dR))

# if fire was super bad? 
# cutting burnt forests ----
# getwd()
# severityPath <- "Saved Output/PnET+Fire+Fuel+A2Climate 50Years/fire"
# fls <- list.files(file.path(getwd(), severityPath), "ty-", full.names = T)
# 
# severityRaster <- raster(fls[1])
# plot(severityRaster)
# 
# values(severityRaster)

cells <- which(values(severityRaster) > 2 & values(severityRaster)<6)
hist(values(severityRaster)[cells])

#df
rm(df)
df <- data.frame(cellNum=adjacent(dR, cells, directions=8, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T), slopes=values(slopeRaster)[adjacent(dR, cells, directions=8, pairs=F, target=NULL, sorted=T,  include=FALSE, id=T)]) 

values(dR)[subset(df, df$slopes <30)$cellNum] <- 1020
values(dR)[subset(df, df$slopes >30)$cellNum] <- 1021 
plot(dR)
plot(severityRaster)
#values(dR)[which(values(dR) < 585)] <-0 # set the development areas to be cut??? 
values(dR)[which(values(dR) == 1020)] <-30 
values(dR)[which(values(dR) == 1021)] <-31 

plot(dR)

# Section - change navalue, output plots, write land-use raster ---- 
values(dR)[is.na(values(dR))] <-0

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

# Section Extra code (not run) ----
if (FALSE){
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
print(total_time)