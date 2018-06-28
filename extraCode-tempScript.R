# Evan Waldmann
# 6/28/18 
# land-use-FireResponseToRun.R Extra Code 


#old NA code (just in case)
#old NA code delete later ----
# values(dR)[is.na(values(dR))] <-0
# values(cutHistory)[is.na(values(dR))] <-0
#more old na code 
# values(dR)[which(values(dR)>1) %in% c(wildernessCells ,unforestedCells)] <- 1 
# values(dR)[which(values(dR)>1) %in% recentCuts] <-
# values(dR)[which(values(dR)>0) %in% outOfBoundsCells] <-0
# plot(dR)

#these lines are probably crap ----  
#which( values(slopeRaster)[adjacent(dR, cells, directions=16, pairs=F, target=NULL, sorted=T,  include=FALSE, id=FALSE)] > 30 )

#values(dR)[ values(slopeRaster)[adjacent(dR, cells, directions=16, pairs=F, target=NULL, sorted=T,  include=FALSE, id=FALSE)] > 30 ] <- 1020 
#values(dR)[which( values(slopeRaster)[adjacent(dR, cells, directions=16, pairs=F, target=NULL, sorted=T,  include=FALSE, id=FALSE)] < 30 )] <- 1021


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
  
  # image <- im(matrix(values(severityRaster), 310, 312))
  # ci <- connected(image)
  # ci <- connected(image, method= "C", background = 0)
  # plot(ci)
  # plot(severityRaster)
  # tR <- severityRaster
  # values(tR) <- as.integer(ci$v)
  # plot(tR)
  # freq(tR)
  # writeRaster(tR, filename ="test.img", format="HFA", dataType="INT2S")
  # 

  freq(fuelRaster)
  # fuelVal in 1:12. 
  fuelVal = 4
  i<-0
  for (fuelVal in 1:7){
    i<- i+1
  tR <- fuelRaster
  destroys <- which(values(tR != i))
  values(tR)[destroys] <- 0 
  
  imageF <- im(matrix(values(tR), 310, 312))
  plot(imageF)

  ci <- connected(imageF, method= "C",background = 0)
  plot(ci)
  
  values(tR) <- as.integer(ci$v)
  plot(tR)
 # freq(tR)
  
  fq <-as.data.frame( freq(tR))
  
  #TODO -- weird error -----

  nfq<-na.omit(fq[(fq$count>20 & fq$count<12000),])
  nfq<- nfq[order(-nfq$count),]
  nfq<- cbind(nfq, rank=length(nfq$value):1 )
  print(nfq)
  
  temp <- values(tR)
  mapper <- function (x){ y<-nfq$rank[which(nfq$value== x)];  if(length(y) == 0){return(NA) }else{return(y)}} #which returns integer(0) on not found, we need NA 
  temp2 <- sapply(temp, mapper)
  
  hist(temp2)
  tR1 <- fuelRaster
  replace<-!is.na(temp2)
  values(tR1)[replace]<- temp2[replace]*(10^i)
  
  }
  plot(tR1)
  
  # take the fuels that are connected to 20 or more for each fuel type and tag them... 
  
  
  
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