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




# Section -- plotting fire stats 

library(ggplot2)
library(reshape2)
library(gridExtra)

setwd("C:/Users/hfintern/Desktop/")

setwd("Klamath_ForestXSiskiyouCounty/Saved Output/PnET+Fire+Fuel+A2Climate 50Years") # scenerio folder 


generateSimpleCI <- function(plot, avg, stddev, linetype, mColor, sdColor ,size) {
  ci <- plot + geom_hline(yintercept = avg, linetype=linetype, color = mColor, size=size)
  ci <- ci + geom_hline(yintercept = (avg+ 2*stddev), linetype=linetype, color = sdColor, size=size)
  ci <- ci + geom_hline(yintercept = (avg- 2*stddev), linetype=linetype, color = sdColor, size=size)
  return(ci) 
}

constrainPlot <- function(plot, xlim= c(0,50), ylim)
{
  cp <- plot + scale_x_continuous(limits = xlim) + scale_y_continuous(limits= ylim)
  return(cp)
}


#AGB log 

agbPath<- "output/agbiomass/AGBiomass_.txt"
agb<- read.table(agbPath, header=T)
head(agb)
meltagb <- melt(agb, id = "Time") 
head(meltagb)
p1<- ggplot(meltagb , aes(x= Time, y= value, color=variable)) +geom_point() + labs(title="Change in AGB over time", y="AGB in g/m2") + theme_classic() #theme_linedraw()

agbgrowthdiffs <- agb
meangrowth <- numeric(51)
for (i in 2:51)
{
  agbgrowthdiffs[i,2:length(agb)] <- agbgrowthdiffs[i,2:length(agb)] -agb[i-1,2:length(agb)]
  meangrowth[i] <- sum(agbgrowthdiffs[i,2:6]) /5
}
agbgrowthdiffs[1, 2:length(agb)] <- 0
head(agbgrowthdiffs)
meltgrowthperyear <- melt(agbgrowthdiffs, id = "Time") 
p1.5 <- ggplot(meltgrowthperyear , aes(x= Time, y= value, color=variable)) +geom_point() + labs(title="AGB growth per year", y="AGB in g/m2") + theme_classic() #theme_linedraw()
p1.5 <- p1.5 + geom_hline(yintercept=0)

meanVal <- mean(meangrowth ,na.rm=T)
sdVal <- sd(meangrowth, na.rm=T)
p1.5<- generateSimpleCI(plot=p1.5, avg= meanVal, stddev=sdVal, linetype="dashed", mColor="black", sdColor= "grey", size= 1.5)
p1.5 <- constrainPlot(p1.5, ylim = c(-75,100))

p1.5

tail(agbgrowthdiffs)
grid.arrange(p1,p1.5)


#fire summary log 

fireSumPath <- "dynamic-fire-summary-log.csv"
dfs <- read.csv(fireSumPath, header = T, stringsAsFactors = F)
head(dfs)

#dfs <- dfs[dfs$FireRegion != " fire3",]  # with out stringsAsFactors=F dfs$FireRegion == levels(dfs$FireRegion)[3]
dfs$TotalBurnedSites[dfs$FireRegion == " fire3"] <- dfs$TotalBurnedSites[dfs$FireRegion == " fire1"] +   dfs$TotalBurnedSites[dfs$FireRegion == " fire2"]   
dfs$NumberFires[dfs$FireRegion == " fire3"] <- dfs$NumberFires[dfs$FireRegion == " fire1"] +   dfs$NumberFires[dfs$FireRegion == " fire2"]   


meltdfs <- melt(dfs, id= c("Time","TotalBurnedSites","NumberFires"))
head(meltdfs)

# this and below are changed .......
meltdfs2<- meltdfs[complete.cases(meltdfs),]

p22 <- ggplot(meltdfs2, aes(x=Time , y =TotalBurnedSites*7.29, color = value)) + geom_smooth() +theme_classic()+labs(title="Burned Area per year (no fuelbreaks)") + scale_y_continuous(limits= c(0,8000))


p32 <- ggplot(meltdfs2, aes(x=Time , y =NumberFires, color = value)) + geom_smooth() +theme_classic() +labs(title="Number Fires per year (no fuelbreaks)")+ scale_y_continuous(limits= c(0,8.5))




#fire events log 

fireEventPath <- "dynamic-fire-events-log.csv"
dfe <- read.csv(fireEventPath, header=T)
ms <- numeric(50)
meds <- numeric(50) 
mck <- numeric(50)
medck<- numeric(50)
for (i in 1:50)
{
  ms[i] <- mean(dfe[dfe$Time==i,]$MeanSeverity)
  meds[i] <- median(dfe[dfe$Time==i,]$MeanSeverity) 
  mck[i] <- mean(dfe[dfe$Time==i,]$CohortsKilled)
  medck[i] <- median(dfe[dfe$Time==i,]$CohortsKilled)
}
df <-data.frame(Time = 1:50, MeanSeverity=ms, MedianSeverity=meds)
meltdf<- melt(df, id="Time")
p4<- ggplot(meltdf, aes(x=Time, y=value, color= variable)) + geom_point() + theme_classic() +  labs(title="Fire Severity per year")

meanVal <- mean(ms ,na.rm=T)
sdVal <- sd(ms, na.rm=T)
p4<- generateSimpleCI(plot=p4, avg= meanVal, stddev=sdVal, linetype="dashed", mColor="black", sdColor= "grey", size= 1.5)
p4 <- constrainPlot(p4, ylim = c(0,6))


dfck <-data.frame(Time = 1:50, MeanCohortsKilled=mck, MedianCohortsKilled=medck)
meltdfck<- melt(dfck, id="Time")
p5<- ggplot(meltdfck, aes(x=Time, y=value, color= variable)) + geom_point() + theme_classic() +  labs(title="Cohorts Killed per year")
meanVal <- mean(mck ,na.rm=T)
sdVal <- sd(mck, na.rm=T)
p5<- generateSimpleCI(plot=p5, avg= meanVal, stddev=sdVal, linetype="dashed", mColor="black", sdColor= "grey", size= 1.5)
p5 <- constrainPlot(p5, ylim = c(0,3000))



grid.arrange(p1,p1.5,p2,p3,p4,p5, ncol=1)
grid.arrange(p1.5,p2,p3,p4,p5, ncol=1)


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
# 
#   freq(fuelRaster)
  # fuelVal in 1:12. 
# Section - prototype of the findfuelbreak function ==== 
  # connected components with images 
  library(raster)
  library(spatstat)
  tR1<-fuelRaster
  values(tR1)[which(values(tR1) >0)] <- -1 
  plot(tR1)
  # take the fuels that are connected to 20 or more for each fuel type and tag them with a unique label... 
  for (fuelVal in 1:12){

  tR <- fuelRaster
  print(fuelVal)
  destroys <- which(values(tR != fuelVal))
  if(length(destroys)>= length(values(fuelRaster))-20)
  {
    print("bad")
    next() 
  }
   values(tR)[destroys] <- 0 
  
  imageF <- im(matrix(values(tR), 310, 312))
  plot(imageF)
  
  ci <- connected(imageF, method= "C",background = 0)
  plot(ci)
  
  values(tR) <- as.integer(ci$v)
  plot(tR)
 # freq(tR)
  
  fq <-as.data.frame( freq(tR))

  nfq<-na.omit(subset(fq,(fq$count>20 & fq$count<12000)))
  nfq<- nfq[order(-nfq$count),]
  if (length(nfq$value) == 0) {print("empty nfq"); next()}
  nfq<- cbind(nfq, rank=length(nfq$value):1 )
  print(nfq)
  
  temp <- values(tR)
  mapper <- function (x){ y<-nfq$rank[which(nfq$value== x)];  if(length(y) == 0){return(NA) }else{return(y)}} #which returns integer(0) on not found, we need NA 
  temp2 <- sapply(temp, mapper)
  
  hist(temp2)
  
  replace<-!is.na(temp2)
  values(tR1)[replace]<- temp2[replace]*(10^fuelVal)
  }
  
  temp <- values(tR1)
  values(tR1)[which(temp==-1 | temp==0)] <-NA
  plot(tR1)
  
  
  freq(tR1)
  
  newfq <- data.frame(freq(tR1))
  newerfq<- na.omit(newfq[order(-newfq$count),])
  toDivide <- head(newerfq, n=12)

 # writeRaster(tR1, filename ="test.img", format="HFA", dataType="INT2S")
  
  toFind <- toDivide$value[1]
  
  
  emptyR<-fuelRaster
  values(emptyR)[which(values(emptyR) >0)] <- -1 
  plot(emptyR)
  
  svals <- values(tR1) 
  
  cellsFound <- which(svals == toFind)
  values(emptyR)[cellsFound] <- 3
  plot(emptyR)
  freq(emptyR)
  
  maxWidth <- 300 
  
  scf<- sort(cellsFound)
  head(scf)
#  values(emptyR)[head(cellsFound, n= length(cellsFound)/2)] <-5
  
 # values(emptyR)[(cellsFound[1400:(length(cellsFound)/2)])] <-25
  
  values(emptyR)[which(values(emptyR) ==-1)] <- 0 
  eVals <- matrix(values(emptyR), c(310,312) ) 
  
  hist(colSums(eVals)) 
  
  # equilibrium index of list with tolerance for equality #https://www.geeksforgeeks.org/equilibrium-index-of-an-array/ 
  equalibrium <- function(arr, tol) {
    sum <- 0 #initialize sum of whole array
    leftsum <- 0 # initialize leftsum
    n<- length(arr)
    sum <- sum(arr)
    
    for (i  in 1:(n-1)) {
      sum <- sum - arr[i]# // sum is now right sum for index i
      print(sum)
      if (leftsum <= (sum+tol) & leftsum >= (sum-tol))
        return( i)
      
      leftsum <- leftsum + arr[i]
    }
     # /* If no equilibrium index found, then return 0 */
      return(-1)
  }
  
  cols <- colSums(eVals)
  ret <-equalibrium(arr=cols, tol=max(cols))
  eVals[which(eVals[,ret]==3), ret]<-40

  rows <- rowSums(eVals)
  ret <- equalibrium(arr=rows, tol=max(rows))
  eVals[ret, which(eVals[ret,]==3)]<-41

  values(emptyR) <-as.vector(eVals)
  
  plot(emptyR)
  freq(emptyR)
  
  #end findfuelbreak prototype ====
  
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