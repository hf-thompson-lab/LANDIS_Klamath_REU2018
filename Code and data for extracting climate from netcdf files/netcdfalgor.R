# SECTION - taking data from .nc files based on ecoregions ----
#this first section can take hours to run over a large area and time frame. 
#can take 3 - 12 hours depending on how long the resample function takes for the two input maps. Larger ecoregion maps mean longer resample times. 

start_time <- Sys.time()

getwd()
#setwd("C:/Users/hfintern/Desktop") # I just used the desktop to hold all my files. 

library(lattice)
library(ncdf4)
library(raster)
library(rgdal)

#fileName for the brick (.nc file)
fileName <- "Data for A2 climate scenario klamath region_ALL VARIABLES_Monthly_EH5.ncml.nc"

#read in ecoregion file that matches the projection of the .nc file brick.  
dirEcoRegions <- "klamathEcoWGS84_ASCIITo_asc1_ProjectRaster1.tif"  
ecoRast <- raster(dirEcoRegions)

#get which ecoregions are in the klamath.
ecos <- values(ecoRast)
ecoVals <- unique(ecos) 
ecoVals <- sort(ecoVals) #sort and get rid of NA value

#          Tmax     Tmin     PAR   Prec
vars <- c("TAMAX", "TAMIN", "SWI", "RT") # these are the codes for the variables in the .nc file.

#create a list that will hold the data frames for each variable 
l <- vector("list", length(vars) )
for (v in 1:length(vars))
{
  l[[v]] <- data.frame(Year = numeric(), Month=numeric() , Value=numeric(), ecoRegID = character())
}
df <- data.frame(Year = numeric(), Month=numeric() , ecoRegion=character(),VAR= character(), Min=numeric(), stQu=numeric(), Median=numeric() , Mean=numeric(), rdQu=numeric() ,Max=numeric(),stringsAsFactors=FALSE)


#loop throgh the different variables that you need to extract
for (j in 1:length(vars))
{
  #read in the brick of raster layers of the coresponding variable
  brickData <- brick(fileName, varname = vars[j]) # replace varname param with the actual var name.
  
  #loop through all of the months that have data and record the averages (summary statistics)
  for (i in 1:nlayers(brickData)) #this is what should be parallelized if you want to increase the speed
  {
    upscaledraster <- resample(brickData[[i]], ecoRast) #method? bilinear by default  ### this is by far the slowest call in this code  
    upsr <- values(upscaledraster) # take values from raster
    
    #take year and month from the layer name (in format "XYYYY.MM.DD")
    lyrname<- names(brickData[[i]])
    yr<- as.numeric(substr(lyrname, 2,5)) 
    mnth<- as.numeric(substr(lyrname,7,8))
    
    #loop through all of the ecoregions and take the summary stats for each one #    
    for (r in 1:length(ecoVals))
    {
      ecoID <- paste0("eco", ecoVals[r])
      
      #find indices of cells that match the ecoregion values 
      ecoIndices <- which(ecos == ecoVals[r], arr.ind = T) #find indices that match the ecoRegion code
      ecoNums<- upsr[ecoIndices]#take the values from the specificed ecoregions using the eco regions indices 
      
      #summary statistics 
      meanVal  <- mean(ecoNums)
      statsVal <- summary(ecoNums)
      
      #record the mean for each ecoregion at each month in a data frame. 
      #         year  month  mean    ecoRegionName 
      newRow<- c(yr, mnth, meanVal, ecoID)
      # 
      # if you want max temp obsevered in the month or sum of preciptation you should add that in here and change meanVal
      #
      l[[j]] <- rbind(l[[j]], data.frame(Year=yr, Month=mnth, Value=meanVal, ecoRegID = ecoID),make.row.names = F)
      
      #record the summary stats for ecoregion at each month just in case we want it later. 
      df[nrow(df)+1,] <- c(yr, mnth, ecoID, vars[j], statsVal)
    }
    
    print(paste0(yr, " ", mnth, " ...")) # print progress to console. 
  }
}



# SECTION - Writing gathered data ----
#rename columns to vars names 
for (v in 1:length(vars))
{
  colnames(l[[v]])<-c("Year", "Month", vars[v], "ecoRegID")
  write.csv(l[[v]],file=paste0("C:/Users/hfintern/Desktop/MEAN_ONLY_", vars[v],".csv"))#write data
}

#write data 
write.csv(df, file=paste0("C:/Users/hfintern/Desktop/ALL_VARS_Large.csv"))


end_time <- Sys.time()
total_time <- end_time - start_time
total_time



#model the future parallelization off of this ----
# if you want it to run a bit faster, I would think the best loop to parallelize is the nlayers(brickData) part. 
#http://biostat.mc.vanderbilt.edu/wiki/pub/Main/MinchunZhou/HPC_SNOW.rwn.pdf
#library(doSNOW)
#NumberOfCluster <- 4
#cl <- makeCluster(NumberOfCluster)
#registerDoSNOW(cl)
#x <- foreach(i = 1:5, .combine = "rbind") %dopar% {
#  y <- (1:5)^i
#  return(y)
#}
#stopCluster(cl)
#x


# SECTION - Convert the gathered data into PnET format ---- 

#climateFile <- "C:/Users/hfintern/Desktop/climate_A2_scenario" #variable not used?

#make sure that working directoy is set correctly -- I used the desktop. 

#take the correct variables in the list from the vars definition from the top of the file. 
tmax<- l[[1]]
tmin<- l[[2]]
par <- l[[3]]
prec<- l[[4]]


#steal the year and month format from this file. ----
table<- read.table(paste0(getwd(), "/climate_A2_scenario/eco11_klamath_climate.txt"), header=T) #this could be any file with the data formated as PnET wants
table[3:8]<-  NULL
head(table)

#Create model for predicting CO2 values from the A2 scenario --  http://www.ipcc-data.org/ancilliary/tar-isam.txt ----
table2<- read.table(paste0(getwd(), "/co2 scenarios.txt"), header=T)
model <- lm(A2 ~ Year + Year2 + Year3, data=data.frame(A2=table2$A2, Year=table2$Year, Year2=table2$Year*table2$Year , Year3=(table2$Year)^3))
summary(model)
plot(table2$Year, table2$A2)
#extapolate for predictions 
timeSteps <- seq(1947.01,2100,1/12) #gets monthly data
predictedvalues <- predict(model, newdata=data.frame(Year=timeSteps, Year2=(timeSteps * timeSteps), Year3=(timeSteps* timeSteps * timeSteps)))
lines( timeSteps,predictedvalues)
CO2Vals <- cbind(table, CO2= predictedvalues)
head(CO2Vals)
# temp - write out the co2 predicted values to file as back up 
#write.table(CO2Vals, paste0(getwd(), "/CO2_predictedVals_A2scenario.txt"), row.names = F, quote=F)





#framing and writing the gathered data into separate csv files based on ecoregion tag ----
library(plyr)
#get ecoregions that were in the ecoregion map and make the strings for the file writing. 
ecoregs <- paste("eco", ecoVals, sep="")
for (i in 1:length(ecoregs))#add columns to df based on year and month
{
  #gather all the regional data 
  ecoTmax <- tmax[which(tmax$ecoRegID==ecoregs[i]),]
  ecoTmin <- tmin[which(tmin$ecoRegID==ecoregs[i]),]
  ecoPAR  <- par[which(par$ecoRegID==ecoregs[i]),]
  ecoprec <- prec[which(prec$ecoRegID==ecoregs[i]),]
  
  #get rid of the id columns 
  ecoprec$ecoRegID<-NULL 
  ecoTmin$ecoRegID<-NULL 
  ecoTmax$ecoRegID<-NULL 
  ecoPAR$ecoRegID<-NULL 
  
  #rename the columns 
  colnames(ecoTmax) <- c("Year","Month", "Tmax")
  colnames(ecoTmin) <- c("Year","Month", "Tmin")
  colnames(ecoPAR) <- c("Year","Month", "PAR")
  colnames(ecoprec) <- c("Year","Month", "Prec")
  
  ecoPAR$PAR <- ecoPAR$PAR*.45976*4.57  # conversion of PAR and change of units 
  
  #join all the other variables to merge in order of the columns need for PnET. Year, Month, Tmax,Tmin, PAR, Prec, CO2, O3
  newm <- join(table, ecoTmax, by=c("Year", "Month"))
  newm <- join(newm, ecoTmin, by=c("Year", "Month"))
  newm <- join(newm, ecoPAR, by=c("Year", "Month"))
  newm <- join(newm, ecoprec, by=c("Year", "Month")) # danger zone with newm instead of table right now... 
  newm <- join(newm, CO2Vals, by=c("Year", "Month"))
  newm <- cbind(newm, data.frame(O3 = rep(0,length(CO2Vals$Year) ) ))  # add in O3 
  
  #write out to file. 
  climatedir<- "C:/Users/hfintern/Desktop/climate_A2_scenario_convertedPAR_redo/"
  climatedir<- paste0(climatedir, ecoregs[i],"_A2_climate_PnET",".txt")
  write.table(newm, file = climatedir,row.names = F, quote = F)
}




# SECTION - Fill in missing year values by sampling random years ----
# make sure that the directories all work out. This section has both reading and writing from files. 
#these were the sampled years for the files written at 6/18/2018 9:52 am

#firstSampledYear <- sample(1968:1999, 1) #for 1200-1948
#[1] 1985
firstSampledYear<- 1985

#sampledYears<- sample(1968:1999, length(1949:1967), replace = T) # for 1949-1967
#[1] 1971 1998 1989 1983 1971 1971 1979 1983 1968 1974 1995 1990 1996 1971 1989 1999 1970 1974 1986
sampledYears1 <- c(1971, 1998, 1989, 1983, 1971, 1971, 1979, 1983, 1968, 1974, 1995, 1990, 1996, 1971, 1989, 1999, 1970, 1974, 1986)

#sampledYears2 <- sample(c(1968:1999, 2010:2099), length(2000:2009), replace =T) #for 2000-2009
#[1] 2044 2023 2061 2084 2014 2081 2050 2025 2093 2048
sampledYears2 <- c(2044, 2023, 2061, 2084, 2014 ,2081 ,2050 ,2025, 2093, 2048)

#sampledYears3<- sample(2010:2099, 1) #for 2100 
#[1] 2039
sampledYears3 <- 2039

#get the files for each eco region 
pathToClimateFiles<- "C:/Users/hfintern/Desktop/climate_A2_scenario_convertedPAR_redo/"
ecoFiles <- list.files(path=pathToClimateFiles,pattern = "_A2_climate_PnET.txt")

for (j in 1:length(ecoFiles))
{
  #read in the text file 
  cTable <- read.table(file.path(pathToClimateFiles,ecoFiles[j]) , header =T)
  
  #replace the first year #1200-1948
  #toChange<- cTable[ which(cTable$Year=="1200-1948"),] 
  valsToInsert <- cTable[ which(cTable$Year==firstSampledYear),] 
  cTable[ which(cTable$Year=="1200-1948"),3:6] <- valsToInsert[,3:6] #add in the Tmax, Tmin, PAR, and Prec values from the sampled year
  head(cTable, n=25)
    
  #loop through the first gap of missing values  
  currentYear <- 1949  
  for (i in 1:length(sampledYears1))
  {
    valsToInsert<- cTable[ which(cTable$Year== sampledYears1[i]),] 
    cTable[which( cTable$Year == currentYear ),3:6] <- valsToInsert[,3:6]
    currentYear <- currentYear+1 
  }
  
  #loop through the second gap of missing values 
  currentYear <- 2000   
  for (i in 1:length(sampledYears2))
  {
    valsToInsert<- cTable[ which(cTable$Year== sampledYears2[i]),] 
    cTable[which( cTable$Year == currentYear ),3:6] <- valsToInsert[,3:6]
    currentYear <- currentYear+1 
  }
  
  #replace last year 
  valsToInsert <- cTable[ which(cTable$Year==sampledYears3),] 
  cTable[ which(cTable$Year=="2100"),3:6] <- valsToInsert[,3:6] #add in the Tmax, Tmin, PAR, and Prec values from the sampled year
  
  if (length(cTable$Tmin) != length(complete.cases(cTable)))
    print(paste("ERROR at step", j, ecoFiles[j]))  #if this is printed then there are still NAs in the data
  
  #write to file 
  climatedir<- paste0(pathToClimateFiles, ecoFiles[j])
  write.table(cTable, file = climatedir,row.names = F, quote = F)
}




# SECTION - Make non climate change scenario by sampling random years from 1968-1999 ----
# use same data that was produce above but from 2000-2100 sample years from 1968-1999 for prec,par,tmin, and tmax
# use CO2 as constant value from 2010 --390...  


#sampledYears<- sample(1968:1999, length(2010:2100), replace = T) # for 2010-2100  #sampled years from 6/18/18
#  [1] 1990 1986 1999 1972 1968 1985 1983 1968 1978 1982 1984 1971 1974 1996 1970 1981 1973 1977 1981 1985 1998 1972 1975 1974 1999 1998 1997 1970
#[29] 1983 1980 1978 1981 1995 1970 1990 1979 1987 1986 1974 1998 1970 1995 1969 1980 1990 1976 1993 1976 1995 1995 1989 1978 1991 1996 1977 1971
#[57] 1968 1992 1999 1978 1973 1974 1996 1974 1969 1973 1989 1991 1978 1984 1994 1970 1979 1969 1980 1991 1976 1997 1968 1975 1997 1969 1989 1968
#[85] 1978 1994 1983 1974 1969 1994 1977
sampledYears <- c(1990, 1986, 1999, 1972, 1968, 1985, 1983 ,1968 ,1978, 1982, 1984 ,1971 ,1974 ,1996 ,1970, 1981 ,1973 ,1977 ,1981 ,1985 ,1998, 1972, 1975, 1974, 1999, 1998, 1997, 1970, 1983, 1980, 1978, 1981, 1995, 1970, 1990, 1979, 1987, 1986, 1974, 1998, 1970, 1995, 1969, 1980, 1990, 1976, 1993, 1976, 1995, 1995, 1989, 1978, 1991, 1996, 1977, 1971, 1968, 1992, 1999, 1978, 1973, 1974, 1996, 1974, 1969 ,1973, 1989, 1991, 1978, 1984, 1994, 1970, 1979, 1969, 1980, 1991, 1976, 1997, 1968, 1975, 1997, 1969, 1989, 1968, 1978, 1994, 1983, 1974, 1969, 1994, 1977)

#get the files for each eco region 
pathToClimateFiles<- "C:/Users/hfintern/Desktop/climate_A2_scenario_convertedPAR_redo/"
ecoFiles <- list.files(path=pathToClimateFiles,pattern = "_A2_climate_PnET.txt")

for (j in 1:length(ecoFiles))
{
  #read in the text file 
  cTable <- read.table(file.path(pathToClimateFiles,ecoFiles[j]) , header =T)
  
  #loop through the gap of missing values 
  currentYear <- 2010   
  for (i in 1:length(sampledYears))
  {
    valsToInsert<- cTable[ which(cTable$Year== sampledYears[i]),] 
    cTable[which( cTable$Year == currentYear ),3:6] <- valsToInsert[,3:6]
    cTable[which( cTable$Year == currentYear ),7] <- 390 #Assume that 2010 and on held constant co2 concentration of 390 (opposite of A2 scenerio)
    currentYear <- currentYear+1 
  }
  
  if (length(cTable$Tmin) != length(complete.cases(cTable)))
    print(paste("ERROR at step", j, ecoFiles[j]))  #if this is printed then there are still NAs in the data
  
  #write to file 
  climatedir<- paste0("C:/Users/hfintern/Desktop/climate_noClimateChange_scenario_redo/", substring(ecoFiles[j] , 1, 5), "_noChange_climate_PnET.txt")
  write.table(cTable, file = climatedir,row.names = F, quote = F)
}

