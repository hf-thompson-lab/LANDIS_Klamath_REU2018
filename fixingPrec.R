#Multiplying the prec column by the number of days in each month in all of the Ecoregions 
#fles are in the PnET Climate data format

pathToClimateFiles<- "C:/Users/hfintern/Desktop/climate_noClimateChange_scenario_redo"
ecoFiles <- list.files(path=pathToClimateFiles,pattern = "_noChange_climate_PnET.txt")
dir.create("C:/Users/hfintern/Desktop/climate_noClimateChange_scenario_convertedPAR_secondredo")


isLeapYear <- function(year){ return( ( ((year) %% 4 == 0) & ((year) %% 100 != 0)) | ((year) %% 400 == 0) ) }

for (j in 1:length(ecoFiles))
{
  #read in the text file 
  cTable <- read.table(file.path(pathToClimateFiles,ecoFiles[j]) , header =T)
  cTableBegin <-cTable[1:12,] 
  cTableEnd <- cTable[13:length(cTable$Year),]
  
  #multiply the precipation values of specific months by the number of days in those months 
  #take into account leap years 
  d.31 <- c(1,3,5,7,8,10,12)
  d.30 <- c(4,6,9,11)
  
  cTableBegin$Prec[which( cTableBegin$Month ==2 )] <- cTableBegin$Prec[which( cTableBegin$Month ==2)]*28
  cTableBegin$Prec[which( cTableBegin$Month %in% d.31 )] <- cTableBegin$Prec[which( cTableBegin$Month %in% d.31 )]*31
  cTableBegin$Prec[which( cTableBegin$Month %in% d.30 )] <- cTableBegin$Prec[which( cTableBegin$Month %in% d.30 )]*30
  
  #feb has an extra day in a leap year
  cTableEnd$Prec[which(isLeapYear(as.numeric(as.character(cTableEnd$Year))) & cTableEnd$Month == 2 )] <- cTableEnd$Prec[which(isLeapYear(as.numeric(as.character(cTableEnd$Year))) & cTableEnd$Month == 2 )]*29

  cTableEnd$Prec[which( !isLeapYear(as.numeric(as.character(cTableEnd$Year))) & cTableEnd$Month == 2 )] <- cTable$Prec[which( !isLeapYear(as.numeric(as.character(cTableEnd$Year))) & cTableEnd$Month == 2 )]*28
  
  #31 and 30 days in all the other months     
  cTableEnd$Prec[which( cTableEnd$Month %in% d.31 )] <- cTableEnd$Prec[which( cTableEnd$Month %in% d.31 )]*31
    
  cTableEnd$Prec[which( cTableEnd$Month %in% d.30 )] <- cTableEnd$Prec[which( cTableEnd$Month %in% d.30 )]*30

  #combine the cTable to what it once was 
  newcTable <- rbind(cTableBegin, cTableEnd)

  #write to file 

  climatedir<- paste0("C:/Users/hfintern/Desktop/climate_noClimateChange_scenario_convertedPAR_secondredo/", substring(ecoFiles[j] , 1, 5), "_noChange_climate_PnET.txt")
  write.table(newcTable, file = climatedir,row.names = F, quote = F)
}


