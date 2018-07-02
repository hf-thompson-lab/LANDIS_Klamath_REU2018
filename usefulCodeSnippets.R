#Evan Waldmann
#6/28/18

# Useful code with descriptions 


## Section ploting with pauses ---- 
sim <- function(path, n=100, pause=0.25) {
  for (i in 1:n) {
    x<- raster(paste0(path1,i,".img"))
    plot(x, legend=T, asp=NA, main=i)
    dev.flush()
    Sys.sleep(pause)
  }
  invisible(x)
}


setwd("C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/Saved Output/PnET+Fire+Fuel+NoClimate 50Years/")
setwd("C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/")
sim(path = "fire/fuels/FuelType-", n=50, pause=1.5)


###########################################################
# Changing initial map codes to exclude all but 5 species #----
###########################################################
dir<- "C:/Users/hfintern/Desktop/movedFiles/BASE_historicalClimate/ICsimplified_E4_v3_270M.txt"
writedir<- "C:/Users/hfintern/Desktop/ICsimplified_E4_v4_270M_5Species.txt"
dir

lines<- readLines(dir)
lines

speciesToKeep<- c("LandisData", "MapCode","ABGRC","ACMA3","ALRU2","PSME","PIMO3")
for (i in 1: length(lines))
{
  flag <- FALSE 
  for (j in 1:length(speciesToKeep))
  {
    if (grepl(speciesToKeep[j], lines[i])) #looks for all the species and changes flag if we should keep the line
    {
      flag<- TRUE
    }
  }
  if (!flag) #if line was not flagged then delete it. 
  {
    lines[i] <- "" 
  }
}
writeLines(lines, writedir)


# Section - plot climate data ----- 

library(ggplot2)
library(reshape2)
library(gridExtra)

setwd("C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/climate_scenerios_klamath_all_eco_regions")
setwd("climate_A2_scenario_convertedPAR_redo")

climateFiles <- list.files(pattern = "PnET.txt", full.names = T)
climateFiles

tab <-  read.table(file = climateFiles[1], header =T,stringsAsFactors = FALSE)
endtab<-cbind(tab, EcoReg= rep(substr(climateFiles[1], 3,7), length(tab$Year)) )

for (i in 2:length(climateFiles))
{
  tab <-  read.table(file = climateFiles[i], header =T,stringsAsFactors = FALSE)
  tab1<-cbind(tab, EcoReg= rep(substr(climateFiles[i], 3,7), length(tab$Year)) )
  endtab<-rbind(endtab, tab1)
}

levels(endtab$Year)
endtab$Year[endtab$Year=="1200-1948"] <- 1948 

decimalYear <- as.double(endtab$Year )+ as.double(endtab$Month)/12.0 
endtab <- cbind(endtab, decimalYear)
head(endtab)
meltedCD <- melt(endtab, id.vars =c("Year", "Month", "decimalYear", "EcoReg", "CO2", "PAR", "O3","Prec")) # "Tmax", "Tmin", "PAR", "Prec", "CO2"))


head(meltedCD)

ggplot(data =meltedCD, aes(x=decimalYear , y=value  , color=variable)) + geom_point()+ facet_wrap(~EcoReg ) + labs(title ="A2 Scenario")


# Section - Mean fire return interval (working) ----

mask.active.cells <- raster('C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/clippedRaster/mask_activeCells.img')
simus.selection <- c("PnET+Fire+Fuel+A2Climate 50Years")

lapply (simus.selection, function (sim){
  general.root.path <- "C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/Saved Output"
  
  setwd(general.root.path)
  setwd(sim)
  
  list.files(path = './fire')
  ras.list <- paste0('fire/severity-',1:50,'.img') 
  rastack  <- stack (ras.list)
  
  rastack <- reclassify(rastack,rcl = matrix (ncol = 3,byrow=T,data =c(-1,2.5,0,2.6,20,1)))
  
  try1 <- calc (rastack, fun= function (a){
    m <- rle(a)
    o <- mean(m$lengths [which (m$values==0)],na.rm=T)
    if (is.nan(o)){o<-NA}
    return(o)
  })
  extent(try1) <- extent (mask.active.cells)
  mfri <- try1 * mask.active.cells
  writeRaster(mfri,paste0('C:/Users/hfintern/Desktop/MeanFireInterval plain.tif'),overwrite=T)
})

