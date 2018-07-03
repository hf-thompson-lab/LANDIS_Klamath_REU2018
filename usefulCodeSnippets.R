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
simus.selection <- c("7-2-18 PnET+FIRE+FUEL 50 a2 scenario") # 7-2-18 PnET+FIRE+FUEL+DevCUT+FBreak 50 a2 scenario

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
  writeRaster(mfri,paste0('C:/Users/hfintern/Desktop/MeanFireInterval 7-2 no response.img'),overwrite=T,format="HFA", datatype="INT2S", NAvalue=0 )
})



# Section - AGB over time ---- 


simus.selection <- c("7-2-18 PnET+FIRE+FUEL 50 a2 scenario", "7-2-18 PnET+FIRE+FUEL+DevCUT+FBreak 50 a2 scenario")



constrainPlot <- function(plot, xlim= c(0,50), ylim){
  cp <- plot + scale_x_continuous(limits = xlim) + scale_y_continuous(limits= ylim)
  return(cp)
}



graphs <- list()
graphs <- lapply (simus.selection, function (sim){
  general.root.path <- "C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/Saved Output"
  
  setwd(general.root.path)
  setwd(sim)
  
  agbPath<- "output/agbiomass/AGBiomass_.txt"
  agb<- read.table(agbPath, header=T)

  meltagb <- melt(agb, id = "Time") 
  
  p1<- ggplot(meltagb , aes(x= Time, y= value, color=variable)) +geom_smooth() + labs(title="Change in AGB over time ", subtitle= sim, y="AGB in g/m2") + theme_linedraw()
  p1 <- constrainPlot(p1, ylim = c(0,5000))
  return(p1)
})


g1<- grid.arrange(graphs[[1]], graphs[[2]])


graphs1 <- list()
graphs1 <- lapply (simus.selection, function (sim){
  general.root.path <- "C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/Saved Output"
  
  setwd(general.root.path)
  setwd(sim)
  
  agbPath<- "output/agbiomass/AGBiomass_.txt"
  agb<- read.table(agbPath, header=T)
  
 
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
  p1.5 <- ggplot(meltgrowthperyear , aes(x= Time, y= value, color=variable)) +geom_smooth() + labs(title="AGB growth per year", y="AGB in g/m2", subtitle= sim) + theme_linedraw()
  p1.5 <- p1.5 + geom_hline(yintercept=0, linetype="dashed", color = "red" , size=2)
  p1.5 <- constrainPlot(p1.5, ylim = c(-50,75))
  
  return(p1.5)
})

g2<- grid.arrange(graphs1[[1]], graphs1[[2]])


# Section - fire stats over time ---- 

dataFrames <- lapply (simus.selection, function (sim){
  general.root.path <- "C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/Saved Output"
  
  setwd(general.root.path)
  setwd(sim)
  
  fireEventPath <- "dynamic-fire-events-log.csv"
  dfe <- read.csv(fireEventPath, header=T)
  
  ms <- numeric(50)
  meds <- numeric(50)
  mck <- numeric(50)
  medck<- numeric(50)
  size <- numeric(50)
  for (i in 1:50)
  {
    ms[i] <- mean(dfe[dfe$Time==i,]$MeanSeverity)
    meds[i] <- median(dfe[dfe$Time==i,]$MeanSeverity)
    mck[i] <- sum(dfe[dfe$Time==i,]$CohortsKilled)
    medck[i] <- sum(dfe[dfe$Time==i,]$TotalSites)
    size[i] <- max(dfe[dfe$Time==i,]$SizeBin)

  }

  df <-data.frame(Time = 1:50, MeanSeverity=ms, MedianSeverity=meds,CohortsKilled=mck, TotalSites=medck, Size= size,  SIM=rep(sim, 50))
  return(df)
})

completedf<- do.call( rbind, dataFrames)

meltdf <- melt(completedf, id=c("Time", "MedianSeverity", "CohortsKilled", "TotalSites", "MeanSeverity", "Size" ))

p4<- ggplot(meltdf, aes(x=Time, y=MeanSeverity, color= value)) + geom_smooth() +geom_point() + theme_linedraw() +  labs(title="MeanSeverity");p4

p5<- ggplot(meltdf, aes(x=Time, y=CohortsKilled, color= value)) + geom_smooth() + theme_linedraw()+  labs(title="CohortsKilled");p5



p6<- ggplot(meltdf, aes(x=Time, y=TotalSites, color= value))+geom_point() + geom_smooth() + theme_linedraw()+  labs(title="TotalSites");p6

meltdf$Size[meltdf$Size=="-Inf"]<-0 
p7<- ggplot(meltdf, aes(x=Time, y=Size, color= value))+geom_point() + geom_smooth() + theme_linedraw()+  labs(title="Max fire Size");p7


p = ggplot(meltdf, aes(x=Size)) + theme_classic()+
  geom_histogram(position="identity", colour="grey40", alpha=0.2, bins = 6) +  labs(title="Max recorded fire size over all years")+
  facet_grid(. ~ value );p

