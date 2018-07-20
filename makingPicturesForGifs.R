library(ggpubr)
library(ggplot2)
library(reshape2)
library(raster)
library(scales)

#set up 
scenarioPath <-"C:/Users/hfintern/Desktop/sa3_10YTS"

sims <- list.dirs(scenarioPath, recursive = F, full.names = F)
sims<- sims[!grepl("Graphs",sims)]

simus.selection <- sims 
general.root.path <- scenarioPath
common.names <- c("A2 Climate","A2 Climate & LU","Recent Trends","Recent Trends & LU")#the order has to match the order of the simus.selection

timesteps <- 1:9 * 10 
timesteps


#read in rasters 
SeverityrasterStacks <- lapply (simus.selection, function (sim){
  
  setwd(general.root.path)
  setwd(sim)
  
  if (length(list.files(path = './fire')) ==0) { return(NULL) }
  ras.list <- paste0('fire/severity-',timesteps,'.img') 
  rastack  <- stack (ras.list)
  return(rastack)
})

CutrasterStacks<- lapply (simus.selection, function (sim){
  
  setwd(general.root.path)
  setwd(sim)
  
  if (!dir.exists("land-use-maps")) { return(NULL) }
  ras.list <- paste0('land-use-maps/land-use-',timesteps,'.img') 
  rastack  <- stack (ras.list)
  return(rastack)
})


#A2 Climate gif 
setwd(scenarioPath)
dir.create("a2GIF")
setwd("a2GIF")

breakpoints <- c(1,10,17,25,37,45,57)
colors <- c("green","grey","grey","brown","brown","red","black", "black")

arg <- list(at=c(4,16,35,52),      labels=c("none","Fuel\nReduction","Salvage\nlogging","Fire\nBreak"))
arg2 <- list(at=c(0,1,2,3,4,5,6,7), labels=c("0   ","1   \n        ","2      \n       ","3   \n     "),"4","5","6","7")
png(file="example%02d.png", width=1220, height=700)
for (i in timesteps){
  pplts <-stack( rasterStacks[[1]][[paste0("severity.",i)]], rasterStacks[[2]][[paste0("severity.",i)]])
  temp<- spplot(as(pplts, 'SpatialGridDataFrame'),
         names.attr=c("A2 Climate Fire History", "A2 Climate & LU Fire History"),  
         main=list(paste("Fire Severity History", i), cex=2),colorkey = list(labels = arg2))
  par.strip.text = list(cex = 2)
  
  ras<-CutrasterStacks[[2]][[paste0("land.use.",i)]]
  values(ras)[values(ras)==21] <- 24
  values(ras)[values(ras)==31] <- 34
  values(ras)[values(ras)==155] <- 50
  values(ras)[values(ras)==156] <- 55
  ras2 <- ras
  values(ras2) <-1
  temp2<- spplot(as(stack(ras2, ras), 'SpatialGridDataFrame'),
                names.attr=c("A2 Climate","A2 Climate & LU Cut History"),  
                main=list(paste("Cutting History", i), cex=2),
                breaks=breakpoints,col=colors, axis.arg=arg, colorkey = list(labels = arg))
  par.strip.text = list(cex = 2)
  grid.arrange(temp, temp2, nrow=1, top = textGrob("A2 Climate Scenario",gp=gpar(fontsize=20,font=3)))
  
}
dev.off()



#A2 Climate gif 
setwd(scenarioPath)
dir.create("NOGIF")
setwd("NOGIF")

breakpoints <- c(1,10,17,25,37,45,57)
colors <- c("green","grey","grey","brown","brown","red","black", "black")

arg <- list(at=c(4,16,35,52),      labels=c("none","Fuel\nReduction","Salvage\nlogging","Fire\nBreak"))
arg2 <- list(at=c(0,1,2,3,4,5,6,7), labels=c("0   ","1   \n        ","2      \n       ","3   \n     "),"4","5","6","7")
png(file="example%02d.png", width=1220, height=700)
for (i in timesteps){
  pplts <-stack( rasterStacks[[3]][[paste0("severity.",i)]], rasterStacks[[4]][[paste0("severity.",i)]])
  temp<- spplot(as(pplts, 'SpatialGridDataFrame'),
                names.attr=c("Recent Trends Fire History", "Recent Trends & LU Fire History"),  
                main=list(paste("Fire Severity History", i), cex=2),colorkey = list(labels = arg2))
  par.strip.text = list(cex = 2)
  
  ras<-CutrasterStacks[[4]][[paste0("land.use.",i)]]
  values(ras)[values(ras)==21] <- 24
  values(ras)[values(ras)==31] <- 34
  values(ras)[values(ras)==155] <- 50
  values(ras)[values(ras)==156] <- 55
  ras2 <- ras
  values(ras2) <-1
  temp2<- spplot(as(stack(ras2, ras), 'SpatialGridDataFrame'),
                 names.attr=c("Recent Trends","Recent Trends & LU Cut History"),  
                 main=list(paste("Cutting History", i), cex=2),
                 breaks=breakpoints,col=colors, axis.arg=arg, colorkey = list(labels = arg))
  par.strip.text = list(cex = 2)
  grid.arrange(temp, temp2, nrow=1, top = textGrob("Recent Trends Scenario",gp=gpar(fontsize=20,font=3)))
  
}
dev.off()
