library(ggpubr)
library(ggplot2)
library(reshape2)
library(raster)
library(scales)


scenarioPath <-"C:/Users/hfintern/Desktop/sa3_10YTS"## "C:/Users/hfintern/Desktop/sa3_Scenarios"

sims <- list.dirs(scenarioPath, recursive = F, full.names = F)
sims<- sims[!grepl("Graphs",sims)]

simus.selection <- sims 
general.root.path <- scenarioPath
common.names <- c("A2 Climate","A2 Climate & LU","Recent Trends","Recent Trends & LU")#the order has to match the order of the simus.selection
#general.root.path <- #"C:/Users/hfintern/Desktop/sa3_Scenarios"#"C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/Saved Output"

timesteps <- 1:9 * 10 
timesteps

## making and plot mean fire return intervals ---- 

mask.active.cells <- raster('C:/Users/hfintern/Desktop/masksa3.img')

rasters <- lapply (simus.selection, function (sim){
  
  setwd(general.root.path)
  setwd(sim)
  
  if (length(list.files(path = './fire')) ==0) { return(NULL) }
  ras.list <- paste0('fire/severity-',timesteps,'.img') 
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
  #plot(mfri, add=T, title=sim)
  #writeRaster(mfri,paste0('fire/MeanFireInterval.img'),overwrite=T,format="HFA", datatype="INT2S", NAvalue=0 )
  return(mfri)
})


# s <- stack(rasters[[1]],rasters[[2]],rasters[[3]],rasters[[4]])
# sp<- as(s, 'SpatialGridDataFrame')
# seededplots<- spplot(sp, names.attr= simus.selection[c(1, 2, 3, 4)], main = "Mean Fire Return Interval (Years) SEED=3333",
#                      colorkey=list(height =.75, at=c(seq(10, 60, 5))), labels=c(seq(10, 60, 5)) , col.regions = colorRampPalette(c("red", "orange", "yellow","green","darkgreen") ) )
# plot(seededplots)

s1 <- stack(rasters)
sp1<- as(s1, 'SpatialGridDataFrame')
notseededplots<- spplot(sp1,names.attr= common.names, main = list("Mean Fire Return Interval (Years)", cex=2),
                        colorkey=list(height =.75, at=seq(10, 100, 10) ), labels=seq(10, 100, 10) ,  col.regions = colorRampPalette(c("red", "yellow","grey","white") ) )
plot(notseededplots)

# plotting cut history and fire severity 
# 
# chRas <- lapply (simus.selection, function (sim){
#   setwd(general.root.path)
#   setwd(sim)
#   
#   if (length(list.files(path = './land-use-maps')) ==0) { return(NULL) }
#   ras<- paste0('land-use-maps/cutHistory.img') 
#   # ras<- paste0('logs/severityHistory.img')
#   
#   return(raster(ras))
# })
# commonNames <- c("A2 & LU", "Recent Trends & LU") #simus.selection[c(5, 6, 11, 12)]
# s2 <- stack(chRas[[2]],chRas[[4]])
# sp2<- as(s2, 'SpatialGridDataFrame')
# plots2<- spplot(sp2,names.attr= commonNames, main = "Cut History ")
# plot(plots2)


# ploting AGB for each simulation ---- 

constrainPlot <- function(plot, xlim= c(0,max(timesteps)), ylim){
  cp <- plot  + scale_y_continuous(limits= ylim)
  return(cp)
}

graphs <- list()
graphs <- lapply (simus.selection, function (sim){
  
  setwd(general.root.path)
  setwd(sim)
  
  agbPath<- "output/agbiomass/AGBiomass_.txt"
  agb<- read.table(agbPath, header=T)
  
  colnames(agb)[colnames(agb)=="ABGRCg.m2"] <- "Grand fir"
  colnames(agb)[colnames(agb)=="ACMA3g.m2"] <- "Bigleaf maple"
  colnames(agb)[colnames(agb)=="ALRU2g.m2"] <- "Red alder"
  colnames(agb)[colnames(agb)=="PIMO3g.m2"] <- "Western white pine"
  colnames(agb)[colnames(agb)=="PSMEg.m2"]  <- "Douglas fir"
  print(colnames(agb))
  
  meltagb <- melt(agb, id = "Time") 
  
  p1<- ggplot(meltagb , aes(x= Time, y= value, color=variable)) +geom_smooth() + labs(subtitle= common.names[which(simus.selection == sim)], y="", x="", color="Species") + theme_bw(base_size = 20)
  p1 <- constrainPlot(p1, ylim = c(0,10500)) + scale_x_continuous(labels= 2010+c(0,timesteps), breaks = c(0,timesteps), limits = c(0,max(timesteps))) + guides(color=guide_legend(override.aes=list(fill=NA)))#+
  # scale_color_manual(labels = c("1", "2","3", "4", "5"), values=  hue_pal()(5))
  return(p1)
})

agbFig<- ggarrange(graphs[[1]],graphs[[2]],graphs[[3]],graphs[[4]], ncol=2, nrow=2, common.legend = TRUE, legend="top") 
annotate_figure(agbFig,  top = text_grob(paste("Change in Aboveground Biomass"), color = "Black", face = "bold", size = 25),
                bottom = text_grob("Year", color = "Black", size = 25),
                left = text_grob(expression(paste(
                  "Aboveground Biomass (",
                  g, "/", m^2,
                  ")", sep="")), color = "Black", rot = 90,size = 25))

# g_legend<-function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
# 
# mylegend<-g_legend(p1)



# ggarrange(graphs[[1]],graphs[[3]],graphs[[5]],graphs[[7]],graphs[[9]],graphs[[11]], ncol=3, nrow=2, common.legend = TRUE, legend="bottom")

# ggarrange(graphs[[2]],graphs[[4]],graphs[[6]],graphs[[8]],graphs[[10]],graphs[[12]], ncol=3, nrow=2, common.legend=T, legend="bottom")


# graphing fire stats ---- 
dataFrames <- lapply (simus.selection, function (sim){
  setwd(general.root.path)
  setwd(sim)
  if (length(list.files(path = './fire')) ==0) { return(NULL) }
  fireEventPath <- "dynamic-fire-events-log.csv"
  dfe <- read.csv(fireEventPath, header=T)
  len <- length(timesteps)
  ms <- numeric(len)
  meds <- numeric(len)
  mck <- numeric(len)
  medck<- numeric(len)
  size <- numeric(len)
  for (i in 1:length(timesteps))
  {
    ms[i] <- mean(dfe[dfe$Time==timesteps[i],]$MeanSeverity)
    meds[i] <- median(dfe[dfe$Time==timesteps[i],]$MeanSeverity)
    mck[i] <- sum(dfe[dfe$Time==timesteps[i],]$CohortsKilled)
    medck[i] <- sum(dfe[dfe$Time==timesteps[i],]$TotalSites)
    size[i] <- max(dfe[dfe$Time==timesteps[i],]$SizeBin)
    
  }
  
  df <-data.frame(Time = timesteps, MeanSeverity=ms, MedianSeverity=meds,CohortsKilled=mck, TotalSites=medck, Size= size,  SIM=rep(sim, len))
  return(df)
})

completedf<- do.call( rbind, dataFrames)

meltdf <- melt(completedf, id=c("Time", "MedianSeverity", "CohortsKilled", "TotalSites", "MeanSeverity", "Size" ))

p4<- ggplot(meltdf, aes(x=Time, y=MeanSeverity, color= value)) + geom_smooth(level=.9) + theme_bw() +  labs(title="MeanSeverity");p4

p5<- ggplot(meltdf, aes(x=Time, y=CohortsKilled, color= value)) + geom_smooth(level=.9) + geom_jitter() + theme_bw()+  labs(title="CohortsKilled");p5



p6<- ggplot(meltdf, aes(x=Time, y=TotalSites, color= value))+geom_point() + geom_smooth() + theme_bw()+  labs(title="TotalSites");p6

meltdf$Size[meltdf$Size=="-Inf"]<-0 
p7<- ggplot(meltdf, aes(x=Time, y=Size, color= value))+geom_point() + geom_smooth() + theme_bw()+  labs(title="Max fire Size");p7


hist1<- ggplot(meltdf) + geom_histogram(aes(MeanSeverity), bins=10) + facet_wrap(~value, ncol=1) + theme_bw()
hist2<- ggplot(meltdf) + geom_histogram(aes(TotalSites*7.29), bins=10) + facet_wrap(~value, ncol=1) + theme_bw()
hist3<- ggplot(meltdf) + geom_histogram(aes(CohortsKilled), bins=10) + facet_wrap(~value, ncol=1) + theme_bw()
ggarrange(hist1, hist2, hist3, ncol=3)

p = ggplot(meltdf, aes(x=Size)) + theme_bw()+
  geom_histogram(position="identity", colour="grey40", alpha=0.2, bins = 6, breaks=c(0,1,2,3,4,5)) +  labs(title="Max recorded fire size over all years")+
  facet_grid(. ~ value);p


# testing of dividing the data into early, mid, and late periods and then graphing box plots ----

# early  1:30
# mid    31 :60 
# late   61:91 

earlyTest <- subset(meltdf , meltdf$Time %in% 1:30)
t1<- ggplot(earlyTest, aes(x=value, y=MeanSeverity, color= value)) + geom_boxplot() + ylim(c(3,5)) + labs(title="Early" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())

midTest <- subset(meltdf, meltdf$Time %in% 31:60)
t2<- ggplot(midTest, aes(x=value, y=MeanSeverity, color= value)) + geom_boxplot()+ ylim(c(3,5))+ labs(title="Middle" ) + theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))


lateTest <- subset(meltdf,  meltdf$Time %in% 61:91)
t3<- ggplot(lateTest, aes(x=value, y=MeanSeverity, color= value)) + geom_boxplot()+ ylim(c(3,5)) + labs(title="Late" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30))) +theme(plot.title = element_text(hjust = 0.5))+   theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))

figure <- ggarrange(t1,t2,t3, common.legend=T, legend="bottom", ncol=3) 

annotate_figure(figure,  top = text_grob("Mean Severity", color = "Black", face = "bold", size = 14))#,bottom = text_grob("Data source: \n ToothGrowth data set", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))


vars.toGraph <- c("MeanSeverity", "CohortsKilled", "TotalSites", "Size" )


## TOTAL AREA AFFECTED BY FIRES ---- 
labs <- c("A2 scenario", "A2 scenario & LU+", "Recent Trends", "Recent Trends & LU+")

earlyTest <- subset(meltdf,  meltdf$Time %in% 1:15)
midTest <- subset(meltdf, meltdf$Time %in% 16:30)
maxVal <- max(max(earlyTest$TotalSites*7.29), max(midTest$TotalSites*7.29))

t1<- ggplot(earlyTest, aes(x=value, y=TotalSites*7.29, color= value)) + geom_boxplot() + labs(title="Early" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank() )+ theme(axis.title.y=element_blank())+
  scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75)) + ylim(0, maxVal) # add in limits 



t2<- ggplot(midTest, aes(x=value, y=TotalSites*7.29, color= value)) + geom_boxplot()+ labs(title="Middle" ) + theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+#theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))+
  scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75)) +ylim(0, maxVal)


lateTest <- subset(meltdf, grepl(3333,meltdf$value, ignore.case = T ) & meltdf$Time %in% 61:91)
t3<- ggplot(lateTest, aes(x=value, y=TotalSites*7.29, color= value)) + geom_boxplot() + labs(title="Late" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30))) +theme(plot.title = element_text(hjust = 0.5))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))+
  scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75))

figure <- ggarrange(t1,t2,t3, common.legend=T, legend="none", ncol=3) 

comp <- annotate_figure(figure,  top = text_grob(paste("Total area affected by fires", "(Seed 3333)"), color = "Black", face = "bold", size = 14),
                        bottom = text_grob("Scenarios", color = "Black", face = "bold", size = 12),
                        left = text_grob("Area (ha)", color = "Black",face = "bold", rot = 90,size = 12))
#,bottom = text_grob("Data source: \n ToothGrowth data set", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
comp


## cohorts killed   ---- 
labs <- c("A2 scenario", "A2 scenario & LU+", "Recent Trends", "Recent Trends & LU+")

earlyTest <- subset(meltdf, grepl(3333,meltdf$value, ignore.case = T ) & meltdf$Time %in% 1:30)
t1<- ggplot(earlyTest, aes(x=value, y=CohortsKilled, color= value)) + geom_boxplot() + labs(title="Early" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank() )+ theme(axis.title.y=element_blank())+
  scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75)) +# geom_jitter(width=.3)+
  stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 2)


midTest <- subset(meltdf, grepl(3333,meltdf$value, ignore.case = T ) & meltdf$Time %in% 31:60)
t2<- ggplot(midTest, aes(x=value, y=CohortsKilled, color= value)) + geom_boxplot()+ labs(title="Middle" ) + theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))+
  scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75))+# geom_jitter(width=.3)+
  stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 2)


lateTest <- subset(meltdf, grepl(3333,meltdf$value, ignore.case = T ) & meltdf$Time %in% 61:91)
t3<- ggplot(lateTest, aes(x=value, y=CohortsKilled, color= value)) + geom_boxplot() + labs(title="Late" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30))) +theme(plot.title = element_text(hjust = 0.5))+   theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))+
  scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75)) +# geom_jitter(width=.3)+
  stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 2)

figure <- ggarrange(t1,t2,t3, common.legend=T, legend="none", ncol=3) 

comp <- annotate_figure(figure,  top = text_grob(paste("CohortsKilled", "(Seed 3333)"), color = "Black", face = "bold", size = 14),
                        bottom = text_grob("Scenarios", color = "Black", face = "bold", size = 12),
                        left = text_grob("Number of cohorts killed", color = "Black",face = "bold", rot = 90,size = 12))

comp

## MeanSeverity
labs <- c("A2 scenario", "A2 scenario & LU+", "Recent Trends", "Recent Trends & LU+")

earlyTest <- subset(meltdf, grepl(3333,meltdf$value, ignore.case = T ) & meltdf$Time %in% 1:30)
t1<- ggplot(earlyTest, aes(x=value, y=MeanSeverity, color= value)) + geom_boxplot() + labs(title="Early" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank() )+ theme(axis.title.y=element_blank())+
  scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75)) +# geom_jitter(width=.3)+
  stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 2)+ ylim(c(3.5,4.75))


midTest <- subset(meltdf, grepl(3333,meltdf$value, ignore.case = T ) & meltdf$Time %in% 31:60)
t2<- ggplot(midTest, aes(x=value, y=MeanSeverity, color= value)) + geom_boxplot()+ labs(title="Middle" ) + theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))+
  scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75))+# geom_jitter(width=.3)+
  stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 2) + ylim(c(3.5,4.75))


lateTest <- subset(meltdf, grepl(3333,meltdf$value, ignore.case = T ) & meltdf$Time %in% 61:91)
t3<- ggplot(lateTest, aes(x=value, y=MeanSeverity, color= value)) + geom_boxplot() + labs(title="Late" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30))) +theme(plot.title = element_text(hjust = 0.5))+   theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))+
  scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75)) +# geom_jitter(width=.3)+
  stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 2)+ ylim(c(3.5,4.75))

figure <- ggarrange(t1,t2,t3, common.legend=T, legend="none", ncol=3) 

comp <- annotate_figure(figure,  top = text_grob(paste("MeanSeverity", "(Seed 3333)"), color = "Black", face = "bold", size = 14),
                        bottom = text_grob("Scenarios", color = "Black", face = "bold", size = 12),
                        left = text_grob("Fire severity", color = "Black",face = "bold", rot = 90,size = 12))

comp

## extracting number of highest severity cells ----

rdata <- lapply (simus.selection, function (sim){
  #general.root.path <- "C:/Users/hfintern/Desktop/sa3_Scenarios"#"C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/Saved Output"
  
  setwd(general.root.path)
  setwd(sim)
  
  if (length(list.files(path = './fire')) ==0) { return(NULL) }
  ras.list <- paste0('fire/severity-',timesteps,'.img') 
  count <- 1
  df <- do.call( rbind, lapply(ras.list ,  function(ras) {
    
    r <- raster(ras)
    vr <- values(r)
    row <- data.frame(ID= sim, Time= count , three= sum(vr==3) , four= sum(vr==4),  five = sum(vr==5) , six= sum(vr==6), seven= sum(vr==7))
    count <- count +1 
    return(row)
    
  }))
  
  return(df)
})

new <- do.call(function(l){
  return(cbind(l,Time= timesteps))
}, rdata)
complete <- do.call(rbind, rdata)
complete$Time <- rep(timesteps, length(simus.selection))
complete
#write.csv(complete ,"D:/Evan/severityData40years.csv")

meltSeverity <- melt(complete, id=c("Time","three", "four", "five", "six", "seven"))

#severity -- split by early middle late 
# #max(meltSeverity$seven)
# #min(meltSeverity$seven)
# # graphing highest severity area ----
# earlyTest <- subset(meltSeverity,  meltSeverity$Time %in% 1:30)
# t1<- ggplot(earlyTest, aes(x=value, y=seven*7.29, color= value)) + geom_boxplot() + labs(title="Early" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank() )+ theme(axis.title.y=element_blank())+
#   scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75)) +# geom_jitter(width=.3)+
#   stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 2)  + 
# 
# 
# midTest <- subset(meltSeverity,  meltSeverity$Time %in% 31:60)
# t2<- ggplot(midTest, aes(x=value, y=seven*7.29, color= value)) + geom_boxplot()+ labs(title="Middle" ) + theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+ #theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))+
#   scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75))+# geom_jitter(width=.3)+
#   stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 2) 
# 
# 
# lateTest <- subset(meltSeverity,  meltSeverity$Time %in% 61:91)
# t3<- ggplot(lateTest, aes(x=value, y=seven*7.29, color= value)) + geom_boxplot() + labs(title="Late" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30))) +theme(plot.title = element_text(hjust = 0.5))+   theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+ #theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))+
#   scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75)) +# geom_jitter(width=.3)+
#   stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 2)
# 
# figure <- ggarrange(t1,t2,t3, common.legend=T, legend="none", ncol=3) 
# 
# comp <- annotate_figure(figure,  top = text_grob(paste("Highest Severity Fires Occurrence"), color = "Black", face = "bold", size = 14),
#                         bottom = text_grob("Scenarios", color = "Black", face = "bold", size = 12),
#                         left = text_grob("Area affected by highest severity fire (ha)", color = "Black",face = "bold", rot = 90,size = 12))
# comp
# 


# scatter plot of fire severites

nms <- meltSeverity
head(nms)
nms[,7] <- NULL
colnames(nms)[7]<- "sim"
nms1<- melt(nms , id=c("Time","sim"))
head(nms1)



ggplot(nms1, aes(x= Time , y=value*7.29 , color= variable)) + geom_point() +geom_smooth(level=.9) + facet_wrap(~sim) + theme_bw()+ 
  labs(y="Area affected by different severity fires (ha)", x="Year", title= "Fire severity distributions across simulations", color="Severity levels")+
  scale_color_manual(labels = c("1", "2","3", "4", "5"), values=  hue_pal()(5)) + guides(color=guide_legend(override.aes=list(fill=NA))) 



#nice severity level box plot with inset plot ----

sumOfAffectedAreas <- t(data.frame(totalSites = sapply(unique(nms1$sim), function(simi){
  return(  sum(nms1$value[nms1$sim == simi]) )
})))



library(grid)

g <- ggplot(nms1, aes(variable, value*7.29))
g <- g + geom_boxplot(aes(fill=factor(sim))) + theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Affected area by severity level across simulations (2010-2100)", 
       x="Fire severity levels", fill = "Scenarios",
       y="Affected area (ha)")+
  theme(legend.position = c(0.325, 0.89)) +
  scale_fill_manual(labels = c("A2 Climate","A2 Climate & LU+", "Recent Trends", "Recent Trends & LU+"), values=  hue_pal()(4))+ 
  scale_x_discrete(labels= c("1","2","3","4","5 (All affected cohorts killed)"))#, values= c("three","four", "five", "six", "seven"))


saamelt <- melt(sumOfAffectedAreas)
subplot <- ggplot(saamelt, aes(Var2, value*7.29 , fill=factor(Var2))) + theme_bw() +geom_bar(stat="identity")+
  theme(legend.position = "none") + labs(subtitle="Total affected area", y="Affected Area (ha)", x="Scenarios")+ 
  theme(axis.text.x =element_blank(), axis.ticks.x = element_blank(),text = element_text(size=12))

vp <- viewport(width = 0.2, height = 0.2, x = .1,y = .725, just = c("left","bottom"))
full <- function() {
  print(g)
  theme_set(theme_bw(base_size = 20))
  print(subplot, vp = vp)
  theme_set(theme_bw())
}
full()


# zooming in and ignoring the fire severity of 1 and 2 (three and four) 
nms2 <- nms1[73:180,]

g <- ggplot(nms2, aes(variable, value*7.29))
g <- g + geom_boxplot(aes(fill=factor(sim))) + theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Affected area by severity level across simulations (2010-2100)", 
       x="Fire severity levels", fill = "Scenarios",
       y="Affected area (ha)")+
  theme(legend.position = c(0.325, 0.89)) +  coord_cartesian(ylim=c(0, 11000))+
  scale_fill_manual(labels = c("A2 Climate","A2 Climate & LU+", "Recent Trends", "Recent Trends & LU+"), values=  hue_pal()(4))+ 
  scale_x_discrete(labels= c("3","4","5 (All affected cohorts killed)"))#, values= c("three","four", "five", "six", "seven")) +


saamelt <- melt(sumOfAffectedAreas)
subplot <- ggplot(saamelt, aes(Var2, value*7.29 , fill=factor(Var2))) + theme_bw() +geom_bar(stat="identity")+
  theme(legend.position = "none") + labs(subtitle="Total affected area", y="Affected Area (ha)", x="Scenarios")+ 
  theme(axis.text.x =element_blank(), axis.ticks.x = element_blank(),text = element_text(size=12))

vp <- viewport(width = 0.2, height = 0.2, x = .08,y = .725, just = c("left","bottom"))
full <- function() {
  print(g)
  theme_set(theme_bw(base_size = 8))
  print(subplot, vp = vp)
  theme_set(theme_bw())
}
full()


## Total cohorts exploration ---- 


cohortData <- list()
cohortList<- lapply (simus.selection, function (sim){

  setwd(general.root.path)
  setwd(sim)
  
  path<- "output/TotalCohorts.txt"
  #library(gdata)
  #df <- read.xls(path, sheet = 1, row.names=1)
  L <- readLines(path) ##
  for(i in 1:13) L <- sub("\\s+", ",", L)
  cohortdf <- read.csv(text = L)
  
  #cohortdf<- read.table(path, header=T, sep = '\t')
  
  
  cohortdf <- cbind(cohortdf, sim= rep(sim, nrow(cohortdf)))
  
  
  return(cohortdf)
})
TotalCohortData<- do.call( rbind, cohortList)

cohortMelt <- melt(TotalCohortData, id=c(colnames(TotalCohortData)[1:13]))

head(cohortMelt)

ggplot(cohortMelt, aes(x=Time, y=X.Cohorts , color=value)) + geom_point() +geom_smooth()+ theme_bw() + 
  labs(y="Number of cohorts", x="Time (years)", title= "Total cohorts over time", color="Scenarios")#+
#scale_color_manual(labels = c("1", "2","3", "4", "5", "6"), values=hue_pal()(6)) + guides(color=guide_legend(override.aes=list(fill=NA)))

ggplot(cohortMelt, aes(x=Time, y=WoodyDebris.kgDW.m2., color=value)) + geom_smooth()+ theme_bw() + 
  labs(y="Amount of woody debris (kgDW/m2)", x="Time (years)", title= "Wood Debris over time", color="Scenarios")#+
#scale_color_manual(labels = c("1", "2","3", "4", "5", "6"), values=hue_pal()(6)) + guides(color=guide_legend(override.aes=list(fill=NA)))
cohortMelt<-subset(cohortMelt, cohortMelt$Time<90)
ggplot(cohortMelt, aes(Time, WoodyDebris.kgDW.m2., fill=value, color=value)) + geom_area(stat="identity",position=position_dodge(.5), alpha=.25, size=1)+ 
  coord_cartesian(ylim=c(850, max(cohortMelt$WoodyDebris.kgDW.m2.)+100))+ geom_point() + theme_bw(base_size = 20)+
  labs(y=expression(paste(
    "WoodyDebris (",
    kg,DW, "/", m^2,
    ")", sep="")),
    x="Year", title= "Woody debris across scenarios", color="Scenarios", fill="Scenarios")+
  scale_color_manual("Scenarios",labels = c("A2 Climate", "A2 Climate & LU","Recent Trends", "Recent Trends & LU"), values=  hue_pal()(4)) +
  scale_fill_manual("Scenarios",labels = c("A2 Climate", "A2 Climate & LU","Recent Trends", "Recent Trends & LU"), values=  hue_pal()(4))+ 
  scale_x_continuous(labels=c(0,timesteps)+2010, breaks = c(0,timesteps) )+
  theme(legend.position = c(0.85, 0.1))



ggplot(cohortMelt, aes(Time, Litter.kgDW.m2., fill=value, color=value)) + geom_area(stat="identity",position=position_dodge(.5), alpha=.25, size=1)+ 
  coord_cartesian(ylim=c(0, max(cohortMelt$Litter.kgDW.m2.)+100))+ geom_point() + theme_bw(base_size = 20)+
  labs(y=expression(paste(
    "Litter (",
    kg,DW, "/", m^2,
    ")", sep="")),
    x="Year", title= "Litter across scenarios", color="Scenarios", fill="Scenarios")+
  scale_color_manual("Scenarios",labels = c("A2 Climate", "A2 Climate & LU","Recent Trends", "Recent Trends & LU"), values=  hue_pal()(4)) +
  scale_fill_manual("Scenarios",labels = c("A2 Climate", "A2 Climate & LU","Recent Trends", "Recent Trends & LU"), values=  hue_pal()(4))+ 
  scale_x_continuous(labels=c(0,timesteps)+2010, breaks = c(0,timesteps) )+
  theme(legend.position = c(0.85, 0.1))




bioList <- list()
bioList<- lapply (simus.selection, function (sim){
  setwd(general.root.path)
  setwd(sim)
  
  path<- "output/biomass/biomass.txt"
  
  biomassdf<- read.table(path, header=T, sep = '\t')
  
  biomassdf <- cbind(biomassdf, sim= rep(sim, nrow(biomassdf)))
  
  return(biomassdf)
})
biomassDF<- do.call( rbind, bioList)
biomassDF$X<-NULL 

bioMelt <- melt(biomassDF, id=c("Time", "sim")) 


ggplot(bioMelt, aes(x=Time, y=value, color=sim, linetype=variable)) +geom_smooth(size=2)+ theme_bw() +# facet_grid(~variable)+
  labs(y="Biomass (g/m2)", x="Time (years)", title= "Biomass over time", color="Scenarios")+guides(legend.position="bottom")


#unsure if the facet is more clear than the linetype 
subBioMelt<- bioMelt
ggplot(subBioMelt, aes(x=Time, y=value, color=variable, linetype=sim)) +geom_smooth()+ theme_bw() + #facet_grid(~sim)+
  labs(y="Biomass (g/m2)", x="Time (years)", title= "Biomass over time", color="Species")+guides(legend.position="bottom")


## plotting climate data 
# Section - plot climate data -----

library(ggplot2)
library(reshape2)
library(gridExtra)

climate.list<- c("climate_A2_scenario_convertedPAR_secondredo","climate_noClimateChange_scenario_convertedPAR_secondredo")

climate.data <- list()
climate.data<- lapply (climate.list, function (clim){
  
  setwd("C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/climate_scenerios_klamath_all_eco_regions")
  setwd(clim)
  
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
  endtab <- cbind(endtab, climate = rep(clim, NROW(decimalYear)))
  
  head(endtab)
  
  return(endtab)
})

mergedClimData <- do.call(rbind, climate.data)
ecras <- raster("C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/clippedRaster/ecoregsa3.tif")
#meltedCD <- melt(subset(mergedClimData, grepl( paste(sort(unique(values(ecras))), collapse='|') ,mergedClimData$EcoReg) & mergedClimData$Year<2051), id.vars =c("Year", "Month", "decimalYear", "EcoReg", "CO2", "PAR", "O3","Prec","climate"))#,"Tmax", "Tmin")) # "Tmax", "Tmin", "PAR", "Prec", "CO2"))
meltedCD <- melt(subset(mergedClimData, grepl( paste(sort(unique(values(ecras))), collapse='|') ,mergedClimData$EcoReg) ), id.vars =c("Year", "Month", "decimalYear", "EcoReg", "CO2", "PAR", "O3","Prec","climate"))#,"Tmax", "Tmin")) # "Tmax", "Tmin", "PAR", "Prec", "CO2"))


#head(meltedCD)

plot1<- ggplot(data =meltedCD, aes(x=decimalYear , y=value, linetype=variable , color=climate))+
  annotate("rect", fill = "cornsilk3", alpha = .5, 
           xmin = -Inf, xmax = 2010 ,
           ymin = -Inf, ymax = Inf) +
  geom_smooth()+ facet_wrap(~EcoReg )+
  geom_vline(xintercept=2010) + theme_bw(base_size=25)+ 
  labs(y="Temperature (C)", x="Year", title= "Climate Scenarios: Average of Yearly Temperature Extremes", color="Scenarios", linetype="Temperatures", fill="Shading")+
  scale_color_manual(labels = c("A2 Climate", "Recent Trends"), values=  hue_pal()(2)) + guides(color=guide_legend(override.aes=list(fill=NA)))+
  scale_linetype_manual(labels = c("Avg. max", "Avg. min"), values = c(1, 4))+ guides(linetype=guide_legend(override.aes=list(fill=NA))) + 
  theme(legend.position = c(0.9, 0.1));plot1  # + geom_rect(aes(xmin=1948, xmax=2010, ymin=0, ymax=Inf, color="orange"))
#legend(labels = c("Historic Climate", "Simulated Climate"), values=  c( "burlywood4", "white" ))



#meltedCD <- melt(subset(mergedClimData, grepl( paste(sort(unique(values(ecras))), collapse='|') ,mergedClimData$EcoReg) & mergedClimData$Year<2051 & mergedClimData$Year>2009), id.vars =c("Year", "Month", "decimalYear", "EcoReg", "CO2", "PAR", "O3","Prec","climate"))#,"Tmax", "Tmin")) # "Tmax", "Tmin", "PAR", "Prec", "CO2"))
meltedCD <- melt(subset(mergedClimData, grepl( paste(sort(unique(values(ecras))), collapse='|') ,mergedClimData$EcoReg) & mergedClimData$Year>2009), id.vars =c("Year", "Month", "decimalYear", "EcoReg", "CO2", "PAR", "O3","Prec","climate"))#,"Tmax", "Tmin")) # "Tmax", "Tmin", "PAR", "Prec", "CO2"))

plotprec<- ggplot(data =meltedCD, aes(x=Month , y=Prec/10 , color=climate))+
  geom_smooth()+# facet_wrap(~EcoReg )+#geom_vline(xintercept=2010) + 
  theme_bw(base_size=25)+ 
  labs(y="Precipitation (cm)", x="Month", title= "Climate Scenarios: Average Monthly Precipitation (2010-2100)", color="Scenarios")+
  scale_color_manual(labels = c("A2 Climate", "Recent Trends"), values=  hue_pal()(2)) + guides(color=guide_legend(override.aes=list(fill=NA)))+
  scale_x_continuous(labels=c("Jan", "Mar", "May", "Jul", "Sept", "Nov"), breaks=c(1,3,5,7,9,11))+
  theme(legend.position = c(0.9, 0.1));plotprec 


#meltedCD <- melt(subset(mergedClimData, grepl( paste(sort(unique(values(ecras))), collapse='|') ,mergedClimData$EcoReg) & mergedClimData$Year<2051 & mergedClimData$Year>1948), id.vars =c("Year", "Month", "decimalYear", "EcoReg", "CO2", "PAR", "O3","Prec","climate"))#,"Tmax", "Tmin")) # "Tmax", "Tmin", "PAR", "Prec", "CO2"))
meltedCD <- melt(subset(mergedClimData, grepl( paste(sort(unique(values(ecras))), collapse='|') ,mergedClimData$EcoReg)  & mergedClimData$Year>1948), id.vars =c("Year", "Month", "decimalYear", "EcoReg", "CO2", "PAR", "O3","Prec","climate"))#,"Tmax", "Tmin")) # "Tmax", "Tmin", "PAR", "Prec", "CO2"))
plotprec<- ggplot(data =meltedCD, aes(x=decimalYear , y=CO2 , color=climate))+
  annotate("rect", fill = "cornsilk3", alpha = .5, 
           xmin = -Inf, xmax = 2010 ,
           ymin = -Inf, ymax = Inf) +
  geom_smooth()+# facet_wrap(~EcoReg )+
  geom_vline(xintercept=2010)+ theme_bw(base_size=25)+
  labs(y="CO2 (ppm)", x="Year", title= "Climate Scenarios: Mean Atmospheric CO2 Concentration", color="Scenarios")+
  scale_color_manual(labels = c("A2 Climate", "Recent Trends"), values=  hue_pal()(2)) + guides(color=guide_legend(override.aes=list(fill=NA)))+
  theme(legend.position = c(0.9, 0.35));plotprec 


#early mid late climate graphs----

#PRECIPITATION
meltedCD$Prec <- meltedCD$Prec/10 # mm to cm

earlyTest <- subset(meltedCD, meltedCD$Year  %in% 2010:2040)
t1<- ggplot(earlyTest, aes(x=Month, y=Prec, color=climate)) + geom_smooth() + labs(title="Early\n(2010-2040)" )+theme_bw(base_size=20) +theme(plot.title = element_text(margin = margin(t = 10, b = -70)))+theme(plot.title = element_text(hjust = 0.5))+
           coord_cartesian(ylim=c(0, 43))+ 
           scale_color_manual("Scenarios",labels = c("A2 Climate", "Recent Trends"), values=  hue_pal()(2)) + guides(color=guide_legend(override.aes=list(fill=NA)))+
           scale_x_continuous(labels=c("Jan", "Mar", "May", "Jul", "Sept", "Nov"), breaks=c(1,3,5,7,9,11))+
           theme(axis.title.x = element_blank(), axis.title.y = element_blank())

midTest <- subset(meltedCD,  meltedCD$Year  %in% 2041:2070)
t2<- ggplot(midTest, aes(x=Month, y=Prec,color=climate))  + geom_smooth()+ labs(title="Middle\n(2041-2070)" )+theme_bw(base_size=20) +theme(plot.title = element_text(margin = margin(t = 10, b = -70)))+theme(plot.title = element_text(hjust = 0.5))+
           coord_cartesian(ylim=c(0, 43))+  
           scale_color_manual("Scenarios",labels = c("A2 Climate", "Recent Trends"), values=  hue_pal()(2)) + guides(color=guide_legend(override.aes=list(fill=NA)))+
           scale_x_continuous(labels=c("Jan", "Mar", "May", "Jul", "Sept", "Nov"), breaks=c(1,3,5,7,9,11))+
           theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
lateTest <- subset(meltedCD, meltedCD$Year  %in% 2071:2101)
t3<- ggplot(lateTest, aes(x=Month, y=Prec,color=climate))  + geom_smooth() + labs(title="Late\n(2071-2100)" )+theme_bw(base_size=20)+theme(plot.title = element_text(margin = margin(t = 10, b = -70))) +theme(plot.title = element_text(hjust = 0.5))+
            coord_cartesian(ylim=c(0, 43))+
            scale_color_manual("Scenarios",labels = c("A2 Climate", "Recent Trends"), values=  hue_pal()(2)) + guides(color=guide_legend(override.aes=list(fill=NA)))+
            scale_x_continuous(labels=c("Jan", "Mar", "May", "Jul", "Sept", "Nov"), breaks=c(1,3,5,7,9,11))+
            theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
figure <- ggarrange(t1,t2,t3, common.legend=T, legend="top", ncol=3) 

comp <- annotate_figure(figure,  top = text_grob(paste("Precipitation (2010-2100)"), color = "Black", face = "bold", size = 25),
                        bottom = text_grob("Months", color = "Black", face = "bold", size = 25),
                        left = text_grob("Precipitation (cm)", color = "Black",face = "bold", rot = 90,size = 25))
comp



#TEMPERATURE 
earlyTest <- subset(meltedCD, meltedCD$Year  %in% 2010:2040)
t1<- ggplot(earlyTest, aes(x=Month, y=value, color=climate, linetype=variable))  + geom_smooth() + labs(title="Early\n(2010-2040)"  )+theme_bw(base_size=20)+theme(plot.title = element_text(margin = margin(t = 10, b = -70))) +theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-3, 32))+
  scale_color_manual("Scenarios",labels = c("A2 Climate", "Recent Trends"), values=  hue_pal()(2)) + guides(color=guide_legend(override.aes=list(fill=NA)))+
  scale_linetype_manual("Temperatures",labels = c("Avg. max", "Avg. min"), values = c(1, 4))+ guides(linetype=guide_legend(override.aes=list(fill=NA))) + 
  scale_x_continuous(labels=c("Jan", "Mar", "May", "Jul", "Sept", "Nov"), breaks=c(1,3,5,7,9,11))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

midTest <- subset(meltedCD,  meltedCD$Year  %in% 2041:2070)
t2<- ggplot(midTest, aes(x=Month, y=value,color=climate, linetype=variable))  + geom_smooth() + labs(title="Middle\n(2041-2070)" )+theme_bw(base_size=20)+theme(plot.title = element_text(margin = margin(t = 10, b = -70))) +theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-3, 32))+
  scale_color_manual("Scenarios",labels = c("A2 Climate", "Recent Trends"), values=  hue_pal()(2)) + guides(color=guide_legend(override.aes=list(fill=NA)))+
  scale_linetype_manual("Temperatures",labels = c("Avg. max", "Avg. min"), values = c(1, 4))+ guides(linetype=guide_legend(override.aes=list(fill=NA))) + 
  scale_x_continuous(labels=c("Jan", "Mar", "May", "Jul", "Sept", "Nov"), breaks=c(1,3,5,7,9,11))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

lateTest <- subset(meltedCD, meltedCD$Year  %in% 2071:2101)
t3<- ggplot(lateTest, aes(x=Month, y=value,color=climate, linetype=variable))  + geom_smooth() + labs(title="Late\n(2071-2100)" )+theme_bw(base_size=20)+theme(plot.title = element_text(margin = margin(t = 10, b = -70))) +theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-3, 32))+
  scale_color_manual("Scenarios",labels = c("A2 Climate", "Recent Trends"), values=  hue_pal()(2)) + guides(color=guide_legend(override.aes=list(fill=NA)))+
  scale_linetype_manual("Temperatures",labels = c("Avg. max", "Avg. min"), values = c(1, 4))+ guides(linetype=guide_legend(override.aes=list(fill=NA))) + 
  scale_x_continuous(labels=c("Jan", "Mar", "May", "Jul", "Sept", "Nov"), breaks=c(1,3,5,7,9,11))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

figure <- ggarrange(t1,t2,t3, common.legend=T, legend="top", ncol=3) 


comp <- annotate_figure(figure,  top = text_grob("Temperature (2010-2100)", color = "Black", face = "bold", size = 25),
                        bottom = text_grob("Month", color = "Black", face = "bold", size = 25),
                        left = text_grob("Temperature (°C)", color = "Black",face = "bold", rot = 90,size = 25))
comp

