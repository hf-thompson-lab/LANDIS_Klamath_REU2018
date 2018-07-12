library(ggpubr)
library(ggplot2)
library(reshape2)
library(raster)

library(scales)
scenarioPath <- "D:/Evan/91 Year Runs Output"

sims <- list.dirs(scenarioPath, recursive = F, full.names = F)

sims

## making and plot mean fire return intervals ---- 

mask.active.cells <- raster('C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/clippedRaster/mask_activeCells.img')
simus.selection <- sims # 7-2-18 PnET+FIRE+FUEL+DevCUT+FBreak 50 a2 scenario

rasters <- lapply (simus.selection, function (sim){
  general.root.path <- "D:/Evan/91 Year Runs Output"#"C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/Saved Output"
  
  setwd(general.root.path)
  setwd(sim)
  
  if (length(list.files(path = './fire')) ==0) { return(NULL) }
  ras.list <- paste0('fire/severity-',1:91,'.img') 
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


s <- stack(rasters[[3]],rasters[[5]],rasters[[9]],rasters[[11]])
sp<- as(s, 'SpatialGridDataFrame')
seededplots<- spplot(sp, names.attr= simus.selection[c(3, 5, 9, 11)], main = "Mean Fire Return Interval (Years) SEED=3333",
                     colorkey=list(height =.75, at=c(seq(10, 20, 5), seq(20, 50, 10),70, 100)), labels=c(seq(10, 20, 5) ,seq(20, 50, 10),70, 100) , col.regions = colorRampPalette(c("red", "orange", "yellow","green","darkgreen") ) )
plot(seededplots)

s1 <- stack(rasters[[4]],rasters[[6]],rasters[[10]],rasters[[12]])
sp1<- as(s1, 'SpatialGridDataFrame')
notseededplots<- spplot(sp1,names.attr= simus.selection[c(4, 6, 10, 12)], main = "Mean Fire Return Interval (Years) SEEDnotSET",
                        colorkey=list(height =.75, at=c(seq(10, 20, 5), seq(20, 50, 10),70, 100)), labels=c(seq(10, 20, 5) ,seq(20, 50, 10),70, 100) , col.regions = colorRampPalette(c("red", "orange", "yellow","green","darkgreen") ) )
plot(notseededplots)

# plotting cut history and fire severity 

chRas <- lapply (simus.selection, function (sim){
  general.root.path <- "D:/Evan/91 Year Runs Output"#"C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/Saved Output"
  
  setwd(general.root.path)
  setwd(sim)
  
  if (length(list.files(path = './land-use-maps')) ==0) { return(NULL) }
  ras<- paste0('land-use-maps/cutHistory.img') 
 # ras<- paste0('logs/severityHistory.img')

  return(raster(ras))
})
commonNames <- c("A2 & LU seed=3333", "A3 and LU seed=RND", "Recent Trends & LU seed=3333", "Recent Trends & LU seed=RND") #simus.selection[c(5, 6, 11, 12)]
s2 <- stack(chRas[[5]],chRas[[6]],chRas[[11]],chRas[[12]])
sp2<- as(s2, 'SpatialGridDataFrame')
plots2<- spplot(sp2,names.attr= commonNames, main = "Cut History ")
plot(plots2)


# ploting AGB for each simulation ---- 

constrainPlot <- function(plot, xlim= c(0,91), ylim){
  cp <- plot + scale_x_continuous(limits = xlim) + scale_y_continuous(limits= ylim)
  return(cp)
}

graphs <- list()
graphs <- lapply (simus.selection, function (sim){
  general.root.path <- "D:/Evan/91 Year Runs Output"
  
  setwd(general.root.path)
  setwd(sim)
  
  agbPath<- "output/agbiomass/AGBiomass_.txt"
  agb<- read.table(agbPath, header=T)
  
  meltagb <- melt(agb, id = "Time") 
  
  p1<- ggplot(meltagb , aes(x= Time, y= value, color=variable)) +geom_smooth() + labs(title="Change in AGB over time ", subtitle= sim, y="AGB in g/m2") + theme_bw()
  p1 <- constrainPlot(p1, ylim = c(0,6000))
  return(p1)
})


# g_legend<-function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
# 
# mylegend<-g_legend(p1)



ggarrange(graphs[[1]],graphs[[3]],graphs[[5]],graphs[[7]],graphs[[9]],graphs[[11]], ncol=3, nrow=2, common.legend = TRUE, legend="bottom")

ggarrange(graphs[[2]],graphs[[4]],graphs[[6]],graphs[[8]],graphs[[10]],graphs[[12]], ncol=3, nrow=2, common.legend=T, legend="bottom")


# graphing fire stats ---- 
dataFrames <- lapply (simus.selection, function (sim){
  general.root.path <- "D:/Evan/91 Year Runs Output"
  
  setwd(general.root.path)
  setwd(sim)
  if (length(list.files(path = './fire')) ==0) { return(NULL) }
  fireEventPath <- "dynamic-fire-events-log.csv"
  dfe <- read.csv(fireEventPath, header=T)
  
  ms <- numeric(91)
  meds <- numeric(91)
  mck <- numeric(91)
  medck<- numeric(91)
  size <- numeric(91)
  for (i in 1:91)
  {
    ms[i] <- mean(dfe[dfe$Time==i,]$MeanSeverity)
    meds[i] <- median(dfe[dfe$Time==i,]$MeanSeverity)
    mck[i] <- sum(dfe[dfe$Time==i,]$CohortsKilled)
    medck[i] <- sum(dfe[dfe$Time==i,]$TotalSites)
    size[i] <- max(dfe[dfe$Time==i,]$SizeBin)
    
  }
  
  df <-data.frame(Time = 1:91, MeanSeverity=ms, MedianSeverity=meds,CohortsKilled=mck, TotalSites=medck, Size= size,  SIM=rep(sim, 91))
  return(df)
})

completedf<- do.call( rbind, dataFrames)

meltdf <- melt(completedf, id=c("Time", "MedianSeverity", "CohortsKilled", "TotalSites", "MeanSeverity", "Size" ))

p4<- ggplot(subset(meltdf, grepl(3333,meltdf$value, ignore.case = T )), aes(x=Time, y=MeanSeverity, color= value)) + geom_smooth(level=.9) + theme_bw() +  labs(title="MeanSeverity");p4

p5<- ggplot(subset(meltdf, grepl("not",meltdf$value, ignore.case = T )), aes(x=Time, y=CohortsKilled, color= value)) + geom_smooth(level=.9) + geom_jitter() + theme_bw()+  labs(title="CohortsKilled");p5



p6<- ggplot(meltdf, aes(x=Time, y=TotalSites, color= value))+geom_point() + geom_smooth() + theme_bw()+  labs(title="TotalSites");p6

meltdf$Size[meltdf$Size=="-Inf"]<-0 
p7<- ggplot(meltdf, aes(x=Time, y=Size, color= value))+geom_point() + geom_smooth() + theme_bw()+  labs(title="Max fire Size");p7


p = ggplot(subset(meltdf, grepl(3333,meltdf$value, ignore.case = T )), aes(x=Size)) + theme_bw()+
  geom_histogram(position="identity", colour="grey40", alpha=0.2, bins = 6, breaks=c(0,1,2,3,4,5)) +  labs(title="Max recorded fire size over all years")+
  facet_grid(. ~ value);p


# testing of dividing the data into early, mid, and late periods and then graphing box plots ----

# early  1:30
# mid    31 :60 
# late   61:91 

earlyTest <- subset(meltdf, grepl(3333,meltdf$value, ignore.case = T ) & meltdf$Time %in% 1:30)
t1<- ggplot(earlyTest, aes(x=value, y=MeanSeverity, color= value)) + geom_boxplot() + ylim(c(3,5)) + labs(title="Early" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())

midTest <- subset(meltdf, grepl(3333,meltdf$value, ignore.case = T ) & meltdf$Time %in% 31:60)
t2<- ggplot(midTest, aes(x=value, y=MeanSeverity, color= value)) + geom_boxplot()+ ylim(c(3,5))+ labs(title="Middle" ) + theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))


lateTest <- subset(meltdf, grepl(3333,meltdf$value, ignore.case = T ) & meltdf$Time %in% 61:91)
t3<- ggplot(lateTest, aes(x=value, y=MeanSeverity, color= value)) + geom_boxplot()+ ylim(c(3,5)) + labs(title="Late" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30))) +theme(plot.title = element_text(hjust = 0.5))+   theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))

figure <- ggarrange(t1,t2,t3, common.legend=T, legend="bottom", ncol=3) 

annotate_figure(figure,  top = text_grob("Mean Severity Seed=3333", color = "Black", face = "bold", size = 14))#,bottom = text_grob("Data source: \n ToothGrowth data set", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))


vars.toGraph <- c("MeanSeverity", "CohortsKilled", "TotalSites", "Size" )


## TOTAL AREA AFFECTED BY FIRES ---- 
labs <- c("A2 scenario", "A2 scenario & LU+", "Recent Trends", "Recent Trends & LU+")
  
  earlyTest <- subset(meltdf, grepl(3333,meltdf$value, ignore.case = T ) & meltdf$Time %in% 1:30)
  t1<- ggplot(earlyTest, aes(x=value, y=TotalSites*7.29, color= value)) + geom_boxplot() + labs(title="Early" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank() )+ theme(axis.title.y=element_blank())+
     scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75)) 

                
  midTest <- subset(meltdf, grepl(3333,meltdf$value, ignore.case = T ) & meltdf$Time %in% 31:60)
  t2<- ggplot(midTest, aes(x=value, y=TotalSites*7.29, color= value)) + geom_boxplot()+ labs(title="Middle" ) + theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))+
              scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75))
  
  
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
    general.root.path <- "D:/Evan/91 Year Runs Output"#"C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/Saved Output"
    
    setwd(general.root.path)
    setwd(sim)
    
    if (length(list.files(path = './fire')) ==0) { return(NULL) }
    ras.list <- paste0('fire/severity-',1:91,'.img') 
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
    return(cbind(l,Time= 1:91))
  }, rdata)
  complete <- do.call(rbind, rdata)
  complete$Time <- rep(1:91, 8)
  complete
  write.csv(complete ,"D:/Evan/severityData.csv")
  
  meltSeverity <- melt(complete, id=c("Time","three", "four", "five", "six", "seven"))
# graphing highest severity area ----
  earlyTest <- subset(meltSeverity, grepl("not",meltSeverity$value, ignore.case = T ) & meltSeverity$Time %in% 1:30)
  t1<- ggplot(earlyTest, aes(x=value, y=seven*7.29, color= value)) + geom_boxplot() + labs(title="Early" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank() )+ theme(axis.title.y=element_blank())+
    scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75)) +# geom_jitter(width=.3)+
    stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 2)
  
  
  midTest <- subset(meltSeverity, grepl("not",meltSeverity$value, ignore.case = T ) & meltSeverity$Time %in% 31:60)
  t2<- ggplot(midTest, aes(x=value, y=seven*7.29, color= value)) + geom_boxplot()+ labs(title="Middle" ) + theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))+  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))+
    scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75))+# geom_jitter(width=.3)+
    stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 2) 
  
  
  lateTest <- subset(meltSeverity, grepl("not",meltSeverity$value, ignore.case = T ) & meltSeverity$Time %in% 61:91)
  t3<- ggplot(lateTest, aes(x=value, y=seven*7.29, color= value)) + geom_boxplot() + labs(title="Late" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30))) +theme(plot.title = element_text(hjust = 0.5))+   theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_line(colour="grey40", linetype = "dashed"))+
    scale_x_discrete(labels=labs) + theme(axis.text.x = element_text(angle = 75)) +# geom_jitter(width=.3)+
    stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 2)
  
  figure <- ggarrange(t1,t2,t3, common.legend=T, legend="none", ncol=3) 
  
  comp <- annotate_figure(figure,  top = text_grob(paste("Highest Severity Fires Occurrence", "(Seed RND)"), color = "Black", face = "bold", size = 14),
                          bottom = text_grob("Scenarios", color = "Black", face = "bold", size = 12),
                          left = text_grob("Area affected by highest severity fire (ha)", color = "Black",face = "bold", rot = 90,size = 12))
  comp
  
  
  
  # scatter plot of fire severites
  nms <- subset(meltSeverity, grepl("not",meltSeverity$value, ignore.case = T ))
  head(nms)
  nms[,7] <- NULL
  colnames(nms)[7]<- "sim"
  nms1<- melt(nms , id=c("Time","sim"))
  head(nms1)


       
  ggplot(nms1, aes(x= Time , y=value*7.29 , color= variable)) + geom_point() +geom_smooth(level=.9) + facet_wrap(~sim) + theme_bw()+ 
    labs(y="Area affected by different severity fires (ha)", x="Time (years)", title= "Fire severity distributions across simulations", color="Severity levels")+
    scale_color_manual(labels = c("1", "2","3", "4", "5"), values=  hue_pal()(5)) + guides(color=guide_legend(override.aes=list(fill=NA)))
    
  
  
## Total cohorts exploration ---- 
  
  
  cohortData <- list()
  cohortList<- lapply (simus.selection, function (sim){
    general.root.path <- "D:/Evan/91 Year Runs Output"
    
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
  
  ggplot(cohortMelt, aes(x=Time, y=X.Cohorts, color=value)) + geom_point() +geom_smooth()+ theme_bw() + 
    labs(y="Number of cohorts", x="Time (years)", title= "Total cohorts over time", color="Scenarios")#+
    #scale_color_manual(labels = c("1", "2","3", "4", "5", "6"), values=hue_pal()(6)) + guides(color=guide_legend(override.aes=list(fill=NA)))
    
  ggplot(cohortMelt, aes(x=Time, y=WoodyDebris.kgDW.m2., color=value)) + geom_point() +geom_smooth()+ theme_bw() + 
    labs(y="Amount of woody debris (kgDW/m2)", x="Time (years)", title= "Wood Debris over time", color="Scenarios")#+
  #scale_color_manual(labels = c("1", "2","3", "4", "5", "6"), values=hue_pal()(6)) + guides(color=guide_legend(override.aes=list(fill=NA)))
  
  
  bioList <- list()
  bioList<- lapply (simus.selection, function (sim){
    general.root.path <- "D:/Evan/91 Year Runs Output"
    
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

  ggplot(bioMelt, aes(x=Time, y=value, color=sim)) +geom_smooth()+ theme_bw() + facet_grid(~variable)+
    labs(y="Biomass (g/m2)", x="Time (years)", title= "Biomass over time", color="Scenarios")+guides(legend.position="bottom")
  
  
  #unsure if the facet is more clear than the linetype 
  subBioMelt<- subset(bioMelt, grepl("3333",bioMelt$sim, ignore.case = T ))
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
  
  meltedCD <- melt(mergedClimData, id.vars =c("Year", "Month", "decimalYear", "EcoReg", "CO2", "PAR", "O3","Prec","climate"))#,"Tmax", "Tmin")) # "Tmax", "Tmin", "PAR", "Prec", "CO2"))
  
  
  #head(meltedCD)
  
  #ggplot(data =meltedCD, aes(x=decimalYear , y=Prec  , color=climate)) + geom_point()+ facet_wrap(~EcoReg ) + labs(title ="A2 Scenario")
  
  meltedCD$Prec <- meltedCD$Prec/10 # mm to cm
  
  earlyTest <- subset(meltedCD, meltedCD$Year  %in% 2010:2040)
  t1<- ggplot(earlyTest, aes(x=Month, y=Prec, color=climate)) + geom_smooth() + labs(title="Early" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))
  
  midTest <- subset(meltedCD,  meltedCD$Year  %in% 2041:2070)
  t2<- ggplot(midTest, aes(x=Month, y=Prec,color=climate))  + geom_smooth()+ labs(title="Middle" ) + theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30)))+theme(plot.title = element_text(hjust = 0.5))
  
  lateTest <- subset(meltedCD, meltedCD$Year  %in% 2071:2101)
  t3<- ggplot(lateTest, aes(x=Month, y=Prec,color=climate))  + geom_smooth() + labs(title="Late" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -30))) +theme(plot.title = element_text(hjust = 0.5))
  
  figure <- ggarrange(t1,t2,t3, common.legend=T, legend="bottom", ncol=3) 
  
  comp <- annotate_figure(figure,  top = text_grob(paste("Precipation"), color = "Black", face = "bold", size = 14),
                          bottom = text_grob("Months", color = "Black", face = "bold", size = 12),
                          left = text_grob("Precipation (cm)", color = "Black",face = "bold", rot = 90,size = 12))
  comp
  
  
  earlyTest <- subset(meltedCD, meltedCD$Year  %in% 2010:2040)
  t1<- ggplot(earlyTest, aes(x=Month, y=value, color=variable, linetype=climate)) + geom_smooth() + labs(title="Early" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -5)))+theme(plot.title = element_text(hjust = 0.5))+ 
    coord_cartesian(ylim=c(-3, 32))
  
  midTest <- subset(meltedCD,  meltedCD$Year  %in% 2041:2070)
  t2<- ggplot(midTest, aes(x=Month, y=value,color=variable, linetype=climate))  + geom_smooth()+ labs(title="Middle" ) + theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -5)))+theme(plot.title = element_text(hjust = 0.5))+ 
    coord_cartesian(ylim=c(-3, 32))
  
  lateTest <- subset(meltedCD, meltedCD$Year  %in% 2071:2101)
  t3<- ggplot(lateTest, aes(x=Month, y=value,color=variable, linetype=climate))  + geom_smooth() + labs(title="Late" )+ theme_bw() +theme(plot.title = element_text(margin = margin(t = 10, b = -5))) +theme(plot.title = element_text(hjust = 0.5))+ 
    coord_cartesian(ylim=c(-3, 32))
  
  figure <- ggarrange(t1,t2,t3, common.legend=T, legend="bottom", ncol=3) 
  

  comp <- annotate_figure(figure,  top = text_grob("Temperature", color = "Black", face = "bold", size = 14),
                          bottom = text_grob("Month", color = "Black", face = "bold", size = 12),
                          left = text_grob("Temperature (°C)", color = "Black",face = "bold", rot = 90,size = 12))
  comp
  
  