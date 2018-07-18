#inset plots using example data sets

library(grid)
library(datasets)
library(ggplot2)
library(reshape2)

data(iris) # get example dataset

meltedIris <- melt(iris)

#Main plot
g <- ggplot(meltedIris, aes(Species, value))
g <- g + geom_boxplot(aes(fill=factor(Species))) + theme_bw()+
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) +   theme(legend.position = c(0.315, 0.89)) + # legend position can be adjusted accordingly
 labs(title="graph title", x="title", fill = "legend title",y="y title")#+
#  scale_fill_manual(labels = c("A2 Climate","A2 Climate & LU+", "Recent Trends"), values=  hue_pal()(3))+  #these lines can be used to change the x labs or legend labels
#  scale_x_discrete(labels= c("1","2","3","4","5 (All affected cohorts killed)"))#, values= c("three","four", "five"))


#get the total values for the bar plot 
totalSumBars <- t(data.frame(totals= sapply( unique(meltedIris$Species), function(speci){
  return(  sum(meltedIris$value[ meltedIris$Species == speci]) )
})))

colnames(totalSumBars) <- unique(meltedIris$Species)
barplotDataMelt <- melt(totalSumBars)
subplot <- ggplot(barplotDataMelt, aes(Var2, value , fill=factor(Var2))) + theme_bw() +geom_bar(stat="identity")+
  theme(legend.position = "none") + labs(subtitle="small graph title", y="small graph y axis", x="small graph x axis")+ 
  theme(axis.text.x =element_blank(), axis.ticks.x = element_blank(),text = element_text(size=9))

vp <- viewport(width = 0.2, height = 0.2, x = .07,y = .76, just = c("left","bottom")) # view port determines the size of the small graph
full <- function() {
  print(g)
  theme_set(theme_bw(base_size = 8))
  print(subplot, vp = vp)
  theme_set(theme_bw())
}

full() # this graphs the two together