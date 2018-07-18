#masking empty initial communitites 

getwd()
root.path <- "C:/Users/hfintern/Desktop/"
initialCommFilePath<- "ICsimplified_E4_v4_270M_5Species.txt"
setwd(root.path) 


lines<- readLines(initialCommFilePath)
lines
newLines <- lines[ lines!="" ] 
newLines
boolVect <- grepl("MapCode" , newLines)
newVect <- rep(F, length(boolVect))

for (i in 1:(length(boolVect)-1))
{
  
  if (boolVect[i] ==T & boolVect[i+1]==T)
  {
    newVect[i] <- T 
  }
    
}

codes <- newLines[newVect]

codes <- as.numeric(substring(codes,9))


initCommRaster <- raster("initComClipped16S.img")
plot(initCommRaster)
values(initCommRaster)[which(values(initCommRaster) %in% codes)] <- 5000


ecoReg <- raster("ecoregclip.tif")
plot(ecoReg)
freq(ecoReg)
values(ecoReg)[which(values(initCommRaster) ==5000)] <- NA

writeRaster(ecoReg, "ecoregclipMasked", overwrite =T, format="HFA", datatype="INT2S", NAvalue=0)

# i then had to drag map into arc map and use copy raster to get landis to read it properly 

setwd("D:/")
