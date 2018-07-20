#Evan Waldmann
#6/28/18

# Useful code with descriptions 


## Section - ploting with pauses ---- 
sim <- function(path, n=100, pause=0.25) {
  for (i in 1:n) {
    x<- raster(paste0(path,i,".img"))
    plot(x, legend=T, asp=NA, main=i)
    dev.flush()
    Sys.sleep(pause)
  }
  invisible(x)
}


setwd("C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/Saved Output/PnET+Fire+Fuel+NoClimate 50Years/")
setwd("C:/Users/hfintern/Desktop/Klamath_ForestXSiskiyouCounty/7-10 test/land-use-maps/")
sim(path = "land-use-", n=91, pause=1)


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


# masking the empty inital communites out

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

#masking empty initial communitites END 

