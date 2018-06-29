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

