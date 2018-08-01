# LANDIS_Klamath_REU2018
This repository should contain a working LANDIS-II simulation of the Klamath National Forest study area where PnET, Dynamic Fire and Fuels, and Land Use Plus run at 10 year time steps.

The purpose of this project was to show that the Land Use Plus extension for LANDIS can be used to simulate dynamic human response (which is preformed by the land-use-FireResponseToRun.R and FindFuelBreaks.R files) using LU+'s ablity to run external scripts/executables. I have also included some of the code used for analysis and graphing, as well as some of the code for extracting climate data from netcdf files.  

To run the LANDIS simulation, you have to install LANDIS-II and all other extensions mentioned above (and most likely some others that I will try to state later on). Once everything is installed, you can navigate to the Landis_Parameters folder in which the Runit.bat will start the simulation. I have include a .zip file of the Dynamic Fire and Fuels dll because I believe the new dll causes errors in what LANDIS expects for the fire parameter text files. If you are able to get LANDIS to run, you will notice that some of the output still prints to the parameter folder; I think that this is caused by hard coded paths for the some of the outputs in the verison of dynamic fire and fuels that I am using. 


I have included some extra information below that might help with running and modifying the r code that simulates reactive land management. 



-------------------------------------------------------------------
What to Change?
-------------------------------------------------------------------
I have tired to list everything that you could change if you wanted to adapt the R Script "land-use-FireResponseToRun.R" and "FindFuelBreaks.R", but this is probably not exhaustive. 

If you change the input maps, you need to change the paths and names to them. 


The nrow and ncol variables should match your map size as well. 


The if statement "if (timestep ==10)" should be "if (timestep =={insert your first timestep here})", so if you were running it yearly it would be "if (timestep ==1)". This is to write and clear out log files. I am pretty sure could delete all the log files made here except the tabulist.csv, which is used to make sure that cuts made by private owner do not repeat if they are within a certain tolerance (number of cells within each other). 


The "# Section - cut limits and determining inactive cells and prohibited cells from cut history ----" should change if the land owner map changes the 1111 is specific map code that can be seen below. 


"maxPercentCut =.075" is also something that should change if the mapsize changes or if you want to change the dynamics of how much people are cutting. 


"cells <- which(values(developRaster) > 580)" gets all the developed cells from the land cover data which is in the developRaster 


This section is worth talking about. 
"BEGIN CODE 
    # Section - get fuel break cuts ---- 
    source("FindFuelBreaks.R")
    #slim down the size of so that we are only planning fuel breaks on the private side of the land
    tempFuelRas <- fuelRaster
    vals <- values(tempFuelRas)
    matVals <- matrix(vals, nrow=ncol, ncol=nrow) # have to flip nrow and ncol for raster to matrix conversion 
    matVals[1:95,]<- 0 #95 is about where there forest service land stops and and the private land begins in the land use map. 
    values(tempFuelRas)<- as.vector(matVals)
    fuelBreaks <- findFuelBreaks(tempFuelRas, fuelValsToConsider= (c(1:6,10,11)+1), minCellsConnect=20, numFuelsToSplit=15, numOfLargestFuelsToPickFrom=30, rownum=ncol, colnum =  nrow,pathAddition=pathAddition)# have to flip nrow and ncol for the raster to matrix conversion
    values(dR)[which(!is.na(values(fuelBreaks)))] <- 155
END CODE"
The find fuel breaks algorithm works better when you limit the size to where you want the fuel breaks placed. Around column 95 of the study area is where the transition from Forest Service Owned Land to Privately Owned land occurs, since the Forest Service does not use the reactive fuel breaks it is best to cut them out of the fuel map by using map code 0, which is no fuel. Additionally the call to findFuelBreaks can be rather confusing - See the documentation in the code of the FindFuelBreaks.R file.  


"ONLYFIREBREAK <-F # if TRUE it skips over the proximity to development and salvage cutting. 
PERMANENTFSFIREBREAKS <- T # if TRUE this forces FS land to use the same  fire break cuts" 
The only firebreak code in its else statement (around line 238) will need to be tweaked, especially in what it passes as parameters in its call to findFuelBreaks(). 

------------------------------------------------------------------------
Map Codes that should be useful when reading the R Scripts. 
------------------------------------------------------------------------
The land owner map has the following codes:  (luMaster)
1	- in study area 	
101	- in study area & in Klamath National forest (Private Owners)
111 	- in study area & in Klamath National forest & in National Wilderness area (Private Owners) 
1101	- in study area & in Klamath National forest (Owned by Forest Service) 
1111	- in study area & in Klamath National forest & in National Wilderness area (Owned by Forest Service) 
NA 	- Outside of study area


The land cover map codes that we care about are:  (developRaster) 
580 	- Mines, pit, oilwells
581	- Developed open space
582	- Developed low intensity 
583	- Developed medium intesity 
584	- Developed high intensity


Fuel type of 0 means that there was no fuel on that site.  (fuelRaster)


Forest Service Permanent Fuel Breaks Map Codes:  (fsCutRaster) 
1 	- Left FS land 
2 	- Middle FS land
3 	- Right FS land 
50 	- Wilderness Firebreak 
98 	- private right land in National Forest
99	- private left land in National Forest 
