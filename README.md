# LANDIS_Klamath_REU2018
Working LANDIS-II simulation of the Klamath National Forest study area. PnET, Dynamic Fire and Fuels, and Land Use Plus are running at 10 year time steps.

This project was done to show that the Land Use Plus extension for LANDIS can be used to simulate dynamic human response (which is simulated by the land-use-FireResponseToRun.R and FindFuelBreaks.R files) using LU+'s ablity to run external scripts/executables. I have also include some of the code used for analysis and graphing, as well as some of the code for extracting climate data from netcdf files.  

To run the LANDIS simulation, you have to install LANDIS-II and all other extension mentioned above. I have include a .zip file of the Dynamic Fire and Fuels dll because I believe the new dll causes errors in what LANDIS expects for the fire parameter text files. 
