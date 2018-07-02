
# equilibrium index of list with tolerance for equality #modified from https://www.geeksforgeeks.org/equilibrium-index-of-an-array/ 
equalibrium <- function(arr, tol) {
  sum <- 0 #initialize sum of whole array
  leftsum <- 0 # initialize leftsum
  n<- length(arr)
  sum <- sum(arr)
  
  for (i  in 1:(n-1)) {
    sum <- sum - arr[i]# // sum is now right sum for index i
    #print(sum)
    if (leftsum <= (sum+tol) & leftsum >= (sum-tol))
      return( i)
    
    leftsum <- leftsum + arr[i]
  }
  # /* If no equilibrium index found with in the tolerance, then return 0 */ 
  return(-1) # i dont know if this can ever happen with how I call it in findfuelbreaks. 
}

# find connected components of fuelTypes that can be spilt into 4's  
# @param fuelRaster      - is the fuelType raster from Dyanmic Fire and Fuels Landis-II extension 
# @param minCellsConnect - is the smallest cells connected you want to search for
# @param numFuelsToSplit - is the number of the largest connected components that you want to split in 4's 
findFuelBreaks <- function(fuelRaster, minCellsConnect, numFuelsToSplit){
  require(raster)
  require(spatstat)
  
  tR1<-fuelRaster
  values(tR1)[which(values(tR1) >0)] <- -1 
  
  # take the fuels that are connected to 20 or more for each fuel type and tag them with a unique label... 
  for (fuelVal in 1:12){
    
    tR <- fuelRaster
    print(fuelVal)
    
    destroys <- which(values(tR != fuelVal))
    if(length(destroys)>= length(values(fuelRaster))-minCellsConnect)
    {
      print("bad -- too few fuels in fuel type. ")
      next() 
    }
    values(tR)[destroys] <- 0  
    
    # convert to image object to use the connected function from spatstat library
    imageF <- im(matrix(values(tR), 310, 312)) #the dimesion are flipped from what the raster dims are 
    ci <- connected(imageF, method= "C",background = 0) # get the connected components for that fuelVal
    values(tR) <- as.integer(ci$v) #convert the image back to a raster matrix format
    
    # get the largest occuring connected components that are with minCellsConnect and 12000
    fq <-as.data.frame( freq(tR)) 
    nfq<-na.omit(subset(fq,(fq$count>minCellsConnect & fq$count<12000)))
    nfq<- nfq[order(-nfq$count),]
    if (length(nfq$value) == 0) {print("empty nfq - no connected components"); next()}
    nfq<- cbind(nfq, rank=length(nfq$value):1 )
    
    # TODO - print(nfq) #probably want to log these----
    
    temp <- values(tR) 
    
    # changes the values that connected() returns to the respective rank
    mapper <- function (x){ y<-nfq$rank[which(nfq$value== x)];  if(length(y) == 0){return(NA) }else{return(y)}} #which returns integer(0) on not found, we need NA 
    temp2 <- sapply(temp, mapper)
    
    # combine the connected components to be on a single raster with a unique label 
    replace<-!is.na(temp2)
    values(tR1)[replace]<- temp2[replace]*(10^ (fuelVal-1)) 
  }
  
  temp <- values(tR1)
  values(tR1)[which(temp==-1 | temp==0)] <-NA #switch the background and out of bounds cells to be NA. 
  
  # get the largest connected components that we want to split up with fuel breaks 
  newfq <- data.frame(freq(tR1))
  newerfq<- na.omit(newfq[order(-newfq$count),])
  toDivide <- head(newerfq, n=numFuelsToSplit)
  
  # set up for the spliting (changing NA values and outOfBounds values)
  emptyR<-fuelRaster
  values(emptyR)[which(values(emptyR) >0)] <- -1 
  values(emptyR)[which(values(emptyR) ==-1)] <- 0

  #df to storage data about the splits in (used to see if it was working properly)
  storage <- data.frame(colEq=numeric(numFuelsToSplit), rowEq=numeric(numFuelsToSplit), step=numeric(numFuelsToSplit))
  
  #holder stores the cuts, holder1 stores the large connected components (where if there is a conflict on cell the larger are the ones overwrite) 
  holder <- emptyR
  holder2 <- emptyR
  holder1 <- matrix(values(holder), c(310,312) )
  
  #split up 
  for (i in 1:(numFuelsToSplit)){
    emptyRTemp <-emptyR
    print(i)
    toFind <- toDivide$value[i]
    
    cellsFound <- which(values(tR1) == toFind)
    values(emptyRTemp)[cellsFound] <- i #temp var... 
    values(holder2)[cellsFound] <- i 
    
    eVals <- matrix(values(emptyRTemp), c(310,312) ) 
    
    #balance the values of the row and column sum using the equilibrium function to obtian the middle of the connected components
    #row and col are flipped (probably because of projection?)
    cols <- colSums(eVals)
    ret <-equalibrium(arr=cols, tol=max(cols))
    holder1[which(eVals[,ret]==i), ret]<- i*100 
    
    rows <- rowSums(eVals)
    retr <- equalibrium(arr=rows, tol=max(rows))
    holder1[retr, which(eVals[retr,]==i)]<- i*100 +1 
    
    storage[i,] <- data.frame(colEq=ret, rowEq=retr, step=i)# log this? 
  }
  print(storage) 
  
  values(holder) <-as.vector(holder1)
  # convert the temporary NA values (0) back to NA 
  values(holder)[which(values(holder) ==0)] <- NA
  
  return(holder)
}