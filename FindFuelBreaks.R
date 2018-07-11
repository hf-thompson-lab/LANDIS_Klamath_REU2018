
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
    #this works better if you fill the areas of the fuelRaster that you don't want fuel breaks with 0
# @param fuelValsToConsider - index of fuelVal +1 that you want to cut breaks for
# @param minCellsConnect - is the smallest cells connected you want to search for
# @param numFuelsToSplit - is the number of the largest connected components that you want to split in 4's 
# @param numOfLargestFuelsToPickFrom - is the number of the largest connected components that you want to split in 4's that you want to randomly sample from to split
# @param tabuList - if TRUE, implement the createTabuList function
findFuelBreaks <- function(fuelRaster, fuelValsToConsider= (c(1:6,10,11)+1), minCellsConnect , numFuelsToSplit, numOfLargestFuelsToPickFrom, tabuList=T, rownum=310, colnum=312 ){
  require(raster)
  require(spatstat)
  
  tR1<-fuelRaster
  values(tR1)[which(values(tR1) >0)] <- -1 
  
  # take the fuels that are connected to minCellsConnect or more for each fuel type and tag them with a unique label... 
  for (fuelVal in fuelValsToConsider){
    
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
    imageF <- im(matrix(values(tR), rownum, colnum)) #the dimesion are flipped from what the raster dims are 
    ci <- connected(imageF, method= "C",background = 0) # get the connected components for that fuelVal
    values(tR) <- as.integer(ci$v) #convert the image back to a raster matrix format
    
    # get the largest occuring connected components that are with minCellsConnect and 12000
    fq <-as.data.frame( freq(tR)) 
    nfq<-na.omit(subset(fq,(fq$count>minCellsConnect & fq$count< (rownum*colnum)/5)  )) # if connect components are too large then the fuel breaks would be too long
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
  
  if (length(newerfq$value) < numOfLargestFuelsToPickFrom) {numOfLargestFuelsToPickFrom <- length(newerfq$value)}
  if (length(newerfq$value) < numFuelsToSplit) {numFuelsToSplit <- length(newerfq$value)}
  toDivide <- head(newerfq, n=numOfLargestFuelsToPickFrom)
  print(toDivide)
  print(paste("sampling ", numFuelsToSplit," from this list"))
  toDivide <- toDivide[sample(1:numOfLargestFuelsToPickFrom, numFuelsToSplit),]  # randomized the fuel beaks that we avoid cutting the same thing over and over

  # set up for the spliting (changing NA values and outOfBounds values)
  emptyR<-fuelRaster
  values(emptyR)[which(values(emptyR) >0)] <- -1 
  values(emptyR)[which(values(emptyR) ==-1)] <- 0

  #df to storage data about the splits in (used to see if it was working properly)
  storage <- data.frame(colEq=numeric(numFuelsToSplit), rowEq=numeric(numFuelsToSplit), step=numeric(numFuelsToSplit))
  
  #holder stores the cuts, holder1 stores the large connected components (where if there is a conflict on cell the larger are the ones overwrite) 
  holder <- emptyR
  holder2 <- emptyR
  holder1 <- matrix(values(holder), c(rownum,colnum) )
  
  #split up 
  for (i in 1:(numFuelsToSplit)){
    emptyRTemp <-emptyR
    print(i)
    toFind <- toDivide$value[i]
    
    cellsFound <- which(values(tR1) == toFind)
    values(emptyRTemp)[cellsFound] <- i #temp var... 
    values(holder2)[cellsFound] <- i 
    
    eVals <- matrix(values(emptyRTemp), c(rownum,colnum) ) 
    
    #balance the values of the row and column sum using the equilibrium function to obtian the middle of the connected components
    #row and col are flipped (probably because of projection?)
    cols <- colSums(eVals)
    ret <-equalibrium(arr=cols, tol=max(cols))
    holder1[which(eVals[,ret]==i), ret]<- i*100 
    
    rows <- rowSums(eVals)
    retr <- equalibrium(arr=rows, tol=max(rows))
    holder1[retr, which(eVals[retr,]==i)]<- i*100 +1 
    
    storage[i,] <- data.frame(colEq=ret, rowEq=retr, step=i*100)# log this? 
  }
  print(storage) 

  values(holder) <-as.vector(holder1)
  # convert the temporary NA values (0) back to NA 
  values(holder)[which(values(holder) ==0)] <- NA
  
  #if tabuList is TRUE then make sure that the cuts are not too close
  if (tabuList){ holder <- createTabuList(cuts=storage, cutRaster= holder, tabuLength = 10)} 
  
  return(holder)
}

# reduce the amount of cuts that are stacking next to each other
# @param cuts - the row and column that make the cuts
# @param cutRaster - the location of the cuts in the raster where the values corresponds to an ID from cuts 
# @param proximityTol - cuts within this tolerance will be disregarded
# @param tabuLength - number of years that the tabu list will remember
createTabuList <- function(cuts, cutRaster, proximityTol =10 , tabuLength){
  #get the current timestep to use as id
  timestep <- as.numeric(readLines( "lockfile",n = 1))
  
  #read in the past cuts 
  tabuList <- read.csv(file.path("logs","tabuList.csv"), header=T)
  
  #throw out cuts that are within tolerance
  for (i in 1:length(cuts$colEq))
  {
    if ( checkTolerance(cuts[i,]$colEq, cuts[i,]$rowEq, tabuList, tol=proximityTol) ) #if cuts are within the tolerance 
    {
      cutVals <- values(cutRaster)
      cutVals[ which(cuts[i,]$step== cutVals | cuts[i,]$step == (cutVals+1) )]<- NA
      values(cutRaster) <- cutVals
      print("removing private cuts ...." )
    }
  }
  
  #update tabulist
  if ( length(tabuList$colEq) < tabuLength)  # if tabu list is not at capacity, then fill it
  {
    tabuList <- rbind(tabuList, cbind(cuts, id=rep(timestep,length(cuts$colEq)))) 
  }
  else { #tabu list is full. replace the oldest id.
    tabuList <- read.csv(file.path("logs","tabuList.csv"), header = T)
    minID <- min(tabuList$id)
    rowsToReplace <- which(minID==tabuList$id)
    tabuList <- tabuList[-rowsToReplace,]
    tabuList<- rbind(tabuList,cbind(cuts, id=rep(timestep,length(cuts$colEq))))
  }
  
  #write out updated tabu list 
  write.csv(tabuList, file.path("logs","tabuList.csv"), row.names = F, quote = F)
  
  #return updated cutRaster
  return(cutRaster)
  
}

checkTolerance <- function(c, r, tabuList, tol){
  
  if (length(tabuList$colEq) == 0){return(FALSE)} #if the tabuList is empty then there is nothing to check 
  
  checks <- apply (tabuList,1, function (row){ 
    row<-as.data.frame(t(row)); 
    return( (((row$rowEq-tol)<r) & (r<(row$rowEq +tol))) | (((row$colEq-tol)<c) & (c<(row$colEq +tol))) ) 
  })
  #print(Reduce("|", checks))
  return(Reduce("|", checks)) # if there is at least one true value that means we are within the tolerance and we do not want to keep that cut. 
}
