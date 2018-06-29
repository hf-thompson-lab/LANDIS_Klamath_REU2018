
# equilibrium index of list with tolerance for equality #https://www.geeksforgeeks.org/equilibrium-index-of-an-array/ 
equalibrium <- function(arr, tol) {
  sum <- 0 #initialize sum of whole array
  leftsum <- 0 # initialize leftsum
  n<- length(arr)
  sum <- sum(arr)
  
  for (i  in 1:(n-1)) {
    sum <- sum - arr[i]# // sum is now right sum for index i
    print(sum)
    if (leftsum <= (sum+tol) & leftsum >= (sum-tol))
      return( i)
    
    leftsum <- leftsum + arr[i]
  }
  # /* If no equilibrium index found, then return 0 */
  return(-1)
}

#
findFuelBreaks <- function(fuelRaster, minCellsConnect, numFuelsToSplit){
  
  require(raster)
  require(spatstat)
  
  tR1<-fuelRaster
  values(tR1)[which(values(tR1) >0)] <- -1 
  #plot(tR1)
  
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
    
    imageF <- im(matrix(values(tR), 310, 312))
    #plot(imageF)
    
    ci <- connected(imageF, method= "C",background = 0)
    #plot(ci)
    
    values(tR) <- as.integer(ci$v)
    #plot(tR)
    # freq(tR)
    
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
    
    #hist(temp2)
    
    replace<-!is.na(temp2)
    values(tR1)[replace]<- temp2[replace]*(10^ (fuelVal-1)) 
  }
  
  temp <- values(tR1)
  values(tR1)[which(temp==-1 | temp==0)] <-NA #switch the background and out of bounds cells to be NA. 
  
  plot(tR1)
  
  newfq <- data.frame(freq(tR1))
  newerfq<- na.omit(newfq[order(-newfq$count),])
  
  toDivide <- head(newerfq, n=numFuelsToSplit)
  
  #??
  emptyR<-fuelRaster
  values(emptyR)[which(values(emptyR) >0)] <- -1 
  plot(emptyR)
  
  for (i in 1:(length(toDivide))){
    
    toFind <- toDivide$value[i]
    
    cellsFound <- which(values(tR1) == toFind)
    values(emptyR)[cellsFound] <- i #temp var... 
    

    values(emptyR)[which(values(emptyR) ==-1)] <- 0 
    eVals <- matrix(values(emptyR), c(310,312) ) 

    #hist(colSums(eVals)) 
    
    #row and col are flipped (probably because of projection?)
    cols <- colSums(eVals)
    ret <-equalibrium(arr=cols, tol=max(cols))
    eVals[which(eVals[,ret]==i), ret]<- i*10 
    
    rows <- rowSums(eVals)
    ret <- equalibrium(arr=rows, tol=max(rows))
    eVals[ret, which(eVals[ret,]==i)]<- i*10 +1 
    
    values(emptyR) <-as.vector(eVals)
  }
  
  plot(emptyR)
  freq(emptyR)
  
}