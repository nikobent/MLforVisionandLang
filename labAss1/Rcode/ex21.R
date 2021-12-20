#input 30x30xn ,set of M filters 3x3xn
#output 28X28xM
conv <-function(input_layer, input_filters){
  helper <- input_filters[[1]]
  #creating an array with 28,28,M to store the results for every filter
  fm <- array(0,dim=c(input_layer.shape[1] - helper.shape[1] + 1 ,input_layer.shape[2] - helper.shape[2] + 1,length(input_filters)))
  #examine each filter on its own, if the filter and image uses RGB coloring we examine each color feature map on its own and sum the results
  #if its a greyscale image/filter we only have to call the conv_sliding function
  for (num in 1:length(input_filters)){
    filt <- input_filters[[num]]
    if(filt.shape[3]>2){#if RGB
      out <- conv_sliding(input_layer[ , ,1], filt[ , , 1])
      for( cl in 2:filt.shape[3]){
        #conv_sliding helper function that does the sliding window process
        out<- out + conv_sliding(input_layer[ , ,cl], filt[ , , cl])
      }
    }
    else{
      out <- conv_sliding(input_layer, filt)
    }
    fm[, ,num]<- out #paste the result for the filter and iterate again
  }
  return(fm)
}

conv_sliding <- function(image, filter){
  #helper function that performs the convolution for an image and a filter
  #get the dimensions, create empty array to store results
  #for loops to iterate through the image and create the area of the image to be examined
  #multiplication on the area and sum them
  fsize <- filter.shape[1]
  imgsize1 <- image.shape[1]
  imgsize2<- image.shape[2]
  res <- array(0,dim=c(input_layer.shape[1] - helper.shape[1] + 1 ,input_layer.shape[2] - helper.shape[2] + 1))
  for(i in fsize/2:(imgsize1 - fsize/2 -2)){
    for (j in fsize/2:(imgsize2 - fsize/2 - 2)){
      examined <- image[i:i+fsize, j: j+fsize]
      result <- examined * filter
      res[i][j] <- sum(result)
    }
  }
  return(res)
}
  


conv_sliding <- function(image, filter){
  #helper function that performs the convolution for an image and a filter
  #get the dimensions, create empty array to store results
  #for loops to iterate through the image and create the area of the image to be examined
  #multiplication on the area and sum them
  fsize <- dim(filter)[1]
  imgsize1 <- dim(image)[1]
  imgsize2<- dim(image)[2]
  res <- array(0,dim=c(dim(image)[1] - fsize + 1 ,dim(image)[2] - fsize + 1))
  for(i in fsize/2:(imgsize1 - fsize/2 -2)){
    for (j in fsize/2:(imgsize2 - fsize/2 - 2)){
      examined <- image[i:i+fsize, j: j+fsize]
      result <- examined * filter
      res[i][j] <- sum(result)
    }
  }
  return(res)
}
