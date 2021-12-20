#for an input 30x30xn ,set of M filters 3x3xn
#output 28X28xM
conv <-function(input_layer, input_filters){
  helper <- input_filters[[1]]
  #creating an array with 28,28,M to store the results for every filter
  fm <- array(0,dim=c(dim(input_layer)[1] - dim(helper)[1] +1,dim(input_layer)[2] - dim(helper)[2] +1,length(input_filters)))
  #examine each filter on its own, if the filter and image uses RGB coloring we examine each color feature map on its own and sum the results
  #if its a greyscale image/filter we only have to call the conv_sliding function
  for (num in 1:length(input_filters)){
    filt <- input_filters[[num]]
    k <-dim(filt)[3]
    if(k>1){#if multiple layers
      helper2 <- filt[,,1]
      out <- conv_sliding(input_layer[ , ,1], helper2)
      for(cl in 2:k){
        #conv_sliding helper function that does the sliding window process
        helper3 <-filt[ , , cl]
        out<- out + conv_sliding(input_layer[ , ,cl], helper3)
      }
    }
    else{
      out <- conv_sliding(input_layer[,,1], filt)
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
  fsize <- dim(filter)[1]
  imgsize1 <- dim(image)[1]
  imgsize2<- dim(image)[2]
  res <- array(0,dim=c(dim(image)[1]-fsize+1,dim(image)[2]-fsize+1))
  for(i in (1:(imgsize1 - fsize+1 ))){
    for(j in (1:(imgsize2 -fsize+1))){
      examined <- image[i:i+fsize-1, j: j+fsize-1]
      result <- examined * filter
      res[i,j] = sum(result)
    }
  }
  return(res)
}


x1 <- array(3, dim=c(3,3,3))  # uniform
x2 <- array(0.5, dim=c(3,3,3))/ 16
z <- list(x1,x2)


# matrix
image_matrix <- array(2, dim=c(5,5,3))

res <- conv(image_matrix,z)
print(res)
