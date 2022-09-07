# Pixel drop out rate expt 4

library(magick) # Library that can read/write .gif files
library(Rvision)
library(plot.matrix)
library(pixelproj)

#255 = white
#0   = black

# Path to a .gif file
#root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set2/20 feather pattern/"
root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set3/extract 10 region from registeredCB06/"

#get directory contents
fnames <- list.files(root.pth)

#for(i in 1:length(fnames)) {
for(i in 9) {
  print(paste0("Image ",i, ", filename: ", fnames[i]))
  pth1     <- paste0(root.pth, fnames[i]) # Path to an image in a stack
  stk1     <- thresh.img.obj(image_read(pth1))
  stk1.doi <- emp.pix.dropout.rates(stk1)
  print(stk1.doi)
}

dim(stk1[[1]])
plot(stk1[[4]])
stk1.doi

# feathering pattern template:
fpt <- stk1[[4]][,,1]
fpt[as.matrix(stk1.doi[,c(1,2)])]
stk1.doi
#stk1.doi
#sample(c(255,0), size = 1, prob = c(stk1.doi[1,3], 1-stk1.doi[1,3]))

plot(stk1[[1]][,,1], key=NULL)
plot(stk1[[2]][,,1], key=NULL)
plot(stk1[[3]][,,1], key=NULL)
plot(stk1[[4]][,,1], key=NULL)
plot(stk1[[5]][,,1], key=NULL)
nsim <- 50
for(i in 1:nsim){

  # Feathering pattern template for the simulation. Doesn't matter which real one we pick:
  fpt <- stk1[[1]][,,1]

  # Null out just in case
  fpt[as.matrix(stk1.doi[,c(1,2)])] <- NA

  # Use drop out rates (probability the pixel will be white (255)) for non-constant pixels
  fpt[as.matrix(stk1.doi[,c(1,2)])] <- sapply(1:nrow(stk1.doi), function(xx){sample(c(255,0), size = 1, prob = c(stk1.doi[xx,3], 1-stk1.doi[xx,3]))})
  plot(fpt, main=paste("Sim#:", i), key=NULL)
  Sys.sleep(0.25)

}



