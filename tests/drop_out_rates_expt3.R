# Pixel drop out rate expt 3

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

for(i in 1:length(fnames)) {
  print(paste0(i, ": ", fnames[i]))
  pth1     <- paste0(root.pth, fnames[i]) # Path to an image in a stack
  stk1     <- thresh.img.obj(image_read(pth1))
  stk1.doi <- emp.pix.dropout.rates(stk1)
  print(stk1.doi)
}

# pth1 <- paste0(root.pth,"7.gif") # Path to an image in a stack
#
# # Load and mean threshold registered image stack(s):
# stk1 <- thresh.img.obj(image_read(pth1))
# emp.pix.dropout.rates(stk1)

