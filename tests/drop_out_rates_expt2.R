# Pixel drop out rate expt 2

library(magick) # Library that can read/write .gif files
library(Rvision)
library(plot.matrix)
library(pixelproj)

#255 = white
#0   = black

# Path to a .gif file
#root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set2/20 feather pattern/"
root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set3/extract 10 region from registeredCB06/"

pth1 <- paste0(root.pth,"5.gif") # Path to an image in a stack

# Load and mean threshold registered image stack(s):
stk1 <- thresh.img.obj(image_read(pth1))

plot(stk1[[1]][,,1])
plot(stk1[[2]][,,1])
plot(stk1[[3]][,,1])
plot(stk1[[4]][,,1])
plot(stk1[[5]][,,1])

emp.pix.dropout.rates(stk1)

