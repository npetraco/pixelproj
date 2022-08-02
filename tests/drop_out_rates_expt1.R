# Pixel drop out rate expt 1

library(magick) # Library that can read/write .gif files
library(Rvision)
library(plot.matrix)
library(pixelproj)

#255 = white
#0   = black

# Path to a .gif file
#root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set2/20 feather pattern/"
root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set3/extract 10 region from registeredCB06/"

pth1 <- paste0(root.pth,"7.gif") # Path to an image in a stack

# Load and mean threshold registered image stack(s):
stk1 <- thresh.img.obj(image_read(pth1))

stk1[[1]][,,1]
stk1[[2]][,,1]
stk1[[3]][,,1]
stk1[[4]][,,1]
stk1[[5]][,,1]

stka <- array(c(stk1[[1]][,,1],
        stk1[[2]][,,1],
        stk1[[3]][,,1],
        stk1[[4]][,,1],
        stk1[[5]][,,1]), c(8,9,5))

stkam <- rowMeans(stka, dims = 2)
fidxs <- which((stkam != 255) & (stkam != 0), arr.ind = T)
rbind(
  stk1[[1]][,,1][fidxs],
  stk1[[2]][,,1][fidxs],
  stk1[[3]][,,1][fidxs],
  stk1[[4]][,,1][fidxs],
  stk1[[5]][,,1][fidxs]
)

rbind(
  stk1[[1]][,,1][fidxs],
  stk1[[2]][,,1][fidxs],
  stk1[[3]][,,1][fidxs],
  stk1[[4]][,,1][fidxs],
  stk1[[5]][,,1][fidxs]
) == 255

colMeans(
  rbind(
    stk1[[1]][,,1][fidxs],
    stk1[[2]][,,1][fidxs],
    stk1[[3]][,,1][fidxs],
    stk1[[4]][,,1][fidxs],
    stk1[[5]][,,1][fidxs]
  ) == 255
)


