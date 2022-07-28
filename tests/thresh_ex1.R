library(magick) # Library that can read/write .gif files
library(Rvision)

# Path to a .gif file
pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set2/20 feather pattern/1.gif"

# Load .gif file:
dat <- image_read(pth)
length(dat) # Number of images in .gif file

# Separate and convert images in .gif file to integer gray levels:
# Image 1:
img1 <- as.integer(dat[1][[1]])[,,1] # All three channels should be the same
img2 <- as.integer(dat[2][[1]])[,,1]
img3 <- as.integer(dat[3][[1]])[,,1]
img4 <- as.integer(dat[4][[1]])[,,1]
img5 <- as.integer(dat[5][[1]])[,,1]

dim(img1)

plot(as.raster(img1, max = 255))
plot(as.raster(img2, max = 255))
plot(as.raster(img3, max = 255))
plot(as.raster(img4, max = 255))
plot(as.raster(img5, max = 255))

mthresh <- round(mean(c(img1,img2,img3,img4,img5)))

255*!(img1 > mthresh)
255*!(img2 > mthresh)
255*!(img3 > mthresh)
255*!(img4 > mthresh)
255*!(img5 > mthresh)


img1t <- zeros(nrow = nrow(img1), ncol = ncol(img1), nchan = 1, bitdepth = "8U")
img1t[] <- 255*!(img1 > mthresh)
img1t[]

