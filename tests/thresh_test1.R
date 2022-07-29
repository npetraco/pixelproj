library(magick) # Library that can read/write .gif files
library(Rvision)
library(plot.matrix)
library(pixelproj)


# Path to a .gif file
pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set2/20 feather pattern/1.gif"

# Load .gif file (need magick):
dat <- image_read(pth)
length(dat) # Number of images in .gif file

# Load a .tif (use Rvision)
p1  <- "tests/test_data/A1_KM_C-B06_reg-2-0004.tif"
p1i <- image(p1)


stk.imgs <- thresh.img.obj(dat)
one.img  <- thresh.img.obj(p1i)

stk.imgs[[1]][]
stk.imgs[[2]][]
stk.imgs[[3]][]
stk.imgs[[4]][]
stk.imgs[[5]][]

img1 <- as.integer(dat[1][[1]])[,,1] # All three channels should be the same
plot(as.raster(img1, max = 255))
plot(flip(stk.imgs[[1]]))

img2 <- as.integer(dat[2][[1]])[,,1] # All three channels should be the same
plot(as.raster(img2, max = 255))
plot(flip(stk.imgs[[2]]))

img3 <- as.integer(dat[3][[1]])[,,1] # All three channels should be the same
plot(as.raster(img3, max = 255))
plot(flip(stk.imgs[[3]]))

img4 <- as.integer(dat[4][[1]])[,,1] # All three channels should be the same
plot(as.raster(img4, max = 255))
plot(flip(stk.imgs[[4]]))

img5 <- as.integer(dat[5][[1]])[,,1] # All three channels should be the same
plot(as.raster(img5, max = 255))
plot(flip(stk.imgs[[5]]))
