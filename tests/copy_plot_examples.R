library(Rvision)
library(plot.matrix)
library(pixelproj)

# **NOTE: pixel codes:
# 255 = white
# 0   = black

p1 <- "tests/test_data/A1_KM_C-B06_reg-2-0004.tif"
p2 <- "tests/test_data/A2_KM_C-B06_reg-2-0001.tif"
p3 <- "tests/test_data/B_C-B06_reg-2-0004.tif"
p4 <- "tests/test_data/C-B06_reg-16_ref.tif"

p1i  <- image(p1)
p2i  <- image(p2)
p3i  <- image(p3)
p4i  <- image(p4)

plot(p1i)
plot(p2i)
plot(p3i)
plot(p4i)

p1ic <- cloneImage(p1i, target = "new")
p2ic <- cloneImage(p2i, target = "new")
p3ic <- cloneImage(p3i, target = "new")
p4ic <- cloneImage(p4i, target = "new")

plot(p1ic)
plot(p2ic)
plot(p3ic)
plot(p4ic)



# threshold img func
#flip
#zeros	Create a Zero-Filled Image
#computeECC(template, image)
#connectedComponents
