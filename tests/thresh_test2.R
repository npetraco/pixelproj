library(magick) # Library that can read/write .gif files
library(Rvision)
library(plot.matrix)
library(pixelproj)

#255 = white
#0   = black

# Path to a .gif file
root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set2/20 feather pattern/"

pth1 <- paste0(root.pth,"15.gif")
pth2 <- paste0(root.pth,"5.gif")

# Load and mean threshold registered image stack(s):
stk1 <- thresh.img.obj(image_read(pth1))
stk2 <- thresh.img.obj(image_read(pth2))

A.img <- stk1[[sample(1:5, size = 1)]] # Select one of the images in the registered stack
B.img <- stk2[[sample(1:5, size = 1)]] # Select one of the images in the registered stack

par(mfrow=c(1,2))
plot(A.img)
plot(B.img)
dev.off()

regA  <- register.template(template.img  = A.img,
                           reference.img = B.img,
                           fill.typ = "255", return.imgQ = T, printQ = T, plotQ = T)
# fill.typ = "NA", "opposite", "random", "devils.advocate", "0", "1", "255"

# Result, image coords:
plot(A.img)
plot(regA)
plot(B.img)

# Result, matrix coords:
plot(A.img[,,1])
plot(regA[,,1])
plot(B.img[,,1])

dev.off()
par(mfrow=c(1,2))
plot(flip(regA)[,,1], key=NULL)
plot(flip(B.img)[,,1], key=NULL)
