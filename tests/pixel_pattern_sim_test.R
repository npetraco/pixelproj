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

i <- 4
print(paste0("Image ",i, ", filename: ", fnames[i]))
pth1      <- paste0(root.pth, fnames[i]) # Path to an image in a stack
stk1      <- thresh.img.obj(image_read(pth1))
stk1.sims <- simulate.pix.pattern(stk1, nsims=10)
plot.stack(stk1.sims, num.cycles=3, delay = 0.1, type="matrix")
plot.stack(stk1.sims, num.cycles=3, delay = 0.1, type="Rvision")
plot.stack(stk1.sims, num.cycles=3, delay = 0.1, type="image.matrix")

plot.stack(c(stk1, stk1.sims), num.cycles=3, delay = 0.1, type="Rvision")


# Nothing drops out case:
stk1a <- stk1
stk1a[[2]] <- stk1a[[1]]
stk1a[[3]] <- stk1a[[1]]
stk1a[[4]] <- stk1a[[1]]
stk1a[[5]] <- stk1a[[1]]
emp.pix.dropout.rates(stk1a)
stk1a.sims <- simulate.pix.pattern(stk1a, nsims=10)
plot.stack(stk1a.sims, num.cycles=3, delay = 0.1, type="matrix")
