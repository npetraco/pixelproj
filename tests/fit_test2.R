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

i <- 5
print(paste0("Image ",i, ", filename: ", fnames[i]))
pth1      <- paste0(root.pth, fnames[i]) # Path to an image in a stack
stk1      <- thresh.img.obj(image_read(pth1))
#plot.stack(stk1, num.cycles=1, delay = 0.1, type="matrix")

stk1.sims <- simulate.pix.pattern(stk1, nsims=100)
#plot.stack(stk1.sims, num.cycles=5, delay = 0.1, type="matrix")
#plot.stack(stk1.sims, num.cycles=3, delay = 0.1, type="Rvision")
#plot.stack(stk1.sims, num.cycles=3, delay = 0.1, type="image.matrix")

#plot.stack(c(stk1, stk1.sims), num.cycles=3, delay = 0.1, type="Rvision")

image.list2vec.mat(stk1.sims)
#

# This should be used for Q<->K comparisons but shouldnt be needed for K image stacks
rqj  <- register.template(template.img  = stk1.sims[[5]],
                          reference.img = stk1[[1]],
                          fill.typ      = "devils.advocate",
                          printQ        = F,
                          plotQ         = T)
rqj

