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
plot.stack(stk1, num.cycles=5, delay = 0.1, type="matrix")

stk1.sims <- simulate.pix.pattern(stk1, nsims=10)
plot.stack(stk1.sims, num.cycles=5, delay = 0.1, type="matrix")
plot.stack(stk1.sims, num.cycles=3, delay = 0.1, type="Rvision")
plot.stack(stk1.sims, num.cycles=3, delay = 0.1, type="image.matrix")

plot.stack(c(stk1, stk1.sims), num.cycles=3, delay = 0.1, type="Rvision")

rqj  <- register.template(template.img  = stk1.sims[[5]],
                          reference.img = stk1[[1]],
                          fill.typ      = "devils.advocate",
                          printQ        = F,
                          plotQ         = T)
rqj

# Flatten pixel pattern and 255s -> 1 for CRF routines
as.numeric(rqj == 255) # white = 1, black = 0
array2vec(rqj)

# Convert from vector to matrix pixel pattern
rqj.rec <- array(NA, dim(rqj))
rqj.rec
rqj.rec[ t(sapply(1:(nrow(rqj)*ncol(rqj)), function(xx){ind2sub(xx, num.row = nrow(rqj) )})) ] <- as.numeric(rqj == 255)
rqj.rec
rqj

jv <- array2vec(rqj)
vec2array(jv, num.rows = nrow(rqj), num.cols = ncol(rqj), white.pix = 255, black.pix = 1)

image.list2vec.mat(stk1)
stk1[[1]]
