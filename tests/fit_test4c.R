library(pixelproj)
library(CRFutil)

#255 (or 1) = white
#0   (or 2) = black

# Path to a .gif file
#root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set2/20 feather pattern/"
root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set3/extract 10 region from registeredCB06/"

#get directory contents
fnames <- list.files(root.pth)

i <- 3
print(paste0("Image ",i, ", filename: ", fnames[i]))
pth1    <- paste0(root.pth, fnames[i]) # Path to an image in a stack
stk1    <- thresh.img.obj(image_read(pth1))
fit.obj <- mle.fit.stack(stk1, num.sims=20, num.iters=4, grad.tol=1e-6)
logZ    <- infer.junction(fit.obj$fit)$logZ

prob.pattern(pattern.mat=stk1[[5]][,,1], fit.obj$fit, logZ)

# Next, add registration to stack for Q pattern
