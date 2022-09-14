library(pixelproj)
library(CRFutil)

#255 (or 1) = white
#0   (or 2) = black

# Path to a .gif file
#root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set2/20 feather pattern/"
root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set3/extract 10 region from registeredCB06/"

#get directory contents
fnames <- list.files(root.pth)

i <- 8
print(paste0("Image ",i, ", filename: ", fnames[i]))
pth1    <- paste0(root.pth, fnames[i]) # Path to an image in a stack
stk1    <- thresh.img.obj(image_read(pth1), type = "group.mean")
stk1a   <- thresh.img.obj(image_read(pth1), type = "individual.mean") # Because when Q come in, we don't know what group to threshold them with, so just threshold individually
plot.stack(stk1, num.cycles = 3)
plot.stack(stk1a, num.cycles = 3)

fit.obj <- mle.fit.stack(stk1, num.sims=20, num.iters=15, grad.tol=1e-6)
logZ    <- infer.junction(fit.obj$fit)$logZ
logZ

fit.obja <- mle.fit.stack(stk1a, num.sims=20, num.iters=15, grad.tol=1e-6)
logZa    <- infer.junction(fit.obja$fit)$logZ
logZa

prob.pattern(pattern.mat=stk1[[1]][,,1], fit.obj$fit, logZ)
prob.pattern(pattern.mat=stk1a[[1]][,,1], fit.obja$fit, logZa)

# Next, add registration to stack for Q pattern

