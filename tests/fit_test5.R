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
pth1 <- paste0(root.pth, fnames[i]) # Path to an image in a stack

# **CONSIDER Adaptive threshold and threshold in Rvision update
stk1 <- thresh.img.obj(image_read(pth1), type = "individual.mean") # Choose threshold each image individually
plot.stack(stk1, num.cycles = 3, delay = 0.1)


# Next1, add HOO for checking performance
hoo.pr.mat <- array(NA, c(length(stk1), 2))
for(i in 1:length(stk1)){
  fit.obj <- mle.fit.stack(stk1[-i], num.sims=20, num.iters=6, grad.tol=1e-4)
  logZ    <- infer.junction(fit.obj$fit)$logZ
  pri     <- prob.pattern(pattern.mat=stk1[[i]][,,1], fit.obj$fit, logZ)
  hoo.pr.mat[i,] <- pri
  print("======================")
  print(paste0("Hold out: ", i))
  print(pri)
}
hoo.pr.mat


# Next2, add registration to stack for Q pattern
# The K stack fit for theta:
fit.obj <- mle.fit.stack(stk1, num.sims=20, num.iters=6, grad.tol=1e-4)
logZ    <- infer.junction(fit.obj$fit)$logZ

# Some KNM Qs:
Qi <- 1
print(paste0("Q Image ",Qi, ", filename: ", fnames[Qi]))
pth.Qi <- paste0(root.pth, fnames[Qi]) # Path to an image in a stack

stk.Qi <- thresh.img.obj(image_read(pth.Qi), type = "individual.mean")
plot.stack(stk.Qi, num.cycles = 3, delay = 0.1)

# A specific KNM Q to compute Pr(X_Q | K):
Q  <- register.template(
  template.img  = stk.Qi[[3]],  # The specific Q
  reference.img = stk1[[1]],    # Should we do this for each K image as a reference??
  fill.typ      = "255",     # Fill for missing parts: "devils.advocate" (assume missing parts are the same as the reference), "opposite" (assume missing parts the opposite of the reference), "random" (random values for missing parts), "0" (missing parts all black), "255" (missing parts all white)
  printQ        = F,
  plotQ         = T)
plot(Q, key=NULL)

pri     <- prob.pattern(pattern.mat=Q, fit.obj$fit, logZ)
pri

