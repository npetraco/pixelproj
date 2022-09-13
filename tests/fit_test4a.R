library(pixelproj)
library(CRFutil)

#255 = white
#0   = black

# Path to a .gif file
#root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set2/20 feather pattern/"
root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/feathering_pattern_cropping_experiments/test_set3/extract 10 region from registeredCB06/"

#get directory contents
fnames <- list.files(root.pth)

i <- 3
print(paste0("Image ",i, ", filename: ", fnames[i]))
pth1      <- paste0(root.pth, fnames[i]) # Path to an image in a stack
stk1      <- thresh.img.obj(image_read(pth1))
plot.stack(stk1, num.cycles=3, delay = 0.1, type="matrix")
stk1.sims <- simulate.pix.pattern(stk1, nsims=20)
#plot.stack(c(stk1, stk1.sims), num.cycles=3, delay = 0.1, type="matrix")

samps           <- image.list2vec.mat(c(stk1, stk1.sims), white.pix.val = 1, black.pix.val = 2)
nr              <- nrow(stk1[[1]][,,1])
nc              <- ncol(stk1[[1]][,,1])
colnames(samps) <- 1:(nr*nc)

grphf      <- make.lattice(num.rows = nr, num.cols = nc, cross.linksQ = F, node.names.vec = 1:(nr*nc))
adj        <- ug(grphf, result="matrix") # adjacency (connection) matrix
node.names <- colnames(adj)
node.names

# Check the graph:
gp <- ug(grphf, result = "graph")
gpi <- graph_from_graphnel(gp)
#gpi$layout <- layout_on_grid
dev.off()
plot(gpi)
dev.off()



s1 <- "white" # State 1 name
s2 <- "black" # State 2 name
# s1 <- 1 # State 1 name
# s2 <- 2 # State 2 name
fit <- fit_mle_params(grphf, samps,
                      parameterization.typ = "flexible",
                      opt.method           = "L-BFGS-B",
                      #opt.method           = "CG",
                      inference.method     = infer.junction,
                      state.nmes           = c(s1,s2),
                      num.iter             = 6,
                      mag.grad.tol         = 1e-6,
                      plotQ                = T)
fit$par
fit$nll
fit$gradient
sqrt(sum(fit$gradient^2))


# Check config probabilities:
fit.bels <- infer.junction(fit)
logZ <- fit.bels$logZ
logZ

# Node beliefs:
nb.vec   <- as.numeric(fit.bels$node.bel[,1] > fit.bels$node.bel[,2])
nb.array <- vec2array(arr.vec = nb.vec, num.rows = nr, num.cols = nc, white.pix = 255, black.pix = 0)
plot(nb.array, key=NULL)

# Most likely configuration
max.array <- vec2array(arr.vec = decode.junction(fit), num.rows = nr, num.cols = nc, white.pix = 255, black.pix = 0)
plot(max.array, key=NULL)

plot.stack(c(stk1, stk1.sims), num.cycles=3, delay = 0.1, type="matrix")



# Compute the configuration probability: \Pr({\bf X}) = \frac{1}{Z} e^{E({\bf X})}
s1a <- 1 # white pixel
s2a <- 2 # black pixel
f0  <- function(y){ as.numeric(c((y==s1a),(y==s2a))) }

img.num  <- 25
fp.array <- vec2array(arr.vec = samps[img.num,], num.rows = nr, num.cols = nc, white.pix = 255, black.pix = 0)
plot(fp.array, key=NULL)
EX <- config.energy(config       = samps[img.num,],
                    edges.mat    = fit$edges,
                    one.lgp      = fit$node.energies,
                    two.lgp      = fit$edge.energies, # make sure use same order as edges!
                    ff           = f0)
EX
EX - logZ       # log(Pr(X))
exp(EX - logZ)  # Pr(X)


# If happy with calculation, store?:
# file name
# real/sim samples
# theta and corresponding potentials (or just crf object for easier re-use??)

