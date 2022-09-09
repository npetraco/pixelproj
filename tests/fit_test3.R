library(pixelproj)
library(CRFutil)

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
plot.stack(stk1, num.cycles=3, delay = 0.1, type="matrix")
stk1.sims <- simulate.pix.pattern(stk1, nsims=20)
#plot.stack(c(stk1, stk1.sims), num.cycles=3, delay = 0.1, type="matrix")

samps           <- image.list2vec.mat(c(stk1, stk1.sims), white.pix.val = 1, black.pix.val = 2)
nr              <- nrow(stk1[[1]][,,1])
nc              <- ncol(stk1[[1]][,,1])
colnames(samps) <- 1:(nr*nc)
samps
dim(samps)

grphf      <- make.lattice(num.rows = nr, num.cols = nc, cross.linksQ = F, node.names.vec = 1:(nr*nc))
adj        <- ug(grphf, result="matrix") # adjacency (connection) matrix
node.names <- colnames(adj)
node.names

# Check the graph:
gp <- ug(grphf, result = "graph")
dev.off()
plot(gp)
dev.off()

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
                      num.iter             = 5,
                      mag.grad.tol         = 1e-3,
                      plotQ                = T)
fit$par
