library(pixelproj)
library(CRFutil)

#255 (or 1) = white
#0   (or 2) = black

# Path to a .gif files
sample.root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/sally_thesis/all_fp/"
fits.root.pth   <- "/Users/karen2/latex/papers/irina/Chen_group_workups/sally_thesis/analysis/feathering_pattern_fit_files/"

# Get directory contents and build systematic, unique and short label names for each feathering pattern
fnames           <- list.files(sample.root.pth)
fnames           <- fnames[-c(241,242)] # Drop last two file names which are just info files
lbl              <- gl(240,5)
lbl.str          <- as.character(sapply(1:length(fnames), function(xx){paste0(strsplit(fnames[xx], ".", fixed=T)[[1]][1], ".",1:5)}))
fp.num           <- 1:length(lbl)
banknote.nms     <- sapply(1:length(lbl.str), function(xx){strsplit(lbl.str[xx], "-", fixed = T)[[1]][1]})
note.fp.nums     <- as.numeric(sapply(1:length(lbl.str), function(xx){strsplit(strsplit(lbl.str[xx], split = c("-"))[[1]][2], ".", fixed = T)[[1]][1]}))
note.fp.rep.nums <- as.numeric(sapply(1:length(lbl.str), function(xx){strsplit(strsplit(lbl.str[xx], split = c("-"))[[1]][2], ".", fixed = T)[[1]][2]}))
fp.lbl.info      <- data.frame(fp.num, lbl.str, lbl,banknote.nms, note.fp.nums, note.fp.rep.nums)

# For each of the 1200 feathering patterns X:
# Get Pr(X | K) for K = 1:240 feathering patterns

# Most likely configuration for each fit? What is it's prob?

#Since we have the FPs stored as stacks and not individually, grab a stack to serve as a set of Qs:
ii <- 1 # 1:240 length(fnames)
print(paste0("Q Image stack: ",ii, ", filename: ", fnames[ii]))
Q.pth <- paste0(sample.root.pth, fnames[ii])
Q.stk <- thresh.img.obj(image_read(Q.pth), type = "individual.mean")

# Loop over all the FPs in the Q stack
jj <- 1 # 1:5 length(Q.stk)
Q.img     <- Q.stk[[jj]]
#plot(Q.img)
#plot.stack(list(Q.img), num.cycles = 1, delay = 0.1, type = "image.matrix")

# Loop over all the FP stacks. One will be the M, the remaining NM
kk <- 15 # 1:240 length(fnames)
print(paste0("K Image stack: ",kk, ", filename: ", fnames[kk]))
K.pth          <- paste0(sample.root.pth, fnames[kk])
K.stk          <- thresh.img.obj(image_read(K.pth), type = "individual.mean")
K.mean.img     <- mean.stack.img(K.stk) # This will be the reference for registration
#plot(K.mean.img)
#plot.stack(list(K.mean.img), num.cycles = 1, delay = 0.1, type = "image.matrix")

# Register Q to K
Q.img.mat.reg <- register.template(
  template.img  = Q.img,         # The specific Q in the stack
  reference.img = K.mean.img, # Take mean image of stack as the reference to align to
  fill.typ      = "255",               # Fill for missing parts: "devils.advocate" (assume missing parts are the same as the reference), "opposite" (assume missing parts the opposite of the reference), "random" (random values for missing parts), "0" (missing parts all black), "255" (missing parts all white)
  printQ        = F,
  plotQ         = F)
#plot(Q.img.mat.reg[nrow(Q.img.mat.reg):1,], key=NULL)
#plot.stack(list(Q.img), num.cycles = 1, delay = 0.1, type = "image.matrix")

load(file = paste0(fits.root.pth, fnames[kk], "_fit_info.RData") ) # Load parameters and logZ for the K
plot(fit.obj$fit$par, typ="h")
pri     <- prob.pattern(pattern.mat=Q.img.mat.reg, fit.obj$fit, logZ)
pri
