library(Rvision)
library(plot.matrix)
#255 = white
#0   = black

# Reference (to shift over)
ref <- Rvision::image("Desktop/cor_tests/A1_KM_C-B06_reg-2-0004.tif")
dim(ref)
plot(ref)

# Template (to be shifted)
#tmpl <- image("Desktop/cor_tests/B_C-B06_reg-2-0004.tif")
tmpl <- image("Desktop/cor_tests/C-B06_reg-16_ref.tif")
dim(tmpl)
plot(tmpl)

# Make a padded ref wrt the tmp which will be shifting around over the ref:
w <- ncol(tmpl)
h <- nrow(tmpl)

h.mo <- 1 # Min overlap in height (min row overlap)
w.mo <- 1 # Min overlap in width (min col overlap)

# Padding lengths
hpadl <- h - h.mo
wpadl <- w - w.mo
hpadl
wpadl

ref.pad <- border(ref,
                  top    = hpadl, 
                  bottom = hpadl, 
                  left   = wpadl, 
                  right  = wpadl, 
                  border_color = "white",
                  border_type = "constant",
                  target = "new")
dim(ref)
dim(ref.pad)
plot(ref)
plot(ref.pad)

match <- matchTemplate(image = ref.pad, template = tmpl, target = "new", method = "SQDIFF")
match
plot(match)

mm <- minMaxLoc(match)
min_loc <- mm[1,c(2,3)]
x <- min_loc[1]
y <- min_loc[2]

# Embed the template in an array the same size as the padded reference
# Any NAs should be from padding here on out
tmpl.in.ref <- array(NA, dim(ref.pad)[c(1,2)])
dim(tmpl.in.ref)

# Extract and re-scale the  reference's pixels
ref2 <- ref[,,1]
ref2[which(ref2==255, arr.ind = T)] <- 2
ref2
#ref.pad2 <- ref.pad[,,1]
#which(ref.pad2==255, arr.ind = T) 

# First Embed re-scaled ref in the tmpl.in.ref:
tmpl.in.ref[(hpadl+1):(hpadl+nrow(ref)), (wpadl+1):(wpadl+ncol(ref))] <- ref2
plot(tmpl.in.ref)
plot(ref.pad[,,1])

# Extract and re-scale the  template's pixels
tmpl2 <- tmpl[,,1]
tmpl2[which(tmpl2==0, arr.ind = T)] <- 4
tmpl2[which(tmpl2==255, arr.ind = T)] <- 6
plot(tmpl2)

# overlay re-scaled template onto padded rescaled reference and see what we get:
tmpl.in.ref[y:(y+h-1), x:(x+w-1)] <- tmpl2[,]
plot(tmpl.in.ref, col=c("blue","red","green","orange"), breaks = c(0,1,3,5,7))
plot(ref.pad[,,1])

ref.pad2 <- ref.pad[,,1]
ref.pad2[(hpadl+1):(hpadl+nrow(ref)), (wpadl+1):(wpadl+ncol(ref))] <- ref2
plot(ref.pad2, col=c("blue","red","green","orange"), breaks = c(0,1,3,5,7))

# Compare ********
plot(ref.pad2, col=c("blue","red","green","orange"), breaks = c(0,1,3,5,7))
plot(tmpl.in.ref, col=c("blue","red","green","orange"), breaks = c(0,1,3,5,7))
cbind(c("blue","red","green","orange"), c("0   ref", "255 ref", "0   tmpl", "255 tmpl"))
#255 = white
#0   = black

# Now take off border
tri <- tmpl.in.ref[(hpadl+1):(hpadl+nrow(ref)), (wpadl+1):(wpadl+ncol(ref))]
plot(tri, col=c("blue","red","green","orange"), breaks = c(0,1,3,5,7))
plot(tmpl.in.ref, col=c("blue","red","green","orange"), breaks = c(0,1,3,5,7))
plot(ref.pad2, col=c("blue","red","green","orange"), breaks = c(0,1,3,5,7))

dim(tri)
dim(ref2)

plot(tri, col=c("blue","red","green","orange"), breaks = c(0,1,3,5,7), digits=1, text.cell=list(cex=0.5))
