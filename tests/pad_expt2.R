library(Rvision)
library(plot.matrix)
#255 = white
#0   = black

# Reference (to shift over)
ref <- Rvision::image("Desktop/cor_tests/A1_KM_C-B06_reg-2-0004.tif")
dim(ref)
plot(ref)

# Template (to be shifted)
tmpl <- image("Desktop/cor_tests/B_C-B06_reg-2-0004.tif")
#tmpl <- image("Desktop/cor_tests/C-B06_reg-16_ref.tif")
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
tmpl.in.ref <- array(NA, dim(ref.pad)[c(1,2)])
dim(tmpl.in.ref)
plot(tmpl.in.ref)


#




#tmpl.in.ref[y:(y+h-1), x:(x+w-1)] <- -3
tmpl.in.ref[y:(y+h-1), x:(x+w-1)] <- tmpl[,,1]
plot(tmpl.in.ref, col=c("green", "blue"))
plot(ref.pad[,,1], col=c("red", "green"))

tmpl[,,1]
plot(tmpl)
dim(tmpl)

# Take off border
ref.pad2 <- ref.pad[,,1]
tmpl2    <- tmpl[,,1]
#tmpl2[which(tmpl2 == 0, arr.ind = T)] <- 0
#tmpl2[which(tmpl2 == 255, arr.ind = T)] <- 100

tmpl.in.ref2 <- ref.pad[,,1]
tmpl.in.ref2[y:(y+h-1), x:(x+w-1)] <- tmpl2[,]
plot(tmpl.in.ref2, col=c("red", "green", "blue","orange"))
plot(tmpl.in.ref2, breaks=range(tmpl.in.ref2))
tmpl.in.ref2
plot(ref.pad[,,1], col=c("red", "green"))
#

tmpl.in.ref[(hpadl+1):(hpadl+nrow(ref)), (wpadl+1):(wpadl+ncol(ref))]
nrow(ref)
dim(ref)

plot(ref.pad[,,1])
