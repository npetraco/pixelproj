library(Rvision)
library(plot.matrix)
#255 = white
#0   = black

# Reference (to shift over)
ref <- Rvision::image("Desktop/cor_tests/A1_KM_C-B06_reg-2-0004.tif")
dim(ref)
plot(ref)

# Template (to be shifted)
#tmp <- image("Desktop/cor_tests/B_C-B06_reg-2-0004.tif")
tmp <- image("Desktop/cor_tests/C-B06_reg-16_ref.tif")
dim(tmp)

# Make a padded ref wrt the tmp which will be shifting around over the ref:
h.tmp <- nrow(tmp) # Template height
w.tmp <- ncol(tmp) # Template width
#w <- ncol(tmp)
#h <- nrow(tmp)


h.bar <- 1 # Min overlap in height (min row overlap)
w.bar <- 1 # Min overlap in width (min col overlap)

hrow <- h.tmp - h.bar
wcol <- w.tmp - w.bar
hrow
wcol

ref.pad <- border(ref, 
                  top    = hrow, 
                  bottom = hrow, 
                  left   = wcol, 
                  right  = wcol, 
                  border_color = "white",
                  border_type = "constant",
#                  border_type  = "constant",
#                  border_value = -1
                  target = "new"
                )
dim(ref)
dim(ref.pad)
plot(ref)
plot(ref.pad)


match <- matchTemplate(image = ref.pad, template = tmp, target = "new", method = "SQDIFF")
match
plot(match)
match$toR()
which(match$toR() == min(match$toR()), arr.ind = T)
plot(match$toR()[,,1])

# If the method is TM_SQDIFF or TM_SQDIFF_NORMED, take minimum
mm <- minMaxLoc(match)
mm # NOTE: images are indexed upside down because rows are loaded in reverse order

min_loc <- mm[1,c(2,3)]
min_loc

x <- min_loc[1]
y <- min_loc[2]

tmp.in.ref <- array(-1, dim(ref.pad)[c(1,2)])
plot(tmp.in.ref)
y: 
tmp[,,1]


#################
refpr <- ref.pad
drawRectangle(refpr, 
              pt1_x=x,     pt1_y=y, 
              pt2_x=x+w-1, pt2_y=y+h-1, 
              color = "white", thickness = 1)
plot(refpr)
plot(ref.pad)
