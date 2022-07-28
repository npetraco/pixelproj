library(Rvision)
library(plot.matrix)

ref <- image("Desktop/cor_tests/C-B06_reg-16_ref.tif")
dim(ref)
#255 = white
#0   = black
plot(ref)
plot(ref[13:1,,1]) # Rows are loaded in reversed order
plot(ref[,,1])

sm <- image("Desktop/cor_tests/C-B06_reg-16_small.tif")
plot(sm)
dim(sm)
sm[7:1,,1]       
plot(sm[7:1,,1])  # Rows are loaded in reversed

match <- matchTemplate(image = ref, template = sm, target = "new", method = "SQDIFF")
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

w <- ncol(sm)
h <- nrow(sm)
x <- min_loc[1]
y <- min_loc[2]
y:(y+h-1) # Row span of patch "matching" template-sm
x:(x+w-1) # Col span of patch "matching" template-sm

plot(ref[y:(y+h-1), x:(x+w-1),1]) # Slice out matched portion
plot(sm[,,1])
ref[y:(y+h-1), x:(x+w-1),1]
y:(y+h-1) # Matched row span in ref
x:(x+w-1) # Matched col span in ref

ref <- image("Desktop/cor_tests/C-B06_reg-16_ref.tif")
drawRectangle(ref, 
              pt1_x=x,     pt1_y=y, 
              pt2_x=x+w-1, pt2_y=y+h-1, 
              color = "white", thickness = 1)
plot(ref)
plot(sm)
