library(Rvision)
library(plot.matrix)

ref <- image("Desktop/cor_tests/C-B06_reg-16_ref.tif")
plot(ref)
dim(ref)
ref[1:13,,1]
ref[13:1,,1]
ref[13:1,8:1,1]
#255 = white
#0   = black
plot(ref[13:1,8:1,1]) #??
plot(ref[13:1,,1])
plot(ref[,,1])

sm <- image("Desktop/cor_tests/C-B06_reg-16_small.tif")
plot(sm)
dim(sm)
sm[7:1,,1]
plot(sm[7:1,,1])

match <- matchTemplate(image = ref, template = sm, target = "new", method = "SQDIFF")
match
plot(match)
match$toR()
which(match$toR() == min(match$toR()), arr.ind = T)
plot(match$toR()[,,1])
  
mm <- minMaxLoc(match)
mm # NOTE: images are indexed upside down

min_loc <- mm[1,c(2,3)]
# top_left <- min_loc
# top_left

w <- ncol(sm)
h <- nrow(sm)
x <- min_loc[1]
y <- min_loc[2]
ref[y:(y+h), x:w,1]
ref[(y+h):y, x:w,1]
plot(ref[y:(y+h), x:w,1])
plot(ref[(y+h):y, x:w,1])

plot(ref[y:(y+h-1), x:(x+w-1),1])
plot(sm[,,1])
ref[y:(y+h-1), x:(x+w-1),1]
y:(y+h-1)
x:(x+w-1)

y
y+h

sm[,,1]


bottom_right <- c(top_left[1] + w, top_left[2] + h)

top_left
w
bottom_right

#cv.rectangle(img,top_left, bottom_right, 255, 2)
drawRectangle(ref, 
              pt1_x=top_left[1], pt1_y=top_left[2], 
              pt2_x=bottom_right[1], pt2_y=bottom_right[2], 
              color = "green", thickness = 1)
plot(ref)
plot(sm)



ref[c(2:8),c(2:7),1]
sm[,,1]


sub <- subImage(ref, x = 2, y =6, width = 6, height = 7)
plot(sub)
plot(ref)
ref[13:1,,1]

# ys
nrow(ref)-6
nrow(ref)-6 + nrow(sm) 
plot(sm)
dim(sm)
dim(ref)
