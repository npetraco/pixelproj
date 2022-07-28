library(Rvision)
library(plot.matrix)
#255 = white
#0   = black

ref <- image("Desktop/cor_tests/C-B06_reg-16_small.tif")
dim(ref)
plot(ref)
plot(ref[,,1])
ref2 <- border(ref, 
               top    = 6, 
               bottom = 0, 
               left   = 2, 
               right  = 0,
               border_type = "constant", border_color = "white")
dim(ref2)
plot(ref2)
plot(ref2[,,1])


sm <- image("Desktop/cor_tests/C-B06_reg-16_ref.tif")
dim(sm)
plot(sm)
plot(sm[,,1])

match <- matchTemplate(image = ref2, template = sm, target = "new", method = "SQDIFF")
match
plot(match)
match$toR()
which(match$toR() == min(match$toR()), arr.ind = T)
plot(match$toR()[,,1])

