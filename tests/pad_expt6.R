library(Rvision)
library(plot.matrix)
library(pixelproj)
#255 = white
#0   = black

# Reference (to shift over)
#ref <- Rvision::image("tests/test_data/A1_KM_C-B06_reg-2-0004.tif")
ref <- image("tests/test_data/A1_KM_C-B06_reg-2-0004.tif")
dim(ref)
plot(ref)

# Template (to be shifted)
#tmpl <- Rvision::image("tests/test_data/B_C-B06_reg-2-0004.tif")     # KNM
#tmpl <- Rvision::image("tests/test_data/C-B06_reg-16_ref.tif")       # KNM
#tmpl <- Rvision::image("tests/test_data/A2_KM_C-B06_reg-2-0001.tif")  # KM
#tmpl <- image("tests/test_data/B_C-B06_reg-2-0004.tif")     # KNM
#tmpl <- image("tests/test_data/C-B06_reg-16_ref.tif")       # KNM
tmpl <- image("tests/test_data/A2_KM_C-B06_reg-2-0001.tif")  # KM

dim(tmpl)
plot(tmpl)

rqj  <- register.template(tmpl, ref, fill.typ = "devils.advocate", printQ = T, plotQ = T)
rqj  <- register.template(tmpl, ref, fill.typ = "opposite", printQ = T, plotQ = T)
rqj  <- register.template(tmpl, ref, fill.typ = "random", printQ = T, plotQ = T)
rqj  <- register.template(tmpl, ref, fill.typ = "0", printQ = T, plotQ = T)
rqj  <- register.template(tmpl, ref, fill.typ = "255", printQ = T, plotQ = T)
plot(rqj[nrow(rqj):1,])
plot(ref[nrow(ref):1,,1])

plot.overlay(tmpl, ref, x.max=14, y.max=6)
