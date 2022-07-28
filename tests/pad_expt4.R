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


rqj  <- register.template(tmpl, ref, 
                          h.min.overlap=6, 
                          w.min.overlap=7, printQ = T, plotQ = T)
plot(rqj[nrow(rqj2):1,])

rqj2  <- register.template(tmpl, ref, h.min.overlap=1, w.min.overlap=1, 
                           fill.typ = "devils.advocate", printQ = T, plotQ = T)
plot(rqj2[nrow(rqj2):1,])

#HMMMMMM.......
plot.overlay(tmpl, ref, h.min.overlap=6, w.min.overlap=7, x.max=7, y.max=1, ref.lo=0, ref.hi=2, templ.lo=4, templ.hi=6)



plot.overlay(tmpl, ref, h.min.overlap=1, w.min.overlap=1, x.max=3, y.max=4, ref.lo=0, ref.hi=2, templ.lo=4, templ.hi=6)
