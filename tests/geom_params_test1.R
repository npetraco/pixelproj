library(pixelproj)
library(CRFutil)

#255 (or 1) = white
#0   (or 2) = black

root.pth <- "/Users/karen2/latex/papers/irina/Chen_group_workups/sally_thesis/"

sample.root.pth <- paste0(root.pth, "all_fp/")   # Path to a .gif files

# Load proper feathering pattern set file names. Needed by some analysis functions below.
fp.file.names <- list.files(sample.root.pth)
fp.file.names <- fp.file.names[-c(241,242)] # Drop last two file names which are just info files

# Path to an fp image in a stack. There are 240 of these. Fit one at a time:
# fp image stack number to obtain MRF parameters for:
ii <- 240
print(paste0("Image stack: ",ii, ", filename: ", fp.file.names[ii]))
pth <- paste0(sample.root.pth, fp.file.names[ii])
pth

# Choose threshold each image individually. Plot patterns to check:
stk <- thresh.img.obj(image_read(pth), type = "individual.mean")
stk[[1]]
# Check fp's that had low to low-ish LRs:
# Anything common about these? num rows, cols; num black pixels; %black pixels/total pixels
dim(stk[[1]][,,1])       # num rows
sum(stk[[1]][,,1]==0)    # num black pixels
prod(dim(stk[[1]][,,1])) # num pixels

sum(stk[[1]][,,1]==0) / prod(dim(stk[[1]][,,1])) *100 # %black pixels

stk[[1]][,,1]


m <- which(stk[[1]][,,1] == 0, arr.ind = T)
m
ch <- chull(m)
ch.m <- stk[[1]][,,1]
ch.m
for(i in 1:length(ch)) {
  rn <- ch[i]
  rw <- m[rn,1]
  cl <- m[rn,2]
  #print(c(rn, rw, cl))
  ch.m[rw, cl] <- 8
}

ch.m
m[ch,] # vertices of convex hull
library(splancs)
areapl(m[ch,])
#areapl(m)
plot(as.data.frame(m[ch,]), type="b")
#plot(as.data.frame(m), type="b")

# Shape complexity index
#SCI = 1 - A / Ah
#Where where A is the polygon's area and Ah is the area of the convex hull containing the polygon.

stk <- thresh.img.obj(image_read(pth), type = "individual.mean")
geom_analysis(stk[[1]])
