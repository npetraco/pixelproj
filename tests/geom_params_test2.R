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
geom_analysis(stk[[5]], plotQ = T)
plot.stack(stk)
