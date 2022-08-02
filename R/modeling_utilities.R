#' Utility function for XXXXXX
#'
#' XXXX
#'
#' The function depends on the Rvision library which must be installed
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
emp.pix.dropout.rates <- function(img.list){

  num.imgs <- length(img.list) # Assumes images are in Rvision format
  pix.vals <- as.numeric(sapply(1:num.imgs, function(xx){img.list[[xx]][,,1]}))
  nr <- nrow(img.list[[1]][,,1]) # Should be the same for all images
  nc <- ncol(img.list[[1]][,,1]) # Should be the same for all images

  pixel.3d.array <- array(pix.vals, c(nr,nc,num.imgs))

  pixel.means   <- rowMeans(pixel.3d.array, dims = 2)
  drop.out.idxs <- which((pixel.means != 255) & (pixel.means != 0), arr.ind = T) # These pixels flip value
  print(drop.out.idxs)

  # Flipping pixel values, num.imgs x num drop.out.idxs
  flipping.pixel.vals <- t(sapply(1:num.imgs, function(xx){img.list[[xx]][,,1][drop.out.idxs]}))

  drop.out.rates <- colMeans(flipping.pixel.vals == 255)
  #print(data.frame(drop.out.idxs, drop.out.rates, t(flipping.pixel.vals)))

  return(data.frame(drop.out.idxs, drop.out.rates))



}
