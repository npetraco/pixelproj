#' Utility function for computing pixel drop out rates in an array of black and white pixels
#'
#' The pixel drop out rate is the probability the pixel will be white (255) given that it was
#' black (0) at least once in a stack of registered images
#'
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
  #print(length(drop.out.idxs))

  if(length(drop.out.idxs) == 0) {

    # If there are no drop out pixels just return NULL to indicate no simulation needed
    drop.out.info.mat <- NULL

  } else {

    # Flipping pixel values, num.imgs x num drop.out.idxs
    flipping.pixel.vals <- t(sapply(1:num.imgs, function(xx){img.list[[xx]][,,1][drop.out.idxs]}))
    #print(flipping.pixel.vals)

    drop.out.rates <- colMeans(flipping.pixel.vals == 255)
    #print(drop.out.rates)
    #print(data.frame(drop.out.idxs, drop.out.rates, t(flipping.pixel.vals)))

    drop.out.info.mat <- data.frame(drop.out.idxs, drop.out.rates)

  }

  return(drop.out.info.mat)

}


#' Simulate pixel patterns
#'
#'
#' Given an input set of pixel patterns, simulate new patterns based on the input pixel
#' patterns dropout rates.
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
simulate.pix.pattern <- function(img.list, nsims=1){

  # Identify the drop out pixels and compute the drop out rates
  drop.out.info.mat <- emp.pix.dropout.rates(img.list)
  #print(drop.out.info.mat)

  img.list.sims <- rep(list(NULL), nsims)

  for(i in 1:nsims) {

    # Pick one of the real pixel patterns from the image stack to serve as a template. Doesn't matter which real one we pick:
    template.img <- cloneImage(img.list[[1]], target = "new")

    # If there are no dropouts, the sims will just be copies of image1 since all images in the stack are the same
    if(!is.null(drop.out.info.mat)) {
      # Pixel pattern template matrix to hold the simulation.
      template.img.mat <- template.img[,,1]


      # Null out just in case
      template.img.mat[as.matrix(drop.out.info.mat[,c(1,2)])] <- NA
      #plot(template.img.mat, key=NULL)

      # Use drop out rates (probability the pixel will be white (255)) for non-constant pixels
      sim.pixs <- sapply(1:nrow(drop.out.info.mat), function(xx){sample(c(255,0), size = 1, prob = c(drop.out.info.mat[xx,3], 1-drop.out.info.mat[xx,3]))})
      #print(sim.pixs)

      template.img.mat[as.matrix(drop.out.info.mat[,c(1,2)])] <- sim.pixs

      # Put simulated pixel pattern nack into template matrix
      template.img[]     <- template.img.mat

    }

    img.list.sims[[i]] <- template.img

  }

  return(img.list.sims)

}
