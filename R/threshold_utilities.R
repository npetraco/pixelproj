#' Threshold an image object (stack or individual image), by it's overall mean
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
thresh.img.obj <- function(img.obj, type="group.mean", scale.factor=255) {

  if(class(img.obj) == "magick-image"){ # See if it's a stack otherwise assume 1 image
    #print("GIF image stack")

    num.imgs <- length(img.obj)
    #print(num.imgs)

    img.list <- rep(list(NULL), num.imgs)
    for(i in 1:num.imgs) {
      img.list[[i]] <- as.integer(img.obj[i][[1]])[,,1] # All three channels should be the same
    }

  } else { # Expand this later if we use different image types

    if(class(img.obj) != "Rcpp_Image"){
      stop("Single images must be in Rvision format!")
    }

    num.imgs <- 1

    # Assume image is an Rvision object right now:
    img.list <- list(img.obj[,,1]) # All three channels should be the same

  }

  # Threshold types:
  if(type=="group.mean") {

    thresh.val <- mean(unlist(img.list))

    # threshold the image(s) by the threshold value and store in Rvision format
    for(i in 1:num.imgs) {

      img.tmp       <- zeros(nrow = nrow(img.list[[i]]), ncol = ncol(img.list[[i]]), nchan = 1, bitdepth = "8U")
      img.tmp[]     <- scale.factor * (img.list[[i]][] > thresh.val)
      img.list[[i]] <- img.tmp

    }

  } else if(type=="individual.mean") {

    # threshold the image(s) by the threshold value and store in Rvision format
    for(i in 1:num.imgs) {

      thresh.val    <- mean(img.list[[i]])
      img.tmp       <- zeros(nrow = nrow(img.list[[i]]), ncol = ncol(img.list[[i]]), nchan = 1, bitdepth = "8U")
      img.tmp[]     <- scale.factor * (img.list[[i]][] > thresh.val)
      img.list[[i]] <- img.tmp

    }

  } else {
    stop("Choose type = group.mean, individual.mean")
  }

  return(img.list)
}


#' Mean image of a stack
#'
#' Mean image of a stack, thresholded by the scale.factor
#'
#' The function depends on the Rvision library which must be installed
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
mean.stack.img <- function(img.list, scale.factor=255) {

  avg.img <- thresh.img.obj( # Threshold to make sure pixel vals are on binary scale
                             mean(img.list, target="new"), # From Rvision mean.list
                             type = "individual.mean")
  return(avg.img)
}
