#' Vector index to matrix subscript
#'
#' Vector index to matrix subscript
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
ind2sub <- function(idx, num.row, col.majQ=F) {

  r        <- ((idx-1) %% num.row) + 1
  c        <- floor((idx-1) / num.row) + 1
  if(col.majQ==T) {
    subscrpt <- c(c,r) # column-major order
  } else {
    subscrpt <- c(r,c) # row-major order
  }

  return(subscrpt)
}


#' Convert node number to matrix indices in a rectangular array of nodes
#'
#' Convert node number to matrix indices in a rectangular array of nodes
#'
#' Re-name of ind2sub and removed cmQ option for speed
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
node.num2idxs <- function(idx, num.row) {

  r        <- ((idx-1) %% num.row) + 1
  c        <- floor((idx-1) / num.row) + 1
  subscrpt <- c(r,c) # row-major order

  return(subscrpt)
}


#' Convert array to vector in row major order
#'
#' Convert array to vector in row major order. If no state names are supplied,
#' binary vector is returned with "white" pixels = 1 and "black" pixels = 0
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
array2vec <- function(arr.mat, state.names=NULL){

  # Flatten pixel pattern and 255s -> 1 for CRF routines
  arr.vec <- as.numeric(arr.mat == 255) # white = 1, black = 0 NOTE: this is now a BINARY vector
  if(!is.null(state.names)) {
    arr.vec[which(arr.vec == 1)] <- state.names[1]
    arr.vec[which(arr.vec == 0)] <- state.names[2]
  }

  return(arr.vec)
}


#' Convert binary vector to an array assuming row major order
#'
#' Convert binary vector to an array assuming row major order
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
vec2array <- function(arr.vec, num.rows, num.cols, white.pix=1, black.pix=0){

  arr.mat <- array(NA, c(num.rows, num.cols))

  arr.mat[ t(sapply(1:(num.rows*num.cols), function(xx){ind2sub(xx, num.row = num.rows )})) ] <- arr.vec
  arr.mat[ which(arr.mat == 1, arr.ind = T) ] <- white.pix
  arr.mat[ which(arr.mat == 0, arr.ind = T) ] <- black.pix

  return(arr.mat)

}


#' Convert a list of images to a matrix of row vectors
#'
#' Convert a list of images (i.e. an image stack) to a matrix of row vectors
#'
#' Assumes all images in the stack are of the same dimension
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
image.list2vec.mat <- function(img.list, white.pix.val=NULL, black.pix.val=NULL){

  num.images <- length(img.list)
  #print(num.images)
  #print(img.list[[1]])

  vec            <- array2vec(img.list[[1]][,,1])
  config.mat     <- array(NA, c(num.images, length(vec)))
  config.mat[1,] <- vec

  for(i in 2:num.images){
    vec <- array2vec(img.list[[i]][,,1])
    #print(vec)

    config.mat[i,] <- vec
  }


  # Change white/black pixel values if requested:
  # Note: right now black = 0, white = 1 from array2vec
  config.mat2 <- config.mat          # Make a copy of config.mat in case we want to switch 0s for 1s and 1s for 0s
  if(!is.null(white.pix.val)) {
    config.mat2[ which(config.mat == 1, arr.ind = T) ] <- white.pix.val
  }
  if(!is.null(black.pix.val)) {
    config.mat2[ which(config.mat == 0, arr.ind = T) ] <- black.pix.val
  }
  config.mat <- config.mat2


  return(config.mat)

}
