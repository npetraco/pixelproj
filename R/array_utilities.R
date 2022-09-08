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
#' Convert array to vector in row major order
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
array2vec <- function(arr.mat){

  # Flatten pixel pattern and 255s -> 1 for CRF routines
  arr.vec <- as.numeric(arr.mat == 255) # white = 1, black = 0

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
image.list2vec.mat <- function(img.list){

  num.images <- length(img.list)
  #print(num.images)
  #print(img.list[[1]])

  vec <- array2vec(img.list[[1]][,,1])
  config.mat <- array(NA, c(num.images, length(vec)))
  config.mat[1,] <- vec

  for(i in 2:num.images){
    vec <- array2vec(img.list[[i]][,,1])
    #print(vec)

    config.mat[i,] <- vec
  }

  return(config.mat)

}