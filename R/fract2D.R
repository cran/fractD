#' @title Calculates the fractal dimension of a 2D image
#' @description This function calculates the fractal dimension of a black area in a bidimensional image using the method of box-counting.
#' @param dir Specify a folder containing the image/s to be analysed.
#' @param box.size A vector specifing the box size steps (in pixel) to be used to calculate fractal dimension. By default (NULL) box.size = c(1,2,4,8,16,32,64,128,256,512). Minimun box size cannot be less then 1, corresponding to 1 pixel of the image. Box size steps must be integer.
#' @param save.dir Optional. Set the name of an output folder to save the data as *.rds.
#' @param save.name Optional. Provide a name for the output data.
#' @details The fuction calculates the fractal dimension (D) by the method of box-counting. Box-counting method is useful to calculate the fractal dimension of various sets of any dimension and patterns with or withouth self-similarity (Klinkenberg, 1994).
#'The method overlay a series of squares of different size (box.size), with minimun box size that is equal to the resolution of the image. Then, for each box size step the function keep track of the number of squares occupied by the black area into the image. Finally, fractal dimension (D) is estimated by linear regression of log(n°boxes) on log(box.size).
#'
#' @return \code{fract2D} returns a list containing the following components:\cr
#'
#' \code{D} - A dataframe with the estimated fractal dimension.\cr
#'
#' \code{raw.dat} - A dataframe with the raw data used to calculate the fractal dimension. The data frame contains the name of the image (id), the box size steps (box.size), and the number of boxes for each box size step (box).
#'
#' @references
#' \itemize{
#' \item Mandelbrot B.B. (1982) - \emph{The fractal Geometry of Nature}. San Francisco: W.H. Freman.
#' \item Klinkenberg B. (1994) - \emph{A Review of methods used to determine the fractal dimension of linear features}. Mathematical Geology, vol. 26, n° 1. doi: 10.1007/BF02065874
#' \item Dubuc B., Quiniou J.F., Roques-Carmes C., Tricot C., Zucker S.W. (1989) - \emph{Evaluating the fractal dimension of profiles}. Physical Review A, vol. 39, n° 3. doi: https://doi.org/10.1103/PhysRevA.39.1500
#' }
#' @note The function analyze the fractal dimension of a black area into an image. Then, the images must be converted in black and white before analysis.\cr
#' Usually, the box sizes should change as a function of a power of two so that they will be evenly spaced in the log space. However, this can result in too few points to fit linear regression. Therefore, according to Dubuc et al. (1989) the function allow to use box.size from 1 to +inf not necessarly with dyadic step.
#' @seealso \code{fract3D} to estimate the fractal dimension of a 3D (sliced) image/s.
#' @examples
#' \dontrun{
#' # The example below calculates the fractal dimension
#' # of image/s contained in the folder "source.dir".
#' # Then, a file named "res.rds" is saved into the "output" folder.
#'
#' fct2D <- fract2D(dir = "source.dir",
#'                  box.size = c(1,2,4,8,16,32,64,128,256,512),
#'                  save.dir = "output/",
#'                  save.name = "res")
#'
#' fct2D$D # a data frame with the estimated fractal dimension
#' fct2D$raw.dat # the raw data from which fractal dimension was calculated
#'}
#' @export
fract2D <- function(dir = NULL, box.size = NULL, save.dir = NULL, save.name = NULL){
  if (!requireNamespace("imager", quietly = TRUE)) {
    stop("Package \"imager\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package \"parallel\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("Package \"plyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("Package \"utils\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
 # require(imager)
 # require(parallel)
 # require(plyr)

  if(is.null(dir)) stop('dir argument not specified')

  if(!is.null(save.dir) & is.null(save.name)) stop('please provide the output file name')
  if(is.null(save.dir) & !is.null(save.name)) stop('please provide the output directory')

  if(is.null(box.size)){
    box.size = c(1,2,4,8,16,32,64,128,256,512)
  }


  files <- list.files(path = dir, pattern="*.jpg", full.names=T, recursive=FALSE)

  box <- c()
  dat <- NULL

  prog.bar <- utils::txtProgressBar(min = 0, max = length(files)*length(box.size), style = 3)
  k<-0

  for(i in (files)) {
    for (l in box.size) {

      if(l == 1){
        img <- as.matrix(imager::load.image(i))
        box[paste(i,l)] <- sum(img == 0, na.rm = T)
      } else{


        img <- as.matrix(imager::load.image(i))

        nr <- ceiling(nrow(img)/l)
        nc <- ceiling(ncol(img)/l)
        newM <- matrix(NA, nr*l, nc*l)
        newM[1:nrow(img), 1:ncol(img)] <- img

        div_k <- kronecker(matrix(seq_len(nr*nc), nr, byrow = TRUE), matrix(1, l, l))

        div_k <- as.numeric(div_k)
        newM <- as.numeric(newM)

        imgs <- newM[order(div_k)]

        d <- gl(length(imgs)/(l*l),(l*l))

        n_count <- sum(tapply(imgs, d, prod, na.rm = T) ==0)
        box[paste(i,l)] <- n_count
      }

      # inform progress on files
      Sys.sleep(0.05)
      k <- k + 1
      utils::setTxtProgressBar(prog.bar, k)

    }

  }
  dat <- rbind(dat, data.frame(box))
  dat$box.size <- sub(".* ", "", rownames(dat))
  dat$id <- gsub(".*/","", rownames(dat))
  dat$id <- gsub("\\..*","", dat$id)

  rownames(dat) <- NULL

  fractal_dim <- plyr::ddply(dat, c("id"),  plyr::summarize,
        D = round(as.numeric(-stats::lm(log(box)~ log(as.numeric(box.size)))$coefficients[2]), 4)
  )



  if(!is.null(save.dir) & !is.null(save.name)){
    saveRDS(list(D = fractal_dim, raw.dat = dat[,c(3,2,1)]), file = paste(save.dir, save.name,".rds", sep = ""))
  }

  return(out <- list(D = fractal_dim, raw.dat = dat[,c(3,2,1)]))


}




