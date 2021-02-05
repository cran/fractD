#' @title Calculates the fractal dimension of a 3D (sliced) image
#' @description This function calculates the fractal dimension of a black area in a three-dimensional image using the method of box-counting.
#' @param dir Specify a directory containing the folder/s with the image/s to be analysed.
#' @param box.size A vector specifing the box size steps (in pixel) to be used to calculate fractal dimension. By default (NULL) box.size = c(1,2,4,8,16,32,64,128,256,512). Minimun box size cannot be less then 1, corresponding to 1 pixel of the image. Box size steps must be integer.
#' @param dist.slice The distance between slices. By default (NULL) dist.slice = 1. Distance between slice must be an integer number.
#' @param save.dir Optional. Set the name of an output folder to save the data as *.rds.
#' @param save.name Optional. Provide a name for the output data.
#' @details The fuction calculates the fractal dimension (D) of a 3D image by the method of box-counting. Box-counting method is useful to calculate the fractal dimension of various sets of any dimension and patterns with or withouth self-similarity (Klinkenberg, 1994).
#'The method overlay a series of cubes of different size (box.size x box.size x boxsize), with minimun size that is equal to the resolution of the image. Then, for each box size step the function keep track of the number of cubes occupied by the black area into the image. Finally, fractal dimension (D) is estimated by linear regression of log(n°boxes) on log(box.size).
#'
#' @return \code{fract3D} returns a list containing the following components:\cr
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
#' \item Taud H and Parrot J-F (2005) - \emph{Measurement of DEM roughness using the local fractal dimension}. Géomorphologie: relief, processus, environnement: 4, 327-338. doi: 10.4000/geomorphologie.622
#' }
#' @note The function analyze the fractal dimension of a black area into an image. Then, the images must be converted in black and white before analysis.\cr
#' IMPORTANT: images slices must be numerated as 00, 01,02 ... 10, 11 etc...\cr
#' Usually, the box sizes should change as a function of a power of two so that they will be evenly spaced in the log space. However, this can result in too few points to fit linear regression. Therefore, according to Dubuc et al. (1989) the function allow to use box.size from 1 to +inf not necessarly with dyadic step.
#' @seealso \code{fract3D} to estimate the fractal dimension of a 3D (sliced) image/s.
#' @examples
#' \dontrun{
#' # The example below calculates the fractal dimension of 3D sliced image/s
#' # contained into folder/s that are placed into "source.dir".
#' # Then, a file named "res.rds" is saved into the "output" folder.
#' # The distance between sliced image is 10 px.
#'
#' fct3D <- fract3D(dir = "source.dir",
#'                  dist.slice = 10,
#'                  box.size = c(1,2,4,8,16,32,64,128,256,512),
#'                  save.dir = "output/", save.name = "res")
#'
#' fct3D$D # a data frame with the estimated fractal dimension
#' fct3D$raw.dat # the raw data from which fractal dimension was calculated
#'}
#' @export
fract3D <- function(dir = NULL, box.size= NULL, dist.slice = NULL, save.dir = NULL, save.name = NULL){
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
#  require(imager)
 #require(parallel)
  #require(plyr)

  if(is.null(dir)) stop('dir argument not specified')

  if(!is.null(save.dir) & is.null(save.name)) stop('please provide the output file name')
  if(is.null(save.dir) & !is.null(save.name)) stop('please provide the output directory')

  dir <- list.dirs(path = dir, full.names = T, recursive = F)

  if(is.null(box.size)){
    box.size = c(1,2,4,8,16,32,64,128,256,512)
  }
  if(is.null(dist.slice)){
    dist.slice = 1
  }


  box <- c()
  dat <- NULL


  pb <- utils::txtProgressBar(min = 0, max = length(dir) * length(box.size), style = 3)
  b<-0

  for (k in dir) {

    files <- list.files(path= k, pattern="*.jpg", full.names=T, recursive=FALSE)


    imgs <- c()
    for (l in box.size) {
      if(l == 1){

        for(i in (files)) {
          imgs[[i]] <- as.numeric(imager::load.image(i))
        }

        imgs2 <- c()

        for(a in 1:length(imgs)) {
          if(a == 1 | a ==  length(imgs)){
            imgs2[[a]] <- sum(imgs[[a]]==0, na.rm = T) * ceiling(dist.slice/2)
          }else{
            imgs2[[a]] <- sum(imgs[[a]]==0, na.rm = T) * ceiling(dist.slice)

          }
        }

        box[paste(i,l)] <- sum(imgs2, na.rm = T)


      } else{

        for(i in (files)) {
          img <- as.matrix(imager::load.image(i))

          nr <- ceiling(nrow(img)/l)
          nc <- ceiling(ncol(img)/l)

          div_k <- kronecker(matrix(seq_len(nr*nc), nr, byrow = TRUE), matrix(1, l, l))
          div_k <- as.numeric(div_k)

          newM <- matrix(NA, nr*l, nc*l)
          newM[1:nrow(img), 1:ncol(img)]  <- img
          newM <- as.numeric(newM)

          imgs[[i]] <- newM[order(div_k)]

           }


        imgs2 <- c()

        for(a in 1:length(imgs)) {
          if(a == 1 | a ==  length(imgs)){
            imgs2[[a]] <- rep(list(imgs[[a]]), dist.slice/2)
          }else{
            imgs2[[a]] <- rep(list(imgs[[a]]), dist.slice)

          }
        }


        imgs2 <-unlist(imgs2, recursive = F, use.names = F)


        if(l > length(imgs2)) {
          d <- gl(1,length(imgs2))
        } else  if(length(imgs2)%%l==0) {
          d <- gl(length(imgs2)/l,l)
        } else  if(l < length(imgs2)){
          d <- factor(rep(1:(ceiling(length(imgs2)/l)), times = c( rep(l,floor(length(imgs2)/l)), (length(imgs2)-(floor(length(imgs2)/l)*l))  )))
        }


        f <- split(imgs2,d)

        sds3 <- list()

        for(e in 1:length(f)){
          sds3[[e]] <- Reduce("*", f[[e]])
        }

        d2 <- gl(length(sds3[[1]])/(l*l),(l*l))

        sds4 <-  unlist(parallel::mclapply(1:length(sds3), function(i){ as.vector(tapply(sds3[[i]], d2, prod, na.rm = T))}), use.names = F)

        box[paste(i,l)] <- sum(sds4==0)

      }

      Sys.sleep(0.05)
      b <- b + 1
      utils::setTxtProgressBar(pb, b)

    }}


  dat <- rbind(dat, data.frame(box))

  dat$box.size <- sub(".* ", "", rownames(dat))
  dat$id <- sub("^(.*)[/].*", "\\1",rownames(dat))

  dat$id <- gsub("^.*\\/","", dat$id)

  rownames(dat) <- NULL

  fractal_dim <- plyr::ddply(dat, c("id"),  plyr::summarize,
                       D = round(as.numeric(-stats::lm(log(box) ~ log(as.numeric(box.size)))$coefficients[2]), 4)
  )

  if(!is.null(save.dir) & !is.null(save.name)){
    saveRDS(list(D = fractal_dim, raw.dat = dat[,c(3,2,1)]), file = paste(save.dir, save.name,".rds", sep = ""))
  }

  return(out <- list(D = fractal_dim, raw.dat = dat[,c(3,2,1)]))



}






