## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example2D ,results = "hide", message = FALSE, warning = FALSE------------
library(fractD)
## basic example code for fract2D
fct2D <- fract2D(dir = "examples/source.dir", box.size = c(1,2,4,8,16,32,64,128,256,512))

## the following code saves the data in a file named "es3D.rds" into the "output"" folder 
## not run ## 
##fct2D <- fract2D(dir = "examples/source.dir", box.size = c(1,2,4,8,16,32,64,128,256,512), save.dir = "examples/output/", save.name = "es2D")

## -----------------------------------------------------------------------------
# the function create a list with two objects: 
fct2D$D # Estimated fractal dimension

fct2D$raw.dat # Raw data from which fractal dimension was calculated 

## ----example3D ,results = "hide", message = FALSE, warning = FALSE------------
library(fractD)
## basic example code for fract3D
# save.dir and save.name provide optional 
fct3D <- fract3D(dir = "examples/source.dir", dist.slice = 10, box.size = c(1,2,4,8,16,32,64,128,256,512))

## the following code saves the data in a file named "es3D.rds" into the "output"" folder 
## not run ## 
## fct3D <- fract3D(dir = "examples/source.dir", dist.slice = 1, box.size = c(1,2,4,8,16,32,64,128,256,512), save.dir = "examples/output/", save.name = "es3D")

## -----------------------------------------------------------------------------
# the function create a list with two objects: 
fct3D$D # Estimated fractal dimension

fct3D$raw.dat # Raw data from which fractal dimension was calculated 

