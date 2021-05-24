source('../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r')

sourceAllLibs('../gitProjectExtras/gitBasedProjects/R')
source("libs/return_multiple_from_functions.r")
sourceAllLibs()
#source('../gitProjectExtras/package_git2r.r')
#config(repository(), user.name="Douglas Kelley", user.email="douglas.i.kelley@gmail.com")

library(benchmarkMetrics)
#library(gitBasedProjects)
library(raster)
library(ncdf4)
library(rasterExtras)
library(rasterPlot)
library(plotrix)
library(mapdata)
library(mapplots)
library(ellipse)

sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/')
sourceAllLibs()
data(worldHiresMapEnv)
library(raster) ## package for reading in model output

sourceAllLibs()
