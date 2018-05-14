################################
## paths, files and paramters ##
################################
source('../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r')
sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/')

data_dir   = 'data' ## where you data is stored

titles = c('MIROC3.2 - LGM', 'MIROC3.2 - LGM')

#Note the number of x climate variables must be the same as number of titles.
x_file     = c('LGM_R20C2_detr_MIROC3.2_hdx_tmp_ave_cropped.nc', ## which climate file you want
               'LGM_R20C2_detr_MIROC3.2_hdx_tmp_ave_cropped.nc') ## along the x-axis
x_scale    = c(1, 1)  ## how much you need to multiply your x-axis climate average. 
                      ## i.e as Mean Annual temp is a straight, there's no need to scale 
                      ## so we multiply it here by 1.
x_names    = c('MAT', 'MAT') ## What we call our x variables on the plot

## All as with x. 
y_file     = c('LGM_R20C2_detr_MIROC3.2_hdx_pre_ave_cropped.nc',
               'LGM_R20C2_detr_MIROC3.2_hdx_pre_ave_cropped.nc')
y_scale    = c(12, 12)  ## i.e as Mean Annual Precip is cummultive, you need to multiply 
                        ## it by 12 to go from mean monthly to mean annual 
y_names    = c('MAP', 'MAP')


## Number of z variables does not need to match no. of x and  y.
z_file     = 'MIROC_TEST_LGM_FON_5380_138_R20C_CRUnonclim-5379.nc' ## LPX output for scatter color
z_varnames = c('fpc_grid', 'mfire_frac') ## variable you want to plot
z_layers   = list(1:7, NULL)  ## Layers you want to average over from each varaible.
                              ## For FPC, we are interested in tree cover, so select 1:7 (LPX tree PFTS).
                              ## NULL means select all layers for fire.
z_names    = c('Tree Cover', 'Burnt area') ## what we want our variables to be called on the plot
z_scale    = c(100 * 9, 100 * 12) ## scaling as per x_scale and y_scale. the 100 converts from a fraction to a %
z_cols     = list(c('black', 'yellow', 'green'), ## The colour map stages for out coloured scatter plot. yellow and green is good for red
                  c('black', 'yellow', 'red')) ## yellow and red is good for fire.
                                               ## The first colour (black) is what we want to colour point with no tree cover/fire. 2nd (yellow) is intermediate and 3rd (green or red) is for highest values of tree or fire.

z_levels   = list(c(1, 2, 5, 10, 20, 40, 60, 80), c(0.01, 0.02, 0.05, 1, 2, 5, 10)) ## Levels cut offs for colors. See plot legend.
##########
## open ##
##########

openDat <- function(filename, scale = 1, layers = NULL, ...) {
    filename = paste(data_dir, filename, sep = '/')
    r = brick(filename, ...)
    
    if (!is.null(layers)) r = r[[layers]]
    if (nlayers(r) > 1) r = mean(r) else r = r[[1]]
    r = r * scale
    r[r > 9E9] = NaN
    return(r)
}

x = mapply(openDat, x_file, x_scale)
y = mapply(openDat, y_file, y_scale)
z = mapply(openDat, z_file, z_scale, z_layers, varname = z_varnames)

##########
## plot ##
##########
par(mfcol = c(length(z), length(x)), mar = c(3.5, 0.1, 0, 1), oma = c(2, 5, 1, 0))

Legend <- function(zlevel, zcol, zname) {
    labs = paste(head(zlevel, -1), zlevel[-1], sep = '-')
    labs = c(paste("<", zlevel[1]),
             labs,
             paste(">", tail(zlevel, 1)))
    legend(x = 'topleft', legend = labs, pch = 19, col = zcol, ncol = 3, title = zname, bty = 'n')
}

plot3scatter <- function(title, x, xname, y, yname, addLegend, z, zcol, zlevel, zname, yaxt, ...) {
    
    mask = !is.na(x + y + z)
    x = x[mask]
    y = y[mask]
    z = z[mask]
    z = cut_results(z, zlevel)
    
    zcol_plot = make.transparent(zcol, 0.9)
    zcol_plot = zcol_plot[z]
    plot(x, y, pch = 19, xlab  = '', ylab = '', yaxt = yaxt, ...)
    for (cex in seq(1.0, 0.05, -0.05)) points(x, y, pch = 19, col = zcol_plot, cex = cex)

    mtext(side = 1, xname, line = 2.5)
    if (yaxt == 's') {
        mtext(side = 2, title, line = 3.5, cex = 1.3)
        mtext(side = 2, yname, line = 2.5)
    }

    if(addLegend) Legend(zlevel, zcol, zname)
   
}

plotZs <-function(z, zcol, zlevel, zname, ...) {
    zcol =  make_col_vector(zcol, ncols = length(zlevel) + 1) 
    addLegend = rep(FALSE, length(x))
    addLegend[1] = TRUE
    mapply(plot3scatter, titles, x, x_names, y, y_names, addLegend, MoreArgs = list(z, zcol, zlevel, zname, ...))
}

yaxt = rep('n', length(z))
yaxt[1] = 's'
mapply(plotZs, z, z_cols, z_levels, z_names, yaxt = yaxt)

#z, z_cols, z_levels
