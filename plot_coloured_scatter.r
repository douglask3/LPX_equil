################################
## paths, files and paramters ##
################################

source('../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r')
sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/')

data_dir   = 'data' ## where you data is stored

titles = c('MIROC3.2 - LGM', 'MIROC3.2 - LGM')

#Note the number of x climate variables must be the same as number of titles.
x_file     = c('LGM_R20C2_detr_MIROC3.2_hdx_tmp_ave_cropped.nc', ## which files to get x-axis
               'LGM_R20C2_detr_MIROC3.2_hdx_pre_ave_cropped.nc') ## variables from

x_varnames = list(NULL, NULL) ## variable name within the file. if NULL, just open whatever variable comes first
x_layers   = list(NULL, NULL) ## Layers you want to average over from each variable.
                              ## NULL means select all layers.
x_scale    = list(1, 12)  ## how much you need to multiply your x-axis climate average. 
                      ## i.e as Mean Annual temp is a straight, there's no need to scale 
                      ## so we multiply it here by 1. Precip is a cumulative annual average
					  ## from monthly data, so time by 12
x_names    = list('MAT', 'MAP') ## What we call our x variables on the plot



## All as with x. 
y_file     = list('LGM_R20C2_detr_MIROC3.2_hdx_pre_ave_cropped.nc',
               'MIROC_TEST_LGM_FON_5380_138_R20C_CRUnonclim-5379.nc')
			   
y_varnames = list(NULL, 'fpc_grid') ## We want fpc_grid  from our 2nd (LPX) y_file
y_layers   = list(NULL, 1:7)  ## For FPC, we are interested in tree cover, so select 1:7 (LPX tree PFTS).
	   
y_scale    = list(12, 100 * 7)  ## he 100 converts from a fraction to a %. The 7 is for the no. pfts.
y_names    = list('MAP', 'Tree Cover')


## Number of z variables does not need to match no. of x and  y.
z_file     = c('MIROC_TEST_LGM_FON_5380_138_R20C_CRUnonclim-5379.nc',
			   'MIROC_TEST_LGM_FON_5380_138_R20C_CRUnonclim-5379.nc')
z_varnames = c('fpc_grid', 'mfire_frac')
z_layers   = list(1:7, NULL)  
z_names    = c('Tree Cover - %', 'Burnt area') 
z_scale    = c(100 * 9, 100 * 12)
z_cols     = list(c('black', 'yellow', 'green'), ## The colour map stages for out coloured scatter plot. yellow and green is good for red
                  c('black', 'yellow', 'red')) ## yellow and red is good for fire.
                                               ## The first colour (black) is what we want to colour point with no tree cover/fire. 2nd (yellow) is intermediate and 3rd (green or red) is for highest values of tree or fire.

z_levels   = list(c(1, 2, 5, 10, 20, 40, 60, 80),  ## Levels cut offs for colors. See plot legend
				   5)							   ## If just a single number is supplied,
												   ## then that is the number of quantiles
												   ## to split our data into
###############
## open data ##
###############

openDat <- function(filename, scale = 1, layers = NULL, varname = NULL) {
    filename = paste(data_dir, filename, sep = '/')
	if (is.null(varname)) r = brick(filename)
		else r = brick(filename, varname = varname)## load data
    
    if (!is.null(layers)) r = r[[layers]] ##required layers (i.e, will load layers 1:7 for fpc to get tree cover
    if (nlayers(r) > 1) r = mean(r) else r = r[[1]] ## meaning remaining layers
    r = r * scale ## scaling
    r[r > 9E9] = NaN ## setting mask
    return(r)
}

x = mapply(openDat, x_file, x_scale, x_layers, x_varnames)
y = mapply(openDat, y_file, y_scale, y_layers, y_varnames)
z = mapply(openDat, z_file, z_scale, z_layers, z_varnames)

##########
## plot ##
##########
## set up plotting widnow.
    # mfcol = c(ncol, nrow) is the plotting grid.
    # mar = c(bottom, left, top, right) is the inner margin (i.e, gap around each plot).
    # oma is the outer margin (gap around the entire grid).
png('colour_scatter_example.png', height = 7, width = 7, uni = 'in', res = 150)
par(mfcol = c(length(z), length(x)), mar = c(3.5, 0.1, 0, 1), oma = c(2, 5, 1, 0))

## Function foradding a legend.
Legend <- function(zlevel, labs, zcol, zname) {
	if (is.null(labs)) {
		labs = paste(head(zlevel, -1), zlevel[-1], sep = '-')
		labs = c(paste("<", zlevel[1]),
				 labs,
				 paste(">", tail(zlevel, 1)))
	}
    legend(x = 'topleft', legend = labs, pch = 19, col = zcol, ncol = 3, title = zname, bty = 'n')
}

## plotting function for individual plot in our grid
plot3scatter <- function(title, x, xname, y, yname, addLegend, z, zcol, zlevel, zlabels, zname, yaxt, ...) {
    
    ## remove values not on a common mask
    mask = !is.na(x + y + z)
    x = x[mask]
    y = y[mask]
    z = z[mask]

    # make colours a bit transparent and assign a colour to each z datapoint
    z = cut_results(z, zlevel)
    zcol_plot = make.transparent(zcol, 0.9)
    zcol_plot = zcol_plot[z]

    # make plotting area and fill with black points
    plot(x, y, pch = 19, xlab  = '', ylab = '', yaxt = yaxt, ...)

    # add our coloured points, at 20 different sizes so we can see detail in areas with lots of points
    for (cex in seq(1.0, 0.05, -0.05)) points(x, y, pch = 19, col = zcol_plot, cex = cex)

    # add various lables
    mtext(side = 1, xname, line = 2.5)
    if (yaxt == 's') {
        mtext(side = 2, title, line = 3.5, cex = 1.3)
        mtext(side = 2, yname, line = 2.5)
    }
    
    # add legend when asked
    if(addLegend) Legend(zlevel, zlabels, zcol, zname)
}

selectLevels <- function(x, level) {
	
	level = quantile(x, seq(0, 1, length.out = level + 1))
	level = head(level[-1], -1)
	level = standard.round(level)
	level = unique(level)
	
	return(level)
}

# plots all plots withb same colopur variable
plotZs <-function(z, zcol, zlevel, zname, ...) {
    # convert our colour chose for our z color variable into a full colour map
	if (length(zlevel) == 1) {
		zlevel = selectLevels(z, zlevel)
		zlabels = 1:(length(zlevel) + 1)
		zname = paste(zname, 'qunantiles', sep = ' - ')
	} else {
		zlabels = NULL
	}
    zcol =  make_col_vector(zcol, ncols = length(zlevel) + 1) 
	
    # descide which of the x-y plots will have our color legend
    addLegend = rep(FALSE, length(x))
    addLegend[1] = TRUE

    # plots all the plots of this colour
    mapply(plot3scatter, titles, x, x_names, y, y_names, addLegend, MoreArgs = list(z, zcol, zlevel, zlabels, zname, ...))
}

# decide which of our plots will have a y axis added
yaxt = rep('n', length(z))
yaxt[1] = 's'

# plots all the plots
mapply(plotZs, z, z_cols, z_levels, z_names, yaxt = yaxt)
dev.off()

#z, z_cols, z_levels
