##################################
## cfg							##
##################################
source("cfg.r")

## LPX output with height, gdd and fpc
fname_in = paste0(biome_dir, biome_files)


## varnames in fname_in
varnames = c(height    = 'height'  ,
			 fpc       = 'fpc_grid', 
			 gdd       = 'gdd_grid', 
			 lai       = 'lai_ind' )

## biome colour for plotting - same order as biome key below.
cols   = list(height    = c("white", "#333300"),
              fpc       = c("white", "#99FF00", "#003300"), 
		      fpc_tree  = c("white", "#99FF00", "#003300"), 
		      fpc_grass = c("white", "#99FF00", "#003300"), 
			  gdd       = c("white", "#AABB00", "#220000"),
		      lai       = c("white", "brown", "#002200"))
		   
dcols  = list(height    = c("#330033", "white" , "#333300"),
              fpc       = c("#210021", "#9900FF", "white", "#99FF00", "#003300"),
			  fpc_tree  = c("#210021", "#9900FF", "white", "#99FF00", "#003300"),
			  fpc_grass = c("#210021", "#9900FF", "white", "#99FF00", "#003300"),
			  gdd       = c("#000022", "#88FF88", "white", "#DDDD00", "#220000"),
			  lai       = c("#002121", "cyan", "white", "brown", "#002200"))
		 
limits = list(height    = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20), 
			  fpc       = seq(0.1, 0.9, 0.1), 
			  fpc_tree  = seq(0.1, 0.9, 0.1), 
			  fpc_grass = seq(0.1, 0.9, 0.1), 
			  gdd       = seq(0, 8000, 1000), 
			  lai       = c(0, 0.2, 0.5, 1, 2, 4, 6, 8, 10))
		
dlimits= list(height    = c(-10, -8, -6, -4, -2, 2, 4, 6, 8, 10),
			  fpc       = c(-0.3, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.3),
			  fpc_tree  = c(-0.3, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.3),
			  fpc_grass = c(-0.3, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.3),
			  gdd       = c(-2000, -1000, -500, -200, -100, 100, 200, 500, 1000, 2000),
			  lai       = c(-1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1))
		
scaling= list(height    = 1,
			  fpc       = 100,
			  fpc_tree  = 100,
			  fpc_grass = 100,
			  gdd       = 1,
			  lai       = 1)
              
units  = list(height    = 'm',
			  fpc       = '%',
			  fpc_tree  = '%',
			  fpc_grass = '%',
			  gdd       = '~DEG~C',
			  lai       = '~m2~/~m2~')
			  
##################################
## open							##
##################################
open_data <- function(fname) {
	dat = lapply(varnames, function(i) stack(fname, varname = i))

	weightByFPC <- function(x)
		sum(layer.apply(1:9,  function(i) dat[[x]][[i]] * dat[['fpc']][[i]]))

	dat[["lai"   ]] = weightByFPC('lai'   )
	dat[["height"]] = weightByFPC('height')
	dat[['fpc_tree' ]] = sum(dat[['fpc']][[1:7]])
	dat[['fpc_grass']] = sum(dat[['fpc']][[8:9]])
	dat[['fpc']] = sum(dat[['fpc']])
					   
	dat = lapply(dat, convert_pacific_centric_2_regular)
	return(dat)
}

dats = lapply(fname_in, open_data)

##################################
## plot							##
##################################

plotAllMaps <- function(i, dats,lims = limits, col = cols, fnm = '', ...) {
    rs =lapply(dats, function(r) r[[i]])
	pdf(paste0('figs/basic_vars-', i, fnm, '.pdf'), height = 9, width = 7)#, res = 300, units = 'in')
        layout(rbind(1:2, 3:4, 5), heights = c(1,1,0.18))
        par(mar = rep(0,4), oma = c(0, 0, 2, 0))
        if(length(rs) == 3) biome_files = biome_files[-1]
        mapply(plot_SA_Map_standard, rs, names(biome_files), MoreArgs = list(limits = lims[[i]], cols = col[[i]]), quick = FALSE)
        #title(i, outer = TRUE)
        if(length(rs) == 3) plot.new()
        add_raster_legend2(cols = col[[i]], limits = lims[[i]] * scaling[[i]], dat = rs[[1]], 
                           add = FALSE, extend_max = TRUE, transpose = FALSE, srt = 0,
                           plot_loc = c(0.1, 0.9, 0.6, 0.8), units = units[[i]], ...) 
    dev.off()
    
}

graphics.off()
mapply(plotAllMaps, names(dats[[1]]), MoreArgs = list(dats = dats))#, c("control", "fire only", "low CO2 only", "fire and low CO2"))

ddats = lapply(dats[-1], function(i) mapply('-', i, dats[[1]]))
mapply(plotAllMaps, names(dats[[1]]),
       MoreArgs = list(dats = ddats, lims = dlimits, col = dcols, fnm = '-diff', extend_min = TRUE))#, c("control", "fire only", "low CO2 only", "fire and low CO2"))


