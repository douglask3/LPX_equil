graphics.off()
source("cfg.r")

dir = "data/Figures_doug/Figure 5/"

files = c("a) <f1> : \nlow ~CO2~, no fire" = "f1_co2_effects.nc", 
          "b) <f2> : \nfire at PI ~CO2~"    = "f2_fire_effects.nc", 
          "c) <f3> : \nfire at LGM ~CO2~"   = "f3_LGM_fire_effects.nc" , 
          "b) <f12> : \nfire & low ~CO2~"    = "f12_fire_co2_effects.nc")
          
dat = lapply(paste0(dir, files), raster)

cols = c('#110011', '#990099', '#FF00FF', 'white', '#99FF00', '#00FF00', '#002200')
limits = c(-0.8, -0.6, -0.4, -0.2, 0.2, 0.4, 0.6, 0.8)

png('figs/figure5.png', height = 7, width = 5, res = 300, units = 'in')
    layout(rbind(1:2, 3:4, 5), heights = c(1,1, 0.3))
    par( mar = rep(0, 4), oma = c(0, 0, 0, 0.5))
    
    plotMap <- function(dat, nm) { 
        plot_map_standrd(dat, limits = limits, cols = cols, readyCut = FALSE)
        mtext.units(nm, side = 3, line = -3, adj = 0, at = -63)
    }

    mapply(plotMap, dat, names(files))


    addStandardLegend <- function(x, limits, cols, units = '', plot_loc = c(0.2, 0.80, 0.6, 0.7), ylabposScling=1, ...)  {
        add_raster_legend2(cols, limits, dat = x, srt = 90,
                   transpose = FALSE, plot_loc = plot_loc, ylabposScling=ylabposScling, oneSideLabels = TRUE, xpd = NA, adj = 1.0, ...)
        mtext(units, side = 3, line = -3)
    }
    addStandardLegend(dat[[4]], limits, cols, add = FALSE)
dev.off()