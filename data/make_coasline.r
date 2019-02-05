source("cfg.r")

file = 'data/21ka_points_ice5g-dqs_fixed_mask.nc'

dat = raster(file, varname = 'landmask')

icemask = dat == 2

dat[dat > 1] = 1

#vdat = as.matrix(dat)

#nr = nrow(vdat)
#nc = ncol(vdat)

#dat_x = dat_y =  dat
#dat_x[] = abs(vdat - vdat[c(2:nr,1),])
#dat_y[] = abs(vdat - vdat[,c(2:nc,1)])

#dat[] = vdat

seamask = dat#(dat_x + dat_y) > 0

writeRaster(icemask, 'data/icemask.nc', overwrite = T)
writeRaster(seamask, 'data/seamask.nc', overwrite = T)