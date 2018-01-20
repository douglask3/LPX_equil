##################################
## cfg							##
##################################
source("cfg.r")

copyNew = TRUE

modNames = c('FGOALS-1.0g', 'MIROC3.2')

fileCopy <- function(varname, modName) {
	inN  = paste(        'data/LGM_R20C2_detr_HadCM3M2_hdx_', varname, '.nc', sep = "")
	outN = paste('outputs/LGM_R20C2_detr_', modName, '_hdx_', varname, '.nc', sep = "")
	if (copyNew) file.copy(inN, outN, overwrite = TRUE)
	return(outN)
}

transferDataVar <- function(varname, modName) {
	outN = fileCopy(varname, modName)
	inN  = paste('data/LGM_R20C2_detr_', modName, '_hdx_', varname, '.nc', sep = "")
	
	in_nc  = nc_open( inN , write = FALSE)
	
	nyears = in_nc$var[[1]]$varsize[3]/12
	
	switchDat <- function(yr) {
		print(yr)
		mn = (yr - 1) * 12 + 1
		ot_nc  = nc_open( outN, write =  TRUE)
	
		in_var = ncvar_get(in_nc, in_nc$var[[1]], c(1,1, mn), c(720, 360, 12))
		ncvar_put(ot_nc, ot_nc$var[[1]], in_var, c(1,1, mn), c(720, 360, 12))
		nc_close(ot_nc)
	}
	lapply(1:nyears, switchDat)
}

transferData <- function(modName) 
	lapply(c('wethat', 'wspd'), transferDataVar, modName) 

lapply(modNames, transferData)
