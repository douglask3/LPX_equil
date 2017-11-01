LPX_biome_assigning <- function(fname_height, vname_height,fname_fpc,vname_fpc,
				fname_gdd,vname_gdd)
	{
	#LPX_biome_assigning
	#Author: Douglas Kelley. douglask3@hotmail.com
	#**************************************************************
	#Assigning biomes based on LPX (Prentice et al, 2011) height,
	#	fractional projected cover (fpc) and growing degree days
	#	(gdd).
	#--------------------------------------------------------------
	#Usage:
	#	biome=LPX_biome_assigning(height_nc_file,height_nc_varname,
	#		fpc_nc_file,fpc_nc_varname,gdd_nc_file,gdd_nc_varname)
	#-------------------------------------------------------------
	#Inputs:
	#	height_nc_file: 	the LPX netcdf output file that contains
	# 		simulated height
	#	height_nc_varname: 	the height netcdf variable name.
	#		Normally = "height"
	#	fpc_nc_file: the LPX netcdf output file that contains
	# 		simulated fpc
	#	fpc_nc_varname: 	the fpc netcdf variable name.
	#		Normally = "fpc_grid"
	#	gdd_nc_file: the LPX netcdf output file that contains
	# 		simulated gdd
	#	gdd_nc_varname: 	the gdd netcdf variable name.
	#		Normally = "gdd_grid"
	#	
	#--------------------------------------------------------------
	#Outputs:
	#	$biome:		Matric containing numbers of 1-12relating to
	#		the corrisponding biome in the biome key for each LPX 
	#		grid cell. -999 is missing value (i.e ocean/ice shelf)
	#	$biome_key:	Key matching numbers in $biome to a specific
	#		biome type
	#--------------------------------------------------------------
	#Example:
	#	LPX_biomes=LPX_biome_assigning
	#		("r184_mod_bench_cont-5140.nc","height",
	#		"r184_mod_bench_cont-5140.nc","fpc_grid",
	#		"r184_mod_bench_cont-5140.nc","gdd")
	#Details:
	#
	#References:
	#	Add Pentice and Phillips
	#**************************************************************
	
	library(ncdf)

	ncfile=open.ncdf(fname_height)
	height=get.var.ncdf(ncfile,vname_height)

	ncfile=open.ncdf(fname_fpc)
	fpc=get.var.ncdf(ncfile,vname_fpc)

	ncfile=open.ncdf(fname_gdd)
	gdd=get.var.ncdf(ncfile,vname_gdd)


	a=dim(height)
	b=dim(fpc)
	c=dim(gdd)

	if (a[1]!=b[1]  || a[2]!=b[2] || a[1]!=c[1] || a[2]!=c[2] || a[3]!=b[3]) {
		print("Error -  dimensions disagree")
		print(" height dimensions:")
		print(a[1:3])
		print(" fpc dimensions:")
		print(b[1:3])
		print(" gdd dimensions:")
		print(c[1:2])
		stop
	}

	biome=array(0,dim=c(a[1],a[2]))
	biome_key=array(0,dim=c(2,12))
	biome_key[1,]=c(1:12)
	biome_key[2,1]='Tropical Forest'
	biome_key[2,2]='Warm Temperate Forest'
	biome_key[2,3]='Temperate Forest'
	biome_key[2,4]='Boreal Forest'
	biome_key[2,5]='Tropical Savannah'
	biome_key[2,6]='Sclerophyll Woodland'
	biome_key[2,7]='Temperate Parkland'
	biome_key[2,8]='Boreal Parkland'
	biome_key[2,9]='Dry Grass/Shrub'
	biome_key[2,10]='Hot Desert'
	biome_key[2,11]='Shrub Tundra'
	biome_key[2,12]='Tundra'

	
	threshold0=5
	threshold=threshold0
	for (i in 1:a[1]) {
		for (j in 1:a[2]) {
			
			if (sum(is.na(height[i,j,]))==0 &&
				sum(is.na(fpc[i,j,]))==0 &&
				sum(is.na(gdd[i,j]))==0){
				aheight=0
				for (k in 1:a[3]) {
					aheight=aheight+height[i,j,k]*fpc[i,j,k]
				}
				if (gdd[i,j]>350) {
					#Hot biome
					if (sum(fpc[i,j,])>0.6) {
						# Forest of Savannah
						if (sum(fpc[i,j,1:2])>0) {
							#Tropical
							if (aheight>10) {
								biome[i,j]=1 # Tropical Forest
							} else {
								biome[i,j]=5 # Tropical Savannah
							}
						} else if (sum(fpc[i,j,4]>
								sum(fpc[i,j,])/2)) {
							#Warm temperate
							if (aheight>10) {
								biome[i,j]=2 #Warm temperate
											 #forest
							} else {
								biome[i,j]=6 #Sclerophyll
											 #Woodland
							}
						} else if (sum(fpc[i,j,c(3,5)])>0) {
							#Temperate
							if (aheight>10) {
								biome[i,j]=3 #Temperate
											 #Forest
							} else {
								biome[i,j]=7 #Temperate
											 #Parkland
							}
						} else {
							# Boreal
							if (aheight>10) {
								biome[i,j]=4 # Boreal Forest
							} else {
								biome[i,j]=8 # Boreal Parkland
							}
						}
					} else if (sum(fpc[i,j,])>0.3) {
						biome[i,j]=9 # Dry Grass/shrub
					} else {
						biome[i,j]=10 # Hot desert
					}
				} else {
					#Cold biome
					if (sum(fpc[i,j,])>0.6) {
						if (aheight>10) {
							#Forest
							if (sum(fpc[i,j,1:2])>0) {
								biome[i,j]=1 # Tropical Forest
							} else if (sum(fpc[i,j,4]>
									sum(fpc[i,j,])/2)) {
								biome[i,j]=2 #Warm temperate
							} else if (sum(fpc[i,j,c(3,5)])>0) {
								biome[i,j]=3 #Temperate
											 #Forest
							} else {
								biome[i,j]=4 # Boreal Forest
							}
						} else {
							biome[i,j]=11 # Shrub Tundra
						}
					} else {
						biome[i,j]=12 # Tundra
					}
				}
			} else {
				biome[i,j]=NaN
			}
		}
	}					
		
	list(biome=biome,biome_key=biome_key)
	write.table(biome,file="biomes_CRU_co2LGM_FireOFF.csv",sep=" ",row.names=TRUE,col.names=TRUE)
	}
