biome_dir = 'data/Figures_doug/Figure 2_6/'

biome_files = c("a) control" = "4ave_pico2_foff.nc",
                "b) fire only" = "4ave_pico2_fon.nc",
                "c) low [~CO2~]\nonly" = "4ave_foff.nc" , 
                "d) fire &\nlow [~CO2~]" = "4ave_fon.nc")
          

tas_file = 'data/Figures_doug/Figure 2_6/LGM_R20C2_detr_ensemble_hdx_tmp_ave_cropped.nc'
         
dat = biomeDat = lapply(paste0(dir, files), biome_assignment_from_file, tas_file)
dat = lapply(dat, function(i) i + 1)

ddat = dat
ddat[-1] = lapply(ddat[-1], function(r) {r[r == ddat[[1]]] = 1; r})
   
biome_cols = 
cols = c(' ' = 'white'  , Thf = '#002200', Tdf = '#338800',
         wtf = '#005555', tef = '#00EE33', tdf = '#66DD00',
         bef = '#000088', bdf = '#330033',
         Ts  = '#999900', sw  = '#BB9999', tp = '#8844FF', 
         bp  = '#880088', dg  = '#FFAA00', hd = '#FFFF77', st = '#00AAEE', t = '#99CCFF')
   
