dir = 'data/Figures_doug/Figure 2_6/'

files = c("a) control" = "4ave_pico2_foff.nc",
          'b) fire only' = "4ave_pico2_fon.nc",
          'c) low [~CO2~]\nonly' = "4ave_foff.nc" , 
          "d) fire &\nlow [~CO2~]" = "4ave_fon.nc")
          
tas_file = 'data/Figures_doug/Figure 2_6/LGM_R20C2_detr_ensemble_hdx_tmp_ave_cropped.nc'
         
dat = lapply(paste0(dir, files), biome_assignment_from_file, tas_file)
dat = lapply(dat, function(i) i + 1)

ddat = dat
ddat[-1] = lapply(ddat[-1], function(r) {r[r == ddat[[1]]] = 1; r})

cols = c(' ' = 'white', Thf = '#114400', Tdf = '#441100',
         wtf = '#005555', tef = '#00EE33', tdf = '#66DD00',
         bef = '#000088', bdf = '#330033',
         Ts  = '#AA5500', sw  = '#777922', tp = '#66DD88', 
         bp  = '#22EEFF', dg  = '#FF9922', hd = '#FEFF44', st = '#BB33FF', t = '#FFBAAA')
         
