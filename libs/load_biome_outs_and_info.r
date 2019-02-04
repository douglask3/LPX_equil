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

      
cols = c(' ' = 'white'  , Thf = '#343383', Tdf = '#94C424',
         wtf = 'black', tef = 'black', tdf = 'black',
         bef = '#black', bdf = '#black',
         Ts  = '#E98763', sw  = '#C85649', tp = '#C5E000', 
         bp  = '#910080', dg  = '#FF7C00', hd = '#FFF600', st = '#FF007A', t = '#00A0FB')
         
cols = c(' ' = 'white'  , Thf = '#114400', Tdf = '#441100',
         wtf = '#005555', tef = '#00EE33', tdf = '#66DD00',
         bef = '#000088', bdf = '#330033',
         Ts  = '#AA5500', sw  = '#777922', tp = '#66DD88', 
         bp  = '#22EEFF', dg  = '#FF9922', hd = '#FEFF44', st = '#BB33FF', t = '#FFBAAA')
# Dark green -> light brown 
#Thf, Tdf, Ts, dg, hd
#          sw
#          tp
#          bp, st, t       


   
cols = c(' ' = 'white'  , Thf = '#002200', Tdf = '#338800',
         wtf = '#005555', tef = '#00EE33', tdf = '#66DD00',
         bef = '#000088', bdf = '#330033',
         Ts  = '#999900', sw  = '#BB9999', tp = '#8844FF', 
         bp  = '#880088', dg  = '#FFAA00', hd = '#FFFF77', st = '#00AAEE', t = '#99CCFF')
   