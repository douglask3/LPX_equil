source("cfg.r")

files = list.files('data/africa_files/all_precip', full.names = TRUE)

MakeOutputSeason <- function(file) {
    out = PolarConcentrationAndPhase(brick(file), phase_units = 'months')
    fname = paste0("outputs/Season/", filename.noPath(file, TRUE), "-", c("phase", "conc"))
    writeRaster(out[[1]], fname[[1]])
    writeRaster(out[[2]], fname[[2]])
}

lapply(files, MakeOutputSeason)