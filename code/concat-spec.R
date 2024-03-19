library(identidrift)
library(PAMpal)

classPlot <- function(species, ylim = c(-25, 0)) {
  calculateAverageSpectra(train[[species]],
                          evNum=1:length(events(train[[species]])),
                          sort=TRUE, showBreaks = FALSE, title="",
                          filterfrom_khz = 100, filterto_khz = 160,
                          ylim = ylim, flim=c(100000,160000),
                          norm=TRUE)
}

par(mfrow=c(3,2))
classPlot("ks")
classPlot("pd")
classPlot("pp")

par(mfrow=c(1,1))
