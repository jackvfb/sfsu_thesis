
# DRIFT MAPS FOR FIGURE 1 ------------------------------------------------

library(ggOceanMaps)
library(ggspatial)

figdata <- list_rbind(drift.gps)
bmap <- basemap(figdata, bathy.style = "rgg")

bmap + geom_spatial_path(data=figdata, aes(x=Longitude,
                                           y=Latitude,
                                           color=as_factor(month(UTC)),
                                           group=DriftName)) +
  scale_color_discrete(name="Month",
                      labels=c("March", "April", "May"))

ggsave("figs/pam-map.jpg", scale=4, width = 4, units = "in")

                         