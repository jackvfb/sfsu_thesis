
# SETUP -------------------------------------------------------------------

source("code/setup.R")


# DRIFT STATS -------------------------------------------------------------

# table shows number of detections and events in each drift db
drift.stats <- drift.ec %>%
  nest(data=-db) %>%
  mutate(tot_det=map_dbl(data, ~length(unique(.$UID))),
         tot_ev=map_dbl(data, ~length(unique(.$eventId)))) %>%
  select(-data)

# GET PREDICTIONS ---------------------------------------------------------

bant <- export_banter(bindStudies(train), dropVars = c("dBPP", "Click_Detector_101_ici", "noiseLevel"))
bant <- split_calls(bant)
bant <- NBHFbanter(bant, 1000, 0.5, 1000, 0.5)

predictions <- predict(bant, drift.bant)$predict.df

locations <- drift.ec %>%
  group_by(eventId) %>%
  summarize(Latitude=median(Latitude),
            Longitude=median(Longitude),
            UTC=median(UTC))

drifts <- inner_join(locations, predictions, by=join_by("eventId"=="event.id")) %>%
  mutate(db=str_extract(eventId, "\\w+_\\d+"))


# SUMMARY FIGS -------------------------------------------------------------

# Bars are drifts
# Heights showing number of events, colored by predicted species
# Arranged in order of median Latitude, S -> N from left to right
drifts %>%
  group_by(db) %>%
  mutate(db_lat = round(median(Latitude), 2)) %>% 
  ggplot() +
  geom_bar(aes(x=fct_reorder(db, db_lat),
               fill=predicted)) +
  theme(axis.text.x = element_text(angle=45))


# DRIFT STATS POST PREDICTION -----------------------------------------------------------

# Give the proportion of each class's predictions in each drift
ex <- expand(drift.ec, db,
       predicted = c("pd", "pp", "ks"))
  

drift.stats.2 <- inner_join(predictions, drift.ec, by=join_by("event.id"=="eventId")) %>% 
  group_by(db, predicted) %>%
  summarize(n_ev = length(unique(event.id))) %>%
  right_join(ex, by=c("db", "predicted")) %>%
  replace_na(list(n_ev = 0)) %>%
  inner_join(select(drift.stats, db, tot_ev), by="db") %>%
  mutate(prop = n_ev/tot_ev)

# summary of range of prop for each class
drift.stats.2 %>%
  nest(data=-predicted) %>%
  mutate(q25=map_dbl(data, \(x) quantile(x$prop, 0.25)),
         q75=map_dbl(data, \(x) quantile(x$prop, 0.75)),
         min=map_dbl(data, \(x) min(x$prop)),
         max=map_dbl(data, \(x) max(x$prop)),
         median=map_dbl(data, \(x) median(x$prop)))

# total proportion across entire survey for each class
nrow(predictions[predictions$predicted=="ks",])/nrow(predictions)
nrow(predictions[predictions$predicted=="pd",])/nrow(predictions)
nrow(predictions[predictions$predicted=="pp",])/nrow(predictions)

# MAKE MAPS ---------------------------------------------------------------

myMap <- function(limits) {
  
  bmap <- basemap(limits = limits,
                  bathymetry = TRUE,
                  bathy.style = "rbg")
  
  bmap + geom_spatial_point(data = drifts,
                            aes(x = Longitude,
                                y = Latitude,
                                color=predicted))
}

myMap(c(-125, -124, 43, 46)) # hum

myMap(c(-125.5, -124, 40.5, 41.5)) # mendo

myMap(c(-124, -122, 37, 38.5)) # sf

myMap(c(-123, -121, 35, 36)) # mby

# GET DEPTHS --------------------------------------------------------------

env <- drifts %>%
  select(Latitude, Longitude, UTC) %>% 
  matchEnvData(nc="erdSrtm30plusSeafloorGradient", var="sea_floor_depth")

drifts_env <- drifts %>%
  inner_join(env, by=c("Latitude", "Longitude", "UTC"))

# Plot showing that harbor porpoise has the greatest proportion of events in shallow
# water < 200 m
drifts_env %>%
  ggplot()+
  geom_bar(aes(x=predicted, fill=cut(sea_floor_depth_mean,
                                     breaks = c(0, -200, min(sea_floor_depth_mean)))),
           position = "fill")
