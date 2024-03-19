# SETUP -------------------------------------------------------------------

source("code/setup.R")

# SAMPLE & TRANSFORM------------------------------------------------------------------

samp <- train.ec %>%
  # drop metadata
  select(-c(UID:noiseLevel, BinaryFile, eventLabel,detectorName, db)) %>%
  # drop variables to avoid creating artifacts in the cluster plot.
  select(species, eventId, duration:peak, Q_10dB:centerkHz_3dB)

samp_log <- samp %>%
  # perform logarithmic transform for non-normally distributed variables
  mutate(log_duration = log(duration), log_Q_3dB = log(Q_3dB), log_Q_10dB = log(Q_10dB), .keep="unused")


# CLUSTER -----------------------------------------------------------------

# calculate Euclidean distances
dist <- samp_log %>%
  select(-c(species, eventId)) %>%
  mutate(id = 1:n()) %>%
  column_to_rownames("id") %>%
  scale() %>%
  dist(method="euclidean")

# set rho and delta values
# 25, 2 is pretty good
# 20, 2 produced 5 clusters
# 20, 1.5 produced 6 clusters
set.seed(123)
train.clust <- densityClust(dist)
train.clust <- findClusters(train.clust, rho=25, delta=2)
#plotDensityClust(train.clust)
#table(samp_log$species, train.clust$clusters)


# TABLE WITH PROPORTION OF EACH CLASS IN EACH CLUSTER ---------------------

samp_clust <- data.frame(samp, clust = train.clust$clusters)
saveRDS(samp_clust, file = "samp_clust.rds")

samp_clust %>%
  count(species, clust) %>%
  mutate(tot = sum(n), .by=species) %>%
  mutate(prop = round(n/tot, 2)) %>%
  select(species, clust, prop) %>%
  pivot_wider(names_from = clust, values_from = prop) %>%
  rename("Cluster 1" = `1`,
         "Cluster 2" = `2`,
         "Cluster 3" = `3`,) %>%
  mutate(species = case_match(species,
                              "ks" ~ "*Kogia* spp.",
                              "pd" ~ "Dall's porpoise",
                              "pp"~ "harbor porpoise")
  )


# LOOK AT HOW MANY CLUSTERS IN EACH EVENT ---------------------------------

samp_clust %>%
  nest(data = -c(eventId, species)) %>%
  mutate(n_clust=map_dbl(data, \(x) length(unique(x$clust)))) %>%
  ggplot()+
  geom_bar(aes(x=species, fill=factor(n_clust)), position="fill")

# RF WITH CLUST VS SPECIES AS RESPONSE VAR------------------------------------------

# Cluster as response variable

rf_data <- samp_clust %>%
  select(-c(species, eventId)) %>%
  mutate(clust = as.factor(clust)) %>%
  as.data.frame()

ss <- balancedSampsize(rf_data$clust)

rf <- randomForest(formula = clust ~ .,
                   data = rf_data,
                   sampsize = ss,
                   proximity = TRUE,
                   importance = TRUE)

confusionMatrix(rf)
plotImportance(rf, plot.type="heatmap")
freqsaveRDS(rf, "models/callrf_cl.rds")

# Species as response variable

rf_data.sp <- samp_clust %>%
  select(-c(clust, eventId)) %>%
  mutate(species = as.factor(species)) %>%
  as.data.frame()

rf.sp <- randomForest(formula = species ~ .,
                   data = rf_data.sp,
                   sampsize = rep(35, 3), # don't change this from last model for better comparison
                   proximity = TRUE,
                   importance = TRUE)

confusionMatrix(rf.sp)
plotImportance(rf.sp, plot.type="heatmap")
saveRDS(rf.sp, "models/callrf_sp.rds")


# MEAN FEATURES OF CLUSTERS -----------------------------------------------

# varsWanted <- c("BW_3dB", "BW_10dB", "centerkHz_3dB", "centerkHz_10dB", "log_duration",
#                 "fmax_10dB", "fmax_3dB", "fmin_3dB", "fmin_10dB", "peak", "log_Q_3dB",
#                 "log_Q_10dB", "peakTime")
#
# samp_rm %>%
#   select(clust, all_of(varsWanted)) %>%
#   pivot_longer(cols=-clust, names_to = "var") %>%
#   group_by(clust, var) %>%
#   summarize(mean=mean(value), sd=sd(value), median=median(value),
#             min=min(value), max=max(value)) %>%
#   mutate_if(is.numeric, round, 2) %>%
#   nest(data=-var) %>%
#   mutate(data=map(data, \(x) as.matrix(column_to_rownames(x, var="clust")))) %>%
#   pull(data, name = "var")
