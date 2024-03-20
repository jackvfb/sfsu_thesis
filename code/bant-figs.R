library(banter)
library(identidrift)
library(randomForest)
library(rfPermute)
library(tidyverse)
library(PAMpal)

bant1 <- export_banter(bindStudies(train), dropVars = c("dBPP", "Click_Detector_101_ici", "noiseLevel"))
# bant <- split_calls(bant)
bant1 <- NBHFbanter(bant1, 1000, 0.5, 10000, 0.5)

bant1det <- getBanterModel(bant1, model="Click_Detector_101")
plotImportance(bant1det, plot.type="heatmap")
ggsave("detrf-heatmap.jpg")

confusionMatrix(bant1det)
getSampSize(bant1, model="Click_Detector_101")
modelPctCorrect(bant1)

# ---

bant <- export_banter(bindStudies(train), dropVars = c("dBPP", "Click_Detector_101_ici", "noiseLevel"))
bant <- split_calls(bant)
bant <- NBHFbanter(bant, 1000, 0.5, 10000, 0.5)
saveRDS(bant, "models/bant.rds")

modelPctCorrect(bant)
propCalls(bant)
classPriors(getBanterModel(bant), sampsize=c(8,8,8))

plotImportance(getBanterModel(bant), plot.type="heatmap")
plotVotes(getBanterModel(bant))

lapply(c("lorange", "midrange", "hirange"),
       \(x) getSampSize(bant, model = x))

