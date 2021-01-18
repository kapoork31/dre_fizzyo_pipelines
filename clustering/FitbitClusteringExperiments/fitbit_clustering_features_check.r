install.packages("corrplot")
library(corrplot)

data = xap.read_table("fitbit_featurise_table_2020_08")

drops <- c("date","userid",'startWindow','endWindow','stepCountQ2','stepCountQ3','stepCountQ4','stepCountQ234',
'activeMinsSteps2','activeMinsSteps5','activeMinsSteps10','activeMinsSteps20','CoefficientOfVariationStepQ1','CoefficientOfVariationStepQ2',
'CoefficientOfVariationStepQ3','CoefficientOfVariationStepQ4','CoefficientOfVariationStepQ234','restingHrProxy',
'CoefficientOfVariationHrQ1','CoefficientOfVariationHrQ2','CoefficientOfVariationHrQ3','CoefficientOfVariationHrQ4','CoefficientOfVariationHrQ234',
'minsWearTotal','minsWear623','minsWear1121','minsWearQ1','minsWearQ2','minsWearQ3','minsWearQ4','wearPercent',
'wearQ1Percent','wearQ2Percent','wearQ3Percent','wearPercentQ4','wearQ234Percent','gapPercent','gapPercentQ1','gapPercentQ2','gapPercentQ3','gapPercentQ4',
'stepCountNorm','stepCountNormQ2','stepCountNormQ3','stepCountNormQ4','activeMinsHrStepsThresh2','activeMinsHrStepsThresh3','activeMinsHrStepsThresh5',
'activeMinsHrStepsThresh10','activeMinsHrStepsThresh20','neighbour15Prev2','neighbour15Prev3','neighbour15Prev5','neighbour15Prev10','neighbour15Prev20')
data = data[ , !(names(data) %in% drops)]


#data_nona = na.omit(data)

m = cor(data)
corrplot(m)
