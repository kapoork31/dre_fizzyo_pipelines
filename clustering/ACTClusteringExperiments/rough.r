data  = xap.read_table('test_act_featurise_new')

plot(data$meanBreathAmplitude, data$meanBreathDuration)

library("ggpubr")
ggscatter(my_data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")
          
          
library(corrplot)
M <-cor(data$meanBreathAmplitude, data$meanBreathDuration)
corrplot(M, type="upper", order="hclust")

cor(data$meanBreathAmplitude, data$meanBreathDuration)
