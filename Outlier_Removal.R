################################################################## GROWTH CURVES FOR WHOLE PLATE #################################################################################

library(ggplot2); require(plyr); require(dplyr); require(splines); library(parallel)

cl <- makeCluster(parallel::detectCores()-2)

source("\\\\imsbnas.d.ethz.ch\\sauer1\\users\\Laurentz\\Mauro_to_Laurentz\\Functions-Import-Plate\\CellCultureAnalysis_4.0.R")

setwd("\\\\imsbnas.d.ethz.ch\\sauer1\\users\\Laurentz\\Mauro_to_Laurentz\\190310_Exometabolites_test\\20190403_CombDrugsExo")

data_384 <- ReadIncuCyteData(FileDirectory = getwd(),
                             read_platemap = T,
                             Number_data_levels = 5, 
                             FileName_IncuCyte = "Exo_Combined_NCIH460.txt", 
                             FileName = "PlateMaps_tecan_384_NCIH460.xlsx",
                             Plate_size = 384)

data_384$Format <- factor(384)

data_384 <- rbind(data_384)

attach(data_384)
new_data_384 <- data_384[ which(Time=='24'),]
detach(data_384)

#Median_all_wells_time24 <- median(new_data_384$Conf, na.rm = TRUE)

#summary(new_data_384$Conf)

#require(rcompanion)

#library(rcompanion)

#Pre_Filter <- groupwiseMedian(Conf ~ 1,
#               data       = new_data_384, 
#              conf       = 0.99, 
#             R          = 5000,
#            percentile = TRUE, 
#           bca        = FALSE,
#          basic      = FALSE,
#         normal     = FALSE,
#        wilcox     = FALSE,
#       digits     = 3)

# remove +/- 10% from median/mean

Mean_all_wells_time24 <- mean(new_data_384$Conf, na.rm = TRUE)


library(rcompanion)

Pre_Filter_Mean <- groupwiseMean(Conf ~ 1, 
                                 data   = new_data_384, 
                                 conf   = 0.99, 
                                 digits = 3)
mean(new_data_384$Conf)                
max(new_data_384$Conf)                
min(new_data_384$Conf)   

# Variance seen in cells at 384 well plate. Then use this variance and a forward loop to remove wells until this variance is reached
# Variance might be cell line dependent

variance <- var(new_data_384$Conf)

summary(new_data_384$Conf)


par(mfrow = c(1,1))
hist(new_data_384$Conf)
sd <- sd(new_data_384$Conf)*1.0                

subset_data_384 <- subset(data_384, data_384[ which(Time=='24'),]$Conf > (Mean_all_wells_time24-sd) & data_384[ which(Time=='24'),]$Conf < (Mean_all_wells_time24+sd))

data <- rbind(subset_data_384)
plot_multi_well(input_df = subset_data_384)
plot_multi_well(input_df = subset_data_384)
View(plot_multi_well)
View(plot_multi_well)
plot_multi_well(input_df = subset_data_384, max_timepoint <= 48)


plot_multi_well(input_df = subset_data_384)
plot_multi_well(input_df = subset_data_384[data_384$Time <= 48,])
View(plot_multi_well)