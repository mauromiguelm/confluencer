################################################################## GROWTH CURVES FOR WHOLE PLATE #################################################################################

library(ggplot2); require(plyr); require(dplyr); require(splines); library(parallel)

source("\\\\imsbnas.d.ethz.ch\\sauer1\\users\\Laurentz\\Mauro_to_Laurentz\\Functions-Import-Plate\\CellCultureAnalysis_4.0.R")

setwd("\\\\imsbnas.d.ethz.ch\\sauer1\\users\\Laurentz\\Mauro_to_Laurentz\\190310_Exometabolites_test\\20190403_CombDrugsExo")

data_384 <- ReadIncuCyteData(FileDirectory = getwd(),
                             read_platemap = T,
                             Number_data_levels = 5, 
                             FileName_IncuCyte = "Exo_Combined_NCIH460.txt", 
                             FileName = "PlateMaps_tecan_384_NCIH460.xlsx",
                             Plate_size = 384)


# Creating a function that removes outliers -------------------------------

filter_growth_outliers <- function(data){
  
  #data format is a long dataframe
  # the filtering will remove the mean +/- 1 * sd
  
    if(!require(rcompanion)){
      
      stop("rcompanion not installed")
      
    }
  
    data_384 = data
    
    # Bellow are two easier ways to subset data, without having to attach variables to environment, both will create the same results
    
    # attach(data_384)
    # new_data_384 <- data_384[ which(Time=='24'),]
    # detach(data_384)
    
    
    new_data_384 <- data_384[data_384$Time == 24,] #this uses R indexing of matrices, where [row,col], and by using "data_384$Time == 24", you get the right number of rows
    
    new_data_384 <- subset(data_384, Time == 24) #this uses the subset function
    
    
    Mean_all_wells_time24 <- mean(new_data_384$Conf, na.rm = TRUE)
    
    Pre_Filter_Mean <- groupwiseMean(Conf ~ 1, 
                                     data   = new_data_384, 
                                     conf   = 0.99, 
                                     digits = 3)
    
    # Variance seen in cells at 384 well plate. Then use this variance and a forward loop to remove wells until this variance is reached
    # Variance might be cell line dependent
    
    variance <- var(new_data_384$Conf)
    
    subset_data_384 <- subset(data_384, data_384[ data_384$Time=='24',]$Conf > (Mean_all_wells_time24-sd) & data_384[ data_384$Time=='24',]$Conf < (Mean_all_wells_time24+sd))
    
    data <- subset_data_384
    
    
    return_data <- list(data = data, summary_stats = Pre_Filter_Mean, variance = variance)
    
    # a function in R can only return a single object.
    # if you omit return, the function will return the last evaluated expression
    # it is recommended to include return so that we clearly know what is the output of a function
    # return() can only have one output, hence I created a list with several output within that lsit.
    
    return(return_data)
}

# Plotting results of function --------------------------------------------

subset_data_384 <- filter_growth_outliers(data_384)


plot_multi_well(input_df = subset_data_384$data[subset_data_384$data$Time <24,])

plot_multi_well(input_df = subset_data_384$data[subset_data_384$data$Time <72,])