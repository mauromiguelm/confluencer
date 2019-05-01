################################################################################## GR50 CALCULATION #################################################################################

# BASED ON: https://bioconductor.org/packages/release/bioc/vignettes/GRmetrics/inst/doc/GRmetrics-vignette.html

############################################## SETWD / LOAD LIBRARIES / IMPORT DATA #################################################################################

# SETWD
setwd("\\\\imsbnas.d.ethz.ch\\sauer1\\users\\Laurentz\\Mauro_to_Laurentz\\190310_Exometabolites_test\\20190323_GI50")

source("\\\\imsbnas.d.ethz.ch\\sauer1\\users\\Laurentz\\Mauro_to_Laurentz\\Functions-Import-Plate\\CellCultureAnalysis_4.0.R")

# LIBRARIES
library(ggplot2)
library(gridExtra)
library(dplyr)
library(plyr)
library(tidyr)

# IMPORT DATA (ALL THREE CELL LINES - A549/HCT15/NCIH460)
data_A549 <- ReadIncuCyteData(FileDirectory = getwd(),
                              read_platemap = T,
                              Number_data_levels = 4, 
                              FileName_IncuCyte = "Exo_GI50_A549.txt", 
                              FileName = "PlateMaps_tecan_A549.xlsx",
                              Plate_size = 96)

data_HCT15 <- ReadIncuCyteData(FileDirectory = getwd(),
                               read_platemap = T,
                               Number_data_levels = 4, 
                               FileName_IncuCyte = "Exo_GI50_HCT15.txt", 
                               FileName = "PlateMaps_tecan_HCT15.xlsx",
                               Plate_size = 96)

data_NCIH460 <- ReadIncuCyteData(FileDirectory = getwd(),
                                 read_platemap = T,
                                 Number_data_levels = 4, 
                                 FileName_IncuCyte = "Exo_GI50_NCIH460.txt", 
                                 FileName = "PlateMaps_tecan_NCIH460.xlsx",
                                 Plate_size = 96)

# MERGE DATA (ALL THREE CELL LINES - A549/HCT15/NCIH460)
total_data <- rbind(data_A549, data_HCT15, data_NCIH460)

########################################################### ORGANIZE DATA #################################################################################

############# ASSIGN CORRECT COLOMN NAME ALTERNATIVE: #colnames(data_GR)[1:4] <- c("time", "cell_count", "agent", "concentration")
# confluence
colnames(total_data)[colnames(total_data)=="Conf"] <- "cell_count"

# cell line
colnames(total_data)[colnames(total_data)=="X2"] <- "cell_line"

# drug
colnames(total_data)[colnames(total_data)=="X3"] <- "agent"

# concentration
colnames(total_data)[colnames(total_data)=="X4"] <- "concentration"

# control
colnames(total_data)[colnames(total_data)=="X5"] <- "control"

total_data_2 <- data.frame(total_data$`cell_line`, total_data$`agent`, total_data$`time`, total_data$`concentration`,
                           total_data$`cell_count`, total_data$`control`)

mydata <- total_data_2

# confluence
colnames(mydata)[colnames(mydata)=="total_data.cell_count"] <- "cell_count"

# confluence
colnames(mydata)[colnames(mydata)=="total_data.Time"] <- "time"

# cell line
colnames(mydata)[colnames(mydata)=="total_data..cell.line."] <- "cell_line"

# drug
colnames(mydata)[colnames(mydata)=="total_data.agent"] <- "agent"

# concentration
colnames(mydata)[colnames(mydata)=="total_data.concentration"] <- "concentration"

# control
colnames(mydata)[colnames(mydata)=="total_data.control"] <- "control"


############# SET UP "conf" AND "conf_ctrl" and "conf_time0" as colomns
mydata_agents <- subset(mydata, mydata$agent == c("PEM", "BPTES", "MTX"))

############# CALCULATE conf_ctrl AND conf_time0 (total_data)
mydata_agents$cell_count__ctrl <- NA

############# create cell_count__ctrl -> do this for one cell line after the other!!!
for(along_time in unique(mydata_agents$time)){  
  mydata_agents[mydata_agents$time == along_time,"cell_count__ctrl"] = median(subset(total_data, Time == along_time & agent == "DMSO")[["confluence"]])
}

rm(along_time)

############# CALCULATE conf_time0
mydata_agents$cell_count__time0 <- subset(mydata_agents, time == min(mydata_agents$time))[["cell_count__ctrl"]][1]

############# ADD REPLICATES
mydata_agents <-
  mydata_agents %>% group_by(time, agent, concentration) %>%
  mutate(replicate = seq(1,2,1))

############# CONVERT concentration FROM CHARACTER TO NUMERIC

mydata_agents$concentration <- as.numeric(as.character(mydata_agents$concentration))

############################################################################ GR50 CALCULATION - A549 CELLS #################################################################################

if (!requireNamespace("BiocManager", quietly=TRUE))
  install.packages("BiocManager")
BiocManager::install("GRmetrics")

install.packages("BiocManager")
BiocManager::install("GRmetrics")

library(GRmetrics)
# data(mydata_agents) #### is this needed?

# data(mydata_agents) #### is this needed?
head(mydata_agents)
tail(mydata_agents)
str(mydata_agents)

drc_output = GRfit(mydata_agents, groupingVariables = c('cell_line','agent'))










