########################################################################## GR50 CALCULATION - A549 CELLS #################################################################################

# BASED ON: https://bioconductor.org/packages/release/bioc/vignettes/GRmetrics/inst/doc/GRmetrics-vignette.html

###################################################################### SETWD / LOAD LIBRARIES / IMPORT DATA #################################################################################

# SETWD
setwd("\\\\imsbnas.d.ethz.ch\\sauer1\\users\\Laurentz\\Mauro_to_Laurentz\\190310_Exometabolites_test\\20190323_GI50")

source("\\\\imsbnas.d.ethz.ch\\sauer1\\users\\Laurentz\\Mauro_to_Laurentz\\Functions-Import-Plate\\CellCultureAnalysis_4.0.R")

# LIBRARIES
library(ggplot2)
library(gridExtra)
library(dplyr)
library(plyr)
library(tidyr)
library(naniar)

# IMPORT DATA (ALL THREE CELL LINES - A549/HCT15/NCIH460)
data_A549 <- ReadIncuCyteData(FileDirectory = getwd(),
                              read_platemap = T,
                              Number_data_levels = 4, 
                              FileName_IncuCyte = "Exo_GI50_A549.txt", 
                              FileName = "PlateMaps_tecan_A549.xlsx",
                              Plate_size = 96)

############################################################################### ORGANIZE DATA #################################################################################

############# ASSIGN CORRECT COLOMN NAME ALTERNATIVE: #colnames(data_GR)[1:4] <- c("time", "cell_count", "agent", "concentration")
# confluence
colnames(data_A549)[colnames(data_A549)=="Conf"] <- "cell_count"

# cell line
colnames(data_A549)[colnames(data_A549)=="X2"] <- "cell_line"

# drug
colnames(data_A549)[colnames(data_A549)=="X3"] <- "agent"

# concentration
colnames(data_A549)[colnames(data_A549)=="X4"] <- "concentration"

# control
colnames(data_A549)[colnames(data_A549)=="X5"] <- "control"

# control
colnames(data_A549)[colnames(data_A549)=="Time"] <- "time"

data_A549_2.0 <- data.frame(data_A549$`cell_line`, data_A549$`agent`, data_A549$`time`, data_A549$`concentration`,
                           data_A549$`cell_count`, data_A549$`control`)

###### CHECK THIS!!!!!!!!!! data_A549_2.0$concentration <- as.numeric(data_A549_2.0$concentration) # make sure concentration is numeric!!

colnames(data_A549_2.0)[1:6] <- c("cell_line", "agent", "time", "concentration", "cell_count", "control")

############# SET UP "conf" AND "cell_count_ctrl" and "cell_count_time0" as colomns
data_A549_agents <- subset(data_A549_2.0, data_A549_2.0$agent == c("PEM", "BPTES", "MTX"))

############# CALCULATE conf_ctrl AND conf_time0 (total_data)
data_A549_agents$cell_count__ctrl <- NA

############# create cell_count__ctrl
for(along_time in unique(data_A549_agents$time)){  
  data_A549_agents[data_A549_agents$time == along_time,"cell_count__ctrl"] = median(subset(data_A549, time == along_time & agent == "DMSO")[["cell_count"]])
}

rm(along_time)

data_A549_agents$cell_count__time0 <- subset(data_A549_agents, time == min(data_A549_agents$time))[["cell_count__ctrl"]][1]

############# ADD REPLICATES
data_A549_agents <-
  data_A549_agents %>% group_by(time, agent, concentration) %>%
  mutate(replicate = seq(1,2,1))

############################################################################ GR50 CALCULATION - A549 CELLS #################################################################################

if (!requireNamespace("BiocManager", quietly=TRUE))
  install.packages("BiocManager")
BiocManager::install("GRmetrics")

install.packages("BiocManager")
BiocManager::install("GRmetrics")

library(GRmetrics)
# data(data_A549_agents) #### is this needed?

# data(data_A549_agents) #### is this needed?
head(data_A549_agents)

data_A549_agents$concentration <- as.numeric(as.character(data_A549_agents$concentration))

drc_output = GRfit(data_A549_agents, groupingVariables = c('cell_line','agent'))










