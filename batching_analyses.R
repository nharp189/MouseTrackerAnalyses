### set working directory to MouseTrap Demo folder ###
setwd("/Users/nicholasharp/Desktop/MouseTrap_Demo")
source('~/Desktop/MouseTrap_Demo/processMT.R')
### install MouseTrap... This only needs to be done once. ###
### ... and readbulk for applying to a set of files ###
install.packages("mousetrap")
install.packages("readbulk")

### load the packages... This needs to be done every time. ###
library(mousetrap)
library(readbulk)

### read in all the MT files from a directory ###
files <- list.files("/Users/nicholasharp/Desktop/MouseTrap_Demo/raw_data", pattern = "*.mt", full.names = TRUE, recursive = FALSE)
group.data  <- lapply(files, FUN = read_mt)

processed.data <- lapply(group.data, FUN = source(processMT.R))

### Send output to a file ###
sink('group_data.csv')

### Create data frame of ratings and reaction times ###
ratings.data <- data.frame(matrix(c(group.data$subjID, group.data$ang_rate, group.data$hap_rate,
                                    group.data$sur_rate, group.data$neg_rate, group.data$pos_rate, 
                                    group.data$amb_rate, group.data$ang_RT, group.data$hap_RT, group.data$sur_p_RT, 
                                    group.data$sur_n_RT, group.data$neg_RT, group.data$pos_RT, group.data$amb_p_RT, 
                                    group.data$amb_n_RT),
                                  nrow = 384, ncol = 15))
colnames(ratings.data) <- c("id", "ang_rate", "hap_rate", "sur_rate", "neg_rate", "pos_rate", "amb_rate",
                            "ang_RT", "hap_RT", "sur_p_RT", "sur_n_RT", "neg_RT", "pos_RT", "amb_p_RT", "amb_n_RT")
colMeans(ratings.data, na.rm = TRUE)



