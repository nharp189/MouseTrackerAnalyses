### set working directory to MouseTrap Demo folder ###
setwd("/Users/nicholasharp/Documents/Desktop_PlzClean/MouseTrap_Demo/")

### install MouseTrap... This only needs to be done once. ###
### ... and readbulk for applying to a set of files ###
install.packages("mousetrap")
install.packages("readbulk")
install.packages("here")
install.packages("foreach")
install.packages("tidyverse")
install.packages("plyr")
### load the packages... This needs to be done every time. ###
library(mousetrap)
library(foreach)
library(tidyverse)
library(readbulk)
library(plyr)

### read in all the MT files from a directory ###
files <- list.files(path = "/Users/nicholasharp/Documents/Desktop_Plzclean/MouseTrap_Demo/raw_data", 
                    pattern = "*.mt", full.names = TRUE, recursive = FALSE)
group.data <- read_bulk("/Users/nicholasharp/Documents/Desktop_Plzclean/MouseTrap_Demo/raw_data", fun = read_mt, 
          extension = ".mt")
preprocessed.data<-mt_import_wide(group.data)
foreach(file = files) %do% {
  ### create "rate" variable (0 = Positive, 1 = Negative) ###
  preprocessed.data$rate <- ifelse(preprocessed.data$response == "POSITIVE", 0, 1)
  
  ### create "correct response" variable for clearly valenced faces (0 = Incorrect, 1 = Correct) ###
  preprocessed.data$correct <- ifelse(preprocessed.data$condition == "Angry",  
                                      ifelse(preprocessed.data$rate == 1, 1, 0), 
                                      ifelse(preprocessed.data$condition == "Happy", 
                                             ifelse(preprocessed.data$rate == 0, 1, 0), 
                                             ifelse(preprocessed.data$condition == "NEG", 
                                                    ifelse(preprocessed.data$rate == 1, 1, 0),
                                                    ifelse(preprocessed.data$condition == "POS", 
                                                           ifelse(preprocessed.data$rate == 0, 1, 0), NA))))
  
  ### Create column to average for each face ###
  preprocessed.data$ang_rate <- ifelse(preprocessed.data$condition == "Angry", preprocessed.data$rate, NA)
  preprocessed.data$hap_rate <- ifelse(preprocessed.data$condition == "Happy", preprocessed.data$rate, NA)
  preprocessed.data$sur_rate <- ifelse(preprocessed.data$condition == "Surprise", preprocessed.data$rate, NA)
  preprocessed.data$neg_rate <- ifelse(preprocessed.data$condition == "NEG", preprocessed.data$rate, NA)
  preprocessed.data$pos_rate <- ifelse(preprocessed.data$condition == "POS", preprocessed.data$rate, NA)
  preprocessed.data$amb_rate <- ifelse(preprocessed.data$condition == "AMBIG", preprocessed.data$rate, NA)
  
  ### Create column to average for each face in Block 1 ###
  preprocessed.data$ang_rate_b1 <- ifelse(preprocessed.data$order < 27,
                                          ifelse(preprocessed.data$condition == "Angry", preprocessed.data$rate, NA),NA)
  preprocessed.data$hap_rate_b1 <- ifelse(preprocessed.data$order < 27,
                                          ifelse(preprocessed.data$condition == "Happy", preprocessed.data$rate, NA),NA)
  preprocessed.data$sur_rate_b1 <- ifelse(preprocessed.data$order < 27,
                                          ifelse(preprocessed.data$condition == "Surprise", preprocessed.data$rate, NA),NA)
  preprocessed.data$neg_rate_b1 <- ifelse(preprocessed.data$order < 27,
                                          ifelse(preprocessed.data$condition == "NEG", preprocessed.data$rate, NA),NA)
  preprocessed.data$pos_rate_b1 <- ifelse(preprocessed.data$order < 27,
                                          ifelse(preprocessed.data$condition == "POS", preprocessed.data$rate, NA),NA)
  preprocessed.data$amb_rate_b1 <- ifelse(preprocessed.data$order < 27,
                                          ifelse(preprocessed.data$condition == "AMBIG", preprocessed.data$rate, NA),NA)
  
  ### Create column to average for each face in Block 2 ###
  preprocessed.data$ang_rate_b2 <- ifelse(preprocessed.data$order > 26 & preprocessed.data$order < 51,
                                          ifelse(preprocessed.data$condition == "Angry", preprocessed.data$rate, NA),NA)
  preprocessed.data$hap_rate_b2 <- ifelse(preprocessed.data$order > 26 & preprocessed.data$order < 51,
                                          ifelse(preprocessed.data$condition == "Happy", preprocessed.data$rate, NA),NA)
  preprocessed.data$sur_rate_b2 <- ifelse(preprocessed.data$order > 26 & preprocessed.data$order < 51,
                                          ifelse(preprocessed.data$condition == "Surprise", preprocessed.data$rate, NA),NA)
  preprocessed.data$neg_rate_b2 <- ifelse(preprocessed.data$order > 26 & preprocessed.data$order < 51,
                                          ifelse(preprocessed.data$condition == "NEG", preprocessed.data$rate, NA),NA)
  preprocessed.data$pos_rate_b2 <- ifelse(preprocessed.data$order > 26 & preprocessed.data$order < 51,
                                          ifelse(preprocessed.data$condition == "POS", preprocessed.data$rate, NA),NA)
  preprocessed.data$amb_rate_b2 <- ifelse(preprocessed.data$order > 26 & preprocessed.data$order < 51,
                                          ifelse(preprocessed.data$condition == "AMBIG", preprocessed.data$rate, NA),NA)
  
  ### Create column to average for each face in Block 3 ###
  preprocessed.data$ang_rate_b3 <- ifelse(preprocessed.data$order > 50 & preprocessed.data$order < 75,
                                          ifelse(preprocessed.data$condition == "Angry", preprocessed.data$rate, NA),NA)
  preprocessed.data$hap_rate_b3 <- ifelse(preprocessed.data$order > 50 & preprocessed.data$order < 75,
                                          ifelse(preprocessed.data$condition == "Happy", preprocessed.data$rate, NA),NA)
  preprocessed.data$sur_rate_b3 <- ifelse(preprocessed.data$order > 50 & preprocessed.data$order < 75,
                                          ifelse(preprocessed.data$condition == "Surprise", preprocessed.data$rate, NA),NA)
  preprocessed.data$neg_rate_b3 <- ifelse(preprocessed.data$order > 50 & preprocessed.data$order < 75,
                                          ifelse(preprocessed.data$condition == "NEG", preprocessed.data$rate, NA),NA)
  preprocessed.data$pos_rate_b3 <- ifelse(preprocessed.data$order > 50 & preprocessed.data$order < 75,
                                          ifelse(preprocessed.data$condition == "POS", preprocessed.data$rate, NA),NA)
  preprocessed.data$amb_rate_b3 <- ifelse(preprocessed.data$order > 50 & preprocessed.data$order < 75,
                                          ifelse(preprocessed.data$condition == "AMBIG", preprocessed.data$rate, NA),NA)
  
  ### Create column to average for each face in Block 4 ###
  preprocessed.data$ang_rate_b4 <- ifelse(preprocessed.data$order > 74,
                                          ifelse(preprocessed.data$condition == "Angry", preprocessed.data$rate, NA),NA)
  preprocessed.data$hap_rate_b4 <- ifelse(preprocessed.data$order > 74,
                                          ifelse(preprocessed.data$condition == "Happy", preprocessed.data$rate, NA),NA)
  preprocessed.data$sur_rate_b4 <- ifelse(preprocessed.data$order > 74,
                                          ifelse(preprocessed.data$condition == "Surprise", preprocessed.data$rate, NA),NA)
  preprocessed.data$neg_rate_b4 <- ifelse(preprocessed.data$order > 74,
                                          ifelse(preprocessed.data$condition == "NEG", preprocessed.data$rate, NA),NA)
  preprocessed.data$pos_rate_b4 <- ifelse(preprocessed.data$order > 74,
                                          ifelse(preprocessed.data$condition == "POS", preprocessed.data$rate, NA),NA)
  preprocessed.data$amb_rate_b4 <- ifelse(preprocessed.data$order > 74,
                                          ifelse(preprocessed.data$condition == "AMBIG", preprocessed.data$rate, NA),NA)
  
  ### Reaction times ###
  preprocessed.data$ang_RT <- ifelse(preprocessed.data$condition == "Angry", 
                                     ifelse(preprocessed.data$correct == 1, preprocessed.data$RT, NA), NA)
  preprocessed.data$hap_RT <- ifelse(preprocessed.data$condition == "Happy", 
                                     ifelse(preprocessed.data$correct == 1, preprocessed.data$RT, NA), NA)
  preprocessed.data$sur_p_RT <- ifelse(preprocessed.data$condition == "Surprise", 
                                       ifelse(preprocessed.data$rate == 0, preprocessed.data$RT, NA), NA)
  preprocessed.data$sur_n_RT <- ifelse(preprocessed.data$condition == "Surprise", 
                                       ifelse(preprocessed.data$rate == 1, preprocessed.data$RT, NA), NA)
  preprocessed.data$neg_RT <- ifelse(preprocessed.data$condition == "NEG", 
                                     ifelse(preprocessed.data$correct == 1, preprocessed.data$RT, NA), NA)
  preprocessed.data$pos_RT <- ifelse(preprocessed.data$condition == "POS", 
                                     ifelse(preprocessed.data$correct == 1, preprocessed.data$RT, NA), NA)
  preprocessed.data$amb_p_RT <- ifelse(preprocessed.data$condition == "AMBIG", 
                                       ifelse(preprocessed.data$rate == 0, preprocessed.data$RT, NA), NA)
  preprocessed.data$amb_n_RT <- ifelse(preprocessed.data$condition == "AMBIG", 
                                       ifelse(preprocessed.data$rate == 1, preprocessed.data$RT, NA), NA)
}

individual.averages <- preprocessed.data$data

ddply(individual.averages, "subjID", summarise, 
      ang_rate = mean(ang_rate, na.rm = TRUE),
      hap_rate = mean(hap_rate, na.rm = TRUE),
      sur_rate = mean(sur_rate, na.rm = TRUE),
      neg_rate = mean(neg_rate, na.rm = TRUE),
      pos_rate = mean(pos_rate, na.rm = TRUE),
      amb_rate = mean(amb_rate, na.rm = TRUE),
      ang_rate_b1 = mean(ang_rate_b1, na.rm = TRUE),
      hap_rate_b1 = mean(hap_rate_b1, na.rm = TRUE),
      sur_rate_b1 = mean(sur_rate_b1, na.rm = TRUE),
      neg_rate_b1 = mean(neg_rate_b1, na.rm = TRUE),
      pos_rate_b1 = mean(pos_rate_b1, na.rm = TRUE),
      amb_rate_b1 = mean(amb_rate_b1, na.rm = TRUE),
      ang_rate_b2 = mean(ang_rate_b2, na.rm = TRUE),
      hap_rate_b2 = mean(hap_rate_b2, na.rm = TRUE),
      sur_rate_b2 = mean(sur_rate_b2, na.rm = TRUE),
      neg_rate_b2 = mean(neg_rate_b2, na.rm = TRUE),
      pos_rate_b2 = mean(pos_rate_b2, na.rm = TRUE),
      amb_rate_b2 = mean(amb_rate_b2, na.rm = TRUE),
      ang_rate_b3 = mean(ang_rate_b3, na.rm = TRUE),
      hap_rate_b3 = mean(hap_rate_b3, na.rm = TRUE),
      sur_rate_b3 = mean(sur_rate_b3, na.rm = TRUE),
      neg_rate_b3 = mean(neg_rate_b3, na.rm = TRUE),
      pos_rate_b3 = mean(pos_rate_b3, na.rm = TRUE),
      amb_rate_b3 = mean(amb_rate_b3, na.rm = TRUE),
      ang_rate_b4 = mean(ang_rate_b4, na.rm = TRUE),
      hap_rate_b4 = mean(hap_rate_b4, na.rm = TRUE),
      sur_rate_b4 = mean(sur_rate_b4, na.rm = TRUE),
      neg_rate_b4 = mean(neg_rate_b4, na.rm = TRUE),
      pos_rate_b4 = mean(pos_rate_b4, na.rm = TRUE),
      amb_rate_b4 = mean(amb_rate_b4, na.rm = TRUE),
      ang_RT = mean(ang_RT, na.rm = TRUE),
      hap_RT = mean(hap_RT, na.rm = TRUE),
      sur_p_RT = mean(sur_p_RT, na.rm = TRUE),
      sur_n_RT = mean(sur_n_RT, na.rm = TRUE),
      neg_RT = mean(neg_RT, na.rm = TRUE),
      pos_RT = mean(pos_RT, na.rm = TRUE),
      amb_p_RT = mean(amb_p_RT, na.rm = TRUE),
      amb_n_RT = mean(amb_n_RT, na.rm = TRUE))

mt_heatmap(preprocessed.data)
