### set working directory to MouseTrap Demo folder ###
setwd("/Users/nicholasharp/Desktop/MouseTrap_Demo")

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
files <- list.files(path = "/Users/nicholasharp/Desktop/MouseTrap_Demo/raw_data", 
                    pattern = "*.mt", full.names = TRUE, recursive = FALSE)
group.data <- read_bulk("/Users/nicholasharp/Desktop/MouseTrap_Demo/raw_data", fun = read_mt, 
          extension = ".mt")
foreach(file = files) %do% {
  ### create "rate" variable (0 = Positive, 1 = Negative) ###
  group.data$rate <- ifelse(group.data$response == "POSITIVE", 0, 1)
  
  ### create "correct response" variable for clearly valenced faces (0 = Incorrect, 1 = Correct) ###
  group.data$correct <- ifelse(group.data$condition == "Angry",  
                               ifelse(group.data$rate == 1, 1, 0), 
                               ifelse(group.data$condition == "Happy", 
                                      ifelse(group.data$rate == 0, 1, 0), 
                                      ifelse(group.data$condition == "NEG", 
                                             ifelse(group.data$rate == 1, 1, 0),
                                             ifelse(group.data$condition == "POS", 
                                                    ifelse(group.data$rate == 0, 1, 0), NA))))
  
  ### Create column to average for each face ###
  group.data$ang_rate <- ifelse(group.data$condition == "Angry", group.data$rate, NA)
  group.data$hap_rate <- ifelse(group.data$condition == "Happy", group.data$rate, NA)
  group.data$sur_rate <- ifelse(group.data$condition == "Surprise", group.data$rate, NA)
  group.data$neg_rate <- ifelse(group.data$condition == "NEG", group.data$rate, NA)
  group.data$pos_rate <- ifelse(group.data$condition == "POS", group.data$rate, NA)
  group.data$amb_rate <- ifelse(group.data$condition == "AMBIG", group.data$rate, NA)
  
  ### Create column to average for each face in Block 1 ###
  group.data$ang_rate_b1 <- ifelse(group.data$order < 27,
                                   ifelse(group.data$condition == "Angry", group.data$rate, NA),NA)
  group.data$hap_rate_b1 <- ifelse(group.data$order < 27,
                                   ifelse(group.data$condition == "Happy", group.data$rate, NA),NA)
  group.data$sur_rate_b1 <- ifelse(group.data$order < 27,
                                   ifelse(group.data$condition == "Surprise", group.data$rate, NA),NA)
  group.data$neg_rate_b1 <- ifelse(group.data$order < 27,
                                   ifelse(group.data$condition == "NEG", group.data$rate, NA),NA)
  group.data$pos_rate_b1 <- ifelse(group.data$order < 27,
                                   ifelse(group.data$condition == "POS", group.data$rate, NA),NA)
  group.data$amb_rate_b1 <- ifelse(group.data$order < 27,
                                   ifelse(group.data$condition == "AMBIG", group.data$rate, NA),NA)
  
  ### Create column to average for each face in Block 2 ###
  group.data$ang_rate_b2 <- ifelse(group.data$order > 26 & group.data$order < 51,
                                   ifelse(group.data$condition == "Angry", group.data$rate, NA),NA)
  group.data$hap_rate_b2 <- ifelse(group.data$order > 26 & group.data$order < 51,
                                   ifelse(group.data$condition == "Happy", group.data$rate, NA),NA)
  group.data$sur_rate_b2 <- ifelse(group.data$order > 26 & group.data$order < 51,
                                   ifelse(group.data$condition == "Surprise", group.data$rate, NA),NA)
  group.data$neg_rate_b2 <- ifelse(group.data$order > 26 & group.data$order < 51,
                                   ifelse(group.data$condition == "NEG", group.data$rate, NA),NA)
  group.data$pos_rate_b2 <- ifelse(group.data$order > 26 & group.data$order < 51,
                                   ifelse(group.data$condition == "POS", group.data$rate, NA),NA)
  group.data$amb_rate_b2 <- ifelse(group.data$order > 26 & group.data$order < 51,
                                   ifelse(group.data$condition == "AMBIG", group.data$rate, NA),NA)
  
  ### Create column to average for each face in Block 3 ###
  group.data$ang_rate_b3 <- ifelse(group.data$order > 50 & group.data$order < 75,
                                   ifelse(group.data$condition == "Angry", group.data$rate, NA),NA)
  group.data$hap_rate_b3 <- ifelse(group.data$order > 50 & group.data$order < 75,
                                   ifelse(group.data$condition == "Happy", group.data$rate, NA),NA)
  group.data$sur_rate_b3 <- ifelse(group.data$order > 50 & group.data$order < 75,
                                   ifelse(group.data$condition == "Surprise", group.data$rate, NA),NA)
  group.data$neg_rate_b3 <- ifelse(group.data$order > 50 & group.data$order < 75,
                                   ifelse(group.data$condition == "NEG", group.data$rate, NA),NA)
  group.data$pos_rate_b3 <- ifelse(group.data$order > 50 & group.data$order < 75,
                                   ifelse(group.data$condition == "POS", group.data$rate, NA),NA)
  group.data$amb_rate_b3 <- ifelse(group.data$order > 50 & group.data$order < 75,
                                   ifelse(group.data$condition == "AMBIG", group.data$rate, NA),NA)
  
  ### Create column to average for each face in Block 4 ###
  group.data$ang_rate_b4 <- ifelse(group.data$order > 74,
                                   ifelse(group.data$condition == "Angry", group.data$rate, NA),NA)
  group.data$hap_rate_b4 <- ifelse(group.data$order > 74,
                                   ifelse(group.data$condition == "Happy", group.data$rate, NA),NA)
  group.data$sur_rate_b4 <- ifelse(group.data$order > 74,
                                   ifelse(group.data$condition == "Surprise", group.data$rate, NA),NA)
  group.data$neg_rate_b4 <- ifelse(group.data$order > 74,
                                   ifelse(group.data$condition == "NEG", group.data$rate, NA),NA)
  group.data$pos_rate_b4 <- ifelse(group.data$order > 74,
                                   ifelse(group.data$condition == "POS", group.data$rate, NA),NA)
  group.data$amb_rate_b4 <- ifelse(group.data$order > 74,
                                   ifelse(group.data$condition == "AMBIG", group.data$rate, NA),NA)
  
  ### Reaction times ###
  group.data$ang_RT <- ifelse(group.data$condition == "Angry", 
                              ifelse(group.data$correct == 1, group.data$RT, NA), NA)
  group.data$hap_RT <- ifelse(group.data$condition == "Happy", 
                              ifelse(group.data$correct == 1, group.data$RT, NA), NA)
  group.data$sur_p_RT <- ifelse(group.data$condition == "Surprise", 
                                ifelse(group.data$rate == 0, group.data$RT, NA), NA)
  group.data$sur_n_RT <- ifelse(group.data$condition == "Surprise", 
                                ifelse(group.data$rate == 1, group.data$RT, NA), NA)
  group.data$neg_RT <- ifelse(group.data$condition == "NEG", 
                              ifelse(group.data$correct == 1, group.data$RT, NA), NA)
  group.data$pos_RT <- ifelse(group.data$condition == "POS", 
                              ifelse(group.data$correct == 1, group.data$RT, NA), NA)
  group.data$amb_p_RT <- ifelse(group.data$condition == "AMBIG", 
                                ifelse(group.data$rate == 0, group.data$RT, NA), NA)
  group.data$amb_n_RT <- ifelse(group.data$condition == "AMBIG", 
                                ifelse(group.data$rate == 1, group.data$RT, NA), NA)
}

individual.averages <- group.data

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


