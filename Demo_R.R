### Set working directory to MouseTrap Demo folder ###
setwd("/Users/nicholasharp/Desktop/MouseTrap_Demo")

### Install MouseTrap... This only needs to be done once. ###
install.packages("mousetrap")

### Load the package... This needs to be done every time. ###
library(mousetrap)

### Read one .mt file ###
some.data <- read_mt("71001_181105_1303.mt")

### create "rate" variable (0 = Positive, 1 = Negative) ###
some.data$rate <- ifelse(some.data$response == "POSITIVE", 0, 1)

### create "correct response" variable for clearly valenced faces (0 = Incorrect, 1 = Correct) ###
some.data$correct <- ifelse(some.data$condition == "Angry",  
                            ifelse(some.data$rate == 1, 1, 0), 
                                   ifelse(some.data$condition == "Happy", 
                                          ifelse(some.data$rate == 0, 1, 0), 
                                              ifelse(some.data$condition == "NEG", 
                                                    ifelse(some.data$rate == 1, 1, 0),
                                                        ifelse(some.data$condition == "POS", 
                                                               ifelse(some.data$rate == 0, 1, 0), NA))))

### Create column to average for each face ###
some.data$ang_rate <- ifelse(some.data$condition == "Angry", some.data$rate, NA)
some.data$hap_rate <- ifelse(some.data$condition == "Happy", some.data$rate, NA)
some.data$sur_rate <- ifelse(some.data$condition == "Surprise", some.data$rate, NA)
some.data$neg_rate <- ifelse(some.data$condition == "NEG", some.data$rate, NA)
some.data$pos_rate <- ifelse(some.data$condition == "POS", some.data$rate, NA)
some.data$amb_rate <- ifelse(some.data$condition == "AMBIG", some.data$rate, NA)

### Reaction times ###
some.data$ang_RT <- ifelse(some.data$condition == "Angry", 
                 ifelse(some.data$correct == 1, some.data$RT, NA), NA)
some.data$hap_RT <- ifelse(some.data$condition == "Happy", 
                 ifelse(some.data$correct == 1, some.data$RT, NA), NA)
some.data$sur_p_RT <- ifelse(some.data$condition == "Surprise", 
                             ifelse(some.data$rate == 0, some.data$RT, NA), NA)
some.data$sur_n_RT <- ifelse(some.data$condition == "Surprise", 
                             ifelse(some.data$rate == 1, some.data$RT, NA), NA)
some.data$neg_RT <- ifelse(some.data$condition == "NEG", 
                           ifelse(some.data$correct == 1, some.data$RT, NA), NA)
some.data$pos_RT <- ifelse(some.data$condition == "POS", 
                           ifelse(some.data$correct == 1, some.data$RT, NA), NA)
some.data$amb_p_RT <- ifelse(some.data$condition == "AMBIG", 
                             ifelse(some.data$rate == 0, some.data$RT, NA), NA)
some.data$amb_n_RT <- ifelse(some.data$condition == "AMBIG", 
                             ifelse(some.data$rate == 1, some.data$RT, NA), NA)

### Send output to a file ###
sink('71001.csv')

### Create data frame of ratings and reaction times ###
ratings.data <- data.frame(matrix(c(some.data$ang_rate, some.data$hap_rate, some.data$sur_rate,
                                    some.data$neg_rate, some.data$pos_rate, some.data$amb_rate,
                                    some.data$ang_RT, some.data$hap_RT, some.data$sur_p_RT, 
                                    some.data$sur_n_RT, some.data$neg_RT, some.data$pos_RT, 
                                    some.data$amb_p_RT, some.data$amb_n_RT),
                                  nrow = 96, ncol = 14))
colnames(ratings.data) <- c("ang_rate", "hap_rate", "sur_rate", "neg_rate", "pos_rate", "amb_rate", "ang_RT",
                            "hap_RT", "sur_p_RT", "sur_n_RT", "neg_RT", "pos_RT", "amb_p_RT", "amb_n_RT")
colMeans(ratings.data, na.rm = TRUE)

### Stop sending output to file ###
sink()
