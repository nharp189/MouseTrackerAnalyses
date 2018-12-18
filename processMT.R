### get subjID ###
id <- print(group.data$subjID)

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
### Send output to a file ###
sink('*.csv')

### Create data frame of ratings and reaction times ###
ratings.data <- data.frame(matrix(c(group.data$ang_rate, group.data$hap_rate, group.data$sur_rate,
                                    group.data$neg_rate, group.data$pos_rate, group.data$amb_rate,
                                    group.data$ang_RT, group.data$hap_RT, group.data$sur_p_RT, 
                                    group.data$sur_n_RT, group.data$neg_RT, group.data$pos_RT, 
                                    group.data$amb_p_RT, group.data$amb_n_RT),
                                  nrow = 96, ncol = 14))
colnames(ratings.data) <- c("ang_rate", "hap_rate", "sur_rate", "neg_rate", "pos_rate", "amb_rate", "ang_RT",
                            "hap_RT", "sur_p_RT", "sur_n_RT", "neg_RT", "pos_RT", "amb_p_RT", "amb_n_RT")
colMeans(ratings.data, na.rm = TRUE)

### Stop sending output to file ###
sink()

