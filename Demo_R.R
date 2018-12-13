### Set working directory to MouseTrap Demo folder ###
setwd("/Users/nicholasharp/Desktop/MouseTrap_Demo")

### Install MouseTrap... This only needs to be done once. ###
install.packages("mousetrap")

### Load the package... This needs to be done every time. ###
library(mousetrap)

### Read one .mt file ###
some.data <- read_mt("71001_181105_1303.mt")

### Create "Rate" variable (0 = Positive, 1 = Negative) ###
some.data$rate <- ifelse(some.data$response == "POSITIVE", 0, 1)

### Create column to average for each face ###
some.data$ang_rate <- ifelse(some.data$condition == "Angry", some.data$rate, NA)
some.data$hap_rate <- ifelse(some.data$condition == "Happy", some.data$rate, NA)
some.data$sur_rate <- ifelse(some.data$condition == "Surprise", some.data$rate, NA)
some.data$neg_rate <- ifelse(some.data$condition == "NEG", some.data$rate, NA)
some.data$pos_rate <- ifelse(some.data$condition == "POS", some.data$rate, NA)
some.data$amb_rate <- ifelse(some.data$condition == "AMBIG", some.data$rate, NA)

### Create data frame of just ratings ###
ratings.data <- data.frame(matrix(c(some.data$ang_rate, some.data$hap_rate, some.data$sur_rate,
                                    some.data$neg_rate, some.data$pos_rate, some.data$amb_rate),
                             nrow = 96, ncol = 6))
colnames(ratings.data) <- c("ang_rate", "hap_rate", "sur_rate", "neg_rate", "pos_rate", "amb_rate")
colMeans(ratings.data, na.rm = TRUE)

### Reaction times ###




