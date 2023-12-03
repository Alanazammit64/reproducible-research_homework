# Install and load necessary packages
install.packages(c("ggplot2", "janitor", "dplyr", "data.table"))
library(ggplot2)
library(janitor)
library(dplyr)
library(data.table)

# Cleaning the dataset to make the names easier to work with 
cuietal <- rename(Cui_etal2014, genome_length = "Genome length (kb)", virion_volume = "Virion volume (nm×nm×nm)")
View(cuietal)


# Subsetting the dataset to include only the columns we need 
cuietal_clean <- cuietal[, c("genome_length", "virion_volume")]
View(cuietal_clean)



# Log-transforming the variables genome length and virion volume so I can do a linear regression 
cuietal_clean$log_virion_volume <- log(cuietal_clean$virion_volume)
cuietal_clean$log_genome_length <- log(cuietal_clean$genome_length)
View(cuietal_clean) # checking these extra log-transformed variables have been added 


# Setting up the linear model 
linear_model <- lm(data = cuietal_clean, log_virion_volume~log_genome_length)
summary(linear_model) 
# This summary yields the same results as in the previous parts to this question

# Plotting the log-transformed data 
ggplot(data = cuietal_clean, aes(x= log_genome_length, y = log_virion_volume)) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw()+
  labs(x = "log [Genome length (kb)]", y = "log [Virion volume (nm3)]") +
  geom_point()