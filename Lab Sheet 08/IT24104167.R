setwd("C:\\Users\\it24104167\\Downloads\\IT24104167")
getwd()

data <- read.table("Exercise - LaptopsWeights.txt", header = TRUE)
head(data)

weights <- data$Weight
weights

#Q1
pop_mean <- mean(weights)
pop_mean

pop_sd <- sd(weights)
pop_sd

#Q2
set.seed(123)  
sample_means <- numeric(25)
sample_sds <- numeric(25)

for (i in 1:25) {
  sample_data <- sample(weights, size = 6, replace = TRUE)
  sample_means[i] <- mean(sample_data)
  sample_sds[i] <- sd(sample_data)
  cat("Sample", i, "- Mean:", sample_means[i], " SD:", sample_sds[i], "\n")
}

#Q3
mean_of_sample_means <- mean(sample_means)
mean_of_sample_means

sd_of_sample_means <- sd(sample_means)
sd_of_sample_means

#comparing 
popmn <- mean(weights)         
popstd <- sd(weights)          

samplemn <- mean(sample_means) 
samplestd <- sd(sample_means)  

popmn
samplemn
popstd
samplestd

