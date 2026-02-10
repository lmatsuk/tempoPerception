#necessary libraries
library(tidyverse)
library(lmerTest)
library(datawizard)
library(buildmer)
library(performance)
library(interactions)
library(patchwork)

#import data
data <- read.csv(file = '', sep = ",")
print(data)

#reshape data to long format (repeated measurements)
data_long <- data %>%
  pivot_longer(
    cols = starts_with("Tempo_"), 
    names_to = "BPM", 
    values_to = "Tempo_Perception"
  ) %>%
  mutate(
    BPM = as.numeric(gsub("Tempo_", "", BPM)),  # Remove 'Tempo_' and convert to numeric
    Tempo_Perception = as.numeric(Tempo_Perception)    # Ensure category is numeric
  )
print(data_long)


#descriptives
#number of participants (N)
nrow(data)
 
#sex (1 = female, 2 = male)
table(data$Sex)

#gender (1 = female, 2 = male, 3 = other)
table(data$Gender)

#height
descript <- t.test(data$Height)
descript
SD = sd(data$Height)
SD
SE = sd(data$Height)/sqrt(length(data$Height))
SE
MinMax = range(data$Height)
MinMax
hist(data$Height)


#weight
descript <- t.test(data$Weight)
descript
SD = sd(data$Weight)
SD
sd_weight <- sd(data$Weight)
SE = sd(data$Weight)/sqrt(length(data$Weight))
SE
MinMax = range(data$Weight)
MinMax
hist(data$Weight)

#age
descript <- t.test(data$Age)
descript
SD = sd(data$Age)
SD
SE = sd(data$Age)/sqrt(length(data$Age))
SE
MinMax = range(data$Age)
MinMax
hist(data$Age)

#MSI
descript <- t.test(data$Musical_Sophistication)
descript
SD = sd(data$Musical_Sophistication)
SD
SE = sd(data$Musical_Sophistication)/sqrt(length(data$Musical_Sophistication))
SE
MinMax = range(data$Musical_Sophistication)
MinMax
hist(data$Musical_Sophistication)

#DSI
descript <- t.test(data$Dance_Sophistication)
descript
SD = sd(data$Dance_Sophistication)
SD
SE = sd(data$Dance_Sophistication)/sqrt(length(data$Dance_Sophistication))
SE
MinMax = range(data$Dance_Sophistication)
MinMax
hist(data$Dance_Sophistication)

#z-standardizing variables
data_long <- data_long %>%
  mutate(
    z_Tempo_Perception = standardize(Tempo_Perception),
    z_BPM = standardize(BPM),
    z_Height = standardize(Height),
    z_Age = standardize(Age),
    z_MSI = standardize(Musical_Sophistication),
    z_DSI = standardize(Dance_Sophistication),
    z_Weight = standardize(Weight)
    )

#build linear mixed model with z-standardized values to identify relevant predictors
built_z_model <- buildmer(z_Tempo_Perception ~ z_BPM + z_Height + z_BPM*z_Height + z_Weight + z_BPM*z_Weight + z_Age + z_BPM*z_Age + z_MSI + z_BPM*z_MSI + z_DSI + z_BPM*z_DSI +(1|Subject), data = data_long, family = gaussian(), buildmerControl = buildmerControl(direction = "backward"))
summary(built_z_model)

#final model
z_model <- lmer(z_Tempo_Perception ~ z_BPM + z_Height + z_BPM*z_Height + z_Age + z_BPM*z_Age + z_MSI + z_BPM*z_MSI + z_DSI + z_BPM*z_DSI +(1|Subject), data = data_long)
summary(z_model)

#check for multicollinearity
multicollinearity(z_model)

#test normality of residual distribution
qqnorm(residuals(z_model))
qqline(residuals(z_model), col = "steelblue", lwd = 2)

#Display Interaction Graph w/ standardized values
z_p1 <- interact_plot (model = z_model, pred = z_BPM, modx = z_Height, plot.points = TRUE, x.label = "Stimulus Tempo", y.label = "Perceived Tempo", colors = "Greys", jitter = 0.03) + theme (text = element_text(family = "Arial", size = 20), plot.title = element_text(face = "bold", size = 20), axis.title = element_text(size = 20), axis.text = element_text(size = 20)) + scale_y_continuous(breaks = -2:2, labels = c("-2", "-1 = Slow", "0", "1 = Fast", "2"))
z_p2 <- interact_plot (model = z_model, pred = z_BPM, modx = z_Age, plot.points = TRUE, x.label = "Stimulus Tempo", y.label = "Perceived Tempo", colors = "Greys", jitter = 0.03) + theme (text = element_text(family = "Arial", size = 20), plot.title = element_text(face = "bold", size = 20), axis.title = element_text(size = 20), axis.text = element_text(size = 20)) + scale_y_continuous(breaks = -2:2, labels = c("-2", "-1 = Slow", "0", "1 = Fast", "2"))
z_p3 <- interact_plot (model = z_model, pred = z_BPM, modx = z_MSI, plot.points = TRUE, x.label = "Stimulus Tempo", y.label = "Perceived Tempo", colors = "Greys", jitter = 0.03) + theme (text = element_text(family = "Arial", size = 20), plot.title = element_text(face = "bold", size = 20), axis.title = element_text(size = 20), axis.text = element_text(size = 20)) + scale_y_continuous(breaks = -2:2, labels = c("-2", "-1 = Slow", "0", "1 = Fast", "2"))
z_p4 <- interact_plot (model = z_model, pred = z_BPM, modx = z_DSI, plot.points = TRUE, x.label = "Stimulus Tempo", y.label = "Perceived Tempo", colors = "Greys", jitter = 0.03) + theme (text = element_text(family = "Arial", size = 20), plot.title = element_text(face = "bold", size = 20), axis.title = element_text(size = 20), axis.text = element_text(size = 20)) + scale_y_continuous(breaks = -2:2, labels = c("-2", "-1 = Slow", "0", "1 = Fast", "2"))

#combine into 2x2 matrix
z_combined_plot <- (z_p1 | z_p2)/(z_p3 | z_p4)
z_combined_plot

#compute model without z-standardization and visualize for more intuitive understanding
model <- lmer(Tempo_Perception ~ BPM + Height + BPM*Height + Age + BPM*Age + Musical_Sophistication + BPM*Musical_Sophistication + Dance_Sophistication + BPM*Dance_Sophistication +(1|Subject), data = data_long)
summary(model)

p1 <- interact_plot (model = model, pred = BPM, modx = Height, plot.points = TRUE, x.label = "Stimulus Tempo in bpm", y.label = "Perceived Tempo", colors = "Greys", jitter = 0.03) + theme (text = element_text(family = "Arial", size = 20), plot.title = element_text(face = "bold", size = 20), axis.title = element_text(size = 20), axis.text = element_text(size = 20)) + scale_y_continuous(breaks = 1:5, labels = c("1 = Very Slow", "2", "3", "4", "5 = Very Fast"))
p2 <- interact_plot(model = model, pred = BPM, modx = Age,  plot.points = TRUE, x.label = "Stimulus Tempo in bpm", y.label = "Perceived Tempo", colors = "Greys", jitter = 0.03) + theme (text = element_text(family = "Arial", size = 20), plot.title = element_text(face = "bold", size = 20), axis.title = element_text(size = 20), axis.text = element_text(size = 20)) + scale_y_continuous(breaks = 1:5, labels = c("1 = Very Slow", "2", "3", "4", "5 = Very Fast"))
p3 <- interact_plot(model = model, pred = BPM, modx = Musical_Sophistication, plot.points = TRUE, x.label = "Stimulus Tempo in bpm", y.label = "Perceived Tempo", colors = "Greys", jitter = 0.03) + theme (text = element_text(family = "Arial", size = 20), plot.title = element_text(face = "bold", size = 20), axis.title = element_text(size = 20), axis.text = element_text(size = 20)) + scale_y_continuous(breaks = 1:5, labels = c("1 = Very Slow", "2", "3", "4", "5 = Very Fast"))
p4 <- interact_plot(model = model, pred = BPM, modx = Dance_Sophistication, plot.points = TRUE, x.label = "Stimulus Tempo in bpm", y.label = "Perceived Tempo", colors = "Greys", jitter = 0.03) + theme (text = element_text(family = "Arial", size = 20), plot.title = element_text(face = "bold", size = 20), axis.title = element_text(size = 20), axis.text = element_text(size = 20)) + scale_y_continuous(breaks = 1:5, labels = c("1 = Very Slow", "2", "3", "4", "5 = Very Fast"))

#combine into 2x2 matrix
combined_plot <- (p1 | p2)/(p3 | p4)
combined_plot
