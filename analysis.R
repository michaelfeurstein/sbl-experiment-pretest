library(dplyr)
library(ggplot2)
library(ggpubr)

######## IMPORT DATA  ####

# crossover trial AB|BA
# csv: https://github.com/michaelfeurstein/sbl-experiment-pretest/blob/master/dataset.csv 

mydata <- read.csv("dataset.csv", sep = ";", dec = ",", header = TRUE)
mydata$time <- as.POSIXct(mydata$time, format="%M:%S")
mydata$duration <- as.numeric(format(mydata$time, "%M")) + as.numeric(format(mydata$time, "%S"))/60

# preparations for testing
# setup mapping and as.factor
syntaxMapping <- c("nl" = 1, "kv" = 2)
rankingMapping <- c("don't know" = 0, "2nd place" = 1, "1st place" = 2)

mydata$notation.r <- syntaxMapping[mydata$notation]
mydata$rank.r <- rankingMapping[mydata$rank]

mydata$notation.r <- as.factor(mydata$notation.r)
mydata$notation.r <- factor(mydata$notation.r, levels = c("1", "2"), labels = c("natural language", "key-value"))
mydata$sequence <- as.factor(mydata$sequence)
mydata$sequence <- factor(mydata$sequence, levels = c("1", "2"), labels = c("NL-KV", "KV-NL"))
mydata$rank.r <- as.numeric(mydata$rank.r)
mydata$period <- as.factor(mydata$period)

# log transform duration
mydata$duration.log = log(mydata$duration)

# the actual dataframe we'll be working with
df <- subset(mydata, select = c("subject", "sequence", "period", "notation.r", "duration", "duration.log", "accuracy", "sus", "rank.r"))

######## CHECK CARRY-OVER EFFECT ####
# Based on: https://www.lexjansen.com/pharmasug/2006/Posters/PO16.pdf

# calculate and plot sum of means for sequence,period
sp <- df %>%
  group_by(sequence, period) %>%
  summarize(mean_duration = mean(duration))

pd = position_dodge(0)
sp.plot <- ggplot(sp, aes(x = period,
               y = mean_duration,
               color = sequence,
               group = sequence)) +
  geom_point(shape  = 15,
             size   = 4,
             position = pd) +
  geom_line() + theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 4, 4, 4),
    axis.title   = element_text(face = "bold"),
    axis.text    = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  ) +
  ylab("Duration mean") +
  xlab("Period") +
  labs(color = "Sequence")

# calculate and plot sum of means for notation,period
np <- df %>%
  group_by(notation.r, period) %>%
  summarize(mean_duration = mean(duration))

pd = position_dodge(0)
np.plot <- ggplot(np, aes(x = period,
               y = mean_duration,
               color = notation.r,
               group = notation.r)) +
  geom_point(shape  = 15,
             size   = 4,
             position = pd) +
  geom_line() + theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 4, 4, 4),
    axis.title   = element_text(face = "bold"),
    axis.text    = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  ) +
  ylab("Duration mean") +
  xlab("Period") +
  labs(color = "Notation")

# arrange both plots in one figure
ggarrange(sp.plot, np.plot, nrow = 1, ncol = 2)

# estimate carry-over effect using sum values by t-test
# Null Hypothesis: there is a significant difference between NL-KV / KV-NL sequences --> this means there is a carry over effect (use only period 1)
# Alternative Hypothesis: there is no signicant difference between NL-KV / KV-NL sequences --> this means there is NO carry over effect (use both periods)
# p-value > 0.05 shows possible carry-over effect is no significantly different between NL-KV / KV-NL sequences
t.test(mean_duration ~ sequence, data = sp)

# based on the result of the test update the dataframe
# option 1: leave it as it is because there is no carry over effect
# option 2: use only period 1 of dataframe, ergo remove period 2

######## SUMMARY OF DATA  ####

## duration: mean, sd ####

df %>%
  group_by(notation.r) %>%
  summarize(mean_duration = mean(duration.log), sd_duration = sd(duration.log))

hist(df$duration.log[df$notation.r == "natural language"], main = "Natural Language", xlab = "Duration")
hist(df$duration.log[df$notation.r == "key-value"], main = "Key-Value", xlab = "Duration")

## accuracy: mean, sd ####

df %>%
  group_by(notation.r) %>%
  summarize(mean_accuracy = mean(accuracy), sd_duration = sd(accuracy))

hist(df$accuracy[df$notation.r == "natural language"], main = "Natural Language", xlab = "Accuracy")
hist(df$accuracy[df$notation.r == "key-value"], main = "Key-Value", xlab = "Accuracy")

## sus scores: mean, sd ####

df %>%
  group_by(notation.r) %>%
  summarize(mean_sus_score = mean(sus), sd_sus_score = sd(sus))

hist(df$sus[df$notation.r == "natural language"], main = "Natural Language", xlab = "SUS Score")
hist(df$sus[df$notation.r == "key-value"], main = "Key-Value", xlab = "SUS Score")

## ranks ####

df %>%
  group_by(notation.r) %>%
  summarize(mean_rank = mean(rank.r), sd_sus_score = sd(rank.r))

##
## Visualization ####
##

# duration
bxp.duration <- ggboxplot(df, x = "notation.r", xlab = "Notation", y = "duration", ylab = "log transformed duration", color = "period", palette = "jco", facet.by = "sequence", panel.labs = list(sequence = c("AB: NL->KV", "BA: KV->NL")), add = "jitter") + scale_x_discrete(labels=rep(c("NL","KV"),2))
bxp.duration

# accuracy
bxp.accuracy <- ggboxplot(df, x = "notation.r", xlab = "Notation", y = "accuracy", ylab = "Accuracy in percent", color = "period", palette = "jco", facet.by = "sequence", panel.labs = list(sequence = c("AB: NL->KV", "BA: KV->NL")), add = "jitter") + scale_x_discrete(labels=rep(c("NL","KV"),2))
bxp.accuracy

# sus
bxp.sus <- ggboxplot(data, x = "notation.r", xlab = "Notation", y = "sus", ylab = "SUS score", color = "experience.r", palette = "jco", facet.by = "sequence", panel.labs = list(sequence = c("AB: NL->KV", "BA: KV->NL")), add = "jitter") + scale_x_discrete(labels=rep(c("NL","KV"),2))
bxp.sus

######## ANALYSIS  ####

t.test(duration ~ notation.r, data = df)

friedman.test(rank.r ~ notation.r|subject, data = df)
