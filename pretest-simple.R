# simple design

datasimple <- read.csv("dataset_simple.csv", sep = ";", dec = ",", header = TRUE)
datasimple$time <- as.POSIXct(datasimple$time, format="%M:%S")
datasimple$duration <- as.numeric(format(datasimple$time, "%M")) + as.numeric(format(datasimple$time, "%S"))/60

# preparations for testing
# setup mapping and factors
syntaxMapping <- c("nl" = 1, "kv" = 2)
rankingMapping <- c("don't know" = 0, "2nd place" = 1, "1st place" = 2)

datasimple$notation.r <- syntaxMapping[datasimple$notation]
datasimple$rank.r <- rankingMapping[datasimple$rank]

datasimple$notation.r <- as.factor(datasimple$notation.r)
datasimple$notation.r <- factor(datasimple$notation.r, levels = c("1", "2"), labels = c("natural language", "key-value"))
datasimple$sequence <- as.factor(datasimple$sequence)
datasimple$sequence <- factor(datasimple$sequence, levels = c("1", "2"), labels = c("NL-KV", "KV-NL"))
datasimple$rank.r <- as.factor(datasimple$rank.r)
datasimple$period <- as.factor(datasimple$period)

# the actual dataframe we'll be working with
df <- subset(datasimple, select = c("subject", "sequence", "period", "notation.r", "duration", "accuracy", "sus", "rank.r"))

# Summary Data

# duration for sample_size.R
df %>%
  group_by(notation.r) %>%
  summarize(mean_duration = mean(duration), sd_duration = sd(duration))

########
######## CHECK CARRY-OVER EFFECT
########
######## Based on: https://www.lexjansen.com/pharmasug/2006/Posters/PO16.pdf

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

# estimate carry-over effect using sm values by t-test
# Null Hypothesis: there is a significant difference between NL-KV / KV-NL sequences --> this means there is a carry over effect (use only period 1)
# Alternative Hypothesis: there is no signicant difference between NL-KV / KV-NL sequences --> this means there is NO carry over effect (use both periods)
# p-value > 0.05 shows possible carry-over effect is no significantly different between NL-KV / KV-NL sequences
t.test(mean_duration ~ sequence, data = sp)

########
########
########

t.test(duration ~ notation.r, data = df)
