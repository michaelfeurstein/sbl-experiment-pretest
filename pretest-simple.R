# simple design

datasimple <- read.csv("dataset_simple.csv", sep = ";", dec = ",", header = TRUE)
datasimple$time <- as.POSIXct(datasimple$time, format="%M:%S")
datasimple$duration <- as.numeric(format(datasimple$time, "%M")) + as.numeric(format(datasimple$time, "%S"))/60

# preparations for testing
# setup mapping and factors
syntaxMapping <- c("nl" = 1, "kv" = 2)
experienceMapping <- c("advanced" = 3)
rankingMapping <- c("don't know" = 0, "2nd place" = 1, "1st place" = 2)

datasimple$notation.r <- syntaxMapping[datasimple$notation]
datasimple$experience.r <- experienceMapping[datasimple$experience]
datasimple$rank.r <- rankingMapping[datasimple$rank]

datasimple$notation.r <- as.factor(datasimple$notation.r)
datasimple$notation.r <- factor(datasimple$notation.r, levels = c("1", "2"), labels = c("natural language", "key-value"))
datasimple$experience.r <- as.factor(datasimple$experience.r)
datasimple$experience.r <- factor(datasimple$experience.r, levels = c("3"), labels = c("advanced"))
datasimple$sequence <- as.factor(datasimple$sequence)
datasimple$sequence <- factor(datasimple$sequence, levels = c("1", "2"), labels = c("NL-KV", "KV-NL"))
datasimple$rank.r <- as.factor(datasimple$rank.r)

datasimple <- subset(datasimple, select = c("id", "sequence", "notation.r", "duration", "accuracy", "sus", "rank.r"))

# Summary

# duration for sample_size.R
datasimple %>%
  group_by(notation.r) %>%
  summarize(mean_duration = mean(duration), sd_duration = sd(duration))

