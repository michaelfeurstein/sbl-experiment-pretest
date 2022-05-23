# load libraries
library(ggplot2)
library(AICcmodavg)

# load data
data <- read.csv("dataset_v4.csv", sep = ";", header = TRUE)
data$time <- as.POSIXct(data$time, format="%M:%S")
data$duration <- as.numeric(format(data$time, "%M")) + as.numeric(format(data$time, "%S"))/60

# preparations for testing
# setup mapping and factors
syntaxMapping <- c("nl" = 1, "kv" = 2)
experienceMapping <- c("advanced" = 3, "novice" = 4)
rankingMapping <- c("don't know" = 0, "2nd place" = 1, "1st place" = 2)

data$notation.r <- syntaxMapping[data$notation]
data$experience.r <- experienceMapping[data$experience]
data$rank.r <- rankingMapping[data$rank]

data$notation.r <- as.factor(data$notation.r)
data$experience.r <- as.factor(data$experience.r)
data$experience.r <- factor(data$experience.r, levels = c("3", "4"), labels = c("advanced", "novice"))
data$sequence <- as.factor(data$sequence)
data$sequence <- factor(data$sequence, levels = c("1", "2"), labels = c("NL-KV", "KV-NL"))
data$rank.r <- as.factor(data$rank.r)

data <- subset(data, select = c("sequence", "experience.r", "notation.r", "rank.r", "duration", "sus"))

##
## Visualization
##

# works
bxp.duration <- ggboxplot(data, x = "notation.r", xlab = "Notation", y = "duration", ylab = "Duration in minutes", color = "experience.r", palette = "jco", facet.by = "sequence", panel.labs = list(sequence = c("AB: NL->KV", "BA: KV->NL")), add = "jitter") + scale_x_discrete(labels=rep(c("NL","KV"),2))
bxp.duration

# not nice -look into formatting of decimal and comma of sus score
bxp.sus <- ggboxplot(data, x = "notation.r", xlab = "Notation", y = "sus", ylab = "SUS score", color = "experience.r", palette = "jco", facet.by = "sequence", panel.labs = list(sequence = c("AB: NL->KV", "BA: KV->NL")), add = "jitter") + scale_x_discrete(labels=rep(c("NL","KV"),2))
bxp.sus

# doesn't work with rank
bxp.rank <- ggboxplot(data, x = "notation.r", xlab = "Notation", y = "rank", ylab = "Ranking", color = "experience.r", palette = "jco", facet.by = "sequence", panel.labs = list(sequence = c("AB: NL->KV", "BA: KV->NL")), add = "jitter") + scale_x_discrete(labels=rep(c("NL","KV"),2))
bxp.rank


##
## Outliers
##

data %>% group_by(notation.r, experience.r, sequence) %>% identify_outliers(duration)

##
## Normality Assumption
##

data %>% group_by(notation.r, experience.r, sequence) %>% shapiro_test(duration)


##
# ANOVA ASSUMPTIONS
##

# (1) assumption of normality
# histogram
hist(data$duration[data$notation.r == "1"], main = "Natural Language", xlab = "Duration")
hist(data$duration[data$notation.r == "2"], main = "Key-Valuee", xlab = "Duration")

# qqplot
# do with anova

# shapiro-wilk test
shapiro.test(data$duration)
shapiro.test(data$duration[data$notation.r == "1"])
shapiro.test(data$duration[data$notation.r == "2"])

# (2) assumption of equal variances
# leven's test
leveneTest(duration ~ notation.r, data)
leveneTest(duration ~ experience.r, data)

# boxplots
boxplot(duration ~ notation.r, data = data2, xlab = "Notation", ylab = "Duration", names = c("1" = "NL", "2" = "KV"))

# bartlett's test
bartlett.test(duration ~ notation.r, data = data)
bartlett.test(duration ~ experience.r, data = data)

# (3) assumption of independence
# scatter plot
ggplot(data, aes(x=notation, y=duration, shape=factor(sequence), color=experience.r)) + geom_point() + theme_minimal()

##
# RESEARCH QUESTIONS
##

## RQ1.a
one.way.notation <- aov(duration ~ notation.r, data)
summary(one.way.notation)

one.way.notation.seq <- aov(duration ~ notation.r + sequence, data)
summary(one.way.notation.seq)

# QUESTION: bin mir da nie sicher ob ich sequence als blocking hinein nehme oder das garnicht berücksichtigen muss, wenn ich per scatterplot cross-over effect ausschliessen kann?

# test aov assumption 2
qqnorm(one.way.notation$residuals)
qqline(one.way.notation$residuals)

# descriptive statistics by group
describeBy(data$duration, data$notation.r)
describeBy(data,list(data$notation.r,data$experience.r)) # use this

# graph
ggplot(data,aes(y=duration, x=notation.r, fill=notation.r)) + stat_summary(fun="mean", geom="bar",position="dodge") + stat_summary(fun.data = mean_se, geom = "errorbar", position="dodge",width=.8)

# post hoc test: tukey's honestly significant difference (tukey's hsd)
tukey.one.way.notation <- TukeyHSD(one.way.notation)
tukey.one.way.notation

tukey.one.way.notation.seq <- TukeyHSD(one.way.notation.seq)
tukey.one.way.notation.seq

## RQ2.a
one.way.experience <- aov(duration ~ experience.r, data)
summary(one.way.experience)

# test aov assumption 2
qqnorm(one.way.experience$residuals)
qqline(one.way.experience$residuals)

# descriptive statistics by group
describeBy(data$duration, data$experience.r)
describeBy(data,list(data$notation.r,data$experience.r))

# graph
ggplot(data,aes(y=duration, x=experience.r, fill=experience.r)) + stat_summary(fun="mean", geom="bar",position="dodge") + stat_summary(fun.data = mean_se, geom = "errorbar", position="dodge",width=.8)

# post hoc test: tukey's honestly significant difference (tukey's hsd)
tukey.one.way.experience <- TukeyHSD(one.way.experience)
tukey.one.way.experience

tukey.one.way.experience.seq <- TukeyHSD(one.way.experience.seq)
tukey.one.way.experience.seq

## RQ3.a + c
# two-way anova (additive model)
additive <- aov(duration ~ notation.r + experience.r, data = data)
summary(additive)

# two-way anova with interaction effect
interaction <- aov(duration ~ notation.r * experience.r, data = data)
summary(interaction)

# two-way anova with blocking variable sequence
blocking <- aov(duration ~ notation.r * experience.r + sequence, data = data)
summary(blocking)

# find best fit model
model.set <- list(additive, interaction, blocking)
model.names <- c("additive", "interaction", "blocking")
aictab(model.set, modnames = model.names)

# check selected model for homoscedasticity
par(mfrow=c(2,2))
plot(blocking)
par(mfroe=c(1,1))

# two-way anova post hoc test
tukey.two.way.blocking <- TukeyHSD(blocking)
tukey.two.way.blocking
