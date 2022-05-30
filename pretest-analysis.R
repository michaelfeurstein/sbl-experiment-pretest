# load libraries
library(ggplot2)
library(AICcmodavg)

# load data
data <- read.csv("dataset_v4.csv", sep = ";", dec = ",", header = TRUE)
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
data$notation.r <- factor(data$notation.r, levels = c("1", "2"), labels = c("natural language", "key-value"))
data$experience.r <- as.factor(data$experience.r)
data$experience.r <- factor(data$experience.r, levels = c("3", "4"), labels = c("advanced", "novice"))
data$sequence <- as.factor(data$sequence)
data$sequence <- factor(data$sequence, levels = c("1", "2"), labels = c("NL-KV", "KV-NL"))
data$rank.r <- as.factor(data$rank.r)

data <- subset(data, select = c("id", "sequence", "experience.r", "notation.r", "duration", "accuracy", "sus", "rank.r"))

##
## Visualization
##

# duration
bxp.duration <- ggboxplot(data, x = "notation.r", xlab = "Notation", y = "duration", ylab = "Duration in minutes", color = "experience.r", palette = "jco", facet.by = "sequence", panel.labs = list(sequence = c("AB: NL->KV", "BA: KV->NL")), add = "jitter") + scale_x_discrete(labels=rep(c("NL","KV"),2))
bxp.duration

# accuracy
bxp.sus <- ggboxplot(data, x = "notation.r", xlab = "Notation", y = "accuracy", ylab = "Accuracy in percent", color = "experience.r", palette = "jco", facet.by = "sequence", panel.labs = list(sequence = c("AB: NL->KV", "BA: KV->NL")), add = "jitter") + scale_x_discrete(labels=rep(c("NL","KV"),2))
bxp.sus

# sus
bxp.sus <- ggboxplot(data, x = "notation.r", xlab = "Notation", y = "sus", ylab = "SUS score", color = "experience.r", palette = "jco", facet.by = "sequence", panel.labs = list(sequence = c("AB: NL->KV", "BA: KV->NL")), add = "jitter") + scale_x_discrete(labels=rep(c("NL","KV"),2))
bxp.sus

##
## Outliers
##

# duration
data %>% group_by(notation.r, experience.r, sequence) %>% identify_outliers(duration)

# accuracy
data %>% group_by(notation.r, experience.r, sequence) %>% identify_outliers(accuracy)

# sus
data %>% group_by(notation.r, experience.r, sequence) %>% identify_outliers(sus)

##
## Normality Assumption
##

# duration
data %>% group_by(notation.r, experience.r, sequence) %>% shapiro_test(duration)
# histograms
hist(data$duration[data$notation.r == "natural language"], main = "Natural Language", xlab = "Duration")
hist(data$duration[data$notation.r == "key-value"], main = "Key-Value", xlab = "Duration")
# qq plot 
# notation
ggqqplot(data, "duration", ggtheme = theme_bw()) +
  facet_grid(sequence ~ notation.r, labeller = "label_both")
# experience
ggqqplot(data, "duration", ggtheme = theme_bw()) +
  facet_grid(sequence ~ experience.r, labeller = "label_both")

# accuracy
data %>% group_by(notation.r, experience.r, sequence) %>% shapiro_test(accuracy)
# histogram
hist(data$accuracy[data$notation.r == "natural language"], main = "Natural Language", xlab = "Accuracy")
hist(data$accuracy[data$notation.r == "key-value"], main = "Key-Value", xlab = "Accuracy")
# qq plot 
# notation
ggqqplot(data, "accuracy", ggtheme = theme_bw()) +
  facet_grid(sequence ~ notation.r, labeller = "label_both")
# experience
ggqqplot(data, "accuracy", ggtheme = theme_bw()) +
  facet_grid(sequence ~ experience.r, labeller = "label_both")

# sus
data %>% group_by(notation.r, experience.r, sequence) %>% shapiro_test(sus)
# histogram
hist(data$sus[data$notation.r == "natural language"], main = "Natural Language", xlab = "SUS score")
hist(data$sus[data$notation.r == "key-value"], main = "Key-Value", xlab = "SUS score")
# qq plot 
# notation
ggqqplot(data, "sus", ggtheme = theme_bw()) +
  facet_grid(sequence ~ notation.r, labeller = "label_both")
# experience
ggqqplot(data, "sus", ggtheme = theme_bw()) +
  facet_grid(sequence ~ experience.r, labeller = "label_both")

##
## Homogneity of variance assumption
##

# Compute Levene’s test at each level of the within-subjects factor -> notation
# duration
data %>%
  group_by(notation.r) %>%
  levene_test(duration ~ experience.r*sequence)

# accuracy
data %>%
  group_by(notation.r) %>%
  levene_test(accuracy ~ experience.r*sequence)

# sus
data %>%
  group_by(notation.r) %>%
  levene_test(sus ~ experience.r*sequence)

##
## Computation: three-way mixed ANOVA
##

# duration
res.aov.duration <- anova_test(
  data = data, dv = duration, wid = id,
  within = notation.r, between = c(experience.r, sequence)
)
get_anova_table(res.aov.duration)

#res.aov.duration2 <- aov(duration ~  experience.r * sequence * notation.r, data = data)
#Anova(res.aov.duration2, type="II")
#report(res.aov.duration2)

# accuracy
res.aov.accuracy <- anova_test(
  data = data, dv = accuracy, wid = id,
  within = notation.r, between = c(experience.r, sequence)
)
get_anova_table(res.aov.accuracy)

# sus
res.aov.sus <- anova_test(
  data = data, dv = sus, wid = id,
  within = notation.r, between = c(experience.r, sequence)
)
get_anova_table(res.aov.sus)

##
## Post hoc tests
##

##
## Two way interaction
##

##
## Statistical tests for ranking question 
## --> preference data
##

# Friedman's test based on: https://stats.stackexchange.com/questions/142903/develop-a-statistical-test-to-distinguish-two-products
friedman.test(rank.r ~ notation.r|id, data = data) 

# Wilcoxon rank sum test
data$rank.r <- as.numeric(data$rank.r)

data %>% sample_n_by(notation.r, size = 2)

data %>%
  group_by(notation.r) %>%
  get_summary_stats(rank.r, type = "median_iqr")

bxp <- ggboxplot(
  data, x = "notation.r", y ="rank.r",
  ylab = "Rank", xlab = "Notation", add = "jitter"
)
bxp

stat.test <- data %>%
  wilcox_test(rank.r ~ notation.r) %>%
  add_significance()
stat.test

data %>% wilcox_effsize(rank.r ~ notation.r)

stat.test <- stat.test %>% add_xy_position(x = "notation.r")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

# Aligned ranks transformation ANOVA (ART anova)
# Based on: https://rcompanion.org/handbook/F_16.html

ar.model <- art(rank.r ~ experience.r + notation.r + experience.r:notation.r, data = data)
ar.model
anova(ar.model)

library(rcompanion)
Sum = groupwiseMedian(rank.r ~ experience.r + notation.r,
                      data=data,
                      bca=FALSE, percentile=TRUE)
Sum

pd = position_dodge(0.3)
ggplot(Sum,
       aes(x = notation.r,
           y = Median,
           color = experience.r)) + 
  geom_point(shape  = 15,
             size   = 4,
             position = pd) + 
  geom_errorbar(aes(ymin  =  Percentile.lower,
                    ymax  =  Percentile.upper),
                width =  0.2,
                size  =  0.7,
                position = pd) + 
  theme_bw() + 
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) + 
  ylab("Median rank.r count") + 
  ggtitle ("rank.r counts for advanced and novice users",
           subtitle = "for two types of notations") + 
  
  labs(caption  = paste0("\nrank.r counts for two experience levels ",
                         "and two types of notation. Boxes indicate \n",
                         "the median. ",
                         "Error bars indicate the 95% confidence ",
                         "interval ",
                         "of the median."),
       hjust=0.5)

#############################
#############################
#############################

##
# ANOVA ASSUMPTIONS
##

# (1) assumption of normality
# histogram
hist(data$duration[data$notation.r == "natural language"], main = "Natural Language", xlab = "Duration")
hist(data$duration[data$notation.r == "key-value"], main = "Key-Value", xlab = "Duration")

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
