##
## Archived script snippets 
## not used in analysis due to wrong fit
## wrong design or of no use
##

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
boxplot(duration ~ notation.r, data = data, xlab = "Notation", ylab = "Duration", names = c("1" = "NL", "2" = "KV"))

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
