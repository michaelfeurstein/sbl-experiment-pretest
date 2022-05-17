# RQ1.a "time" 
# Which type of notation can help participants 
# complete the authoring task in less working time (= faster)? —> one-way ANOVA?

# One-way ANOVA
# Why: statistical test for estimating how a quantitative dependent variable 
# (dependent var = time) changes according to the levels of one or more
# categorical independent variables (independent var = notation)

# prepare data
syntaxMapping <- c("NL" = 1, "KV" = 2)
experienceMapping <- c("advanced" = 1, "novice" = 2)

data$Syntax.r <- syntaxMapping[data$Syntax]
data$Syntax.r <- as.factor(data$Syntax.r)
data$Sequence <- as.factor(data$Sequence)
data$Participant <- as.factor(data$Participant)

data$Experience.r <- experienceMapping[data$Experience]
data$Experience.r <- as.factor(data$Experience.r)

# one-way ANOVA
aov(Duration ~ Syntax.r, data = data)
