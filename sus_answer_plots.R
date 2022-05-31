##
## SUS answer plots
##

# libraries
library(ggplot2)
library(lemon)
library(stringr)
library(scales)

# load data
sus.answers.nl <- read.csv("sus_answers_nl__experienced-nl-kv.csv", sep = ";", header = TRUE)

sus.answers.nl$Answer <- factor(sus.answers.nl$Answer,
                                            levels = 1:5,
                                            labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))

sus.answers.nl$Question <- factor(sus.answers.nl$Question,
                                              levels = c("PQ1. I think that I would like to use this notation frequently.","NQ1. I find the notation unnecessarily complex.","PQ2. I think the notation would be easy to use.","NQ2. I think that I would need the support of a technical person*** to be able to use this notation.","PQ3. I find the various functions* in this notation are well integrated.","NQ3. I think there is too much inconsistency** in this notation.","PQ4. I would imagine that most people would learn to use this notation very quickly.","NQ4. I would find the notation very cumbersome to use.","PQ5. I would feel very confident using the notation.","NQ5. I would need to learn a lot of things before I could get going with this notation."))

ggplot(sus.answers.nl, aes(x=Question, fill=Answer)) + 
  geom_bar(width = 0.7, position = position_stack(reverse = TRUE)) + 
  scale_fill_manual(values=c("darkred","red", "grey", "darkolivegreen1", "darkgreen")) +
  scale_y_continuous(expand = expansion(0)) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0), axis.title.y = element_blank(), axis.title.x =element_blank(), legend.position = "bottom", legend.justification = c(1,1), legend.title = element_blank()) +
  facet_rep_grid(Notion ~ ., scales = "free", repeat.tick.labels = "all") +
  scale_x_discrete(labels = wrap_format(50)) +
  coord_capped_cart(bottom="both", left="both") +
  coord_flip()
