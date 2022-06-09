#
# Power Analysis for sample size planning
#
# Approach: run a simulation of study at different sample sizes where each study is repeated many thousands of times and optimize for power
# We would aim for a power of 80 %

#
# Superpower
# based on vignette: https://cran.r-project.org/web/packages/Superpower/vignettes/intro_to_superpower.html
# and book: https://aaroncaldwell.us/SuperpowerBook/introduction-to-power-analysis.html#overview-of-power-analysis 
# and: https://rdrr.io/cran/Superpower/f/vignettes/more_anova_designs.Rmd 
#
library(Superpower)

design_simple.duration <- ANOVA_design(design = "2w",
                                       n = 6, 
                                       mu <- c(10.0, 8.77), 
                                       sd <- 2.765, 
                                       r = 0, 
                                       label_list = list("Notation" = c( "natural-language", "key-value")),
                                       plot = TRUE)

power_result.simples <- ANOVA_power(design_simple.duration, 
                                alpha = 0.05, 
                                nsims = 100, 
                                seed = 1234)

plot_power(design_simple.duration, max_n = 185, desired_power = 80)
