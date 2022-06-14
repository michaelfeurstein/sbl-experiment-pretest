library(Superpower)

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

### DURATION ####
#### pretest data ####
design.duration.pretest <- ANOVA_design(design = "2w",
                                       n = 6, 
                                       mu <- c(2.27, 2.13), 
                                       sd <- c(0.301, 0.312), 
                                       r = 0, 
                                       label_list = list("Notation" = c( "natural-language", "key-value")),
                                       plot = TRUE)

plot_power(design.duration.pretest, max_n = 100, desired_power = 80)

#### Johanson & Hasselbring (2017) data ####
design.duration.johanson <- ANOVA_design(design = "2w",
                                       n = 40, 
                                       mu <- c(6.4, 2.8), 
                                       sd <- c(5.0, 2.8), 
                                       r = 0, 
                                       label_list = list("Notation" = c( "gpl", "dsl")),
                                       plot = TRUE)

plot_power(design.duration.johanson, max_n = 100, desired_power = 80)

#### Hoisl et al. (2014) data ####
design.duration.hoisl <- ANOVA_design(design = "2w",
                                         n = 20, 
                                         mu <- c(8.67, 15.43), 
                                         sd <- c(2.25, 6.73), 
                                         r = 0, 
                                         label_list = list("Notation" = c( "n", "e")),
                                         plot = TRUE)

plot_power(design.duration.hoisl, max_n = 100, desired_power = 80)


### ACCURACY ####
#### pretest data ####
design.accuracy.pretest <- ANOVA_design(design = "2w",
                                       n = 6, 
                                       mu <- c(91.7, 80.8),
                                       sd <- c(8.16, 15.3),
                                       r = 0, 
                                       label_list = list("Notation" = c( "natural-language", "key-value")),
                                       plot = TRUE)

plot_power(design.accuracy.pretest, max_n = 100, desired_power = 80)

#### Johanson & Hasselbring (2017) data ####
design.accuracy.johanson <- ANOVA_design(design = "2w",
                                         n = 40, 
                                         mu <- c(60.8, 97.8), 
                                         sd <- c(47.3, 6.9), 
                                         r = 0, 
                                         label_list = list("Notation" = c( "gpl", "dsl")),
                                         plot = TRUE)

plot_power(design.accuracy.johanson, max_n = 100, desired_power = 80)

#### Diss: Juhnke (2017) data ####
design.accuracy.juhnke <- ANOVA_design(design = "2w",
                                         n = 10, 
                                         mu <- c(4.9, 1.8), 
                                         sd <- c(2.685, 1.751), 
                                         r = 0, 
                                         label_list = list("Notation" = c( "nl", "dsl")),
                                         plot = TRUE)

plot_power(design.accuracy.juhnke, max_n = 100, desired_power = 80)

### SUS scores ####
#### pretest data ####
design.sus.pretest <- ANOVA_design(design = "2w",
                                        n = 1324, 
                                        mu <- c(49.34, 58.21),
                                        sd <- c(18.87, 19.59),
                                        r = 0, 
                                        label_list = list("Notation" = c( "natural-language", "key-value")),
                                        plot = TRUE)

plot_power(design.sus.pretest, max_n = 100, desired_power = 80)

#### https://measuringu.com/sample-sizes-for-sus-comparisons/ #### --> 46

#### Funk et al. (2007) data ####
## in the study no values for sd are given thus I am using the typical sd form Jim Lewis and Jeff Sauro
## link: https://measuringu.com/sample-sizes-for-sus-comparisons/
design.sus.yarmand <- ANOVA_design(design = "2w",
                                         n = 1324, 
                                         mu <- c(78, 47), 
                                         sd <- c(17.7, 17.7), 
                                         r = 0, 
                                         label_list = list("Notation" = c( "CLOnE", "protege")),
                                         plot = TRUE)

plot_power(design.sus.yarmand, max_n = 100, desired_power = 80)

### Rank ####
#### pretest data ####
design.rank.pretest <- ANOVA_design(design = "2w",
                                   n = 6, 
                                   mu <- c(1.67, 1.17),
                                   sd <- c(0.516, 0.408),
                                   r = 0, 
                                   label_list = list("Notation" = c( "natural-language", "key-value")),
                                   plot = TRUE)

plot_power(design.rank.pretest, max_n = 100, desired_power = 80)

#### pretest data ####
design.rank.pretest <- ANOVA_design(design = "2w",
                                    n = 48, 
                                    mu <- c(2.08, 1.88),
                                    sd <- c(0.85, 0.84),
                                    r = 0, 
                                    label_list = list("Notation" = c( "natural-language", "key-value")),
                                    plot = TRUE)

plot_power(design.rank.pretest, max_n = 100, desired_power = 80)

