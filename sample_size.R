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

#
# Duration
#
# # A tibble: 4 × 4
# Groups:   experience.r [2]
# experience.r notation.r       mean_duration sd_duration
# <fct>        <fct>                    <dbl>       <dbl>
# 1 advanced     natural language         13.8         5.67
# 2 advanced     key-value                 9.38        2.59
# 3 novice       natural language          9.68        2.60
# 4 novice       key-value                12.2         1.96
#

#
# ANOVA design
#

# FRAGE 1: r = correlation for within designs (or 0 for between designs)
# ich habe ein mixed design (richtig? 2b*2w), wie gebe ich r an?
# ich würde auf 0 setzen da ich durch cross over gegen carry over arbeite?
#
# ZUSATZ INFO zu tool: ich bekomme via https://arcstats.io/shiny/anova-exact/ nur das gleiche raus wenn ich bei corrleation einmal 0.1 (oder irgendwas >0) eingebe und dann auf 0 setze. BUG?
# danach bei power analyse komme ich im tool online auf n = 166 und hier auf 180
# Bei https://arcstats.io/shiny/anova-power/ kriege ich ansatzweise die gleichen ergebnisse.... komisch
 
design_result.duration <- ANOVA_design(design = "2b*2w",
                              n = 6,
                              mu <- c(13.8, 9.38, 9.68, 12.2),
                              sd <- c(5.67, 2.59, 2.6, 1.96),
                              r = 0.8,
                              label_list = list("Experience"  = c("advanced", "novice"),
                                                "Notation" = c("natural-language", "key-value")),
                              plot = TRUE)
# 
# ANOVA power simulation
# using nsims --> 1000 simulations
#
power_result.duration <- ANOVA_power(design_result.duration, 
                                  alpha = 0.05, 
                                  nsims = 100, 
                                  seed = 1234)

knitr::kable(confint(power_result.duration, level = .98))

#
# ANOVA exact simulation
# simulates single dataset matching exact desired properties
#

exact_result.duration <- ANOVA_exact(design_result.duration,
                            alpha_level = 0.05,
                            verbose = TRUE)

exact_result.duration.2 <- ANOVA_exact2(design_result.duration,
                              alpha_level = 0.05,
                              verbose = TRUE)

# find the minimum needed sample size n for a given power
plot_power(design_result.duration, max_n = 80, desired_power = 80)
# FRAGE 2: aus plot_power resultierende bedingung für n= 180 macht mein experiment nutzlos? wie kann ich trotzdem noch weiter machen.
# power immer mit kommunizieren, damit man beim ergebnis lesen das mit bedenkt?

############
############
############

#
# Accuracy
#
# A tibble: 4 × 4
# Groups:   experience.r [2]
# experience.r notation.r       mean_accuracy sd_accuracy
# <fct>        <fct>                    <dbl>       <dbl>
# 1 advanced     natural language          64.2       30.3 
# 2 advanced     key-value                 95          7.98
# 3 novice       natural language          95          7.98
# 4 novice       key-value                 65.8       20.2 
#

#
# ANOVA design
#

# FRAGE 3: transformation für accuracy needed - how to do that?

design_result.accuracy <- ANOVA_design(design = "2b*2w",
                                       n = 40, 
                                       mu <- c(64.2, 95, 95, 65.8), 
                                       sd <- c(30.3, 7.98, 7.98, 20.2), 
                                       r = 0, 
                                       label_list = list("Experience"  = c("advanced", "novice"),
                                                         "Notation" = c( "natural-language", "key-value")),
                                       plot = TRUE)
# 
# ANOVA power simulation
# using nsims --> 1000 simulations
#
power_result.accuracy <- ANOVA_power(design_result.accuracy, 
                                     alpha = 0.05, 
                                     nsims = 100, 
                                     seed = 1234)

knitr::kable(confint(power_result.accuracy, level = .98))

#
# ANOVA exact simulation
# simulates single dataset matching exact desired properties
#

exact_result.accuracy <- ANOVA_exact(design_result.accuracy,
                                     alpha_level = 0.05,
                                     verbose = TRUE)

exact_result.accuracy.2 <- ANOVA_exact2(design_result.accuracy,
                                        alpha_level = 0.05,
                                        verbose = TRUE)

############
############
############

#
# SUS scores
#
# A tibble: 4 × 4
# Groups:   experience.r [2]
# experience.r notation.r       mean_sus sd_sus
# <fct>        <fct>               <dbl>  <dbl>
#1 advanced     natural language     53.8   17.3
# 2 advanced     key-value            61.0   19.1
# 3 novice       natural language     59.0   16.5
# 4 novice       key-value            63.1   12.4
#

#
# ANOVA design
#

design_result.sus <- ANOVA_design(design = "2b*2w",
                                       n = 40, 
                                       mu <- c(53.8, 61, 59, 63.1), 
                                       sd <- c(17.3, 19.1, 16.5, 12.4), 
                                       r = 0, 
                                       label_list = list("Experience"  = c("advanced", "novice"),
                                                         "Notation" = c( "natural-language", "key-value")),
                                       plot = TRUE)
# 
# ANOVA power simulation
# using nsims --> 1000 simulations
#
power_result.sus <- ANOVA_power(design_result.sus, 
                                     alpha = 0.05, 
                                     nsims = 100, 
                                     seed = 1234)

knitr::kable(confint(power_result.sus, level = .98))

#
# ANOVA exact simulation
# simulates single dataset matching exact desired properties
#

exact_result.sus <- ANOVA_exact(design_result.sus,
                                     alpha_level = 0.05,
                                     verbose = TRUE)

exact_result.sus.2 <- ANOVA_exact2(design_result.sus,
                                        alpha_level = 0.05,
                                        verbose = TRUE)

############
############
############

# Simple Design

design_simple.duration <- ANOVA_design(design = "2w",
                                       n = 6, 
                                       mu <- c(10.0, 8.77), 
                                       sd <- 2.765, 
                                       r = 0.9, 
                                       label_list = list("Notation" = c( "natural-language", "key-value")),
                                       plot = TRUE)

power_result.simples <- ANOVA_power(design_simple.duration, 
                                alpha = 0.05, 
                                nsims = 100, 
                                seed = 1234)

# FRAGE: liege ich richtig wenn ich die correlation r auf 0 setze wegen carry over?
# wenn ich correlation auf 0.5 setze wird die power höher

plot_power(design_simple.duration, max_n = 185, desired_power = 80)

design_simple.duration <- ANOVA_design(design = "2b*2w",
                                       n = 6, 
                                       mu <- c(9.67, 8.21, 10.3, 9.33), 
                                       sd <- c(3.10, 2.42, 3.51, 3.06), 
                                       r = 0, 
                                       label_list = list("Sequence" = c("NL-KV","KV-NL"),
                                                         "Notation" = c("natural-language", "key-value")),
                                       plot = TRUE)

plot_power(design_simple.duration, max_n = 185, desired_power = 80)

############
############
############

#
# Easypower
# based on: https://cran.r-project.org/web/packages/easypower/vignettes/User_Input.html 
# and: https://cran.r-project.org/web/packages/easypower/easypower.pdf
#
library(easypower)

# Define main effects
#
# name --> name of treatment effect
# levels --> number of levels/groups in the treatment (at least 2)
# eta.sq --> estimated effect size for the treatment effect | “small” (0.01), “med” (0.06), and “large” (0.14)
main.eff1 <- list(name = "Notation", levels = 2, eta.sq = "large")
main.eff2 <- list(name = "Experience", levels = 2, eta.sq = "large")
main.eff3 <- list(name = "Sequence", levels = 2, eta.sq = "large")

# Effect sizes of specific interactions
#int.eff1 <- list(name = "Notation*Experience", eta.sq = "med")
#int.eff2 <- list(name = "Notation*Experience*Sequence", eta.sq = "med")

# interaction.eta2 --> change all the effect sizes simultaneously 
n.multiway(iv1 = main.eff1, iv2 = main.eff2, iv3 = main.eff3, interaction.eta2 = 0.14)
#n.multiway(iv1 = main.eff1, iv2 = main.eff2, iv3 = main.eff3, int1 = int.eff1, int2 = int.eff2)

# Simple Design
main.eff <- list(name = "Notation", levels = 2, eta.sq = 0.50)
# Running the function with default settings
n.oneway(iv = main.eff)

# FRAGE: wäre das ok für ein vereinfachtes design 0.50 aus anderm paper genommen.
