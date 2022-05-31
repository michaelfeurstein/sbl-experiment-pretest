# Sample Size
# based on: https://cran.r-project.org/web/packages/easypower/vignettes/User_Input.html 
library(easypower)

# Define main effects
#
# name --> name of treatment effect
# levels --> number of levels/groups in the treatment (at least 2)
# eta.sq --> estimated effect size for the treatment effect | “small” (0.01), “med” (0.06), and “large” (0.14)
main.eff1 <- list(name = "Notation", levels = 2, eta.sq = "med")
main.eff2 <- list(name = "Experience", levels = 2, eta.sq = "med")
main.eff3 <- list(name = "Sequence", levels = 2, eta.sq = "small")

# Effect sizes of specific interactions
#int.eff1 <- list(name = "Notation*Experience", eta.sq = "med")
#int.eff2 <- list(name = "Notation*Experience*Sequence", eta.sq = "med")

# interaction.eta2 --> change all the effect sizes simultaneously 
n.multiway(iv1 = main.eff1, iv2 = main.eff2, iv3 = main.eff3, interaction.eta2 = 0.06)
#n.multiway(iv1 = main.eff1, iv2 = main.eff2, iv3 = main.eff3, int1 = int.eff1, int2 = int.eff2)
