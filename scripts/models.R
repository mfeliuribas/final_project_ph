
# Load libraries
# library("tidyverse")
library("here")
library("lme4")
# library("dplyr")


# Load data
subset_pbc <- read.csv(here("data", "subsets", "subset_pbc.csv"))
subset_accent <- read.csv(here("data", "subsets", "subset_accent.csv"))
subset_classe <- read.csv(here("data", "subsets", "subset_classe.csv"))
subset_oc <- read.csv(here("data", "subsets", "subset_oc.csv"))



# Build models (parla bé catala, 8)
mod_0_pbc <- lm(value ~ 1, data = subset_pbc)

mod_1_pbc <- lm(value ~ 1 + phoneme, data = subset_pbc)

mod_2_pbc <- lm(value ~ 1 + phoneme + mother_tongue, data = subset_pbc)

mod_3_pbc <- lm(value ~ 1 + phoneme + mother_tongue + province, 
                data = subset_pbc)

mod_4_pbc <- lm(value ~ 1 + phoneme + mother_tongue + province + 
                  phoneme:mother_tongue:province, data = subset_pbc)

summary(mod_0_pbc)
summary(mod_1_pbc)
summary(mod_2_pbc)
summary(mod_3_pbc)
summary(mod_4_pbc)

anova(mod_0_pbc, mod_1_pbc, mod_2_pbc, mod_3_pbc, mod_4_pbc)


subset_pbc %>%
  ggplot() +
  aes(x = mother_tongue, y = value, fill = mother_tongue) +
  facet_grid(.~ phoneme) +
  geom_point() +
  stat_summary(fun.data = mean_se, geom = "pointrange", pch = 23, size = 1)


plot(mod_2_pbc)




# Build models (té un accent bonic, 10)
mod_0_accent <- lm(value ~ 1, data = subset_accent)

mod_1_accent <- lm(value ~ 1 + phoneme, data = subset_accent)

mod_2_accent <- lm(value ~ 1 + phoneme + mother_tongue, data = subset_accent)

mod_3_accent <- lm(value ~ 1 + phoneme + mother_tongue + province, 
                data = subset_accent)

mod_4_accent <- lm(value ~ 1 + phoneme + mother_tongue + province + 
                  phoneme:mother_tongue:province, data = subset_accent)

summary(mod_0_accent)
summary(mod_1_accent)
summary(mod_2_accent)
summary(mod_3_accent)
summary(mod_4_accent)

# canviar: anova(mod_0_pbc, mod_1_pbc, mod_2_pbc, mod_3_pbc, mod_4_pbc)





# Build models (és de classe alta, 2)
mod_0_classe <- lm(value ~ 1, data = subset_classe)

mod_1_classe <- lm(value ~ 1 + phoneme, data = subset_classe)

mod_2_classe <- lm(value ~ 1 + phoneme + mother_tongue, data = subset_classe)

mod_3_classe <- lm(value ~ 1 + phoneme + mother_tongue + province, 
                   data = subset_classe)

mod_4_classe <- lm(value ~ 1 + phoneme + mother_tongue + province + 
                     phoneme:mother_tongue:province, data = subset_classe)

summary(mod_0_classe)
summary(mod_1_classe)
summary(mod_2_classe)
summary(mod_3_classe)
summary(mod_4_classe)

# canviar: anova(mod_0_pbc, mod_1_pbc, mod_2_pbc, mod_3_pbc, mod_4_pbc)




# Build models (és d'origen català, 9)
mod_0_oc <- lm(value ~ 1, data = subset_oc)

mod_1_oc <- lm(value ~ 1 + phoneme, data = subset_oc)

mod_2_oc <- lm(value ~ 1 + phoneme + mother_tongue, data = subset_oc)

mod_3_oc <- lm(value ~ 1 + phoneme + mother_tongue + province, 
                   data = subset_oc)

mod_4_oc <- lm(value ~ 1 + phoneme + mother_tongue + province + 
                     phoneme:mother_tongue:province, data = subset_oc)

summary(mod_0_oc)
summary(mod_1_oc)
summary(mod_2_oc)
summary(mod_3_oc)
summary(mod_4_oc)

