
# Load libraries
library("here")
library("lme4")


# Load data
subset_estudis <- read.csv(here("data", "subsets", "subset_estudis.csv"))
subset_classe <- read.csv(here("data", "subsets", "subset_classe.csv"))
subset_intelligent <- read.csv(here("data", "subsets", "subset_intelligent.csv"))
subset_simpatic <- read.csv(here("data", "subsets", "subset_simpatic.csv"))
subset_amable <- read.csv(here("data", "subsets", "subset_amable.csv"))
subset_fiar <- read.csv(here("data", "subsets", "subset_fiar.csv"))
subset_ciutat <- read.csv(here("data", "subsets", "subset_ciutat.csv"))
subset_pbc <- read.csv(here("data", "subsets", "subset_pbc.csv"))
subset_oc <- read.csv(here("data", "subsets", "subset_oc.csv"))
subset_accent <- read.csv(here("data", "subsets", "subset_accent.csv"))
subset_agradable <- read.csv(here("data", "subsets", "subset_agradable.csv"))


# Build models (té estudis, 1)
mod_0_estudis <- lm(value ~ 1, data = subset_estudis)

mod_1_estudis <- lm(value ~ 1 + phoneme, data = subset_estudis)

mod_2_estudis <- lm(value ~ 1 + phoneme + mother_tongue, data = subset_estudis)

mod_3_estudis <- lm(value ~ 1 + phoneme + mother_tongue + province, 
                    data = subset_estudis)

mod_4_estudis <- lm(value ~ 1 + phoneme + mother_tongue + province + 
                      phoneme:mother_tongue:province, data = subset_estudis)

summary(mod_0_estudis)
summary(mod_1_estudis)
summary(mod_2_estudis)
summary(mod_3_estudis)
summary(mod_4_estudis)

anova(mod_0_estudis, mod_1_estudis, mod_2_estudis, mod_3_estudis, mod_4_estudis)



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

anova(mod_0_classe, mod_1_classe, mod_2_classe, mod_3_classe, mod_4_classe)



# Build models (és intel·ligent, 3)
mod_0_intelligent <- lm(value ~ 1, data = subset_intelligent)

mod_1_intelligent <- lm(value ~ 1 + phoneme, data = subset_intelligent)

mod_2_intelligent <- lm(value ~ 1 + phoneme + mother_tongue, 
                        data = subset_intelligent)

mod_3_intelligent <- lm(value ~ 1 + phoneme + mother_tongue + province, 
                    data = subset_intelligent)

mod_4_intelligent <- lm(value ~ 1 + phoneme + mother_tongue + province + 
                      phoneme:mother_tongue:province, data = subset_intelligent)

summary(mod_0_intelligent)
summary(mod_1_intelligent)
summary(mod_2_intelligent)
summary(mod_3_intelligent)
summary(mod_4_intelligent)

anova(mod_0_intelligent, mod_1_intelligent, mod_2_intelligent, 
      mod_3_intelligent, mod_4_intelligent)



# Build models (és simpàtic, 4)
mod_0_simpatic <- lm(value ~ 1, data = subset_simpatic)

mod_1_simpatic <- lm(value ~ 1 + phoneme, data = subset_simpatic)

mod_2_simpatic <- lm(value ~ 1 + phoneme + mother_tongue, 
                        data = subset_simpatic)

mod_3_simpatic <- lm(value ~ 1 + phoneme + mother_tongue + province, 
                        data = subset_simpatic)

mod_4_simpatic <- lm(value ~ 1 + phoneme + mother_tongue + province + 
                          phoneme:mother_tongue:province, data = subset_simpatic)

summary(mod_0_simpatic)
summary(mod_1_simpatic)
summary(mod_2_simpatic)
summary(mod_3_simpatic)
summary(mod_4_simpatic)

anova(mod_0_simpatic, mod_1_simpatic, mod_2_simpatic, mod_3_simpatic, 
      mod_4_simpatic)



# Build models (és amable, 5)
mod_0_amable <- lm(value ~ 1, data = subset_amable)

mod_1_amable <- lm(value ~ 1 + phoneme, data = subset_amable)

mod_2_amable <- lm(value ~ 1 + phoneme + mother_tongue, 
                     data = subset_amable)

mod_3_amable <- lm(value ~ 1 + phoneme + mother_tongue + province, 
                     data = subset_amable)

mod_4_amable <- lm(value ~ 1 + phoneme + mother_tongue + province + 
                       phoneme:mother_tongue:province, data = subset_amable)

summary(mod_0_amable)
summary(mod_1_amable)
summary(mod_2_amable)
summary(mod_3_amable)
summary(mod_4_amable)

anova(mod_0_amable, mod_1_amable, mod_2_amable, mod_3_amable, mod_4_amable)



# Build models (és de fiar, 6)
mod_0_fiar <- lm(value ~ 1, data = subset_fiar)

mod_1_fiar <- lm(value ~ 1 + phoneme, data = subset_fiar)

mod_2_fiar <- lm(value ~ 1 + phoneme + mother_tongue, data = subset_fiar)

mod_3_fiar <- lm(value ~ 1 + phoneme + mother_tongue + province, 
                 data = subset_fiar)

mod_4_fiar <- lm(value ~ 1 + phoneme + mother_tongue + province + 
                     phoneme:mother_tongue:province, data = subset_fiar)

summary(mod_0_fiar)
summary(mod_1_fiar)
summary(mod_2_fiar)
summary(mod_3_fiar)
summary(mod_4_fiar)

anova(mod_0_fiar, mod_1_fiar, mod_2_fiar, mod_3_fiar, mod_4_fiar)



# Build models (és de ciutat, 7)
mod_0_ciutat <- lm(value ~ 1, data = subset_ciutat)

mod_1_ciutat <- lm(value ~ 1 + phoneme, data = subset_ciutat)

mod_2_ciutat <- lm(value ~ 1 + phoneme + mother_tongue, data = subset_ciutat)

mod_3_ciutat <- lm(value ~ 1 + phoneme + mother_tongue + province, 
                 data = subset_ciutat)

mod_4_ciutat <- lm(value ~ 1 + phoneme + mother_tongue + province + 
                   phoneme:mother_tongue:province, data = subset_ciutat)

summary(mod_0_ciutat)
summary(mod_1_ciutat)
summary(mod_2_ciutat)
summary(mod_3_ciutat)
summary(mod_4_ciutat)

anova(mod_0_ciutat, mod_1_ciutat, mod_2_ciutat, mod_3_ciutat, mod_4_ciutat)



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

plot(mod_2_pbc)




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

anova(mod_0_oc, mod_1_oc, mod_2_oc, mod_3_oc, mod_4_oc)



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

anova(mod_0_accent, mod_1_accent, mod_2_accent, mod_3_accent, mod_4_accent)



# Build models (és agradable d'escoltar, 11)
mod_0_agradable <- lm(value ~ 1, data = subset_agradable)

mod_1_agradable <- lm(value ~ 1 + phoneme, data = subset_agradable)

mod_2_agradable <- lm(value ~ 1 + phoneme + mother_tongue, 
                      data = subset_agradable)

mod_3_agradable <- lm(value ~ 1 + phoneme + mother_tongue + province, 
                   data = subset_agradable)

mod_4_agradable <- lm(value ~ 1 + phoneme + mother_tongue + province + 
                     phoneme:mother_tongue:province, data = subset_agradable)

summary(mod_0_agradable)
summary(mod_1_agradable)
summary(mod_2_agradable)
summary(mod_3_agradable)
summary(mod_4_agradable)

anova(mod_0_agradable, mod_1_agradable, mod_2_agradable, mod_3_agradable, 
      mod_4_agradable)



