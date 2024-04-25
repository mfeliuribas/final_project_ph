
# Load libraries
library("here")
library("tidyverse")
library("ggplot2")



# Load data
subset_estudis <- read.csv(here("data", "subsets", "subset_estudis.csv"))
subset_intelligent <- read.csv(here("data", "subsets", "subset_intelligent.csv"))
subset_simpatic <- read.csv(here("data", "subsets", "subset_simpatic.csv"))
subset_amable <- read.csv(here("data", "subsets", "subset_amable.csv"))
subset_fiar <- read.csv(here("data", "subsets", "subset_fiar.csv"))
subset_ciutat <- read.csv(here("data", "subsets", "subset_ciutat.csv"))
subset_pbc <- read.csv(here("data", "subsets", "subset_pbc.csv"))
subset_oc <- read.csv(here("data", "subsets", "subset_oc.csv"))


# Plots "parla bé català" 
subset_pbc %>%
  ggplot() +
  aes(x = mother_tongue, y = value, fill = mother_tongue) +
  facet_grid(.~ phoneme) +
  geom_point() +
  stat_summary(fun.data = mean_se, geom = "pointrange", pch = 23, size = 1) +
  labs(x = "Mother tongue", y = "Value", fill = "Mother tongue") +
  scale_fill_viridis_d(end = 0.9, labels = c("Spanish", "Catalan", "Both")) +
  scale_x_discrete(labels = c("Spanish", "Catalan", "Both")) +
  theme(legend.key.size = unit(1, "lines"),
    legend.key.width = unit(1.5, "lines"),
    legend.spacing.x = unit(0.5, "lines"),
    legend.position = "bottom") +
  #labs(title = "Statement: Parla bé català") +
  guides(fill = guide_legend(override.aes = list(size = 0.5)))
ggsave(here("manuscript", "includes", "figures", "pbc.png"))

# Sembla que la gent no distingueix entre [s] i [z] o, si ho fa, els hi és igual
# En general, els cat_cast valoren més positivament a la persona que escolten,
# és a dir, pensen que parla millor català
# Si escolen el fonema sonor, sembla que la valoració és més positiva en tots els
# grups,, però la diferència no és significativa
# Hipòtesi: el fonema /s/ sol sembla que no genera cap tipus d'actitud lingüística,
# ni positiva ni negativa (és neutral). Pot ser que sigui la combinació vocal + s
# la que aporta significat social?



# Plots "origen català" 
subset_oc %>%
  ggplot() +
  aes(x = mother_tongue, y = value, fill = mother_tongue) +
  facet_grid(.~ phoneme) +
  geom_point() +
  stat_summary(fun.data = mean_se, geom = "pointrange", pch = 23, size = 1) +
  labs(x = "Mother tongue", y = "Value", fill = "Mother tongue") +
  scale_fill_viridis_d(end = 0.9, labels = c("Spanish", "Catalan", "Both")) +
  scale_x_discrete(labels = c("Spanish", "Catalan", "Both")) +
  theme(legend.key.size = unit(1, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.spacing.x = unit(0.5, "lines"),
        legend.position = "bottom")+
  #labs(title = "Statement: És d'origen català") +
  guides(fill = guide_legend(override.aes = list(size = 0.5)))
ggsave(here("manuscript", "includes", "figures", "oc.png"))

# És pràcticament igual a l'anterior


# Plots "és simpàtic" 
subset_simpatic %>%
  ggplot() +
  aes(x = mother_tongue, y = value, fill = mother_tongue) +
  facet_grid(.~ phoneme) +
  geom_point() +
  stat_summary(fun.data = mean_se, geom = "pointrange", pch = 23, size = 1) +
  labs(x = "Mother tongue", y = "Value", fill = "Mother tongue") +
  scale_fill_viridis_d(end = 0.9, labels = c("Spanish", "Catalan", "Both")) +
  scale_x_discrete(labels = c("Spanish", "Catalan", "Both")) +
  theme(legend.key.size = unit(1, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.spacing.x = unit(0.5, "lines"))+
  labs(title = "Statement: És simpàtic") +
  guides(fill = guide_legend(override.aes = list(size = 0.5)))

subset_simpatic %>%
  ggplot() +
  aes(x = mother_tongue, y = value, fill = mother_tongue) +
  facet_grid(.~ province) +
  geom_point() +
  stat_summary(fun.data = mean_se, geom = "pointrange", pch = 23, size = 1) +
  labs(x = "Mother tongue", y = "Value", fill = "Mother tongue") +
  scale_fill_viridis_d(end = 0.9, labels = c("Spanish", "Catalan", "Both")) +
  scale_x_discrete(labels = c("Spanish", "Catalan", "Both")) +
  theme(legend.key.size = unit(1, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.spacing.x = unit(0.5, "lines"))+
  labs(title = "Statement: És simpàtic") +
  guides(fill = guide_legend(override.aes = list(size = 0.5)))

subset_simpatic %>%
  ggplot() +
  aes(x = province, y = value, fill = province) +
  facet_grid(.~ phoneme) +
  geom_point() +
  stat_summary(fun.data = mean_se, geom = "pointrange", pch = 23, size = 1) +
  labs(x = "Province", y = "Value", fill = "Province") +
  scale_fill_viridis_d(end = 0.9, labels = c("Barcelona", "Girona")) +
  theme(legend.key.size = unit(1, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.spacing.x = unit(0.5, "lines"),
        legend.position = "bottom")+
  #labs(title = "Statement: És simpàtic") +
  guides(fill = guide_legend(override.aes = list(size = 0.5)))
ggsave(here("manuscript", "includes", "figures", "simpatic.png"))



# Plots "és de fiar" 
subset_fiar %>%
  ggplot() +
  aes(x = mother_tongue, y = value, fill = mother_tongue) +
  facet_grid(.~ phoneme) +
  geom_point() +
  stat_summary(fun.data = mean_se, geom = "pointrange", pch = 23, size = 1) +
  labs(x = "Mother tongue", y = "Value", fill = "Mother tongue") +
  scale_fill_viridis_d(end = 0.9, labels = c("Spanish", "Catalan", "Both")) +
  scale_x_discrete(labels = c("Spanish", "Catalan", "Both")) +
  theme(legend.key.size = unit(1, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.spacing.x = unit(0.5, "lines"))+
  labs(title = "Statement: És de fiar") +
  guides(fill = guide_legend(override.aes = list(size = 0.5)))

subset_fiar %>%
  ggplot() +
  aes(x = province, y = value, fill = province) +
  facet_grid(.~ phoneme) +
  geom_point() +
  stat_summary(fun.data = mean_se, geom = "pointrange", pch = 23, size = 1) +
  labs(x = "Province", y = "Value", fill = "Province") +
  scale_fill_viridis_d(end = 0.9, labels = c("Barcelona", "Girona")) +
  theme(legend.key.size = unit(1, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.spacing.x = unit(0.5, "lines"),
        legend.position = "bottom")+
  #labs(title = "Statement: És de fiar") +
  guides(fill = guide_legend(override.aes = list(size = 0.5)))
ggsave(here("manuscript", "includes", "figures", "fiar.png"))

# Sembla que amb la [z] les valoracions són una mica més bones, però no hi ha 
# diferències significatives
# Els de Girona valoren més negativament

