
# Poluição do ar em países democratas e autocratas -----------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 01/10/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/air-pollution -------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Poluição do ar é um dos maiores problemas de saúde e ambietais. Ela se desenvolve
### em dois contextos: indoor (poluição doméstica) e outdoor (poluição ao ar livre).

### Nós observamos os dados em detalhes e pesquisamos sobre os impactos na saúde
### pela poluição indoor e pela poluição outdoor, incluindo as mortes atribuídas 
### ao problema e as causas dela em torno do mundo.

### Globalmente, a poluição do ar contribuiu para 11.65% das causas de morte no
### ano de 2019.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

par <- read.csv("share-deaths-air-pollution.csv")
view(par)
names(par)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

par <- par %>%
  select(-Code) %>%
  rename(poluicao_mortes = Deaths...Cause..All.causes...Risk..Air.pollution...Sex..Both...Age..Age.standardized..Percent.) %>%
  view()

par1 <- par %>%
  filter(Entity %in% c("United States", "Japan", "Germany",
                       "Cuba", "China", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(poluicao_mortes),
            sd = sd(poluicao_mortes), n = n(),
            se = sd/sqrt(n)) %>%
  view()

par2 <- par %>%
  filter(Entity %in% c("United States", "Japan", "Germany",
                       "Cuba", "China", "North Korea")) %>%
  view() 

par3 <- par %>%
  filter(Entity %in% c("United States", "China", "Brazil")) %>%
  view() 

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(par1, aes(x = fct_reorder(Entity, media), y = media, 
                 fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.2) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677", "#DDCC77",
                              "#117733", "#332288", "#AA4499")) +
  scale_x_discrete(labels = c("Japão", "Estados Unidos", "Alemanha",
                               "Cuba", "China", "Coreia do Norte")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  labs(x = "Países", y = "Porcentagem de mortes 
                       atribuídas a poluição do ar") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplot(par2, aes(x = Year, y = poluicao_mortes,
                 group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677", "#DDCC77",
                              "#117733", "#332288", "#AA4499"),
                     labels = c("China", "Cuba", "Alemanha",
                                "Japão", "Coreia do Norte", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Porcentagem de mortes 
                       atribuídas a poluição do ar",
       color = "Países") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(axis.text = element_text(color = "black"))

ggplot(par3, aes(x = Year, y = poluicao_mortes, 
                  group = Entity, col = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Mortes por poluição do ar (%)", 
       color = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))
