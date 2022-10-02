
# Poluição do ar em países capitalistas e comunistas ---------------------------------------------------------------------------------------
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

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------


