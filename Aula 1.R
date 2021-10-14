#dados
dados <- read.csv("dados/Pesquisa-Brasil-e-Espanha-fusionado.csv", encoding = "UTF-8")
backup <- dados

#pacotes
library("tidyverse")

dados %>% 
  glimpse() %>% 
  names()

dados <- dados %>% 
  mutate(eat_soma = rowSums(.[19:44], na.rm=TRUE))

dados <- dados %>%
  mutate(bsq_soma = rowSums(.[45:78], na.rm = TRUE))

dados <- dados %>% 
  mutate(sexo = if_else(sexo == "1", "mulheres", "homens"))

dados <- dados %>% 
  mutate(pais = if_else(país == "1", "Brasil", "Espanha"))

dados %>% 
  group_by(pais, sexo) %>% 
  summarise(media = mean(eat_soma, na.rm = TRUE))

dados <- dados %>% 
  drop_na(sexo)

dados %>% 
  group_by(pais, sexo) %>% 
  summarise_at(vars(eat_soma, bsq_soma),
  funs(mean(.,na.rm = TRUE), sd(., na.rm = TRUE)))

dados_brasil <- dados %>% 
  filter(pais == "Brasil") %>% 
  as_tibble()

dados_brasil %>% glimpse()

ggplot(dados_brasil, aes(sexo, eat_soma, fill = sexo)) +
  geom_bar(fun.y = mean, position = "dodge", stat = "summary") +
  geom_errorbar(fun.data = mean_se, stat = "summary")
