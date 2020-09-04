#<>----------------------------------------------
#               Manipulação de dados
#       Analisando os personagens de Starwars
#<>----------------------------------------------

#install.packages("tidyverse")
#install.packages("viridis")
library(tidyverse)
library(viridis)

# Starwars characters
data("starwars")
dados <- starwars 
dados %>% glimpse() # inspecionando os dados


# filter
# personagens humanos que possuem os olhos azuis
dados %>%  filter(species =="Human" & eye_color=="blue") 

# arrange
# Ordenando os personagens pela altura em cm
dados %>% arrange(height) 
#dados %>% arrange(desc(height)) # ordem descrecente

# filter e arrange
# filtrando os personagens que possuem olhos amarelos e ordenando pelo peso em Kg.
dados %>%  filter(eye_color=="yellow") %>% 
                    arrange(mass)

# select
dados %>% select(name,species,homeworld)

dados %>% select(starts_with("h")) #  seleciona colunas que começam h

dados %>% select(ends_with("or")) # seleciona colunas que terminam or

dados %>% select(contains("w")) # seleciona colunas que contêm w

# mutate

# cria uma nova variavel em metros.
dados %>% select(name,height) %>%
  mutate(heigth_m=height/100)

# summarise
dados %>% summarise(mean(mass, na.rm = TRUE))

dados %>% summarise(max(mass, na.rm = TRUE))

dados %>% summarise(min(mass, na.rm = TRUE))

# group_by
# agrupa por especies 
# Conta o numero de persongens de cada especie
# Ordena pela maior frequencia
# drop_na() exclui as linhas que possuem valores faltantes
dados %>% group_by(species) %>%
             summarise(Total=n()) %>% arrange(desc(Total)) %>% drop_na()


# ggplot2

ggplot(dados, aes(x = eye_color)) + 
  geom_bar()+ # gráfico de barras
  coord_flip() # inverte as coordenadas

# ggplot2 - 2

dados %>% mutate(eye_color=fct_infreq(eye_color)) %>%# fct_infre ordena os fatores por maior frequencia
  ggplot(aes(x = eye_color)) + 
  geom_bar()+
  coord_flip()

# ggplot2 - 3

dados %>% mutate(eye_color=fct_infreq(eye_color)) %>%# ordenando os fatores 
  ggplot(aes(x = eye_color, fill=eye_color)) + 
  geom_bar()+
  coord_flip() +
  theme_bw()+ # muda o tema
  scale_fill_viridis_d(option = "inferno") # adiciona uma nova paleta de cores

# ggplot2 - 3

dados %>% mutate(eye_color=fct_infreq(eye_color)) %>%# ordenando os fatores 
  ggplot(aes(x = eye_color, fill=eye_color)) + 
  geom_bar()+
  coord_flip() + 
  theme_bw()+ 
  scale_fill_viridis_d(option = "inferno")+
  theme(legend.position ="none") # modifica os aspectos do tema: retira a legenda

# ggplot2 - 4

dados %>% mutate(eye_color=fct_infreq(eye_color)) %>%# ordenando os fatores 
  ggplot(aes(x = eye_color, fill=eye_color)) +
  geom_bar()+ 
  coord_flip() +
  theme_bw()+ 
  scale_fill_viridis_d(option = "inferno")+ 
  theme(legend.position ="none", 
  axis.text=element_text(size=18, face="bold", colour="gray24"), # modifica os textos dos eixos
  axis.title=element_text(size=18,face="bold")) # modifica os labels dos eixos

