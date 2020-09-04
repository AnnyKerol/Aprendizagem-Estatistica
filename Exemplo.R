
library(tidyverse)

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


