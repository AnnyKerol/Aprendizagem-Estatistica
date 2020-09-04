
#<>----------------------------
#        Comandos básicos
#-----------------------------

getwd() # Mostrar o diretório de trabalho atual

dir()   # Listar os arquivos do diretório

setwd() # Mudar o diretório de trabalho

?mean()

# Pacotes

#install.packages("tidyverse")
#install.packages("viridis")

library(tidyverse)
require(viridis)

# Sintaxe básica

2+2 # Adição

3-1 # subtração

4/2 # divisão

5*3 # multiplicação

9 %% 5   # resto da divisão de 9 por 5

7 %/% 4  # parte inteira da divisão de 7  por 4 

4^ 2    # potenciação

sqrt(16) # radiciação

# Atribuicao de objetos
x <- 15
x

disciplina <- "Aprendizagem Estatística"
disciplina

Saldo <- TRUE
Saldo

nome <- "Ana"
nome  # sempre com aspas
class(nome)

y <- 4.5
y
class(y)

numero <- 4+5i
numero
class(numero)

Ordem <- c("Compra", "Venda")
Ordem <- as.factor(Ordem) 
Ordem

# Vetores e matrizes 
vetor <- c(1.2,2.5,3.7,4,5)
length(vetor) # retorna o tamanho do vetor
vetor[3] # acessando o elemento da terceira posição do meu vetor

matriz <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
matriz

dim(matriz) # retorna a dimensão da matriz

matriz[1,2]  # acessa o elemento da linha 1 coluna 2
matriz[1,]   # acessa todos os elementos da linha 1
matriz[,2]  # acessa todos os elementos da coluna 2

# Data frames
dados <- data.frame(ano = 2000:2004,
                    prod = c(32, 54, 25, 48, 29))
View(dados)
dim(dados) # dimensãoes do data frame
str(dados) # tipos de variáveis no data frame 

dados[,1]
dados[,2]

# Listas

minha_lista <- list() # cria uma lista vazia

ls() # lista todos os objetos 

minha_lista[[1]] <- dados 
minha_lista[[2]] <-matriz
minha_lista[[3]] <-vetor

minha_lista[[3]] # acessando o vetor na lista
minha_lista[[3]][5] # acessando a posição 5 do vetor na lista

# Operadores Logicos

## & 
x <- 10

x>5 & x>7

## Ou: |
y <- 8
y>7 | y<3

# Negação: **!** 
z <- 3
(!z>3) # Z não é maior que 3?


# Estruturas If e Else

situacao <- "Aprovado"
if(situacao=="Aprovado"){
  print("Parabéns!")
}


situacao1 <- "Reprovado"
if(situacao1=="Aprovado"){
  print("Parabéns")
}else{
  print("Tente outra vez!")
}


ordem <- c(0,1,0,1,0,1) # 0-venda 1- pra compra
ordem <- ifelse(ordem==1,"Compra","Venda")
print(ordem)

## Estrutura de repetição : for
df <- iris

head(df) # possui 6 linhas e 5 colunas
especies <- as.character(df$Species) # as.character = transforma o tipo da variavel especies
for(i in 1:6){
  print(especies[i])
}


## O pipe %>% 

notas <- c(9.555,10,9.889)

media <- round(mean(notas),2)

media2 <- notas %>% mean() %>% round(2)




