
library(tidyverse)
library(glmnet)

set.seed(123)

n <- 100
p <- 1000

erro <- rnorm(n,0,0.5)

x <- matrix(rnorm(n*p),ncol=p)

betas <-c(rep(1,3),rep(0,997))
y <- x %*% betas + erro
dados <- data.frame(y=y,x)
attach(dados)

set.seed(1999)
train <- sample(1:dim(dados)[1],0.7*100) 

dados_treino <- dados[train,]
dados_teste <- dados[-train,]


###### BIC 
null <-  lm(y ~ 1)
full <-  lm(y ~.,dados[,2:1000])
step(null, scope = list(lower=null,upper=full),
     direction="forward", criterion = "BIC", k=log(n))


#<>------------- Exemplo com o Ridge -----------------------------------------------<>

library(glmnet)
x <- model.matrix(y~.,dados_treino)[,-1]  #matriz de preditores
y <- dados_treino$y                            # variável resposta
x_teste <-  model.matrix(y~.,dados_teste)[,-1]  


grid <- seq(1,0.01,by=-0.01) # valores de lambdas

erro_teste <- c()
for(i in 1:length(grid)){ #ajustando um modelo para cada valor de lambda
fitridge <- glmnet(x, y, alpha = 0, lambda = grid[i]) 
yhats <- predict(fitridge,s=grid[i],newx = x_teste)

erro_teste[i] <- mean((yhats-dados_teste$y)^2) ## erro de teste    
}


erro_treino_reg <- c()
  for(j in 1:length(grid)){
  fitridge <- glmnet(x, y, alpha = 0, lambda = grid[j])
  yhats <- predict(fitridge,s=grid[j],newx = x)
  
 erro_treino_reg[j] <- sum((yhats-dados_treino$y)^2) +grid[j]*sum((fitridge$beta)^2)
  
  
}


erro_empi <- c()
for(k in 1:length(grid)){
  fitridge <- glmnet(x, y, alpha = 0, lambda = grid[k]) 
  yhats <- predict(fitridge,s=grid[k],newx = x)
  erro_empi[k] <- sum((dados_treino$y-yhats)^2) 
}

erros <- cbind(grid,mse,erro_treino_reg,erro_empi) %>% data.frame()


ggplot(erros, aes(x=grid))+
  geom_line(aes(y=mse, colour="red"),size=0.7) +
  geom_line(aes(y=erro_treino_reg,colour="blue"),size=.7) +
  geom_line(aes(y=erro_empi, colour="green"),size=0.7) + 
  scale_colour_manual(values=c("red"="red", # editando a legenda
                               "blue"="blue",
                               "green"="green"),
                            breaks = c("red","blue","green"),
  labels=c("Erro de teste", "Erro regularizado",  "Erro empírico"), name="") +
  scale_y_continuous(breaks=seq(0, 4, 0.5)) +
  xlab("lambda") + 
  ylab("erro") +  
  theme_bw() + 
  theme(legend.position = "top", # posição da legenda
        axis.title=element_text(size = 12,color = "black") )
  


  
