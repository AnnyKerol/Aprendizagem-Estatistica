###### Seleção de Modelos com o critério BIC

null = lm(y ~ 1) # modelo nulo
full = lm(y ~.,dados[,2:1000]) # modelo com todas as covariáveis
step(null, scope = list(lower=null,upper=full),
     direction="forward", criterion = "BIC", k=log(n)) # no argumento você pode colocar 
# both, backward ou forward. O k=log(n) refere-se ao BIC
