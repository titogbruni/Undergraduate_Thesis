library(randomForest)
library(tidyverse)
library(HDeconometrics)
library(Metrics)
library(reshape2)
library(ggplot2)
library(ggsci)
library(gridExtra)
library(glmnet)
library(rbcb)
library(lubridate)
library(forecast)
library(stargazer)



#################
# 1) Loading data 
#################

acumulacao <- 1:12 %>% as.list()

data <- acumulacao %>%
  map(~paste("~/GitHub2/MONOGRAFIA_TITO/data_new/df",.x,".rds",sep = "")%>%
        readRDS())



########################################
# 2) Organizando matrizes pros modelos
########################################

# horizonte
k <- 12

# window (fixed) size: vou usar data[[1]], mas poderia ser qualquer outro 
window <- data[[1]] %>% filter(ref.date <"2014-01-01") %>% nrow() 

# variavel dependente 
y <- data[[1]] %>% #(poderia usar qualquer um dos dfs de data)
  select(ipca) %>%
  as.matrix()

# matriz de regressores
X <- data %>%
  map(~.x %>%
        select(-c(ref.date,ipca))%>% 
        select_if(~ !any(is.na(.))) %>% # tirando colunas com NA
        as.matrix())

# numero de previsoes (igual pra todos)
size <- nrow(data[[1]]) - window - 1 #-1 porque nos loops faco 0:size, o que d� size+1 previsoes. 

# valores observados (out of sample)
y_obs <- y[(window+1):nrow(data[[1]])]


#######################
# 3.1) Random Forest ##
#######################

rf <- list()

for (i in 1:length(data)) {
  rf[[i]] <- 0:size %>%
    map_dbl(
      function(x){ randomForest(X[[i]][1:(window+x), ],
                                y[1:(window+ x)]) %>%
          predict(X[[i]][(window+x + 1), ])}
    )
}

###############
# 3.2) LASSO ##
###############

#3.2.1) LASSO forecasts

lasso <- list()

for (i in 1:length(data)) {
  
  lasso[[i]] <- 0:size %>%
    map_dbl(
      function(x){ ic.glmnet(X[[i]][1:(window+ x), ], 
                             y[1:(window+ x)], alpha = 1, crit = "bic") %>%
          predict(X[[i]][(window+x + 1), ])}
    )
  
}


# 3.2.2) LASSO variable selection

lasso_select <- list()

for (i in 1:length(data)) {
  
  lasso_select[[i]] <- 0:size %>%
    map(
      function(x){ 
        ic_fit <- ic.glmnet(X[[i]][1:(window+ x), ], 
                            y[1:(window+ x)], alpha = 1, crit = "bic")
        unlist(coef(ic_fit, s = "lambda.min"))[-1] %>%
          names() %>% # Extract variable names
          as.character() # Convert to character vector
      }
    )
  
}



# Combine all frequency tables into one using dplyr's bind_rows()
lasso_freq <- lasso_select %>% 
  map(~.x %>% unlist() %>% table() %>% as.data.frame() %>% 
        rename("freq" = "."))


combined_df <- bind_rows(lasso_freq, .id = "id") %>%
  group_by(id, freq) %>%  # group by common columns
  summarize(total_freq = sum(Freq))  # summarize the frequency

# Keep only unique combinations of id and freq
combined_df <- unique(combined_df[, c("id", "freq", "total_freq")])


###############
# 3.3) RIDGE ##
###############

ridge <- list()

for (i in 1:length(data)) {
  
  ridge[[i]] <- 0:size %>%
    map_dbl(
      function(x){ ic.glmnet(X[[i]][1:(window+ x), ], 
                             y[1:(window+ x)], alpha = 0, crit = "bic") %>%
          predict(X[[i]][(window+x + 1), ])}
    )
  
}



#############
# 3.4) CSR ##
#############

csr <- list()

for (i in 1:length(data)) {
  csr[[i]] <- 0:size %>%
    map_dbl(
      function(x){ csr(X[[i]][1:(window+ x), ],
                       y[1:(window+ x)]) %>%
          predict(X[[i]][(window+x+1), ])}
    )
}



#####################
# 3.5) Random Walk ##
#####################
rw <- X[[1]] %>% as.data.frame() %>% select(ipca0)

rw <- rw[(window+1):nrow(X[[1]]),1]

rw <- rep(list(rw), 12)


####################
# 3.6) Elastic Net #
####################

# Codigo igual ao do lasso. S� trocar alpha=1 por alpha=0.5

elnet <- list()

for (i in 1:length(data)) {
  
  elnet[[i]] <- 0:size %>%
    map_dbl(
      function(x){ ic.glmnet(X[[i]][1:(window+ x), ], 
                             y[1:(window+ x)], alpha = 0.5, crit = "bic") %>%
          predict(X[[i]][(window+x + 1), ])}
    )
  
}

#################
# 3.7) Adalasso #
#################

lasso_weight <- list()
step1.beta <- list()
omega <- list()
adalasso <- list()

for (i in 1:length(data)) {
  
  # Parte 1: Calculando pesos via LASSO
  
  lasso_weight[[i]] <- 0:size %>%
    map(
      function(x){ ic.glmnet(X[[i]][1:(window+ x), ], 
                             y[1:(window+ x)], alpha = 1, crit = "bic")
      }
    )
  
  step1.beta[[i]] <- 1:(size+1) %>%
    map(
      function(x){
        coef(lasso_weight[[i]][[x]])[-1]
      }
    )
  
  tau <- 1
  
  omega[[i]] <- 1:(size+1) %>%
    map(
      function(x){
        (abs(step1.beta[[i]][[x]]) + 1/sqrt(window))^(-tau)
      }
    )
  
  
  # Parte 2: Calculado adalasso
  
  adalasso[[i]] <- 0:size %>%
    map_dbl(
      function(x){ ic.glmnet(X[[i]][1:(window+ x), ], 
                             y[1:(window+ x)], alpha = 1, penalty.factor = omega[[i]][[x+1]]) %>%
          predict(X[[i]][(window+x + 1), ])}
    )
}


################################
# 4) Data Frame das previsoes ##
################################

prediction <- pmap(
  list(rf,csr,lasso), function(first,second,third){
    cbind(first, second, third)
  }
) %>%
  map(~.x %>%
        as.data.frame %>%
        rename("RF" = first,
               "CSR" = second,
               "LASSO" = third
        ))

prediction <- map2(prediction,adalasso,cbind) %>%
  map(~.x %>%
        rename("adalasso" = ".y[[i]]"))

prediction <- map2(prediction,elnet,cbind) %>%
  map(~.x %>%
        rename("ElNet" = ".y[[i]]"))

prediction <- map2(prediction,ridge,cbind) %>%
  map(~.x %>%
        rename("RIDGE" = ".y[[i]]"))

prediction <- map2(prediction,rw,cbind) %>%
  map(~.x %>%
        rename("RW" = ".y[[i]]"))








#############################
# 5) medidas de performance #
#############################

# i) root mean squared error (RMSE)
# ii) the mean absolute error (MAE) 
# iii) median absolute deviation from the median (MAD)

# PODEMOS QUERER CALCULAR essas medidas pra amostra toda 
# ou pra varias subamostras



###########################
# 5.1) Erros de previs�o ##
###########################

error <- prediction %>%
  map(~y_obs - .x)


###########################
# 5.2) Medidas dos erros ##
###########################

# usarei "." ao inves de ".x", mas tanto faz

rmse <- prediction %>%
  map(. %>%
        summarise_all(~rmse(y_obs, .))
  )

mae <- prediction %>%
  map(. %>%
        summarise_all(~mae(y_obs, .)) %>%
        round(3))

mad <- error %>%
  map(. %>%
        summarise_all(~mad(.)))

#####################################
# 5.3) Data Frame of Error Measures #
#####################################

rmse <- map(rmse,as.data.frame) %>%
  bind_rows()

mae <- map(mae,as.data.frame) %>%
  bind_rows()

mad <- map(mad,as.data.frame) %>%
  bind_rows()


#########################
# 5.4) Normalizing RMSE #
#########################

# 5.4.1) Benchmark RMSE
#focus <- head(focus,-1)#apago a ultima linha pois queria end_date = "2022-01-01", mas tive que escrever end_date = "2022-02-01" por conta de um bug
focus <- data[[1]]$focus

focus <- focus[(window+1):nrow(data[[1]])]

rmse_focus <- rmse(y_obs,focus)

# 5.4.2) Normalizing rmse with rmse_focus
rmse_normal <- rmse/rmse_focus 

# guardando a estatisticas descritivas
avg_rmse_normal <- rmse_normal %>% colMeans()
max_rmse_normal <- apply(rmse_normal, 2, max)
min_rmse_normal <- apply(rmse_normal, 2, min)


# excluo RW
rmse_normal <- rmse_normal %>%
  select(-c("RW")) 

# transposing
rmse_normal <- t(rmse_normal) %>% as.data.frame()

rownames(rmse_normal) <- NULL

########################
# 5.5) Normalizing MAE #
########################

mae_focus <- mae(y_obs,focus)

mae_normal <- mae/mae_focus 

# guardando estatisticas descritivas
avg_mae_normal <- mae_normal %>% colMeans()
max_mae_normal <- apply(mae_normal, 2, max)
min_mae_normal <- apply(mae_normal, 2, min)


mae_normal <- mae_normal %>%
  select(-c("RW")) 

# transposing
mae_normal <- t(mae_normal) %>% as.data.frame()

rownames(mae_normal) <- NULL


########################
# 5.6) Normalizing MAD #
########################

mad_focus <- mad(y_obs,focus)

mad_normal <- mad/mad_focus 

# estatisticas descritivas
avg_mad_normal <- mad_normal %>% colMeans()
max_mad_normal <- apply(mad_normal, 2, max)
min_mad_normal <- apply(mad_normal, 2, min)


mad_normal <- mad_normal %>%
  select(-c("RW")) 

# transposing
mad_normal <- t(mad_normal) %>% as.data.frame()


###########################################################
# 5.7) Tabela das medias das medidas de erro normalizadas #
###########################################################

avg_error <- rbind(avg_rmse_normal,avg_mae_normal,
                   avg_mad_normal) %>%t()

max_error <- rbind(max_rmse_normal,max_mae_normal,
                   max_mad_normal) %>% t()

min_error <- rbind(min_rmse_normal,min_mae_normal,
                   min_mad_normal) %>% t()

stats_error <- cbind(avg_error,max_error,min_error)

stargazer(stats_error,summary = F, digits = 2)












###########################################
# 5.6) Uniting rmse_normal and mae_normal #
###########################################

rmse_mae_normal <- gdata::interleave(rmse_normal, mae_normal)

rownames(rmse_mae_normal) <- c("RF", "1",
                               "CSR", "2 ",
                               "LASSO", "3 ",
                               "adalasso", "4 ",
                               "ElNet","5 ",
                               "RIDGE","6 ")

rmse_mae_normal <- round(rmse_mae_normal,2)

# Save workspace
#setwd("~/GitHub2/MONOGRAFIA_TITO/data_new")
#save.image("FINAL2workspace.RData")






##################
# 6) Output LATEX
##################
stargazer(rmse_mae_normal,summary = F, digits = 2)
