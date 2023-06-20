#####################
# GERANDO AS BASES ##
#####################

# 1) Carregue bibliotecas

library(GetBCBData)
library(rbcb)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)

# 2) Caminho para a pasta
setwd("~/GitHub2/MONOGRAFIA_TITO")

# 3) chamando o .xlsx que descreve as variaveis (lags e transformacoes)
variable_description <- read_excel("variaveis_descricao.xlsx")




###################
## PRICES/MONEY ###
###################

prices = GetBCBData::gbcbd_get_series(
  c(ipca = 433,
    ipca_ali = 1635,
    ipca_hab = 1636,
    ipca_resid = 1637,
    ipca_vest = 1638,
    ipca_transp = 1639,
    ipca_comunic = 1640,
    ipca_saude = 1641,
    ipca_desp = 1642,
    ipca_educ = 1643,
    
    ipc_BR = 191,
    igp_M = 189,
    igp_DI = 190,
    igp10 = 7447,
    ipca15 = 7478,
    
    bm_broad = 1788, 
    bm = 1785, 
    m1 = 1783, 
    m2 = 1786, 
    m3 = 27813,
    m4 = 27815
  ),
  
  first.date = '2006-01-15',
  last.date = '2023-01-01',
  format.data = 'wide'
)


#######################
## COMMODITIE PRICES ##
#######################

commodities <- GetBCBData::gbcbd_get_series(
  c(icbr = 27574,
    icbr_agr = 27575,
    icbr_metal = 27576,
    icbr_energy = 27577),
  
  first.date = '2006-01-15',
  last.date = '2023-01-01',
  format.data = 'wide'
)

#######################
## ECONOMIC ACTIVITY ##
#######################

real_sector <- GetBCBData::gbcbd_get_series(
  c(ibcbr = 24363,
    pimpf = 21859,
    pimpf_extract = 21861,
    pimpf_manufac = 21862, 
    retail_total = 1455,
    retail_fuel = 1483,
    retail_supermarket = 1496,
    retail_clothing = 1509,
    retail_house = 1522,
    retail_drugstore = 20099,
    retail_paper = 20101,
    retail_office = 20102,
    retail_others = 20104,
    retail_building = 20105,
    retail_auto = 1548,
    prod_vehicles = 1373, 
    prod_agr_mach = 1388, 
    vehicle_sales = 7389, 
    tcu = 24352), 
  
  first.date = '2006-01-15',
  last.date = '2023-01-01',
  format.data = 'wide'
)


################
## EMPLOYMENT ##
################

# Vamos criar uma comb lin de desemprego pois ha periodos em que so uma
# dentre as duas medidas de desemprego existe

#pme
unem1 <- GetBCBData::gbcbd_get_series(c(unem1 = 10777),
                                      first.date = '2006-01-15',
                                      last.date = '2023-01-01',
                                      format.data = 'wide')
# pnadc
unem2 <- GetBCBData::gbcbd_get_series(c(unem2 = 24369),
                                      first.date = '2006-01-15',
                                      last.date = '2023-01-01',
                                      format.data = 'wide')

# faco media movel de 3 meses do desemprego da unem1 (pme):
unem1$unem1 <- c(NA, NA, rollmean(unem1$unem1, 3))
unem1 <- unem1[-c(1:2),] # tiando as duas primeiras observacoes, que sao NAs

# junto unem1 e unem2 para criar unem a partir dessas duas:
unem <- full_join(unem1, unem2, by = "ref.date")

unem$unem <- rep(NA, nrow(unem)) # crio coluna de NAs

peso <- 0

for (i in 1:nrow(unem)) {
  if (unem$ref.date[i] < "2012-03-01") { # periodo em que so existe unem1
    unem$unem[i] <- unem$unem1[i]
  }
  if (unem$ref.date[i] >= "2012-03-01" & unem$ref.date[i] < "2016-03-01") { # periodo em que existe unem1 e unem2
    peso <- 1/48 + peso # 48 eh o numero de observacoes no periodo
    unem$unem[i] <- (1 - peso) * unem$unem1[i] + peso * unem$unem2[i] # comeca dando mais peso pra unem1 e depois reverte
  }
  if (unem$ref.date[i] >= "2016-03-01") {
    unem$unem[i] <- unem$unem2[i]
  }
}

rm(i)

unem <- unem %>% select(ref.date, unem)

employment <- GetBCBData::gbcbd_get_series(
  c(min_wage = 1619, 
    aggreg_wage = 10790),
  
  first.date = '2006-01-15',
  last.date = '2023-01-01',
  format.data = 'wide'
)

employment <- full_join(employment, unem, by = "ref.date")



#################
## ELECTRICITY ##
#################

electricity <- GetBCBData::gbcbd_get_series(
  c(elec = 1406,
    elec_com = 1402,
    elec_res = 1403,
    elec_ind = 1404),
  
  first.date = '2006-01-15',
  last.date = '2023-01-01',
  format.data = 'wide'
)

################
## CONFIDENCE ##
################

confidence <- GetBCBData::gbcbd_get_series(
  c(cons_confidence = 4393,
    future_expec = 4395),
  
  first.date = '2006-01-15',
  last.date = '2023-01-01',
  format.data = 'wide'
)

#############
## FINANCE ##
#############

ibovespa <- read_excel("ibovespa.xls") %>%
  filter(Data >2005.12)

ibovespa <- ibovespa[,2]

colnames(ibovespa) <- c("ibovespa")

finance <- GetBCBData::gbcbd_get_series(
  
  c(irf_m = 12461,
    ima_s = 12462, 
    ima_b = 12466,  
    ima = 12469, 
    saving_deposits = 1838,
    selic = 4390, 
    cdi = 4391, 
    tjlp = 256 
  ),
  
  first.date = '2006-01-15',
  last.date = '2023-01-01',
  format.data = 'wide'
  
) %>% select_monthly() 


finance <- finance %>%
  mutate(ibovespa)


############
## CREDIT ##
############

credit = GetBCBData::gbcbd_get_series(
  c(cred_total = 28183,
    cred_gdp = 28215, 
    indebt_house_19882 = 19882, # %
    indebt_house_20400 = 20400), # %
  
  first.date = '2006-01-15',
  last.date = '2023-01-01',
  format.data = 'wide'
)


################
## GOVERNMENT ##
################

gov <- GetBCBData::gbcbd_get_series(
  c(net_debt_gdp = 4513, 
    net_debt = 4478, 
    net_debt_fedgov_bcb = 4468,
    net_debt_states = 4472,
    net_debt_cities = 4473,
    primary_result = 4649,
    debt_fedgov_old = 4502,
    debt_fedgov_new = 13761,
    treasury_emit = 4151, 
    treasury_mkt = 4154, 
    treasury_term = 10616,
    treasury_dur = 10617), 
  
  first.date = '2006-01-15',
  last.date = '2023-01-01',
  format.data = 'wide'
)


################################
## EXCHANGE AND INTERNATIONAL ##
################################

inter <- GetBCBData::gbcbd_get_series(
  c(reer = 11752, 
    usd_brl_end = 3695, 
    usd_brl_avg = 3697, 
    current_account = 22701, 
    trade_balance = 22707, 
    imports = 22709),
  
  first.date = '2006-01-15',
  last.date = '2023-01-01',
  format.data = 'wide'
)



for (k in 2:12) {
  

########################
## unindo dataframes ###
########################

df <- list(prices, commodities, real_sector, employment, electricity,
           confidence,finance, credit, gov, inter) %>% 
  reduce(full_join, by='ref.date')

#################################################################
# 1) extract the variable names from the columns of df
# 2) extract names within the column "variables" in the .xlsl file
# Later we will create a correspondence between the names 
################################################################

# Excluo ref.date e ipca do loop
names_df <- colnames(df[,-(1:2)])

names_variables <- variable_description$variable

##########
# lags ###
##########

for (i in 1:length(names_df)) {
  
  position <- which(names_variables == names_df[i])
  
  lag_value <- variable_description$lag[position]
  
  name <- names_df[i]
  
  df[,name] <- lag(df[,name],n = lag_value)
}

rm(i)














# 4) defina valor de "k": quantos periodos voce quer acumular as variaveis (ou tirar a
#    diferenca X(t) - X(t-k))

#k <- 1
  
  
  ##############################
  ## applying transformations ##
  ##############################
  
  ######################
  # Transformation 1: ##
  ######################
  
  cols_1 <- c("bm_broad",
              "bm",
              "m1",
              "m2",
              "m3",
              "m4",
              "icbr",
              "icbr_agr",
              "icbr_metal",
              "icbr_energy",
              "ibcbr",
              "pimpf",
              "pimpf_extract",
              "pimpf_manufac",
              "retail_total",
              "retail_fuel",
              "retail_supermarket",
              "retail_clothing",
              "retail_house",
              "retail_drugstore",
              "retail_paper",
              "retail_office",
              "retail_others",
              "retail_building",
              "retail_auto",
              "prod_vehicles",
              "prod_agr_mach",
              "vehicle_sales",
              "min_wage",
              "aggreg_wage",
              "elec",
              "elec_com",
              "elec_res",
              "elec_ind",
              "cons_confidence",
              "future_expec",
              "irf_m",
              "ima_s",
              "ima_b",
              "ima",
              "saving_deposits",
              "cred_total",
              "debt_fedgov_old",
              "debt_fedgov_new",
              "treasury_emit",
              "treasury_mkt",
              "reer",
              "usd_brl_end",
              "usd_brl_avg",
              "current_account",
              "trade_balance",
              "imports")
  
  df <- df %>%
    mutate_at(vars(all_of(cols_1)), ~acumula_indice_mensal(., k))
  
  #######################
  # Transformation 2: ###
  #######################
  
  cols_2 <- c("ipca_ali",
              "ipca_hab",
              "ipca_resid",
              "ipca_vest",
              "ipca_transp",
              "ipca_comunic",
              "ipca_saude",
              "ipca_desp",
              "ipca_educ",
              "ipc_BR",
              "igp_M",
              "igp_DI",
              "igp10",
              "ipca15")
  
  df <- df %>%
    mutate_at(vars(all_of(cols_2)), ~acumula_var_mensal(., k))
  
  #######################
  # Transformation 3: ###
  #######################
  
  cols_3 <- c("tcu",
              "unem",
              "cred_gdp",
              "indebt_house_19882",
              "indebt_house_20400",
              "net_debt_gdp",
              "net_debt",
              "net_debt_fedgov_bcb",
              "net_debt_states",
              "net_debt_cities",
              "primary_result",
              "treasury_term",
              "treasury_dur")
  
  
  df <- df %>% 
    mutate_at(vars(all_of(cols_3)), ~ c(rep(NA,k-1),diff(., k-1)))
  
  #######################################################
  # Anualizando ipca e adicionando lags como regressores#
  #######################################################
  
  df[,"ipca"] <- acumula_var_mensal(df[,"ipca"],12)
  
  # Como o ipca só é divulgado em "t+1", entao farei o valor
  # realizado x{t} aparecer apenas em x{t+1}. Dessa forma, o valor x{t}
  # passa a ser o valor disponivel em "t" e nao o valor realizado.
  # Chamarei o valor disponível em "t" de ipca0.
  
  df <- df %>% mutate(ipca0 = lag(ipca,1),
                      ipca1 = lag(ipca,2),
                      ipca2 = lag(ipca,3),
                      ipca3 = lag(ipca,4))
  
  
  ######################################
  # Ajeitando datas e a coluna do ipca #
  ######################################
  
  # quero deixar coluna do ipca um ano a frente, logo:
  
  df <- df %>%
    mutate(ipca = lead(ipca,11)) # usar comando de alguma funcao de dates
  
  # por fim, para deixar as linhas baseadas nas datas das observacoes do ipca, como
  # trouxe o ipca 12 casas pra baixo, fareio mesmo com as datas;
  df$ref.date <- df$ref.date %m+% months(11)
  
  
  #####################
  # ADICIONANDO FOCUS #
  #####################

  focus <- get_twelve_months_inflation_expectations(indic = c("IPCA"),
                                                    start_date = "2005-02-01",
                                                    end_date = "2022-02-01") %>%
    filter(smoothed == "N",
           base == 0) %>%
    select(date,median) %>%
    group_by(year(date), month(date)) %>%
    filter(day(date) == max(day(date)))%>%
    ungroup() %>%
    select(date,median)%>%
    arrange(date)
  
  # forçando dias a serem registrados como "01"
  day(focus$date) <- 01
  
  # Ex: 2005-02-01 tem previsao pra 2006-01-01. Logo, como vou mergear com o df
  #     cujas datas foram movidas 11 periodos a frente, farei o mesmo aqui e depois
  #     darei um merge pela coluna das dates pra evitar confusao (q poderia ocorrer se usasse um cbind)
  
  focus <- focus %>% 
    mutate(date = date %m+% months(11)) %>%
    rename("ref.date" = "date",
           "focus" = "median")
  
  # merging
  df <- merge(df, focus, by = "ref.date")
  
  
  #####################
  # Ajustes finais ###
  ####################
  
  
  # como ultimas 12 linhas nao tem observacoes de ipca, posso deleta-las
  #df <- head(df, -12)
  
  # Apago as 12 primeiras linhas para que todos os dataframes (indpendente dos meses acumulados)
  # tenham o mesmo tamanho.
  
  df <- df[-c(1:12),]
  
  # ajeitando rownames
  df <- remove_rownames(df)
  
  # ver dataframe df
  
  saveRDS(df, file = paste0("df",k,".rds"))
  
}  


  