##########################################
## accumulate monthly index in k months ##
##########################################


acumula_indice_mensal = function(x,k){
  
  y <- rep(NA, length(x))
  
  for (i in k:length(x)) {
    
    if(is.na(x[i]) | is.na(x[i-k+1])){
      
      y[i] <- NA
      
    }else{
      
      y[i] <- (x[i]/x[i-k+1] - 1)*100 
      
    }
  }
  return(y)
}


################################################
## accumulates monthly variations in k months ##
################################################


acumula_var_mensal = function(x,k){
  
  y <- rep(NA, length(x))
  
  for (i in k:c(length(x))) {
    
    if(NA %in% x[c(i-k+1):i]){
      y[i] <- NA
    }else{
      y[i] <- (prod((1+x[c(i-k+1):i]/100))-1)*100
    }
  }
  return(y)
}

########################################
## selects only observation per month ##
## for data with daily frequency      ##
########################################

select_monthly <- function(x){
  
  x <- x %>%
    filter(ref.date %in% seq(as.Date("2006/1/1"), by = "month", length.out = 196))
  
}


