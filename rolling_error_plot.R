error_window <- 12
num_window <- size - error_window
rolling_plots <- list()

##############
# o que esta em cima so eh pra rodar uma vez.
# quando for rodar pra valores diferentes de k, so precisa rodar o codigo abaixo
#############

for (k in 1:12) {
  rolling_rmse <- rep(list(list()),ncol(prediction[[1]]))
  rolling_rmse_focus <- c()
  
  #k = 2 # acumulacao dos regressores
  
  for (i in 1:ncol(prediction[[1]])) {
    for (j in 0:num_window) {
      rolling_rmse[[i]][j+1] <- rmse(y_obs[(1+j):(error_window+j)],prediction[[k]][(1+j):(error_window+j),i])
      rolling_rmse_focus[j+1] <- rmse(y_obs[(1+j):(error_window+j)],focus[(1+j):(error_window+j)])
    }
  }
  
  rolling_rmse <- rolling_rmse %>%
    map(~.x %>% unlist)
  
  rolling_rmse <- rolling_rmse %>%
    map(~.x/rolling_rmse_focus) # normalizo pelo erro do FOCUS
  
  
  
  #############
  #############
  
  date <- data[[1]] %>% select(ref.date)
  date <- date[(nrow(date) - num_window):nrow(date),1]
  
  
  rolling_rmse <- do.call(cbind, rolling_rmse)%>%as.data.frame()
  
  rolling_rmse <- cbind(date, rolling_rmse) %>%
    rename("RF" = "V1",
           "LASSO" = "V2",
           "CSR" = "V3",
           "Ridge" = "V4",
           "RW" = "V5")
  
  
  # plot_data
  rolling_rmse_plot <- melt(rolling_rmse ,
                            id.vars = "date",
                            variable.name = 'series')
  
  # Criando periodo de sombras: 
  start_date1 <- as.Date("2015-01-01")
  end_date1 <- as.Date("2017-01-01")
  start_date2 <- as.Date("2021-03-01")
  end_date2 <- as.Date("2023-01-01")
  
  
  #####
  rolling_rmse_plot <- ggplot(rolling_rmse_plot, aes(date, value, group = series))+
    geom_rect(aes(xmin = start_date1 , xmax = end_date1 , ymin = -Inf, ymax = Inf), fill = "#E7E7E7", alpha = 0.2) +
    geom_rect(aes(xmin = start_date2, xmax = end_date2, ymin = -Inf, ymax = Inf), fill = "#E7E7E7", alpha = 0.2) +
    geom_line(aes(linetype=series, color=series))+ 
    theme_classic(base_size = 6)+
    ylab("Normalized RMSE")+
    xlab("")+
    scale_color_igv()+
    scale_x_date(date_breaks = "6 months",
                 date_labels = "%m/%Y") +
    geom_hline(yintercept = 1, linetype = "solid", color = "black")+ # add horizontal line
    theme(legend.justification=c(1,1), legend.position=c(0.25,1),
          legend.background = element_rect(fill = "white", color = "black"),
          legend.key.size = unit(0.25, "cm")) +
    theme(legend.title=element_blank())+
    ggtitle(paste("Rolling Normalized RMSE : h =", k))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.title = element_text(face="bold"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    ylim(0,3.5)
  
  
  rolling_plots[[k]] <- rolling_rmse_plot
  
}


grid.arrange(rolling_plots[[1]],
             rolling_plots[[2]],
             rolling_plots[[3]],
             rolling_plots[[4]],
             rolling_plots[[5]],
             rolling_plots[[6]],
             ncol = 3)

grid.arrange(rolling_plots[[7]],
             rolling_plots[[8]],
             rolling_plots[[9]],
             rolling_plots[[10]],
             rolling_plots[[11]],
             rolling_plots[[12]],
             ncol = 3)

grid.arrange(rolling_plots[[1]],
             rolling_plots[[3]],
             rolling_plots[[6]],
             rolling_plots[[12]],
             ncol = 2)

















