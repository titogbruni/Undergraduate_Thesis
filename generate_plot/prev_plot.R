date <- data[[1]] %>% select(ref.date)
date <- date[(window+1):nrow(data[[1]]),1] 
plot_list <- list()

# adicionando focus nas previsoes
prediction <- prediction %>%
  map(~mutate(.x, FOCUS = focus))

for (k in 1:12) {
  
  
  pred_plot <- cbind(y_obs, prediction[[k]]) %>% as.data.frame() %>% 
    rename("observed" = "y_obs")
  
  pred_plot <- cbind(date,pred_plot)
  
  pred_plot <- melt(pred_plot ,
                    id.vars = "date",
                    variable.name = 'series')
  
  x <- ggplot(pred_plot, aes(date, value, group = series))+
    geom_line(aes(linetype=series, color=series))+ 
    theme_classic(base_size = 6)+
    scale_color_manual(values = c("#000000", "#000000","#262DA2", "#1AAE37", "#CB4032", "#ff751a", "#FF00FF")) +
    ylab("12-month ahead predicted inflation")+
    scale_x_date(date_breaks = "6 months",
                 date_labels = "%m/%Y") +
    theme(legend.justification=c(1,1), legend.position=c(0.3,0.5))+
    theme(legend.background = element_rect(fill = "white", color = "black"))+
    theme(legend.title=element_blank())+
    ggtitle(paste("Predicted inflation:\n accumulating regressors in", k ,"months"))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.title = element_text(face="bold")) +
    theme(legend.key.size = unit(0.15, "cm"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    ylim(0,12)
  
  plot_list[[k]] <- x
  
}


#ggplot(pred_plot, aes(date, value,group = series))+
#  geom_line(aes(linetype=series, color=series))+ 
#  theme_classic(base_size = 6)+
#  scale_color_igv()+
#  ylab("12-month ahead predicted inflation")+
#  scale_x_date(date_breaks = "6 months",
#               date_labels = "%m/%Y") +
#  theme(legend.justification=c(1,1), legend.position=c(0.2,0.6))+
#  theme(legend.background = element_rect(fill = "white", color = "black"))+
#  theme(legend.title=element_blank())+
#  ggtitle("Predicted inflation without accumulating the regressors")+
#  theme(plot.title = element_text(hjust = 0.5))+
#  theme(plot.title = element_text(face="bold"))



grid.arrange(plot_list[[1]],
             plot_list[[2]],
             plot_list[[3]],
             plot_list[[4]],
             ncol = 2)

grid.arrange(plot_list[[5]],
             plot_list[[6]],
             plot_list[[7]],
             plot_list[[8]],
             ncol = 2)

grid.arrange(plot_list[[9]],
             plot_list[[10]],
             plot_list[[11]],
             plot_list[[12]],
             ncol = 2)


