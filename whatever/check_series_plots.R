library(ggplot2)
library(grid)

load("~/MONOGRAFIA_TITO/my_workspace.RData")

setwd("~/MONOGRAFIA_TITO")

df = data[[6]]

plots <- lapply(names(df)[-1], function(col) {
  plot(df$ref.date, df[[col]], type = "l", main = col, xlab = "ref.date")
})

# Open a PDF file for saving the plots
pdf("my_plots.pdf")

# Loop through the list of plots and print each one to the PDF file
for (i in 1:6) {
  print(plots[[i]])
}

# Close the PDF file
dev.off()






