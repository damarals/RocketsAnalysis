library(tidyverse)
metrics <- read_csv("C:/Users/Daniel/Downloads/metrics.csv")

metrics <- tibble(Kernel = c(rep("RBF", 65), rep("RBF+ExpSin", 65)),
       R2 = c(metrics$R2_RBF, metrics$R2_RBFExpSin),
       MAE = c(metrics$MAE_RBF, metrics$MAE_RBFExpsin))

metrics %>% kruskal.test(R2 ~ Kernel)
metrics %>% kruskal.test(MAE ~ Kernel)
