library(ggplot2)
library(dplyr)

set.seed(10)
sizes2 <- round(log2(sample(sizes, 1000)) / 10, 2)
sizes3 <- round(log2(sizes) / 10, 2)

pl <- ggplot(data = data_frame(size = sizes3)) +
  geom_density(aes(x = size))

print(pl)
