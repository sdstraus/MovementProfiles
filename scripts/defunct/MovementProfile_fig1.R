require(holodeck)
require(dplyr)
require(scatterplot3d)
require(scales)
library(viridis)
library(RColorBrewer)


brewer.pal(3, "Dark")
setseed(100)

df1 <-
  sim_covar(n_obs = 500, n_vars = 3, cov = 0.6, var = 1.2, name = "movement")
scatterplot3d(df1$movement_1,df1$movement_2,df1$movement_3)

df2 <-
  df1 %>%
  sim_cat(n_groups = 3, name = "factor") %>%
  group_by(factor) %>%
  sim_discr(n_vars = 3, var = 0.9, cov = 0.3, group_means = rnorm(n=3, mean = c(1,3,5)),
            name = "trophic")


df2$factor <- as.factor(df2$factor)
colours <- RColorBrewer::brewer.pal(3, "Dark2")
df2$class_col <- colours[as.numeric(df2$factor)]


jpeg(filename = "figures/mock_3dscatter1.jpeg", width = 6, heigh = 4.5, units = "in", res = 1000)
scatterplot3d(df2$movement_1,df2$movement_2,df2$movement_3, color = df2$class_col, pch = 16,grid = FALSE,
              xlab = "Foraging scale",
              ylab = "Dispersal scale",
              zlab = "Migration scale")
par(mar = c(1,1,1,1))
dev.off()


setseed(150)
df3 <-
  sim_covar(n_obs = 500, n_vars = 3, cov = 0.0, var = 1.0, name = "movement")
scatterplot3d(df3$movement_1,df3$movement_2,df3$movement_3)

df4 <-
  df3 %>%
  sim_cat(n_groups = 3, name = "factor") %>%
  group_by(factor) %>%
  sim_discr(n_vars = 3, var = 1.3, cov = 0.0, group_means = rnorm(n=3, mean = c(3,3,3)),
            name = "trophic")

df4$factor <- as.factor(df4$factor)
colours <- RColorBrewer::brewer.pal(3, "Dark2")
df4$class_col <- colours[as.numeric(df4$factor)]

jpeg(filename = "figures/mock_3dscatter2.jpeg", width = 6, heigh = 4.5, units = "in", res = 300)
scatterplot3d(df4$movement_1,df4$movement_2,df4$movement_3, color = df4$class_col, pch = 16,grid = FALSE,
              xlab = "Foraging scale",
              ylab = "Dispersal scale",
              zlab = "Migration scale")
par(mar = c(1,1,1,1))
dev.off()


setseed(100)
df6 <- sim_covar(n_obs = 167, n_vars = 3, cov = -0.3, var = 0.8, name = "movement")
scatterplot3d(df6$movement_1,df6$movement_2,df6$movement_3)
df6 <- df6 %>% 
  mutate(movement_1 = movement_1) %>% 
  mutate(movement_2 = movement_2) %>% 
  mutate(movement_3 = movement_3-4)
df6$group <- "a"
df6$colour <- "#1B9E77"

df7 <-
  sim_covar(n_obs = 167, n_vars = 3, cov = 0.3, var = 0.8, name = "movement")
scatterplot3d(df7$movement_1,df7$movement_2,df7$movement_3)
df7$group <- "b"
df7$colour <- "#D95F02"

df8 <-
  sim_covar(n_obs = 167, n_vars = 3, cov = 0.4, var = 0.8, name = "movement")
scatterplot3d(df8$movement_1,df8$movement_2,df8$movement_3)
df8$group <- "c"
df8$colour <- "#7570B3"

df9 <- rbind(df6, df7, df8)

jpeg(filename = "figures/mock_3dscatter3.jpeg", width = 6, heigh = 4.5, units = "in", res = 300)
scatterplot3d(df9$movement_1,df9$movement_2,df9$movement_3, color = df9$colour, pch = 16, grid = FALSE,
              xlab = "Foraging scale",
              ylab = "Dispersal scale",
              zlab = "Migration scale")
par(mar = c(1,1,1,1))
dev.off()


