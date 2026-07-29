#Microhabitat engineers: Seaweed canopies facilitate thermal microclimate across natural and artificial intertidal habitats

#Nonlinear regression (Use Correlation_Final.csv file)
data <- read.csv("Correlation_Final.csv", sep = ";")

#---loading libraries----

library(ggplot2)
library(ggpubr)

#Temperature: nonlinear least squares(nls)----


#nls() for Day 18 ° C

data_cool <- subset(data, Day == "Day 18 ° C")

nl_temp_cool <- nls(
  Temperature ~ a + b * exp(-c * Thickness),
  data = data_cool,
  start = list(
    a = min(data_cool$Temperature),
    b = max(data_cool$Temperature) - min(data_cool$Temperature),
    c = 0.1
  )
)

data_cool$pred <- predict(nl_temp_cool)

# R2
rss <- sum(residuals(nl_temp_cool)^2)
tss <- sum((data_cool$Temperature - mean(data_cool$Temperature))^2)
r2_cool_temp <- 1 - rss/tss

p_cool_temp <- summary(nl_temp_cool)$coefficients[3,4]


#nls() for Day 20 ° C

data_warm <- subset(data, Day == "Day 20 ° C")

nl_temp_warm <- nls(
  Temperature ~ a + b * exp(-c * Thickness),
  data = data_warm,
  start = list(
    a = min(data_warm$Temperature),
    b = max(data_warm$Temperature) - min(data_warm$Temperature),
    c = 0.1
  )
)

data_warm$pred <- predict(nl_temp_warm)

rss <- sum(residuals(nl_temp_warm)^2)
tss <- sum((data_warm$Temperature - mean(data_warm$Temperature))^2)
r2_warm_temp <- 1 - rss/tss

p_warm_temp <- summary(nl_temp_warm)$coefficients[3,4]


#Plotting for temperature

#Temperature plot for Day 18°C (Figure 7A)
p_temp_cool <- ggplot(data_cool, aes(Thickness, Temperature)) +
  geom_jitter(width = 0.4, height = 0.3, size = 4, alpha = 0.6) +
  geom_line(aes(y = pred), color = "blue", size = 1.4, linetype = "dashed") +
  annotate("text",
           x = Inf, y = Inf,
           label = paste0("italic(R)^2 == ", round(r2_cool_temp, 2),
                          "*','~~italic(p) == ", signif(p_cool_temp, 2)),
           parse = TRUE,
           hjust = 1.1, vjust = 1.2, size = 7) +
  theme_classic2(base_size = 25) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  labs(title = " ",
       x = "Thickness (cm) ", y = "Temperature (°C)")+
  coord_cartesian(xlim = c(0, 20), ylim = c(10, 35))+facet_grid(~Day)

p_temp_cool


#Temperature plot for Day 20 ° C (Figure 7B)
p_temp_warm <- ggplot(data_warm, aes(Thickness, Temperature)) +
  geom_jitter(width = 0.4, height = 0.3, size = 4, alpha = 0.6) +
  geom_line(aes(y = pred), color = "blue", size = 1.4, linetype = "dashed") +
  annotate("text",
           x = Inf, y = Inf,
           label = paste0("italic(R)^2 == ", round(r2_warm_temp, 2),
                          "*','~~italic(p) == ", signif(p_warm_temp, 2)),
           parse = TRUE,
           hjust = 1.1, vjust = 1.2, size = 7) +
  theme_classic2(base_size = 25) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  labs(title = " ",
       x = " Thickness (cm)", y = " ")+
  coord_cartesian(xlim = c(0, 20), ylim = c(10, 35))+facet_grid(~Day)

p_temp_warm

#Light intensity: nonlinear least squares(nls)----

# nls() for Day 18 ° C

data_cool$logIntensity <- log(data_cool$Intensity)

nl_int_cool <- nls(
  logIntensity ~ a + b * exp(-c * Thickness),
  data = data_cool,
  start = list(
    a = min(data_cool$logIntensity),
    b = max(data_cool$logIntensity) - min(data_cool$logIntensity),
    c = 0.1
  )
)

data_cool$pred_int <- predict(nl_int_cool)

rss <- sum(residuals(nl_int_cool)^2)
tss <- sum((data_cool$logIntensity - mean(data_cool$logIntensity))^2)
r2_cool_int <- 1 - rss/tss

p_cool_int <- summary(nl_int_cool)$coefficients[3,4]


#nls() for Day 18 ° C

data_warm$logIntensity <- log(data_warm$Intensity)

nl_int_warm <- nls(
  logIntensity ~ a + b * exp(-c * Thickness),
  data = data_warm,
  start = list(
    a = min(data_warm$logIntensity),
    b = max(data_warm$logIntensity) - min(data_warm$logIntensity),
    c = 0.1
  )
)

data_warm$pred_int <- predict(nl_int_warm)

rss <- sum(residuals(nl_int_warm)^2)
tss <- sum((data_warm$logIntensity - mean(data_warm$logIntensity))^2)
r2_warm_int <- 1 - rss/tss

p_warm_int <- summary(nl_int_warm)$coefficients[3,4]


#Plotting for log intensity

#Light intensity plot for Day 18°C (Figure S5A)
p_int_cool <- ggplot(data_cool, aes(Thickness, logIntensity)) +
  geom_jitter(width = 0.4, height = 0.3, size = 4, alpha = 0.6) +
  geom_line(aes(y = pred_int), color = "blue", size = 1.4, linetype = "dashed") +
  annotate("text",
           x = Inf, y = Inf,
           label = paste0("italic(R)^2 == ", round(r2_cool_int, 2),
                          "*','~~italic(p) == ", signif(p_cool_int, 2)),
           parse = TRUE,
           hjust = 1.1, vjust = 1.2, size = 7)+
  theme_classic2(base_size = 25) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  labs(title = " ",
       x = "Thickness (cm)", y = "Log light intensity (lux)")+
  coord_cartesian(xlim = c(0, 20), ylim = c(5, 15))+facet_grid(~Day)

p_int_cool

#Light intensity plot for Day 20°C (Figure 7B)

p_int_warm <- ggplot(data_warm, aes(Thickness, logIntensity)) +
  geom_jitter(width = 0.4, height = 0.3, size = 4, alpha = 0.6) +
  geom_line(aes(y = pred_int), color = "blue", size = 1.4, linetype = "dashed") +
  annotate("text",
           x = Inf, y = Inf,
           label = paste0("italic(R)^2 == ", round(r2_warm_int, 2),
                          "*','~~italic(p) == ", signif(p_warm_int, 2)),
           parse = TRUE,
           hjust = 1.1, vjust = 1.2, size = 7)+
  theme_classic2(base_size = 25) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  labs(title = " ",
       x = "Thickness (cm)", y = " ")+
  coord_cartesian(xlim = c(0, 20), ylim = c(5, 15))+facet_grid(~Day)

p_int_warm



#Combining figures for for Figure 7----

final_plot_temp<-
  ggarrange(
    p_temp_cool, p_temp_warm,
    ncol = 2,
    nrow = 1,
    labels = c("A", "B"),
    font.label = list(size = 22, face = "bold")
  )

final_plot_temp

ggsave("Figure 7.png",
       plot = final_plot_temp,
       width = 15,
       height = 7,
       dpi = 400)

#Combining figures for for Figure S5----

final_plot_light<-
  ggarrange(
    p_int_cool, p_int_warm,
    ncol = 2,
    nrow = 1,
    labels = c("A", "B"),
    font.label = list(size = 22, face = "bold")
  )

final_plot_light

ggsave("Figure S5.png",
       plot = final_plot_light,
       width = 15,
       height = 7,
       dpi = 400)

