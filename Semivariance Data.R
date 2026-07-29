#Microhabitat engineers: Seaweed canopies facilitate thermal microclimate across natural and artificial intertidal habitats

#Calculating semivariance (Use Logger_Data.csv data)

df <- read.csv("Loggger_Data.csv", header = TRUE, sep = ';')

#Loading libraries----
{
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggpubr)
}

#Computing variogram for temperature by Day-----
data <- df

compute_variogram_temp <- function(df, max_lag = 30) {
  n <- nrow(df)
  
  lag_vec <- 1:max_lag
  gamma <- numeric(max_lag)
  se <- numeric(max_lag)
  
  for (lag in lag_vec) {
    diff <- (df$Temp[(lag + 1):n] - df$Temp[1:(n - lag)])^2
    
    gamma[lag] <- mean(diff, na.rm = TRUE) / 2
    se[lag] <- sd(diff, na.rm = TRUE) / (2 * sqrt(length(diff)))
  }
  
  data.frame(
    lag = lag_vec,
    gamma = gamma,
    lower = gamma - 1.96 * se,
    upper = gamma + 1.96 * se
  )
}

#Computing variogram for light intensity by Day----

compute_variogram_light <- function(df, max_lag = 30) {
  n <- nrow(df)
  
  lag_vec <- 1:max_lag
  gamma <- numeric(max_lag)
  se <- numeric(max_lag)
  
  for (lag in lag_vec) {
    diff <- (df$Intensity[(lag + 1):n] - df$Intensity[1:(n - lag)])^2
    
    gamma[lag] <- mean(diff, na.rm = TRUE) / 2
    se[lag] <- sd(diff, na.rm = TRUE) / (2 * sqrt(length(diff)))
  }
  
  data.frame(
    lag = lag_vec,
    gamma = gamma,
    lower = gamma - 1.96 * se,
    upper = gamma + 1.96 * se
  )
}

vario_temp_list <- list()
vario_light_list <- list()

for (site in unique(data$Site)) {
  for (day in unique(data$Day)) {          
    for (zone in unique(data$Zone)) {
      for (habitat in unique(data$Habitat)) {
        
        subset_data <- data %>%
          filter(Site == site,
                 Day == day,
                 Zone == zone,
                 Habitat == habitat)
        
        if (nrow(subset_data) > 30) {
          
          vt <- compute_variogram_temp(subset_data)
          vt$Site <- site
          vt$Day <- day          
          vt$Zone <- zone
          vt$Habitat <- habitat
          vario_temp_list[[length(vario_temp_list)+1]] <- vt
          
          vl <- compute_variogram_light(subset_data)
          vl$Site <- site
          vl$Day <- day         
          vl$Zone <- zone
          vl$Habitat <- habitat
          vario_light_list[[length(vario_light_list)+1]] <- vl
        }
      }
    }
  }
}

vario_temp <- bind_rows(vario_temp_list)
vario_light <- bind_rows(vario_light_list)

#Colouring by habitat----
habitat_colors <- c(
  "Exposed" = "grey63",
  "Under Fucoids" = "darkolivegreen2"
)



#Plotting variogram for temperature Figure 6----
plot_temp_vario <- ggplot(vario_temp,
                          aes(lag, gamma,
                              color = Habitat,
                              linetype = Zone,
                              group = interaction(Habitat, Zone))) +
  
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "grey70",
              alpha = 0.25,
              color = NA) +
  
  geom_line(size = 2) +
  
  scale_color_manual(values = habitat_colors) +
  
  labs(x = "Time (min)", y = "Semivariance (°C²)") +
  
  theme_classic2(base_size = 35) +
  
  facet_grid(Site ~ Day, switch = "y") +
  
  theme(legend.key.width = unit(2, "cm"),
    axis.text = element_text(size = 25, face = "bold"),
    axis.title = element_text(size = 25, face = "bold"),
    strip.text = element_text(size = 25, face = "bold"),
    legend.position = "top"
  )
plot_temp_vario

ggsave("Figure 6.png",
       plot_temp_vario,
       width = 18,
       height = 11,
       dpi = 350)

#Plotting variogram for light intensity (Figure S4)----
plot_light_vario <- ggplot(vario_light,
                           aes(lag, log(gamma),
                               color = Habitat,
                               linetype = Zone,
                               group = interaction(Habitat, Zone))) +
  
  geom_ribbon(aes(ymin = log(lower), ymax = log(upper)),
              fill = "grey",
              alpha = 0.25,
              color = NA) +
  
  geom_line(size = 2) +
  
  scale_color_manual(values = habitat_colors) +
  
  labs(x = "Time (min)", y = "Log Semivariance (lux²)") +
  
  theme_classic2(base_size = 35) +
  
  facet_grid(Site ~ Day, switch = "y") +
  
  theme(legend.key.width = unit(2, "cm"),
    axis.text = element_text(size = 25, face = "bold"),
    axis.title = element_text(size = 25, face = "bold"),
    strip.text = element_text(size = 25, face = "bold"),
    legend.position = "top"
  )

plot_light_vario

ggsave("Figure S4.png",
       plot_light_vario,
       width = 18,
       height = 11,
       dpi = 350)
