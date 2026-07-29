#Microhabitat engineers: Seaweed canopies facilitate thermal microclimate across natural and artificial intertidal habitats

#Use Thermal_IMAGE.csv file
#Loading data
data <- read.csv("Thermal_IMAGE.csv", sep = ";")

#Loading libraries
{
  library(ggplot2)
  library(dplyr)
  library(ggpubr)
  library(ggResidpanel)
  library(car)
  library(emmeans)
  library(lme4)
}

#Factoring data
data$Site <- as.factor(data$Site)
data$Zone <- as.factor(data$Zone)
data$Day <- as.factor(data$Day)
data$Replicate <- factor(data$Sample.ID)

data$Mean_Temp <- as.numeric(data$Mean_Temp)
data$Coldspot  <- as.numeric(data$Coldspot)
data$Hotspot   <- as.numeric(data$Hotspot)


#Statistical analysis for surface temperature (Figure 2 and Figure S1)
#Splitting data by day
data_by_day <- split(data, data$Day)

#Mean temperature model
results_temp <- lapply(names(data_by_day), function(d) {
  
  df <- data_by_day[[d]]
  
  model <- lm(Mean_Temp ~ Site * Zone, data = df)
  print(summary(model))
  print(car::Anova(model, type = "II"))
  
  #Diagnostics
  print(resid_panel(model))
  
  #Posthoc
  emm <- emmeans(model, ~ Zone | Site)
  print(pairs(emm, adjust = "tukey"))
  
  return(list(model = model, emm = emm))
})

#Minimum temperature model

results_temp_min <- lapply(names(data_by_day), function(d) {
  
  df <- data_by_day[[d]]
  
  model <- lm(Coldspot ~ Site * Zone, data = df)
  
  print(summary(model))
  print(car::Anova(model, type = "II"))
  
  #Diagnostics
  print(resid_panel(model))
  
  #Posthoc
  emm <- emmeans(model, ~ Zone | Site)
  print(pairs(emm, adjust = "tukey"))
  
  return(list(model = model, emm = emm))
})

#Maximum temperature models

results_temp_min <- lapply(names(data_by_day), function(d) {
  
  df <- data_by_day[[d]]
  
  model <- lm(Hotspot ~ Site * Zone, data = df)
  
  print(summary(model))
  print(car::Anova(model, type = "II"))
  
  #Diagnostics
  print(resid_panel(model))
  
  #Posthoc
  emm <- emmeans(model, ~ Zone | Site)
  print(pairs(emm, adjust = "tukey"))
  
  return(list(model = model, emm = emm))
})

#Plotting data (Figure 2 and Figure S2)

#SPlitting data by day
data_18 <- data %>%
  filter(Day == "Day 18 ° C")

data_20 <- data %>%
  filter(Day == "Day 20 ° C")


#Creating a common theme

custom_theme <- theme_classic2(base_size = 25) +
  theme(
    axis.text = element_text(colour = "black", size = 22, face = "bold"),
    axis.title = element_text(face = "bold", size = 22),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    strip.text = element_text(face = "bold")
  )


#Creating a function for cld y-position
add_y_positions <- function(cld_df, dat, response) {
  y_df <- dat %>%
    group_by(Site) %>%
    summarise(y = max(.data[[response]], na.rm = TRUE) * 1.10, .groups = "drop")
  
  left_join(cld_df, y_df, by = "Site")
}


#Creating Compact Letter Displays----

#Mean temperature for Day 18°C
cld_mean_18 <- data.frame(
  Site = c(rep("Breakwater", 3), rep("Wembury", 3)),
  Zone = rep(c("High", "Mid-High", "Mid-Low"), 2),
  label = c(
    "a", "a", "a",
    "a", "b", "c" 
  )
)

#Minimum temperature for Day 18°C
cld_min_18 <- data.frame(
  Site = c(rep("Breakwater", 3), rep("Wembury", 3)),
  Zone = rep(c("High", "Mid-High", "Mid-Low"), 2),
  label = c(
    "a", "a", "a",
    "a", "b", "c"   
  )
)

#Maximum temperature for Day 18°C
cld_max_18 <- data.frame(
  Site = c(rep("Breakwater", 3), rep("Wembury", 3)),
  Zone = rep(c("High", "Mid-High", "Mid-Low"), 2),
  label = c(
    "a", "a", "b",   
    "a", "b", "c" 
  )
)

#Mean temperature for Day 20°C
cld_mean_20 <- data.frame(
  Site = c(rep("Breakwater", 3), rep("Wembury", 3)),
  Zone = rep(c("High", "Mid-High", "Mid-Low"), 2),
  label = c(
    "a", "b", "c",   
    "a", "b", "b" 
  )
)

#Minimum temperature for 20°C
cld_min_20 <- data.frame(
  Site = c(rep("Breakwater", 3), rep("Wembury", 3)),
  Zone = rep(c("High", "Mid-High", "Mid-Low"), 2),
  label = c(
    "a", "b", "b",
    "a", "b", "b" 
  )
)

#Maximum temperature for 20°C
cld_max_20 <- data.frame(
  Site = c(rep("Breakwater", 3), rep("Wembury", 3)),
  Zone = rep(c("High", "Mid-High", "Mid-Low"), 2),
  label = c(
    "a", "b", "c",   
    "a", "b", "b" 
  )
)

#Adding y-positions for each temperature variable and day
cld_mean_18 <- add_y_positions(cld_mean_18, data_18, "Mean_Temp")
cld_min_18  <- add_y_positions(cld_min_18,  data_18, "Coldspot")
cld_max_18  <- add_y_positions(cld_max_18,  data_18, "Hotspot")

cld_mean_20 <- add_y_positions(cld_mean_20, data_20, "Mean_Temp")
cld_min_20  <- add_y_positions(cld_min_20,  data_20, "Coldspot")
cld_max_20  <- add_y_positions(cld_max_20,  data_20, "Hotspot")

#Boxplot function
make_box_plot <- function(dat, y_var, y_lab, cld_df, fill_palette) {
  ggplot(dat, aes(x = Zone, y = .data[[y_var]], fill = Site)) +
    geom_boxplot(outlier.shape = NA, color = "black", alpha = 0.8) +
    facet_wrap(~Site) +
    scale_fill_brewer(palette = fill_palette) +
    geom_text(
      data = cld_df,
      aes(
        x = as.numeric(factor(Zone)) - 0.25,
        y = y,
        label = label
      ),
      inherit.aes = FALSE,
      hjust = 0,
      size = 10
    ) +
    labs(x = " ", y = y_lab) +
    custom_theme
}

#Boxplot for 18°C
Image_mean_18 <- make_box_plot(
  dat = data_18,
  y_var = "Mean_Temp",
  y_lab = "Mean Temperature (°C)",
  cld_df = cld_mean_18,
  fill_palette = "Greens"
)

Image_min_18 <- make_box_plot(
  dat = data_18,
  y_var = "Coldspot",
  y_lab = "Minimum Temperature (°C)",
  cld_df = cld_min_18,
  fill_palette = "Blues"
)

Image_max_18 <- make_box_plot(
  dat = data_18,
  y_var = "Hotspot",
  y_lab = "Maximum Temperature (°C)",
  cld_df = cld_max_18,
  fill_palette = "Reds"
)

combined_plot_18 <- ggarrange(
  Image_mean_18, Image_min_18, Image_max_18,
  ncol = 1, nrow = 3,
  labels = c("A", "B", "C"),
  font.label = list(size = 25, face = "bold", color = "black")
)

combined_plot_18

ggsave(
  filename = "Figure S2.png",
  plot = combined_plot_18,
  width = 15,
  height = 18,
  dpi = 320,
  bg = "white"
)


#Boxplot for 20°C
Image_mean_20 <- make_box_plot(
  dat = data_20,
  y_var = "Mean_Temp",
  y_lab = "Mean Temperature (°C)",
  cld_df = cld_mean_20,
  fill_palette = "Greens"
)

Image_min_20 <- make_box_plot(
  dat = data_20,
  y_var = "Coldspot",
  y_lab = "Minimum Temperature (°C)",
  cld_df = cld_min_20,
  fill_palette = "Blues"
)

Image_max_20 <- make_box_plot(
  dat = data_20,
  y_var = "Hotspot",
  y_lab = "Maximum Temperature (°C)",
  cld_df = cld_max_20,
  fill_palette = "Reds"
)

combined_plot_20 <- ggarrange(
  Image_mean_20, Image_min_20, Image_max_20,
  ncol = 1, nrow = 3,
  labels = c("A", "B", "C"),
  font.label = list(size = 25, face = "bold", color = "black")
)

combined_plot_20

ggsave(
  filename = "Figure 2.png",
  plot = combined_plot_20,
  width = 15,
  height = 18,
  dpi = 320,
  bg = "white"
)
