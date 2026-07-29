#---Loading data (Use Logger_Data.csv file)----
Data <- read.csv("Loggger_Data.csv", header = TRUE, sep = ';')
#----Loading Packages----
{
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lme4)
library(car)
library(emmeans)
library(ggResidpanel)
library(patchwork)
}
#----Colouring habitats and factoring----
{habitat_colors <- c("Under Fucoids" = "darkolivegreen2", "Exposed" = "grey63")
Data$Time <- as.numeric(Data$Time)
}
# LINE PLOTS
# Plots for temperature 18 and 20 C  --------------------------------------

#Data averaging at 5mins intervals
data_line_light <- Data %>%
  mutate(Time = floor(Time/5) * 5) %>%
  group_by(Site, Zone, Time, Day,Habitat) %>%
  summarise(
    Mean_Temp = mean(Temp, na.rm = TRUE),
    .groups = "drop"
  )

View(data_line_light)

#Line plot (Figure 5A)
line_temp <- ggplot(data_line_light,
                    aes(x = Time, y = Mean_Temp,
                        color = Habitat,
                        linetype = Zone,
                        group = interaction(Habitat, Zone))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = habitat_colors) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(x = "Time (min)", y = "Temperature (°C)") +
  facet_grid(Site ~ Day, switch = "y") +
  theme_classic2(base_size = 28) +
  theme(
    axis.text = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    strip.text = element_text(face = "bold", size = 20),
    legend.key.width = unit(3, "cm")
  )
line_temp

## Adding CLD LABELS for figure 5B

cld_df <- Data %>%
  group_by(Site, Day, Zone, Habitat) %>%
  summarise(
    label = ifelse(Habitat == "Exposed", "a", "b"),
    y = max(Temp, na.rm = TRUE) + 2,
    .groups = "drop"
  )


#Boxplot (Figure 5B)

box_temp <- ggplot(Data, aes(x = Zone, y = Temp, fill = Habitat)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  geom_text(data = cld_df,
            aes(x = Zone, y = y, label = label, group = Habitat),
            position = position_dodge(width = 0.75),
            size = 8) +
  scale_fill_manual(values = habitat_colors) +
  facet_grid(Site ~ Day, switch = "y") +
  labs(y = "Temperature (°C)", x = "Zone") +
  theme_classic2(base_size = 28) +
  theme(
    axis.text = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    strip.text = element_text(face = "bold", size = 20),
    legend.position = "none"
  )

box_temp

#Combing line and boxplot for Figure 5
final_plot_temp <- (line_temp / box_temp &
                      theme(legend.position = "top")) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(size = 22, face = "bold")
  )

ggsave("Thermologgers_Temperature_27July.png",
       final_plot_temp,
       width = 16, height = 18, dpi = 350)


# Three way light Intensity plotting for 20° C Day --------------------------------------

#Data averaging at 5mins intervals
data_line_light_20 <- Data %>%
  filter(Day == "Day 20 ° C") %>%
  mutate(Time = floor(Time/5) * 5) %>%
  group_by(Site, Zone, Habitat, Time, Day) %>%
  summarise(
    Mean_light = mean(log(Intensity), na.rm = TRUE),
    .groups = "drop"
  )

data_line_light_20


#Line plot for Figure S3E
line_light_20 <- ggplot(data_line_light_20,
                        aes(x = Time, y = Mean_light,
                            color = Habitat,
                            linetype = Zone,
                            group = interaction(Habitat, Zone))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = habitat_colors) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(x = "Time (min)", y = "Log Light Intensity (lux)") +
  facet_grid(Site ~ Day, switch = "y") +
  theme_classic2(base_size = 28) +
  theme(
    axis.text = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    strip.text = element_text(face = "bold", size = 20),
    legend.key.width = unit(2, "cm")
  )

line_light_20

#Box plot for Figure S3F with CLDs
cld_df_20 <- Data %>%
  filter(Day == "Day 20 ° C") %>%
  group_by(Site, Day, Zone, Habitat) %>%
  summarise(
    label = ifelse(Habitat == "Exposed", "a", "b"),
    y = max(log(Intensity), na.rm = TRUE) + 2,
    .groups = "drop"
  )

box_light_20 <- Data %>%
  filter(Day == "Day 20 ° C") %>%
  mutate(log_light = log(Intensity))


box_light_20_plot <- ggplot(box_light_20,
                            aes(x = Zone, y = log_light, fill = Habitat)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  geom_text(data = cld_df_20,
            aes(x = Zone, y = y, label = label, group = Habitat),
            position = position_dodge(width = 0.75),
            size = 8) +
  scale_fill_manual(values = habitat_colors) +
  labs(y = "Log Light Intensity (lux)", x = "Zone") +
  facet_grid(Site ~ Day, switch = "y")+
  theme_classic2(base_size = 28) +
  theme(legend.key.width = unit(2, "cm"),
        axis.text = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 20, face = "bold"),
        strip.text = element_text(face = "bold", size = 20),
        legend.position = "none"
  )+
  coord_cartesian(ylim = c(5, 15))

box_light_20_plot


# Two way light Intensity plotting 18 C- Habitat x Zone; Habitat x Site -----------------

#Data averaging at 5mins intervals
data_light_18 <- Data %>%
  filter(Day == "Day 18 ° C") %>%
  mutate(
    Time = floor(Time/5) * 5,
    log_light = log(Intensity)
  )

line_zone_18 <- data_light_18 %>%
  group_by(Zone, Habitat, Time, Day) %>%
  summarise(Mean_light = mean(log_light, na.rm = TRUE), .groups = "drop")


#Line plot for Figure S3A (zone effect)
p_line_zone <- ggplot(line_zone_18,
                      aes(x = Time, y = Mean_light,
                          color = Habitat,
                          linetype = Zone,
                          group = interaction(Habitat, Zone))) +
  geom_line(size = 1.2) + facet_grid(~Day, switch = "y")+
  scale_color_manual(values = habitat_colors) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(x = "Time (min)", y = "Log Light Intensity (lux)") +
  theme_classic2(base_size = 28)+
  theme(
    axis.text = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    strip.text = element_text(face = "bold", size = 20),
    legend.key.width = unit(2, "cm")
  )

p_line_zone

#Boxplot for Figure S3B (zone effect) with CLDs for light intensity plot
cld_zone_18 <- data_light_18 %>%
  group_by(Zone, Habitat, Day) %>%
  summarise(
    label = ifelse(Habitat == "Exposed", "a", "b"),
    y = max(log_light, na.rm = TRUE) + 2,
    .groups = "drop"
  )

p_box_zone <- ggplot(data_light_18,
                     aes(x = Zone, y = log_light, fill = Habitat)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.75)) +
  geom_text(data = cld_zone_18,
            aes(x = Zone, y = y, label = label, group = Habitat),
            position = position_dodge(0.75),
            size = 8) + facet_grid(~Day, switch = "y")+
  scale_fill_manual(values = habitat_colors) +
  theme_classic2(base_size = 28) +
  theme(legend.position = "none")+ labs(x= "Zone",y = "Log Light Intensity (lux)")+
  theme(legend.key.width = unit(2, "cm"),
        axis.text = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        strip.text = element_text(face = "bold", size = 20)
  )

p_box_zone

#Line plot for Figure S3C (site effect)
line_site_18 <- data_light_18 %>%
  group_by(Site, Habitat, Time, Day) %>%
  summarise(Mean_light = mean(log_light, na.rm = TRUE), .groups = "drop")

p_line_site <- ggplot(line_site_18,
                      aes(x = Time, y = Mean_light,
                          color = Habitat,
                          group = interaction(Habitat, Site))) +
  geom_line(size = 1.2) + facet_grid(Site~Day, switch = "y")+
  scale_color_manual(values = habitat_colors) +
  labs(x = "Time (min)", y = "Log Light Intensity (lux)") +
  theme_classic2(base_size = 28)+  
  theme(
    axis.text = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    strip.text = element_text(face = "bold", size = 20)
  )

p_line_site

#Box plot for Figure S3C (zone effect)

cld_site_18 <- data_light_18 %>%
  group_by(Site, Habitat, Day) %>%
  summarise(
    label = ifelse(Habitat == "Exposed", "a", "b"),
    y = max(log_light, na.rm = TRUE) + 1,
    .groups = "drop"
  )

p_box_site <- ggplot(data_light_18,
                     aes(x = Site, y = log_light, fill = Habitat)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.75)) +
  geom_text(data = cld_site_18,
            aes(x = Site, y = y, label = label, group = Habitat),
            position = position_dodge(0.75),
            size = 8) + facet_grid(~Day, switch = "y")+
  scale_fill_manual(values = habitat_colors) +
  theme_classic2(base_size = 28) +
  theme(legend.position = "none")+labs(x = "Site", y = "Log Light Intensity (lux)")+
  theme(legend.key.width = unit(2, "cm"),
        axis.text = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        strip.text = element_text(face = "bold", size = 20)
  )

p_box_site

#Combing plots (Figure S3)

final_plot_light_18 <- (
  (p_line_zone | p_box_zone) /
    (p_line_site | p_box_site)/
    (line_light_20 | box_light_20_plot)) +
  plot_layout(guides = "keep") +
  plot_annotation(tag_levels = "A") &
  theme(legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        legend.position = "top",
        plot.tag = element_text(size = 25, face = "bold")
  )

final_plot_light_18

ggsave(
  filename = "final_plot_light1_27 July.png",
  plot = final_plot_light_18,
  width = 23,
  height = 25,
  dpi = 400
)


# Statistical Analysis per day----


# Split data
data_by_day <- split(Data, Data$Day)


# TEMPERATURE MODELS


results_temp <- lapply(names(data_by_day), function(d) {
  
  df <- data_by_day[[d]]
  
  model <- lm(Temp ~ Habitat * Site * Zone, data = df)
  
  print(summary(model))
  print(Anova(model, type = "III"))
  
  # Diagnostics
  print(resid_panel(model))
  
  # Posthoc
  emm <- emmeans(model, ~ Habitat | Site * Zone)
  print(pairs(emm, adjust = "tukey"))
  
  return(list(model = model, emm = emm))
})



# LIGHT MODELS


results_light <- lapply(names(data_by_day), function(d) {
  
  df <- data_by_day[[d]]
  
  model <- lm(log(Intensity) ~ Habitat * Site * Zone, data = df)
  
  print(summary(model))
  
  print(Anova(model, type = "III"))
  
  # Diagnostics
  print(resid_panel(model))
  
  # Posthoc
  emm <- emmeans(model, ~ Habitat | Zone*Site)
  print(pairs(emm, adjust = "tukey"))
  
  return(list(model = model, emm = emm))
})
