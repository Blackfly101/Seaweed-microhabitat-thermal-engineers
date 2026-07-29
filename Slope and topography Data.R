#Microhabitat engineers: Seaweed canopies facilitate thermal microclimate across natural and artificial intertidal habitats

#Use files Topo_All.csv and Slope_.csv

# Load data
Topography <- read.csv("Topo_All.csv", sep = ";")
Slope_aspect <- read.csv("Slope_.csv", sep = ";")

#Loading libraries
library(ggplot2)
library(dplyr)
library(patchwork)



#Converting to factors
Topography$Zone <- as.factor(Topography$Zone)
Topography$Site <- as.factor(Topography$Site)
Slope_aspect$Zone <- as.factor(Slope_aspect$Zone)


# Statistical analysis ----------------------------------------------------

#Slope aspect
model_slope <- lm(Slope ~ Site * Zone,
            data = Slope_aspect)
anova(model_slope)

#Surface rugosity index
model_topography <- lm(SRI ~ Site * Zone,
                  data = Topography)
anova(model_topography)

#Plotting----

#Creating cld labels for slope and surface rugosity index

#Slope compact letter display
cld_slope <- data.frame(
  Zone = c("High", "Mid-High", "Mid-Low"),
  label = c("a", "b", "a")
)

#Topography compact letter display
cld_topo <- data.frame(
  Zone = rep(c("High", "Mid-High", "Mid-Low"), each = 2),
  Site = rep(c("Breakwater", "Wembury"), times = 3),
  label = rep(c("a", "b"), times = 3)
)

#Aligning y-position for slope and topography

#Slope
y_slope <- max(Slope_aspect$Slope, na.rm = TRUE) * 1.1
cld_slope$y <- y_slope

#Topography
topo_y <- Topography %>%
  group_by(Zone) %>%
  summarise(y = max(SRI, na.rm = TRUE) * 1.1)

cld_topo <- merge(cld_topo, topo_y, by = "Zone")

#Slope plot

p1 <- ggplot(Slope_aspect, aes(x = Zone, y = Slope, fill = Zone)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  geom_text(data = cld_slope,
            aes(x = as.numeric(factor(Zone)) - 0.25,
                y = y,
                label = label),
            inherit.aes = FALSE,
            hjust = 0,
            size = 8) +
  labs(x = "Zone",
       y = "Slope (°)") +
  theme_classic2(base_size = 20) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
  )
p1

#Topography plot

p2 <- ggplot(Topography, aes(x = Site, y = SRI, fill = Site)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~ Zone) +
  scale_fill_manual(values = c(
    "Wembury" = "turquoise",
    "Breakwater" = "orange"
  )) +
  geom_text(data = cld_topo,
            aes(x = as.numeric(factor(Site)) - 0.25,
                y = y,
                label = label),
            inherit.aes = FALSE,
            hjust = 0,
            size = 8) +
  labs(x = "Site",
       y = "SRI") +
  theme_classic2(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )+
  coord_cartesian(ylim = c(1, 1.75))

p2

#Combing plots

combined_plot <- p1 / p2

print(combined_plot)

# Save
ggsave("Figure S6.png",
       combined_plot,
       width = 12,
       height = 10,
       dpi = 320)
