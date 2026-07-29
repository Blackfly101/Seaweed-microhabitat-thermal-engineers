#Microhabitat engineers: Seaweed canopies facilitate thermal microclimate across natural and artificial intertidal habitats

#Use Multipatt1.csv
data <- read.csv("Multipatt1.csv", sep = ";")

#Loading libraries
library(indicspecies)
library(permute)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(RColorBrewer)

#Data formatting

abund <- data[, 6:ncol(data)]
abund <- abund + 0.001  #Avoiding zeros

data$Habitat <- as.factor(data$Habitat)   
data$Site <- as.factor(data$Site)

#Combining factors (Habitat × Site)
factor_HS <- interaction(data$Habitat, data$Site, sep = ".")

# Run multipatt ----------

set.seed(123)

inv <- multipatt(
  abund,
  factor_HS,
  func = "r.g",
  control = how(nperm = 9999)
)

summary(inv)

#Extracting results

sign <- as.data.frame(inv$sign)
sign$Species <- rownames(sign)

#standardizing column names
if (!"p.value" %in% names(sign)) {
  p_col <- grep("^p", names(sign), value = TRUE)[1]
  names(sign)[names(sign) == p_col] <- "p.value"
}

if (!"stat" %in% names(sign)) {
  stat_col <- grep("stat|r\\.g|indval", names(sign), value = TRUE)[1]
  names(sign)[names(sign) == stat_col] <- "stat"
}

#Identifying group columns
combo_cols <- grep("^s\\.", names(sign), value = TRUE)

#Assigning groups
sign$Group <- apply(sign[, combo_cols, drop = FALSE], 1, function(x) {
  
  grp <- combo_cols[which(x == max(x, na.rm = TRUE))]
  
  paste(gsub("^s\\.", "", grp), collapse = "+")
})

#Filtering the significant results
sig <- sign %>%
  filter(p.value <= 0.05) %>%
  arrange(desc(stat))

#Replacing dots with spaces so names match plotting legend
sig$Species <- gsub("\\.", " ", sig$Species)

#Plotting

#Preparing data
radial_data <- sig %>%
  
  separate_rows(Group, sep = "\\+") %>%
  
  mutate(
    Group = gsub("\\.", " - ", Group),
    Group = factor(Group),
    Species = factor(Species)
  ) %>%
  
  group_by(Group) %>%
  
  arrange(desc(stat), .by_group = TRUE) %>%
  
  mutate(Species = fct_reorder(Species, stat)) %>%
  
  ungroup()


p_radial <- ggplot(radial_data,
                   aes(x = Group, y = stat, fill = Species)) +
  
  geom_bar(
    stat = "identity",
    width = 0.9,
    color = "black",
    linewidth = 0.2   # replaces deprecated size
  ) +
  
  geom_text(
    aes(label = round(stat, 2)),
    position = position_stack(vjust = 0.5),
    size = 5,
    color = "white",
    fontface = "bold"
  ) +
  
  coord_polar(start = 0) +  
  
  scale_fill_manual(values = c(
    "Amphipods" = "lightgoldenrod3",
    "Limpets" = "wheat4",
    "Anemones" = "orangered",
    "Tube-Worm" = "slategray4",
    "Turbinate gastropods" = "lawngreen",
    "Sponges" = "purple",
    "Turf" = "sienna1"
  )) +
  
  theme_pubclean(base_size = 25) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 15,
      face = "bold"
    ),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 25),
    
    plot.title = element_text(
      face = "bold",
      size = 20,
      hjust = 0.5
    )
  ) +
  
  labs(
    title = " ",
    fill = "Taxonomic group"
  )

# Show plot
p_radial

# Save plot
ggsave(
  "Figure 8B.png",
  plot = p_radial,
  dpi = 320,
  width = 12,
  height = 10
)
