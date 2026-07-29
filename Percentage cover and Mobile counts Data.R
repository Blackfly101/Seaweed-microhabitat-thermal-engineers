#Microhabitat engineers: Seaweed canopies facilitate thermal microclimate across natural and artificial intertidal habitats

#Loading libraries----
library(ggplot2)
library(ggpubr)
library(dplyr)

#Percentage cover (Figure 3A; Use Percentage_Final_bar.csv file)----

Data <- read.csv("Percentage_Final_bar.csv", sep = ";")

Data <- Data %>%
  mutate(
    Site = as.factor(Site),
    Zone = as.factor(Zone),
    Functional_1 = as.factor(Functional_1),
    Percentage.cover = as.numeric(Percentage.cover)
  )

Cover_bar <- ggplot(Data,
                    aes(x = Zone, y = Percentage.cover/15, fill = Functional_1)) +
  geom_bar(stat = "identity", width = 0.8) +
  facet_wrap(~Site, scales = "free_x") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
  scale_fill_manual(values = c("Barnacle" = "grey", 
                               "Canopy" = "darkgreen",
                               "Turf" = "sienna1")) +  # replace/add your functional group colors
  labs(y = "Cover (%)", x = NULL, fill = "Biogenic habitat") +
  theme_classic2(base_size = 30) +
  theme(
    axis.text = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 28)
  )

Cover_bar


# Mobile species (Figure 3B; Use Mobile_Abundance.csv file)----

Data_ <- read.csv("Mobile_Abundance.csv", sep = ";")

Data_ <- Data_ %>%
  mutate(
    Site = as.factor(Site),
    Zone = as.factor(Zone),
    Functional = as.factor(Functional),
    Count = as.numeric(Count)
  )

# Sum counts per Site × Zone × Functional group and plotting
Data_mobile <- Data_ %>%
  group_by(Site, Zone, Functional) %>%
  summarise(Total_Count = sum(Count), .groups = "drop")

Data_mobile$Functional <- gsub("Tube-Worm", "Tube Worm", Data_mobile$Functional)

Mobile_Bar <- ggplot(Data_mobile,
                     aes(x = Zone, y = Total_Count/15, fill = Functional)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 15))+
  facet_wrap(~Site, scales = "free_x") +
  scale_fill_manual(values = c(
    "Amphipod" = "lightgoldenrod3",
    "Limpet" = "wheat4",
    "Sea Anemone" = "orangered",
    "Tube Worm" = "slategray4",
    "Turbinate Gastropod" = "lawngreen"
  )) +
  labs(y = "Counts per quadrat (#)", x = "Zone", fill = "Mobile taxa") +
  theme_classic2(base_size = 30) +
  theme(
    axis.text = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 28)
  )

Mobile_Bar

# Combing figures (Figure 3)----

Final_plot <- ggarrange(
  Cover_bar,
  Mobile_Bar,
  ncol = 1, nrow = 2,
  labels = c("A", "B"),
  heights = c(1, 1),
  font.label = list(size = 30, face = "bold"),
  common.legend = FALSE,
  legend = "top"
)

print(Final_plot)

ggsave("Figure 3.png",
       plot = Final_plot,
       width = 18, height = 18, dpi = 350)
