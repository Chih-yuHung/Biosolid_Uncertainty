# Load packages
library(tidyverse)

# Create data
df <- tibble(
  Category = c(
    "biosolid_n2o",
    "soil_c",
    "avoided_n2o",
    "net_ghg"
  ),
  Emission = c(65.6, -100.0, -76.4, -110.8)
)

# Set factor order and helper columns
df <- df %>%
  mutate(
    Category = factor(
      Category,
      levels = c(
        "biosolid_n2o",
        "soil_c",
        "avoided_n2o",
        "net_ghg"
      )
    ),
    Type = if_else(Emission > 0, "Emission", "Removal or avoided emission"),
    Label_x = if_else(Emission > 0, Emission + 6, Emission - 6),
    Label = sprintf("%.1f", Emission)
  )

# Short y axis labels with N2O subscript
ylab_map <- c(
  biosolid_n2o = "Biosolid~N[2]*O~emissions",
  soil_c       = "Soil~C~storage",
  avoided_n2o  = "Avoided~fertilizer~N[2]*O",
  net_ghg      = "Net~GHG~mitigation~benefit"
)

# Draw plot
p <- ggplot(df, aes(x = Emission, y = Category, fill = Type)) +
  geom_col(width = 0.28) +
  geom_vline(xintercept = 0, linewidth = 0.7, color = "black") +
  geom_hline(yintercept = 3.5, linetype = "dashed", linewidth = 0.5, color = "grey40") +
  # geom_text(
  #   aes(x = Label_x, label = Label),
  #   size = 3.2,
  #   fontface = "bold",
  #   color = "black",
  #   nudge_x = -1
  # ) +
  scale_fill_manual(
    values = c(
      "Emission" = "#ED7D31",
      "Removal or avoided emission" = "#00B050"
    )
  ) +
  scale_y_discrete(
    labels = function(x) parse(text = ylab_map[x])
  ) +
  scale_x_continuous(
    limits = c(-150, 100),
    breaks = seq(-150, 100, 50),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  labs(
    x = expression("Emissions or removals (kt CO"[2] * "eq)"),
    y = NULL
  ) +
  theme_classic(base_size = 9) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(face = "bold", color = "black", size = 10),
    axis.text.x = element_text(face = "bold", color = "black", size = 12),
    axis.title.x = element_text(face = "bold", color = "black", size = 12),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(10, 20, 10, 15)
  )

# Print plot
p

# Save to Outputs folder
dir.create("Outputs", showWarnings = FALSE)

ggsave(
  filename = "Outputs/Figure_4_Biosolid_net_benefit.png",
  plot = p,
  width = 7,
  height = 3.6,
  dpi = 300
)
