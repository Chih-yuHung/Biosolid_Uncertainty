#To produce Figure 2. 

# Load packages
library(tidyverse)


#Enter data
df <- read_csv("Inputs/Figure_2_Biosolid use and population in Canada.csv") %>%
      rename(Population = `Population (million)`)

# Reshape data
df_long <- df %>%
  pivot_longer(
    cols = c(Landfill, Incineration, `Land Application`),
    names_to = "Use",
    values_to = "Biosolid_kt"
  ) %>%
  mutate(
    Year = factor(Year),
    Use = factor(Use, levels = c("Landfill", "Incineration", "Land Application"))
  )

df <- df %>%
  mutate(Year = factor(Year, levels = levels(df_long$Year)))

# Fixed scaling so left axis max = 500 and right axis max = 40
scale_factor <- 500 / 40

# Create output folder if needed
dir.create("Outputs", showWarnings = FALSE)

# Plot
p <- ggplot(df_long, aes(x = Year, y = Biosolid_kt, fill = Use)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65,
    color = "grey30",
    linewidth = 0.2
  ) +
  geom_line(
    data = df,
    aes(x = Year, y = Population * scale_factor, group = 1),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  geom_point(
    data = df,
    aes(x = Year, y = Population * scale_factor),
    inherit.aes = FALSE,
    size = 2
  ) +
  scale_fill_manual(
    values = c(
      "Landfill" = "#AFC6DD",
      "Incineration" = "#E9D2C6",
      "Land Application" = "#C9DEB9"
    )
  ) +
  scale_y_continuous(
    name = expression("Biosolid use in Canada (kt yr"^-1*")"),
    limits = c(0, 500),
    breaks = seq(0, 500, 100),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Canadian population (million)",
      breaks = seq(0, 40, 10)
    )
  ) +
  labs(
    x = "Year",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y.left = element_line(color = "black", linewidth = 0.5),
    axis.line.y.right = element_line(color = "black", linewidth = 0.5),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.y.left = element_line(color = "black", linewidth = 0.5),
    axis.ticks.y.right = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(2, "mm"),
    axis.text.y.left = element_text(color = "black"),
    axis.text.y.right = element_text(color = "black"),
    axis.title.y.left = element_text(color = "black", margin = margin(r = 8)),
    axis.title.y.right = element_text(color = "black", margin = margin(l = 8)),
    plot.margin = margin(8, 12, 8, 8)
  )


# Save figure
ggsave(
  filename = "Outputs/Figure_2_Biosolid_use_population_Canada.png",
  plot = p,
  width = 170,
  height = 110,
  units = "mm",
  dpi = 300
)

p
