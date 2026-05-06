library(tidyverse)
library(patchwork)

# Read data
df <- read_csv("Inputs/Figure_3_Biosolid N2O and SOC in Canada.csv") %>%
  select(Year, Province, Category, Emission) %>%
  mutate(
    Year = factor(Year, levels = c(1990, 1995, 2000, 2005, 2010, 2015)),
    Province = factor(Province, levels = unique(Province))
  )

# Create output folder
dir.create("Outputs", showWarnings = FALSE)

# More distinguishable colors
year_cols <- c(
  "1990" = "#4E79A7",  # blue
  "1995" = "#F28E2B",  # orange
  "2000" = "#59A14F",  # green
  "2005" = "#B07AA1",  # purple
  "2010" = "#9C755F",  # brown
  "2015" = "#76B7B2"   # teal
)

# Shared theme
base_theme <- theme_classic(base_size = 10) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(face = "bold", color = "black"),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )

# Panel A
p_n2o <- df %>%
  filter(Category == "N2O") %>%
  ggplot(aes(x = Province, y = Emission, fill = Year)) +
  geom_col(
    position = position_dodge(width = 0.82),
    width = 0.72,
    color = "black",
    linewidth = 0.25
  ) +
  scale_fill_manual(values = year_cols) +
  labs(
    x = NULL,
    y = expression(
      atop(
        "Soil N"[2]*"O emissions",
        "(biosolid application, kt CO"[2]*"eq)"
      )
    )
  ) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = expression("(A) Soil N"[2]*"O emissions"),
    hjust = 1.02, vjust = 1.2,
    size = 3.8
  ) +
  base_theme +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Panel B
p_soc <- df %>%
  filter(Category == "SOC") %>%
  ggplot(aes(x = Province, y = Emission, fill = Year)) +
  geom_col(
    position = position_dodge(width = 0.82),
    width = 0.72,
    color = "black",
    linewidth = 0.25
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = year_cols) +
  labs(
    x = "Province",
    y = expression(
      atop(
        "Soil C storage inrease",
        "(biosolid application, kt CO"[2]*"eq)"
      )
    )
  ) +
  annotate(
    "text",
    x = Inf,
    y = min(df$Emission[df$Category == "SOC"], na.rm = TRUE),
    label = "(B) Soil C storage",
    hjust = 1.02, vjust = -0.6,
    size = 3.8
  ) +
  base_theme +
  theme(legend.position = "none")

# Combine plots
p_final <- p_n2o / p_soc +
  plot_layout(heights = c(1, 1))

# Save figure
ggsave(
  filename = "Outputs/Figure_3_Biosolid_N2O_SOC.png",
  plot = p_final,
  width = 180,
  height = 140,
  units = "mm",
  dpi = 300
)

p_final
