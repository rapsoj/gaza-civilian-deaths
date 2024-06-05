# Import libraries
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(extrafont)
loadfonts()

# Load data
df <- read_xlsx('urban-combat-deaths.xlsx') %>%
  # Edit columns
  mutate(
    # Fix rounding errors
    defending_civ_deaths = round(defending_civ_deaths),
    defending_militant_deaths = round(defending_militant_deaths),
    # Add asterisk for Gaza
    battle = ifelse(grepl('Gaza', battle), paste(battle, '**'), battle),
    # Add a column to identify the highlighted battle
    battle_highlight = ifelse(battle == "Invasion of the Gaza Strip (2023–) **", TRUE, FALSE),
    # Add a column to identify the highlighted battle
    battle_highlight = ifelse(battle == "Invasion of the Gaza Strip (2023–) **", TRUE, FALSE))


# Create theme for top graph
top_theme <- theme(
  text = element_text(family = "CMU Bright"),
  plot.title = element_text(family = "CMU Bright SemiBold"),
  plot.subtitle = element_text(face = "plain"),
  axis.title.y = element_blank(),
  axis.title.x = element_blank(),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  title = element_text(size = 14),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(colour = "#dedede"),
  plot.background = element_rect(fill = "#E6E6E6", color = NA)
)

# Create theme for facet graphs
facet_theme <- theme(
  text = element_text(family = "CMU Bright"),
  plot.title = element_text(family = "CMU Bright SemiBold"),
  plot.subtitle = element_text(face = "plain"),
  axis.title.y = element_blank(),
  axis.title.x = element_blank(),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  title = element_text(size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(colour = "#dedede"),
  plot.background = element_rect(fill = "#E6E6E6", color = NA)
)

# Define custom functions to format the labels
k_thousands <- function(x) {
  ifelse(x >= 1000, paste0(x/1000, "K"), x)
}
ratio <- function(x) {
  paste0(x, ":1")
}

# Function to customize x-axis labels
custom_x_axis_labels <- function(breaks) {
  sapply(breaks, function(label) {
    if (label == "2023 invasion of the Gaza Strip **") {
      element_text(face = "bold", color = "#e81014", size = 11)
    } else {
      element_text(face = "plain", color = "black", size = 11)
    }
  })
}

# Larger top plot
p1 <- ggplot(df, aes(x = reorder(battle, defending_civ_deaths), y = defending_civ_deaths, fill = ifelse(battle_highlight, "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(defending_civ_deaths >= 1000, paste0(round(defending_civ_deaths / 1000, 1), "K"), defending_civ_deaths), 
                y = defending_civ_deaths, family = ifelse(battle_highlight, "CMU Bright SemiBold", "CMU Bright"), 
                color = ifelse(battle_highlight, "Highlighted", "Normal"),
                face = ifelse(battle_highlight, "bold", "plain")), position = position_dodge(width = 0.9), hjust = -0.1, size = 4.5) +
  coord_flip() +
  scale_y_continuous(labels = k_thousands, limits = c(0, 1.1 * max(df$defending_civ_deaths))) +
  scale_color_manual(values = c("Normal" = "black", "Highlighted" = "#e81014")) + # Define text colors
  ggtitle("Total Civilian Deaths") +
  theme_minimal() +
  top_theme +
  scale_fill_manual(values = c("Normal" = "#615e58", "Highlighted" = "#e81014")) +
  guides(fill = FALSE, color = FALSE) + # Remove the legend
  theme(axis.text.y = do.call(element_text, custom_x_axis_labels(levels(df$battle)))) # Apply custom x-axis labels

# Smaller plots
p2 <- ggplot(df, aes(x = reorder(battle, civ_to_militant_death_ratio), y = civ_to_militant_death_ratio, fill = ifelse(battle_highlight, "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(civ_to_militant_death_ratio, 2), ":1"), 
                y = civ_to_militant_death_ratio, family = ifelse(battle_highlight, "CMU Bright SemiBold", "CMU Bright"), 
                color = ifelse(battle_highlight, "Highlighted", "Normal"), 
                face = ifelse(battle_highlight, "bold", "plain")), position = position_dodge(width = 0.9), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = ratio, limits = c(0, 1.1 * max(df$civ_to_militant_death_ratio))) +
  scale_color_manual(values = c("Normal" = "black", "Highlighted" = "#e81014")) + # Define text colors
  ggtitle("Civilian to Militant Death Ratio") +
  theme_minimal() +
  facet_theme +
  scale_fill_manual(values = c("Normal" = "#615e58", "Highlighted" = "#e81014")) +
  guides(fill = FALSE, color = FALSE) + # Remove the legend
  theme(axis.text.y = do.call(element_text, custom_x_axis_labels(levels(df$battle)))) # Apply custom x-axis labels

p3 <- ggplot(df, aes(x = reorder(battle, civ_deaths_per_month), y = civ_deaths_per_month, fill = ifelse(battle_highlight, "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(civ_deaths_per_month >= 1000, paste0(round(civ_deaths_per_month / 1000, 1), "K"), round(civ_deaths_per_month, 1)), 
                y = civ_deaths_per_month, family = ifelse(battle_highlight, "CMU Bright SemiBold", "CMU Bright"), 
                color = ifelse(battle_highlight, "Highlighted", "Normal"),
                face = ifelse(battle_highlight, "bold", "plain")), position = position_dodge(width = 0.9), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = k_thousands, limits = c(0, 1.1 * max(df$civ_deaths_per_month))) +
  scale_color_manual(values = c("Normal" = "black", "Highlighted" = "#e81014")) + # Define text colors
  ggtitle("Average Civilian Deaths per Month") +
  theme_minimal() +
  facet_theme +
  scale_fill_manual(values = c("Normal" = "#615e58", "Highlighted" = "#e81014")) +
  guides(fill = FALSE, color = FALSE) + # Remove the legend
  theme(axis.text.y = do.call(element_text, custom_x_axis_labels(levels(df$battle)))) # Apply custom x-axis labels

p4 <- ggplot(df, aes(x = reorder(battle, civ_deaths_per_cap_thousand), y = civ_deaths_per_cap_thousand, fill = ifelse(battle_highlight, "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(civ_deaths_per_cap_thousand, 2), 
                y = civ_deaths_per_cap_thousand, family = ifelse(battle_highlight, "CMU Bright SemiBold", "CMU Bright"), 
                color = ifelse(battle_highlight, "Highlighted", "Normal"),
                face = ifelse(battle_highlight, "bold", "plain")), position = position_dodge(width = 0.9), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.1 * max(df$civ_deaths_per_cap_thousand))) +
  scale_color_manual(values = c("Normal" = "black", "Highlighted" = "#e81014")) + # Define text colors
  ggtitle("Civilian Deaths per 1K Population") +
  theme_minimal() +
  facet_theme +
  scale_fill_manual(values = c("Normal" = "#615e58", "Highlighted" = "#e81014")) +
  guides(fill = FALSE, color = FALSE) + # Remove the legend
  theme(axis.text.y = do.call(element_text, custom_x_axis_labels(levels(df$battle)))) # Apply custom x-axis labels

p5 <- ggplot(df, aes(x = reorder(battle, civ_deaths_per_cap_month_million), y = civ_deaths_per_cap_month_million, fill = ifelse(battle_highlight, "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(civ_deaths_per_cap_month_million, 2), 
                y = civ_deaths_per_cap_month_million, family = ifelse(battle_highlight, "CMU Bright SemiBold", "CMU Bright"), 
                color = ifelse(battle_highlight, "Highlighted", "Normal")), position = position_dodge(width = 0.9), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.1 * max(df$civ_deaths_per_cap_month_million))) +
  scale_color_manual(values = c("Normal" = "black", "Highlighted" = "#e81014")) + # Define text colors
  ggtitle("Civilian Deaths per Month per 1M Population") +
  theme_minimal() +
  facet_theme +
  scale_fill_manual(values = c("Normal" = "#615e58", "Highlighted" = "#e81014")) +
  guides(fill = FALSE, color = FALSE) + # Remove the legend
  theme(axis.text.y = do.call(element_text, custom_x_axis_labels(levels(df$battle)))) # Apply custom x-axis labels

# Combine the plots using patchwork
combined_plot <- (p1 / (p2 + p3)) / (p4 + p5)

# Adjust the relative heights and add a big title
combined_plot <- combined_plot + 
  plot_layout(heights = c(1.3, 1, 1)) +
  plot_annotation(
    title = "How do civilian deaths in the Gaza Strip compare to other urban battles?",
    subtitle = "Civilian deaths in defending regions for every urban battle during the 21st century (Global, 2000-2024)* \n",
    # Add caption
    caption = paste0("\n\n*Only includes deaths caused by the main attacking force\n",
                     "**All data for Gaza is based on figures cited by the Israeli PM, Israeli MFA, and IDF"),
    theme = theme(
      text = element_text(family = "CMU Bright"),
      plot.title = element_text(size = 20, family = "CMU Bright Semibold"),
      plot.subtitle = element_text(size = 16, face = "plain"),
      plot.caption = element_text(size = 10, hjust = 0, vjust = 4),
      plot.margin = unit(c(1, 1, 0.2, 1.2), "cm"),
      plot.background = element_rect(fill = "#E6E6E6", color = NA)
    ))

# Display the combined plot
#print(combined_plot)

# Export graph
png("graph-urban.png", units = "in",
    width = 13.5, height = 10, res = 500)
print(combined_plot)
dev.off()