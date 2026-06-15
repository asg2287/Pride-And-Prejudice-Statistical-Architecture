library(tidyverse)
library(readxl)
library(pracma)   
library(MASS)     
library(ggrepel)
library(patchwork)

excel_path <- "~/Desktop/Pride and Prejudice/Pride and Prejudice, Summer 2026.xlsx"
csv_path   <- "~/Desktop/Pride and Prejudice/New Mahalanobis/AUSTEN_NETWORK_MAHALANOBIS.csv"

raw_data         <- read_xlsx(excel_path)
mahalanobis_data <- read_csv(csv_path)

raw_data <- raw_data %>%
  group_by(Character) %>%
  mutate(
    Absolute_Chapter = case_when(
      Volume == 1 ~ Chapter,
      Volume == 2 ~ Chapter + 23,
      Volume == 3 ~ Chapter + 42,
      TRUE ~ Chapter
    )
  ) %>%
  ungroup()

components <- c("N", "DC", "C", "I", "DN", "A")

matrix_6d   <- as.matrix(mahalanobis_data[, components])
cov_matrix  <- cov(matrix_6d)
mean_vector <- colMeans(matrix_6d)

raw_data <- raw_data %>%
  mutate(
    Euclidean_Norm = sqrt(N^2 + DC^2 + C^2 + I^2 + DN^2 + A^2),
    D2             = mahalanobis(as.matrix(raw_data[, components]), mean_vector, cov_matrix)
  )

theme_manuscript <- function() {
  theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      plot.title        = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle     = element_text(size = 10, face = "italic", hjust = 0.5),
      panel.border      = element_blank(),
      axis.line         = element_line(color = "black", size = 0.5),
      strip.background  = element_blank(),
      strip.text        = element_text(face = "bold", size = 10),
      axis.title        = element_text(face = "bold", size = 11),
      legend.position   = "bottom"
    )
}

global_integration <- raw_data %>%
  mutate(Character = str_trim(Character)) %>%
  group_by(Character) %>%
  complete(Absolute_Chapter = 1:61, fill = list(A = 0, C = 0, DN = 0, DC = 0, I = 0, N = 0, D2 = 0)) %>%
  summarise(
    `Action`                            = pracma::trapz(Absolute_Chapter, A),
    `Communication`                     = pracma::trapz(Absolute_Chapter, C),
    `Description by Narrator`           = pracma::trapz(Absolute_Chapter, DN),
    `Discussion of Character by Others` = pracma::trapz(Absolute_Chapter, DC),
    `Global Anomaly Score (D²)`        = pracma::trapz(Absolute_Chapter, D2),
    `Interiority`                       = pracma::trapz(Absolute_Chapter, I),
    `Name Mentions`                     = pracma::trapz(Absolute_Chapter, N),
    .groups = 'drop'
  )

final_8_characters <- c("Elizabeth", "Mr. Darcy", "Jane", "Mrs. Bennet", 
                        "Mr. Bingley", "Mr. Wickham", "Lydia", "Mr. Collins")

elizabeth_row  <- global_integration %>% filter(Character == "Elizabeth")
darcy_row      <- global_integration %>% filter(Character == "Mr. Darcy")
jane_row       <- global_integration %>% filter(Character == "Jane")
mrs_bennet_row <- global_integration %>% filter(Character == "Mrs. Bennet")
bingley_row    <- global_integration %>% filter(Character == "Mr. Bingley")
lydia_row      <- global_integration %>% filter(Character == "Lydia")
collins_row    <- global_integration %>% filter(Character == "Mr. Collins")
wickham_row    <- global_integration %>% 
  filter(Character == "Mr. Wickham" | Character == "Wickham") %>% 
  mutate(Character = "Mr. Wickham") %>% 
  slice(1)

riemann_bars_1_2 <- bind_rows(
  elizabeth_row, darcy_row, jane_row, mrs_bennet_row, 
  bingley_row, wickham_row, lydia_row, collins_row
)

top_12_characters <- raw_data %>%
  group_by(Character) %>%
  summarise(Total_Imp = sum(Euclidean_Norm, na.rm = TRUE)) %>%
  arrange(desc(Total_Imp)) %>%
  slice_max(Total_Imp, n = 12) %>%
  pull(Character)

fig1_1_data <- raw_data %>%
  filter(Character %in% top_12_characters)

fig1_1 <- ggplot(fig1_1_data, aes(x = Absolute_Chapter, y = Euclidean_Norm)) +
  geom_area(fill = "#F3D1D1", alpha = 0.7, color = "#A35C5C", size = 0.3) +
  facet_wrap(~ factor(Character, levels = top_12_characters), ncol = 2, scales = "free_y") +
  scale_x_continuous(limits = c(1, 61), breaks = c(1, 10, 21, 31, 41, 51, 61)) +
  labs(title = "Figure 1.1: 6D Vector Trajectories",
       subtitle = "Continuous Riemann Volume Distribution Across the Top 12 Most Implicated Characters",
       x = "Chapters 1-61", 
       y = "Composite Narrative Footprint Vector Magnitude") +
  theme_manuscript()

ggsave("FIGURE_1_1_6D_VECTOR_TRAJECTORIES.png", plot = fig1_1, width = 9.5, height = 11, units = "in", dpi = 300, type = "cairo")

fig1_2_bars_data <- riemann_bars_1_2 %>%
  rename(`Discussion of Character` = `Discussion of Character by Others`) %>%
  pivot_longer(cols = -Character, names_to = "Dimension", values_to = "Volume")

fig1_2_plots   <- list()
dimensions_list <- c("Action", "Communication", "Description by Narrator", 
                     "Discussion of Character", "Global Anomaly Score (D²)", 
                     "Interiority", "Name Mentions")
colors_list     <- c("#7FB3D5", "#F9E79F", "#BB8FCE", "#F1948A", "#85C1E9", "#F8C471", "#ABEBC6")

for(i in seq_along(dimensions_list)) {
  dim_name <- dimensions_list[i]
  p_color  <- colors_list[i]
  
  p <- ggplot(filter(fig1_2_bars_data, Dimension == dim_name), 
              aes(x = reorder(Character, Volume), y = Volume)) +
    geom_bar(stat = "identity", fill = p_color, color = "grey40", size = 0.2, width = 0.7) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, NA)) +
    coord_flip() +
    labs(title = dim_name, x = NULL, y = NULL) +
    theme_manuscript() +
    theme(plot.title = element_text(size = 10, face = "bold"),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black", size = 0.4))
  
  fig1_2_plots[[dim_name]] <- p
}

fig1_2_final <- (fig1_2_plots[[1]] + fig1_2_plots[[2]]) / 
                (fig1_2_plots[[3]] + fig1_2_plots[[4]]) / 
                (fig1_2_plots[[5]] + fig1_2_plots[[6]]) / 
                fig1_2_plots[[7]] +
  plot_annotation(
    title = "Figure 1.2: True Macro-Structural Volume Landscape",
    subtitle = "Definite Riemann Integration Over an Absolute Sequence of 61 Chapters",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13, family = "serif", hjust = 0.5),
      plot.subtitle = element_text(size = 11, face = "italic", family = "serif", hjust = 0.5)
    )
  )

ggsave("FIGURE_1_2_TRUE_MACRO_STRUCTURAL_VOLUME_LANDSCAPE.png", plot = fig1_2_final, width = 11, height = 12, units = "in", dpi = 300, type = "cairo")

character_ranks <- raw_data %>%
  group_by(Character) %>%
  summarise(Total_Vol = sum(Euclidean_Norm, na.rm = TRUE)) %>%
  arrange(desc(Total_Vol))

miss_bingley_index <- which(character_ranks$Character == "Miss Bingley")

fig1_3_targets <- if(length(miss_bingley_index) > 0) {
  character_ranks$Character[1:(miss_bingley_index - 1)]
} else {
  character_ranks$Character
}

fig1_3_data <- raw_data %>%
  filter(Character %in% fig1_3_targets)

fig1_3 <- ggplot(fig1_3_data, aes(x = Absolute_Chapter, y = Euclidean_Norm, fill = reorder(Character, -Euclidean_Norm, FUN = sum))) +
  geom_area(position = "stack", alpha = 0.85, color = "white", size = 0.1) +
  scale_fill_brewer(palette = "Spectral", name = "Character Volume Hierarchy") +
  scale_x_continuous(limits = c(1, 61), breaks = c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61), expand = c(0,0)) +
  labs(title = "The Geometric Mass of Narrative Importance",
       subtitle = "Visualising Total Cumulative Riemann Integration Volume Across the 61-Chapter Timeline",
       caption = "Figure 1.3: The Geometric Mass of Narrative Importance: Total Cumulative Stacked Riemann Integration Volume Across the 61-Chapter Timeline.",
       x = "Absolute Chronological Timeline (Chapters 1-61)", 
       y = "Stacked Vector Magnitude (Combined Textual Footprint)") +
  theme_manuscript() +
  theme(legend.position = "right")

ggsave("FIGURE_1_3_GEOMETRIC_MASS_OF_NARRATIVE_IMPORTANCE.png", plot = fig1_3, width = 10, height = 6.5, units = "in", dpi = 300, type = "cairo")

fig1_4_targets <- c("Elizabeth", "Jane", "Mr. Darcy", "Mrs. Bennet")

fig1_4_data <- raw_data %>%
  mutate(Character = str_trim(Character)) %>%
  filter(Character %in% fig1_4_targets) %>%
  mutate(Character = factor(Character, levels = fig1_4_targets))

fig1_4 <- ggplot(fig1_4_data, aes(x = Absolute_Chapter, y = I, color = Character, fill = Character)) +
  geom_area(alpha = 0.15, size = 0.5) +
  geom_line(size = 0.8) +
  facet_wrap(~ Character, ncol = 1, scales = "free_y") +
  scale_x_continuous(limits = c(1, 61), breaks = c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61), expand = c(0,0)) +
  scale_color_manual(values = c("Elizabeth" = "#D9534F", "Jane" = "#5CB85C", "Mr. Darcy" = "#5BC0DE", "Mrs. Bennet" = "#9B59B6")) +
  scale_fill_manual(values = c("Elizabeth" = "#D9534F", "Jane" = "#5CB85C", "Mr. Darcy" = "#5BC0DE", "Mrs. Bennet" = "#9B59B6")) +
  labs(title = "Figure 1.4: The Trajectory of Character Interiority Over Time",
       x = "Chapters 1-61", 
       y = "Interiority Score (I)") +
  theme_manuscript() +
  theme(legend.position = "none")

ggsave("FIGURE_1_4_TRAJECTORY_OF_CHARACTER_INTERIORITY.png", plot = fig1_4, width = 9.0, height = 8, units = "in", dpi = 300, type = "cairo")

fig1_5_data <- raw_data %>%
  mutate(Character = str_trim(Character)) %>%
  filter(Character %in% c("Elizabeth", "Mr. Darcy"))

fig1_5 <- ggplot(fig1_5_data, aes(x = Absolute_Chapter, y = I, color = Character)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Elizabeth" = "#C0392B", "Mr. Darcy" = "#2980B9")) +
  scale_x_continuous(limits = c(1, 61), breaks = c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61), expand = c(0,0)) +
  annotate("rect", xmin = 35, xmax = 36, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "#F39C12") +
  labs(title = "Figure 1.5: Macro-Temporal Psychological Realignment: Elizabeth and Darcy",
       subtitle = "Direct Covariance of Interiority Across the 61 Chapters",
       x = "Chapters 1-61", 
       y = "Interiority Intensity Metric (I)") +
  theme_manuscript() +
  theme(legend.position = "right")

ggsave("FIGURE_1_5_ELIZABETH_DARCY_INTERIORITY_COVARIANCE.png", plot = fig1_5, width = 9.5, height = 5.5, units = "in", dpi = 300, type = "cairo")

v_a_mean <- mean(riemann_bars_1_2$Action, na.rm = TRUE)
v_i_mean <- mean(riemann_bars_1_2$Interiority, na.rm = TRUE)

fig1_6 <- ggplot(riemann_bars_1_2, aes(x = Action, y = Interiority, label = Character)) +
  geom_vline(xintercept = v_a_mean, linetype = "dashed", color = "grey60", size = 0.5) +
  geom_hline(yintercept = v_i_mean, linetype = "dashed", color = "grey60", size = 0.5) +
  geom_point(aes(size = `Global Anomaly Score (D²)`, color = `Global Anomaly Score (D²)`), alpha = 0.75) +
  geom_text_repel(
    family        = "serif", 
    size          = 3.5, 
    fontface      = "bold",
    box.padding   = 0.5,
    point.padding = 0.3,
    segment.color = "grey40",
    segment.size  = 0.4
  ) +
  scale_color_gradient(name = "Global Anomaly (D²)", low = "#deebf7", high = "#3182bd") +
  scale_size_continuous(name = "Global Anomaly (D²)", breaks = c(300, 600, 900), range = c(3, 8)) +
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1050), breaks = seq(0, 1000, by = 250), expand = c(0, 0)) +
  labs(
    title = "Figure 1.6: Character Topology Map",
    x     = "Accumulated Action Volume (Plot Mechanics)",
    y     = "Accumulated Interiority Volume (Cognitive Weight)"
  ) +
  theme_manuscript() + 
  theme(
    legend.position = "right",
    legend.title    = element_text(family = "serif", face = "bold", size = 10),
    legend.text     = element_text(family = "serif", size = 9)
  )

ggsave("FIGURE_1_6_CHARACTER_TOPOLOGY_MAP.png", plot = fig1_6, width = 8.5, height = 7, units = "in", dpi = 300, type = "cairo")
