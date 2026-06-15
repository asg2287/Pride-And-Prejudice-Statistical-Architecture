library(tidyverse)
library(readxl)
library(pracma)   
library(MASS)     
library(ggrepel)

excel_path <- "~/Desktop/Pride and Prejudice/Pride and Prejudice, Summer 2026.xlsx"
csv_path <- "~/Desktop/Pride and Prejudice/New Mahalanobis folder/AUSTEN_NETWORK_MAHALANOBIS.csv"

raw_data <- read_xlsx(excel_path)
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

matrix_6d <- as.matrix(mahalanobis_data[, components])
cov_matrix <- cov(matrix_6d)
mean_vector <- colMeans(matrix_6d)

raw_data <- raw_data %>%
  mutate(
    Euclidean_Norm = sqrt(N^2 + DC^2 + C^2 + I^2 + DN^2 + A^2),
    D2 = mahalanobis(as.matrix(raw_data[, components]), mean_vector, cov_matrix)
  )

theme_manuscript <- function() {
  theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      panel.border = element_blank(),
      axis.line = element_line(color = "black", size = 0.5),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 11),
      axis.title = element_text(face = "bold", size = 11),
      legend.position = "none"
    )
}

global_integration <- raw_data %>%
  mutate(Character = str_trim(Character)) %>%
  group_by(Character) %>%
  complete(Absolute_Chapter = 1:61, fill = list(A = 0, C = 0, DN = 0, DC = 0, I = 0, N = 0, D2 = 0)) %>%
  summarise(
    `Action` = pracma::trapz(Absolute_Chapter, A),
    `Communication` = pracma::trapz(Absolute_Chapter, C),
    `Description by Narrator` = pracma::trapz(Absolute_Chapter, DN),
    `Discussion of Character by Others` = pracma::trapz(Absolute_Chapter, DC),
    `Global Anomaly Score (D²)` = pracma::trapz(Absolute_Chapter, D2),
    `Interiority` = pracma::trapz(Absolute_Chapter, I),
    `Name Mentions` = pracma::trapz(Absolute_Chapter, N),
    .groups = 'drop'
  )

elizabeth_row   <- global_integration %>% filter(Character == "Elizabeth")
darcy_row       <- global_integration %>% filter(Character == "Mr. Darcy")
jane_row        <- global_integration %>% filter(Character == "Jane")
mrs_bennet_row  <- global_integration %>% filter(Character == "Mrs. Bennet")
bingley_row     <- global_integration %>% filter(Character == "Mr. Bingley")
lydia_row       <- global_integration %>% filter(Character == "Lydia")
collins_row     <- global_integration %>% filter(Character == "Mr. Collins")

wickham_row     <- global_integration %>% 
  filter(Character == "Mr. Wickham" | Character == "Wickham") %>% 
  mutate(Character = "Mr. Wickham") %>%
  slice(1)

riemann_bars_1_2 <- bind_rows(
  elizabeth_row, darcy_row, jane_row, mrs_bennet_row, 
  bingley_row, wickham_row, lydia_row, collins_row
)

final_8_characters <- c("Elizabeth", "Mr. Darcy", "Jane", "Mrs. Bennet", 
                        "Mr. Bingley", "Mr. Wickham", "Lydia", "Mr. Collins")

riemann_bars_plot <- riemann_bars_1_2 %>%
  pivot_longer(cols = -Character, names_to = "Dimension", values_to = "Volume") %>%
  mutate(
    Character = factor(Character, levels = rev(final_8_characters)),
    Dimension = factor(Dimension, levels = c("Action", "Communication", 
                                             "Description by Narrator", "Discussion of Character by Others", 
                                             "Global Anomaly Score (D²)", "Interiority", "Name Mentions"))
  )

panel_colors <- c(
  "Action" = "#79C694", "Communication" = "#F7DC6F", "Description by Narrator" = "#AF7AC5", 
  "Discussion of Character by Others" = "#E9967A", "Global Anomaly Score (D²)" = "#7FB3D5", 
  "Interiority" = "#F4A460", "Name Mentions" = "#98FB98"
)

ggplot(riemann_bars_plot, aes(x = Character, y = Volume, fill = Dimension)) +
  geom_bar(stat = "identity", color = "grey40", size = 0.2, width = 0.7) +
  scale_fill_manual(values = panel_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, NA)) +
  coord_flip() +
  facet_wrap(~ Dimension, scales = "free_x", ncol = 2) +
  labs(x = "Characters", y = "Total Accumulated Integration Volume (Area Under Trajectory Curve)") +
  theme_manuscript() +
  theme(strip.text = element_text(size = 10, face = "bold"))

ggsave("MACRO_STRUCTURAL_VOLUME_LANDSCAPE.pdf", width = 11, height = 14, units = "in")

v_a_mean <- mean(riemann_bars_1_2$Action, na.rm = TRUE)
v_i_mean <- mean(riemann_bars_1_2$Interiority, na.rm = TRUE)

ggplot(riemann_bars_1_2, aes(x = Action, y = Interiority, label = Character)) +
  geom_vline(xintercept = v_a_mean, linetype = "dashed", color = "grey60", size = 0.5) +
  geom_hline(yintercept = v_i_mean, linetype = "dashed", color = "grey60", size = 0.5) +
  geom_point(aes(size = `Global Anomaly Score (D²)`, color = `Global Anomaly Score (D²)`), alpha = 0.75) +
  geom_text_repel(
    family = "serif", 
    size = 3.5, 
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = "grey40",
    segment.size = 0.4
  ) +
  scale_color_gradient(name = "Global Anomaly (D²)", low = "#deebf7", high = "#3182bd") +
  scale_size_continuous(name = "Global Anomaly (D²)", breaks = c(300, 600, 900), range = c(3, 8)) +
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1050), breaks = seq(0, 1000, by = 250), expand = c(0, 0)) +
  labs(
    x = "Accumulated Action Volume (Plot Mechanics)",
    y = "Accumulated Interiority Volume (Cognitive Weight)"
  ) +
  theme_manuscript() + 
  theme(
    legend.position = "right",
    legend.title = element_text(family = "serif", face = "bold", size = 10),
    legend.text = element_text(family = "serif", size = 9)
  )

ggsave("CHARACTER_TOPOLOGY_MAP.pdf", width = 8.5, height = 7, units = "in")
