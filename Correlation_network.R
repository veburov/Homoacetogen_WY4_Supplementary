# ======================================================================
# Correlation_network.R
# ----------------------------------------------------------------------
# This script performs Pearson correlation analysis among:
#   - AHL concentrations
#   - Bacterial genera abundances
#   - Archaeal genera abundances
#   - Methane yield
#
# Outputs:
#   1) A table of significant correlations (Excel file)
#   2) A co-occurrence network plot
#
# Requirements: Input data should be provided in an Excel file
# with four sheets: "AHLs", "Bacteria", "Arch", "Methane".
# Each sheet should contain a "Group" column (optional) and
# rows representing samples.
# ======================================================================

# --- Load required packages ---
library(readxl)
library(dplyr)
library(Hmisc)
library(reshape2)
library(igraph)
library(ggraph)
library(ggplot2)
library(openxlsx)

# --- 1. Load input data ---
# Replace "your_data.xlsx" with your file name
file_path <- "your_data.xlsx"

AHLs     <- read_excel(file_path, sheet = "AHLs")
Bacteria <- read_excel(file_path, sheet = "Bacteria")
Arch     <- read_excel(file_path, sheet = "Arch")
Methane  <- read_excel(file_path, sheet = "Methane")

# --- 2. Add Sample IDs ---
AHLs     <- AHLs     %>% mutate(Sample = paste0("S", row_number()))
Bacteria <- Bacteria %>% mutate(Sample = paste0("S", row_number()))
Arch     <- Arch     %>% mutate(Sample = paste0("S", row_number()))
Methane  <- Methane  %>% mutate(Sample = paste0("S", row_number()))

# --- 3. Merge all datasets ---
all_data <- AHLs %>%
  left_join(Bacteria, by = c("Sample","Group")) %>%
  left_join(Arch,     by = c("Sample","Group")) %>%
  left_join(Methane,  by = c("Sample","Group"))

num_data <- all_data %>% select(-Sample, -Group)

# --- 4. Pearson correlations ---
cor_res <- rcorr(as.matrix(num_data), type = "pearson")
cor_r <- cor_res$r
cor_p <- cor_res$P

cor_df <- melt(cor_r)
cor_df$pvalue <- melt(cor_p)$value
colnames(cor_df) <- c("Var1","Var2","cor","p")
cor_df <- cor_df %>% filter(!is.na(cor), Var1 != Var2)

# --- 5. Filter strong & significant correlations ---
cor_sig <- cor_df %>% filter(p < 0.03, abs(cor) > 0.6)

# --- 6. Keep only selected variable groups ---
ahl_names      <- colnames(AHLs)[!colnames(AHLs) %in% c("Group","Sample")]
bacteria_names <- colnames(Bacteria)[!colnames(Bacteria) %in% c("Group","Sample")]
arch_names     <- colnames(Arch)[!colnames(Arch) %in% c("Group","Sample")]

cor_sig <- cor_sig %>%
  filter((Var1 %in% c(ahl_names, bacteria_names, arch_names, "Methane")) &
         (Var2 %in% c(ahl_names, bacteria_names, arch_names, "Methane")))

# --- 7. Define nodes ---
nodes <- data.frame(name = unique(c(cor_sig$Var1, cor_sig$Var2)))
nodes$type <- case_when(
  nodes$name %in% ahl_names      ~ "AHLs",
  nodes$name %in% bacteria_names ~ "Bacteria",
  nodes$name %in% arch_names     ~ "Archaea",
  nodes$name == "Methane"        ~ "Methane",
  TRUE                           ~ "Other"
)

# --- 8. Define edges ---
edges <- cor_sig %>%
  select(from = Var1, to = Var2, cor, p) %>%
  mutate(color = ifelse(cor > 0, "red", "blue"))

# --- 9. Plot network ---
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

set.seed(123)
p <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(color = color),
                 width = 1,
                 alpha = 0.3) +
  geom_node_point(aes(color = type), size = 6, alpha = 0.7) +
  geom_node_label(
    aes(
      label = name,
      fontface = case_when(
        type == "AHLs"     ~ "plain",
        type == "Bacteria" ~ "italic",
        type == "Archaea"  ~ "italic",
        type == "Methane"  ~ "bold",
        TRUE               ~ "plain"
      )
    ),
    color = "black",
    repel = TRUE,
    size = 4.5,
    fill = scales::alpha("white", 0.7),
    label.padding = unit(0.15, "lines"),
    label.r = unit(0.15, "lines"),
    stroke = 0.3,
    show.legend = FALSE
  ) +
  scale_edge_color_identity(
    name = "Correlation",
    labels = c("Negative","Positive"),
    guide = "legend"
  ) +
  scale_color_manual(values = c(
    "AHLs"     = "forestgreen",
    "Bacteria" = "steelblue",
    "Archaea"  = "purple",
    "Methane"  = "orange"
  )) +
  theme_void(base_family = "Arial") +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 18, family = "Arial", color = "black"),
    legend.text     = element_text(size = 15, family = "Arial", color = "black")
  )

print(p)

# --- 10. Save results ---
write.xlsx(cor_sig,
           file = "Strong_Correlations.xlsx",
           sheetName = "Strong_Correlations",
           overwrite = TRUE)

ggsave("Correlation_Network.png", plot = p, width = 8, height = 6, dpi = 300)
