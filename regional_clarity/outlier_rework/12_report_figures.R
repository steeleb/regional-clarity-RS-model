# Diagnostic figures for the outlier-removal section of the final report:
# a funnel of row counts through each pipeline stage, the band-RANSAC
# vote_frac distribution, and a map of retained sites/matches.

suppressMessages({
  library(arrow); library(dplyr); library(ggplot2); library(sf); library(readr)
})

out_dir <- "regional_clarity/outlier_rework"
fig_dir <- file.path(out_dir, "report_figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

ROSS_lt_pal <- c("#002EA3", "#E70870", "#256BF5", "#745CFB", "#1E4D2B", "#56104E")

## ---- 1. final training row count: current pipeline vs redesigned pipeline ----
final_counts <- tibble::tribble(
  ~pipeline, ~rows,
  "current pipeline\n(main branch)", 5199,
  "redesigned pipeline\n(this branch)", 12659,
  "10k floor", 10000
)

p1 <- ggplot(final_counts %>% filter(pipeline != "10k floor"),
             aes(x = pipeline, y = rows, fill = pipeline)) +
  geom_col(width = 0.5) +
  geom_hline(yintercept = 10000, linetype = "dashed", color = "grey30") +
  annotate("text", x = 1.5, y = 10300, label = "10k floor", color = "grey30", size = 3.5) +
  geom_text(aes(label = format(rows, big.mark = ",")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c(ROSS_lt_pal[2], ROSS_lt_pal[1])) +
  labs(x = NULL, y = "final training rows", title = "Final training row count: before vs. after redesign") +
  theme_bw() + theme(legend.position = "none")
ggsave(file.path(fig_dir, "final_row_count_comparison.png"), p1, width = 6, height = 5, dpi = 150)

## ---- 2. band-RANSAC vote_frac distribution ----
if (file.exists(file.path(out_dir, "siteSR_regional_vote_scores.feather"))) {
  scores <- read_feather(file.path(out_dir, "siteSR_regional_vote_scores.feather"), col_select = "vote_frac")
  p2 <- ggplot(scores %>% filter(!is.na(vote_frac)), aes(x = vote_frac)) +
    geom_histogram(bins = 50, fill = ROSS_lt_pal[1]) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = ROSS_lt_pal[2]) +
    annotate("text", x = 0.52, y = Inf, label = "cutoff = 0.5", vjust = 2, hjust = 0, color = ROSS_lt_pal[2]) +
    labs(x = "vote_frac (fraction of RANSAC iterations calling this point an inlier)",
         y = "count of reflectance rows",
         title = "Majority-vote band RANSAC score distribution",
         subtitle = "Most points are almost never flagged - median vote_frac = 0.99") +
    theme_bw()
  ggsave(file.path(fig_dir, "band_ransac_vote_frac.png"), p2, width = 7, height = 5, dpi = 150)
}

## ---- 3. filter ablation waterfall (from the round-1 ablation results) ----
if (file.exists(file.path(out_dir, "filter_ablation_results.csv"))) {
  ablation <- read_csv(file.path(out_dir, "filter_ablation_results.csv"), show_col_types = FALSE)
  ablation$config <- factor(ablation$config, levels = ablation$config)
  p3 <- ggplot(ablation, aes(x = config, y = final_training_rows)) +
    geom_col(fill = ROSS_lt_pal[3]) +
    geom_text(aes(label = format(final_training_rows, big.mark = ",")), vjust = -0.5, size = 3.5) +
    labs(x = NULL, y = "final training rows",
         title = "Ablation: cost of each QA filter (5-day window, original geometry QA)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(file.path(fig_dir, "filter_ablation.png"), p3, width = 8, height = 5, dpi = 150)
}

## ---- 4. map of final retained sites, colored by n matches ----
final_data <- read_feather(file.path(out_dir, "filtered_regional_sdd_final.feather"))
site_summary <- final_data %>% summarize(n = n(), .by = c(siteSR_id, lat, lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")

states <- st_read("aquamatch_files/states.gdb", layer = "states", quiet = TRUE)
HUC4s <- st_read("aquamatch_files/huc4_filtered.gdb", layer = "huc4_filtered", quiet = TRUE)
geom_col <- attr(HUC4s, "sf_column")
if (geom_col != "geometry") { names(HUC4s)[names(HUC4s) == geom_col] <- "geometry"; st_geometry(HUC4s) <- "geometry" }

p4 <- ggplot() +
  geom_sf(data = states, fill = "grey95", color = "grey60") +
  geom_sf(data = HUC4s, fill = NA, color = "grey40", linewidth = 0.3) +
  geom_sf(data = site_summary, aes(size = n, color = n), alpha = 0.6) +
  scale_color_viridis_c() +
  labs(title = "Retained sites in the redesigned final training set", size = "n matches", color = "n matches") +
  theme_bw()
ggsave(file.path(fig_dir, "retained_sites_map.png"), p4, width = 8, height = 7, dpi = 150)

cat("report figures written to", fig_dir, "\n")
