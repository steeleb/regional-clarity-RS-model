library(tidyverse)
library(feather)
library(tmap)
library(sf)
library(tigris)
library(nhdplusTools)
ROSS_lt_pal <- c("#002EA3", "#E70870", "#256BF5", 
                 "#745CFB", "#1E4D2B", "#56104E")


filtered_ml <- read_feather("modeling/regional_clarity/filtered_regional_sdd.feather")

total_rows <- nrow(filtered_ml)
target_size <- ceiling(total_rows / 5)

huc4_sizes <- filtered_ml %>%
  group_by(HUC4) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

parts <- vector("list", 5)
part_sizes <- rep(0, 5)

for (i in 1:nrow(huc4_sizes)) {
  smallest_part <- which.min(part_sizes)
  parts[[smallest_part]] <- c(parts[[smallest_part]], huc4_sizes$HUC4[i])
  part_sizes[smallest_part] <- part_sizes[smallest_part] + huc4_sizes$count[i]
}

split_df <- function(df, parts, part_label) {
  map(parts, function(huc4_list) {
    df %>% 
      filter(HUC4 %in% huc4_list)
  }) %>%
    bind_rows() %>% 
    mutate(part = part_label)
}

split_dataframes <- pmap(list(list(filtered_ml),
                              parts, 
                              seq(1:5)),
                         split_df) %>% 
  bind_rows()

sites <- split_dataframes %>% 
  summarize(n = n(), .by = c(rowid, WGS84_Longitude, WGS84_Latitude, part)) %>% 
  st_as_sf(., 
           coords = c("WGS84_Longitude", 
                      "WGS84_Latitude"),
           crs = "EPSG:4326") %>% 
  mutate(split = part)

states <- states() %>% 
  filter(STUSPS %in% c("CO", "WY", "UT", "ID", "MT", "NM"))

HUC4s <- get_huc(states %>% st_union(), type = "huc04")

tm_shape(HUC4s) +
  tm_shape(states) +
  tm_polygons(col = "grey70") +
  tm_borders(col = "grey20") +
  tm_shape(HUC4s) +
  tm_polygons(fill_alpha = 0) +
  tm_borders(col = "black") +
  tm_shape(sites) +
  tm_symbols(fill = "split", 
             col = NA, 
             size = 0.5, 
             fill_alpha = 0.5,  
             fill.scale = tm_scale_discrete(values = ROSS_lt_pal)) +
  tm_shape(states) + 
  tm_text("STUSPS", col = "grey20") +
  tm_layout(#inner.margins = c(0.02, 0.02, 0.02, 0.02),
            frame = NA,
            legend.position = tm_pos_in("right", "top"))
  
write_feather(split_dataframes, "modeling/regional_clarity/split_filtered_regional.feather")
