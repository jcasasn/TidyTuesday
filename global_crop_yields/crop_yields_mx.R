library(tidyverse)
library(ggthemes)

key_crops = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

my_colors = c("#9B9B7A", "#DDA15E", "#A97D5D", "#5C755E")

mx_crops = key_crops %>%
  filter(Code == "MEX")

long_mx_crops = mx_crops %>%
  select(1:7) %>% 
  pivot_longer(cols = 4:last_col(),
               names_to = "crop",
               values_to = "crop_production") %>%
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>%
  set_names(nm = names(.) %>% tolower())

long_mx_crops %>%
  mutate(isWheat = (crop == "Wheat")) %>% 
  ggplot(aes(x = year, y = crop_production, color = crop)) +
  geom_smooth(aes(linetype = isWheat), size = 1.5, alpha = 0.8, span = 0.15, se = FALSE) +
  labs(title = "Crop Yields in Mexico Over Time",
       subtitle = "How does wheat yield differ from other crops?",
       x = "Year",
       y = "Yield (tonnes per hectare)",
       color = "Crop") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  scale_linetype_manual(values = c("dashed", "solid"), guide = "none") +
  scale_color_manual(values = my_colors)


