#' --- 
#' title: "dplyr Problem Set Solutions"
#' date: "October 21st, 2021"
#' author: "Christian Testa"
#' ---

# dependencies
library(tidyverse)
library(palmerpenguins)

# load data
data(penguins)

# take a look at our data
glimpse(penguins)

# problem 1
penguins %>% 
  group_by(species, island) %>% 
  count()


# problem 2, part 1, method 1 -- naive method
penguins %>% 
  group_by(species) %>% 
  summarize(
    bill_length_mm_mean = mean(bill_length_mm, na.rm=T),
    bill_depth_mm_mean = mean(bill_depth_mm, na.rm=T),
    flipper_length_mm_mean = mean(flipper_length_mm, na.rm=T),
    body_mass_g_mean = mean(body_mass_g, na.rm=T))

# problem 2, part 1, method 2 -- using summarize_if
penguins %>% 
  select(-year) %>% 
  group_by(species) %>% 
  summarize_if(is.numeric, mean, na.rm=T)

# problem 2, part 2, method 1 -- naive method
penguins %>% 
  group_by(species) %>% 
  summarize(
    bill_length_mm_sd = sd(bill_length_mm, na.rm=T),
    bill_depth_mm_sd = sd(bill_depth_mm, na.rm=T),
    flipper_length_mm_sd = sd(flipper_length_mm, na.rm=T),
    body_mass_g_sd = sd(body_mass_g, na.rm=T))

# problem 2, part 2, method 2 -- using summarize_if
penguins %>% 
  select(-year) %>% 
  group_by(species) %>% 
  summarize_if(is.numeric, sd, na.rm=T)

# problem 2, parts 1&2, method 2 -- using summarize_if with multiple functions 
penguin_means_and_sds <- 
  penguins %>% 
  select(-year) %>% 
  group_by(species) %>% 
  summarize_if(
    is.numeric,
    .funs = 
      list(
        mean = ~ mean(., na.rm=T), 
        sd = ~ sd(., na.rm=T)
        )
  )

penguin_means_and_sds


# problem 2, part 3
penguin_means_and_sds %>% 
  arrange(desc(species))


# bonus problem 
penguin_bill_length_depth_slopes <- 
  penguins %>% 
  nest_by(species) %>% 
  mutate(
    slope = coef(lm("bill_length_mm ~ bill_depth_mm", data))[[2]]
    ) %>% 
  select(species, slope) 

# visualization of bonus problem

plot_1 <- 
  penguins %>% 
    ggplot(aes(x = bill_depth_mm, y = bill_length_mm, color = species, shape = species)) + 
    geom_point() + 
    geom_smooth(method = 'lm', se = FALSE) + 
    ggtitle("Slope Between Bill Length (mm) and Bill Depth (mm) by Species")

#+ fig.width=8
plot_1

# visualization of bonus problem with slopes labeled

plot_2 <- 
  plot_1 + 
  geom_label(
    data = penguins %>% group_by(species) %>% 
      summarize_if(is.numeric, mean, na.rm=T) %>% 
      left_join(penguin_bill_length_depth_slopes),
    mapping = aes(label = round(slope, 2)),
    show.legend = FALSE
  )

#+ fig.width=8
plot_2
      


