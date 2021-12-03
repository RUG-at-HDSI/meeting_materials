

# resources ---------------------------------------------------------------


# https://ggplot2.tidyverse.org/
# https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/
# https://r4ds.had.co.nz/graphics-for-communication.html




# 0. dependencies ---------------------------------------------------------

library(ggplot2)
library(palmerpenguins)

# if you haven't installed already, please do: e.g.
# install.packages("ggplot2")

data(penguins)



# 1. basic plots ----------------------------------------------------------

# scatter plots 

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point()

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = island, shape = species)) + 
  geom_point() + 
  facet_wrap(~island)

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = island, shape = species)) + 
  geom_point() + 
  facet_grid(species~island)

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = island, shape = species)) + 
  geom_point() + 
  facet_wrap(species~island)

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = island, shape = species)) + 
  geom_point() + 
  facet_wrap(species~island+sex)

# adding a model

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = island, shape = species)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  facet_wrap(~island)

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = bill_depth_mm, color = island, shape = species)) + 
  geom_point() + 
  geom_smooth(aes(group = island), method = 'lm') +
  facet_wrap(~island)

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = bill_depth_mm, color = island, shape = species)) + 
  geom_density_2d() +
  geom_point(color = 'black') + 
  facet_wrap(~island)



# barplots 

ggplot(penguins, aes(x = sex)) + 
  geom_bar()

ggplot(penguins, aes(x = sex, fill = species)) + 
  geom_bar(position = 'dodge')

ggplot(penguins, aes(x = sex, fill = species)) + 
  geom_bar(position = 'dodge') + 
  facet_wrap(~species)


# boxplots 

ggplot(penguins, aes(x = body_mass_g)) + 
  geom_boxplot()

ggplot(penguins, aes(y = body_mass_g)) + 
  geom_boxplot()

ggplot(penguins, aes(x = sex, y = body_mass_g, fill = species)) + 
  geom_boxplot()

ggplot(penguins, aes(x = sex, y = body_mass_g, fill = species)) + 
  geom_violin() 

ggplot(penguins, aes(x = sex, y = body_mass_g, fill = species)) + 
  geom_violin() + 
  geom_boxplot(width = .25, position = position_dodge(width = 0.9)) 

  
ggplot(penguins, aes(x = sex, y = body_mass_g, fill = species, color = species)) + 
  geom_jitter(position = position_jitterdodge(dodge.width = 0.8, jitter.width = .25)) +
  geom_boxplot(color = 'black', alpha = 0.7)

ggplot(penguins, aes(x = sex, y = body_mass_g, fill = species, color = species)) + 
  geom_jitter(position = position_jitterdodge(dodge.width = 0.8, jitter.width = .25)) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  labs(
    x = "Sex",
    y = "Body Mass (g)",
    title = "Relationship Between Body Mass, Sex, and Species in Palmer Penguins",
    fill = "Species",
    color = "Species"
    )

ggsave("body_mass_sex_and_species.png")
ggsave("body_mass_sex_and_species.pdf")




# 2. extensions to ggplot2 ------------------------------------------------

# these will make your life easier
# ggrepel 
# ggridges 
# ggforce 
# gganimate 
# ggtext 
# cowplot / patchwork 
# ggcorrplot 


library(tidyverse)
library(ggrepel)

mtcars %>% 
  tibble::rownames_to_column('name') %>% 
  ggplot(aes(x = disp, y = hp, label = name)) + 
  geom_point() + 
  ggrepel::geom_text_repel()


library(patchwork)

plt1 <- ggplot(penguins, aes(x = body_mass_g)) + 
  geom_histogram()

plt2 <- ggplot(penguins, aes(x = flipper_length_mm)) + 
  geom_histogram()

plt1
plt2

patchwork <- plt1 + plt2 

patchwork + 
  plot_annotation(
    title = 'Histograms of two variables from the penguins dataset',
    tag_levels = "A")


(plt1 + plt2) / (plt1 + plt2) +  plot_annotation(
  title = 'Histograms of two variables from the penguins dataset',
  tag_levels = "A")



# 3. functions to help with ggplot-ing ------------------------------------


make_density_plot <- function(df, var) {
  ggplot(df, aes(x = {{ var }})) + 
    geom_density(fill = 'dimgrey')
}

make_density_plot(penguins, flipper_length_mm)

make_island_species_stratified_histogram_plot <- function(var, label, legend = F) {
  ggplot(penguins, aes(x = {{ var }}, fill = species)) +
    geom_histogram(alpha = 0.6, position = 'identity') +
    facet_wrap(~island) +
    labs(x = label) +
    theme_bw() +
    theme(legend.position = if (legend) 'right' else 'none')
}

plt1 <- make_island_species_stratified_histogram_plot(body_mass_g, label = 'Body Mass (g)') + scale_x_continuous(n.breaks = 3)
plt2 <- make_island_species_stratified_histogram_plot(flipper_length_mm, label = 'Flipper Length (mm)')
plt3 <- make_island_species_stratified_histogram_plot(bill_length_mm, label = 'Bill Length (mm)')
# plt4 <- make_island_species_stratified_histogram_plot(bill_depth_mm, label = 'Bill Depth (mm)')

plt4 <- ggplot(penguins, aes(x = bill_length_mm)) + geom_density()

(plt1 + plt2) / (plt3 + plt4) 
