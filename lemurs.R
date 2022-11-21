library(tidyverse)
library(gt)

# load data (tidytuesday and local file)
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')
taxons <- readr::read_csv('taxon.csv')

# image
# common name
# latin name
# bar chart number of lemurs
# scatter plot of age vs weight
# expandable: meet a lemur
# save 

# Number of lemurs of each species ----------------------------------------

# calculate number of lemurs by species
lemur_nums <- lemurs %>% 
  left_join(taxons, by = "taxon") %>% 
  select(taxon, name, common, latin) %>% 
  distinct() %>% 
  group_by(taxon) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  select(taxon, common, latin, n) %>% 
  distinct() %>% 
  arrange(desc(n)) %>% 
  mutate(taxon = factor(taxon, levels = taxon))

# bar chart of number of each species
taxon_bar <- function(select_taxon, df){
  # prep data
  plot_data <- df %>% 
    filter(taxon == select_taxon)
    
  # make plot
  ggplot(data = plot_data,
         mapping = aes(x = "A", 
                       y = n, 
                       label = n)) +
    geom_col(fill = "#a1b70d") +
    geom_text(aes(y = n - 5), 
              hjust = 1, 
              colour = "black",
              size = 12,
              fontface = "bold") +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, plyr::round_any(max(df$n), 50, ceiling))) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank())
}

# map plots
barcharts <- purrr::map(.x = lemur_nums$taxon, .f = ~taxon_bar(.x, df = lemur_nums))



# Age vs weight scatter plots ---------------------------------------------
taxon_scatter <- function(select_taxon, df){
  # prep data
  plot_data <- df %>% 
    filter(taxon == select_taxon) %>% 
    select(age_at_wt_mo, weight_g) %>% 
    drop_na()
  
  # make plot
  ggplot(data = df,
         mapping = aes(x = age_at_wt_mo, 
                       y = weight_g)) +
    geom_point(colour = "#a1b70d") +
    geom_smooth(method = "lm",
                se = FALSE, 
                colour = "black") +
    labs(x = "age (months)", y = "weight(g)") +
    scale_x_continuous(limits = c(0, plyr::round_any(max(df$age_at_wt_mo), 50, ceiling))) +
    scale_y_continuous(limits = c(0, plyr::round_any(max(df$weight_g), 50, ceiling))) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank())
}

taxon_scatter("PPOT", lemurs)

# map plots
scatters <- purrr::map(.x = lemur_nums$taxon, .f = ~taxon_bar(.x, df = lemurs))


# Meet the lemur ----------------------------------------------------------




# Images ------------------------------------------------------------------





# Turn it into a table ----------------------------------------------------









