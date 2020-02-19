# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(here)

# *****************************************************************************


# *****************************************************************************
# Utility functions -----------------------------------------------------------

save_chart <- function(chart) {
  filename_main <- deparse(substitute(chart))
  ggsave(filename = paste0(filename_main, ".png"), 
         path = here("/outputs"), 
         plot = chart, 
         width = 22, 
         height = 18, 
         units = "cm", 
         dpi = 300)
}

# *****************************************************************************


# *****************************************************************************
# Example chart with default theme --------------------------------------------

default_chart_1 <- mtcars %>%
  as_tibble() %>%
  mutate(transmission = ifelse(am == 0, "Automatic", "Manual")) %>%
  ggplot(mapping = aes(x = disp, y = mpg, colour = factor(cyl), shape = factor(gear))) + 
  geom_point() + 
  facet_wrap(facets = vars(transmission), ncol = 2) + 
  labs(title = "MPG versus displacement", 
       x = "Displacement", 
       y = "MPG") + 
  scale_colour_brewer(name = "No. of cylinders", palette = "Set1") + 
  scale_shape(name = "No. of gears")
save_chart(chart = default_chart_1)

# *****************************************************************************


# *****************************************************************************
# Simple theme adjustments example --------------------------------------------

my_theme <- theme_gray() + 
  theme(
    axis.ticks = element_blank(), 
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_line(colour = grey(0.9)), 
    strip.text = element_text(face = "bold", 
                              hjust = 0, 
                              margin = margin(0, 0, 6, 0, unit = "pt"), 
                              size = rel(1.05)), 
    strip.background = element_blank()
  )

theme_chart_1 <- default_chart_1 + 
  my_theme

save_chart(chart = theme_chart_1)


# *****************************************************************************


# *****************************************************************************
# Illustration of components --------------------------------------------------

highlight_blue <- rgb(red = 4/255, green = 51/255, blue = 255/255)

# plot
chart_plot <- default_chart_1 + 
  theme(plot.background = element_rect(fill = highlight_blue, colour = highlight_blue))
save_chart(chart = chart_plot)

# panel
chart_panel <- default_chart_1 + 
  theme(panel.background = element_rect(fill = highlight_blue, colour = highlight_blue))
save_chart(chart = chart_panel)

# legend
chart_legend <- default_chart_1 + 
  theme(legend.background = element_rect(fill = highlight_blue, colour = highlight_blue))
save_chart(chart = chart_legend)

# legend.box
chart_legend_box <- default_chart_1 + 
  theme(legend.box.background = element_rect(fill = highlight_blue, colour = highlight_blue))
save_chart(chart = chart_legend_box)

# legend.key
chart_legend_key <- default_chart_1 + 
  theme(legend.key = element_rect(fill = highlight_blue, colour = highlight_blue))
save_chart(chart = chart_legend_key)

# strip
chart_strip <- default_chart_1 + 
  theme(strip.background = element_rect(fill = highlight_blue, colour = highlight_blue))
save_chart(chart = chart_strip)

# axes
chart_axes <- default_chart_1 + 
  theme(axis.line = element_line(colour = highlight_blue), 
        axis.ticks = element_line(colour = highlight_blue), 
        axis.text = element_text(colour = highlight_blue), 
        axis.title = element_text(colour = highlight_blue))
save_chart(chart = chart_axes)


# *****************************************************************************


# *****************************************************************************
# Illustration of inheritance -------------------------------------------------

chart_text_inheritance <- default_chart_1 + 
  theme(text = element_text(family = "Fira Sans"))
save_chart(chart = chart_text_inheritance)

# *****************************************************************************


# *****************************************************************************
# Theme creation function -----------------------------------------------------

my_theme <- function(vertical_gridlines = FALSE, ...) {
  t <- theme_gray() + theme(...)
  
  if (vertical_gridlines) {
    t <- t + theme(
      axis.ticks.x = element_blank(), 
      panel.grid.major.x = element_line(colour = grey(0.9))
    )
  }
  
  return(t)
}

chart_without_vertical_gridlines <- default_chart_1 + 
  my_theme(vertical_gridlines = TRUE, axis.ticks.y = element_blank())

# *****************************************************************************