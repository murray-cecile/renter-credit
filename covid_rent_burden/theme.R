#===============================================================================#
# AESTHETIC THEMING
#
# Cecile Murray
#===============================================================================#

# Terner colors
terner_gray <- "#5B6770"
terner_blue <- "#4E748B" 
terner_gold <- "#B7B09D" 
terner_navy <- "#011E41"
terner_red <- "#E74C39"

# define theme
terner_theme <- function(...) {
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray75",
                                        size = rel(0.75),
                                        linetype = "dotted"),
        text = element_text(family = "Lato", size = 11),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.ticks.x = element_blank(),
        legend.background = element_blank()) +
    theme(...)
}
