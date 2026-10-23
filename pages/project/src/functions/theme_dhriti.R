################################################################################
# Script Name: theme_dhriti.R
# Purpose: ggplot2 theme + palettes matching the website design system
#          (styles/custom.scss), so charts look native on every page.
#
# Usage:
#   source(here("pages", "project", "src", "functions", "theme_dhriti.R"))
#   ggplot(df, aes(x, y, colour = group)) +
#     geom_line() +
#     scale_colour_manual(values = dhriti_pal) +
#     theme_dhriti()
#
# Colors: emphasis colour is teal; marigold is a second highlight. For
# "one group vs the rest" charts, use teal for the focus and dhriti_grey
# for everything else.
################################################################################

require(ggplot2)

dhriti_cols <- c(
  ink      = "#18203a",
  body     = "#2c3348",
  muted    = "#586074",
  paper    = "#f6f7f9",
  rule     = "#d9dde5",
  teal     = "#0b6b63",
  marigold = "#d99a1e"
)

# Categorical palette (ordered: use the first n you need)
dhriti_pal <- c("#0b6b63", "#d99a1e", "#18203a", "#7aa9a3", "#b5562f", "#8a8fa0")

# Neutral for de-emphasized groups
dhriti_grey <- "#b9bfcb"

# Sequential (low -> high) and diverging (below -> above) ramps
dhriti_seq  <- c("#e3eeec", "#0b6b63")
dhriti_div  <- c("#b5562f", "#f6f7f9", "#0b6b63")

theme_dhriti <- function(base_size = 12,
                         title_family = "sans",   # set to "Bricolage Grotesque" if installed
                         body_family = "sans",    # set to "Source Serif 4" if installed
                         grid = "y") {
  t <- theme_minimal(base_size = base_size, base_family = body_family) +
    theme(
      plot.background   = element_rect(fill = dhriti_cols["paper"], colour = NA),
      panel.background  = element_rect(fill = dhriti_cols["paper"], colour = NA),
      text              = element_text(colour = dhriti_cols["body"]),
      plot.title        = element_text(family = title_family, face = "bold",
                                       colour = dhriti_cols["ink"],
                                       size = rel(1.35), margin = margin(b = 6)),
      plot.subtitle     = element_text(colour = dhriti_cols["muted"],
                                       size = rel(1), margin = margin(b = 12)),
      plot.caption      = element_text(colour = dhriti_cols["muted"],
                                       size = rel(0.8), hjust = 0),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title        = element_text(colour = dhriti_cols["muted"], size = rel(0.9)),
      axis.text         = element_text(colour = dhriti_cols["muted"]),
      panel.grid.major  = element_line(colour = dhriti_cols["rule"], linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      legend.position   = "top",
      legend.justification = "left",
      legend.title      = element_blank(),
      strip.text        = element_text(family = title_family, face = "bold",
                                       colour = dhriti_cols["ink"], hjust = 0),
      plot.margin       = margin(16, 16, 12, 12)
    )
  if (grid == "y") t <- t + theme(panel.grid.major.x = element_blank())
  if (grid == "x") t <- t + theme(panel.grid.major.y = element_blank())
  t
}
