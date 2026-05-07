library(ggplot2)
library(ggforce)

stages <- data.frame(
  label = c("Item\nGeneration", "Scale\nDevelopment", "Scale\nEvaluation"),
  x = c(1, 3, 5),
  y = 0
)

# Build corner coordinates for each box
boxes <- do.call(rbind, lapply(1:nrow(stages), function(i) {
  cx <- stages$x[i]
  data.frame(
    x     = c(cx - 0.55, cx + 0.55, cx + 0.55, cx - 0.55),
    y     = c(-0.4, -0.4, 0.4, 0.4),
    group = i
  )
}))

arrows <- data.frame(
  x    = c(1.65, 3.65),
  xend = c(2.35, 4.35),
  y    = 0,
  yend = 0
)

ggplot() +
  geom_shape(
    data = boxes,
    aes(x = x, y = y, group = group),
    fill = "#F5F5F5", colour = "black", linewidth = 0.7,
    radius = unit(0.02, "npc")
  ) +
  geom_text(
    data = stages,
    aes(x = x, y = y, label = label),
    size = 6, lineheight = 0.9
  ) +
  geom_segment(
    data = arrows,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
    linewidth = 0.8
  ) +
  coord_equal() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave(
  "images/scale_stages.png",
  width = 8, height = 1.5, dpi = 150,
  bg = "white"
)