library(ggplot2)
library(ggforce)

# ── Geometry ──────────────────────────────────────────────────────────────────
hub_r    <- 2.6
centre_r <- 0.95
domain_r <- 0.8

# ── Domain positions ──────────────────────────────────────────────────────────
ang <- (90 - (0:6) * (360 / 7)) * pi / 180

domains <- data.frame(
  label = c(
    "Digital\nSkills",
    "Digital\nHealth\nLiteracy",
    "Digital\nSelf-Efficacy",
    "Socio-Digital\nNorms",
    "Digital\nTrust",
    "Digital\nOutcome\nExpectancy",
    "Digital\nBehavioural\nRegulation"
  ),
  x = cos(ang) * hub_r,
  y = sin(ang) * hub_r
)

domains$sx0 <- domains$x * centre_r / hub_r
domains$sy0 <- domains$y * centre_r / hub_r
domains$sx1 <- domains$x * (hub_r - domain_r) / hub_r
domains$sy1 <- domains$y * (hub_r - domain_r) / hub_r

# ── Driver boxes ──────────────────────────────────────────────────────────────
box_cx <- -5.5 # Proximity of drivers to hub & spoke
box_hw <- 0.8
box_hh <- 0.5

drivers <- data.frame(
  title  = c("Economic", "Social", "Cultural", "Personal"),
  y = c(2.1, 0.7, -0.7, -2.1)
)

# ── Arrow anchors ─────────────────────────────────────────────────────────────
left_arrow_start <- -(hub_r + domain_r + 1)
left_arrow_end <- -(hub_r + domain_r + 0.3)
right_arrow_start <- hub_r + domain_r + 0.3
right_arrow_end   <- hub_r + domain_r + 1

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot() +
  
  # Spokes
  geom_segment(
    data = domains,
    aes(x = sx1, y = sy1, xend = sx0, yend = sy0),
    arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
    colour = "grey40", linewidth = 0.5
  ) +
  
  # Domain circles
  geom_circle(
    data = domains,
    aes(x0 = x, y0 = y, r = domain_r),
    fill = "white", colour = "black", linewidth = 0.8
  ) +
  geom_text(
    data = domains,
    aes(x = x, y = y, label = label),
    size = 4.5, lineheight = 0.9
  ) +
  
  # Central DHE circle
  geom_circle(
    aes(x0 = 0, y0 = 0, r = centre_r),
    fill = "#DCDCDC", colour = "black", linewidth = 1.0
  ) +
  annotate(
    "text", x = 0, y = 0,
    label = "DHE",
    size = 8.0, fontface = "bold", lineheight = 0.9
  ) +
  
  # Single left arrow
  annotate(
    "segment",
    x = left_arrow_start, y = 0,
    xend = left_arrow_end, yend = 0,
    arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
    colour = "black", linewidth = 0.9
  ) +
  
  # Drivers label and enclosing rectangle
  geom_rect(
    aes(
      xmin = box_cx - box_hw - 0.15,
      xmax = box_cx + box_hw + 0.15,
      ymin = -2.2 - box_hh - 0.15,
      ymax =  2.2 + box_hh + 0.15
    ),
    fill = NA, colour = "black", linewidth = 0.5, linetype = "dashed"
  ) +
  annotate(
    "text",
    x = box_cx, y = 2.2 + box_hh + 0.45,
    label = "Drivers / Causes",
    size = 4, fontface = "italic"
  ) +
  
  # Driver boxes
  geom_rect(
    data = drivers,
    aes(
      xmin = box_cx - box_hw, xmax = box_cx + box_hw,
      ymin = y - box_hh,      ymax = y + box_hh
    ),
    fill = "#F5F5F5", colour = "black", linewidth = 0.7
  ) +
  geom_text(
    data = drivers,
    aes(x = box_cx, y = y + 0.10, label = title),
    size = 5, fontface = "bold", vjust = 1
  ) +
  
  # Single right arrow
  annotate(
    "segment",
    x = right_arrow_start, y = 0,
    xend = right_arrow_end, yend = 0,
    arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
    colour = "black", linewidth = 0.9
  ) +
  annotate(
    "text",
    x = right_arrow_end + 0.3, y = 0,
    label = "Leads to greater:\n\n\u2022 Uptake\n\u2022 Engagement\n\u2022 Effectiveness",
    hjust = 0, size = 5.5, lineheight = 1.1
  ) +
  
  coord_equal(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 160, 20, 20)
  )

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave(
  "images/dhe_model.png",
  plot = p,
  width = 14, height = 7, dpi = 150,
  bg = "white"
)