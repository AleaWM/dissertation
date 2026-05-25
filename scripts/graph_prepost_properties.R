library(tidyverse)

# 1. Fake data that matches the picture structure
#   - Property A: pre–pre  (-t2, -t1)
#   - Property B: pre–post (-t1,  +t1)
#   - Property C: post–post(+t1, +t2)

df <- tribble(
  ~property, ~time, ~price,
  "Property A", -2, 1.3,
  "Property A", -1, 1.0,
  "Property B", -1, 0.9,
  "Property B",  1, 0.8,
  "Property C",  1, 0.7,
  "Property C",  2, 1.1
)

# 2. Base plot
p <- ggplot(df, aes(x = time, y = price, group = property)) +
  geom_line() +
  geom_point(size = 2) +
  # vertical event line at time 0
  geom_vline(xintercept = 0, linetype = "dashed") +
  # axis labels matching -t2, -t1, 0, t1, t2
  scale_x_continuous(
    breaks = c(-2, -1, 0, 1, 2),
    labels = c("-t2", "-t1", "0", "t1", "t2")
  ) +
  labs(x = "Time", y = "Price") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

p


p +
  annotate("text", x = -1.8, y = 1.35,
           label = "Property A: pre-pre", hjust = 0) +
  annotate("text", x = -1.8, y = 0.95,
           label = "Property B: pre-post", hjust = 0) +
  annotate("text", x = 1.2, y = 0.75,
           label = "Property C: post-post", hjust = 0) +
  annotate("text", x = 0, y = 1.45,
           label = "Event (e.g., FIRM updated)",
           hjust = 0.5)

