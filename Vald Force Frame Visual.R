
### Vald Force Frame Visual
library(tidyverse)

theme_set(theme_classic() +
          theme(strip.background = element_rect(fill = "black"),
                strip.text = element_text(size = 13, face = "bold", color = "white"),
                axis.text = element_text(size = 13, face = "bold"),
                axis.title.x = element_text(size = 14, face = "bold"),
                plot.title = element_text(size = 18),
                plot.subtitle = element_text(size = 14),
                legend.position = "none"))

set.seed(678)
dat <- tibble(
  player = rep(c("Tom", "Alan", "Karl", "Sam"), each = 2),
  test = rep(c("Pull", "Squeeze"), times = 4)
  ) %>%
  mutate(l_force = ifelse(test == "Pull", rnorm(n = nrow(.), mean = 305, sd = 30),
                          rnorm(n = nrow(.), mean = 360, sd = 30)),
         r_force = ifelse(test == "Pull", rnorm(n = nrow(.), mean = 305, sd = 30),
                          rnorm(n = nrow(.), mean = 360, sd = 30)),
         pct_imbalance = ifelse(l_force > r_force, ((l_force - r_force) / l_force) * -1,
                            (r_force - l_force) / r_force))

dat %>%
  mutate(left = paste("Left =", round(l_force, 1), sep = " "),
         right = paste("Right =", round(r_force, 1), sep = " "),
         l_r = paste(left, right, sep = "\n"),
         asym_flag = ifelse(abs(pct_imbalance) > 0.1, "flag", "no flag"),
         weakness_flag = ifelse((test == "Pull" & (l_force < 250 | r_force < 250)) |
                                 (test == "Squeeze" & (l_force < 330 | r_force < 330)), "flag", "no flag")) %>%
  ggplot(aes(x = pct_imbalance, y = player)) +
  geom_rect(aes(xmin = -0.1, xmax = 0.1),
            ymin = 0,
            ymax = Inf,
            fill = "light grey",
            alpha = 0.3) +
  geom_col(aes(fill = asym_flag),
           alpha = 0.6,
           color = "black") +
  geom_vline(xintercept = 0, 
             size = 1.3) +
  annotate(geom = "text",
           x = -0.2, 
           y = 4.5,
           size = 6,
           label = "Left") +
  annotate(geom = "text",
           x = 0.2, 
           y = 4.5,
           size = 6,
           label = "Right") +
  geom_label(aes(x = 0, y = player, label = l_r, color = weakness_flag)) +
  scale_color_manual(values = c("flag" = "red", "no flag" = "black")) +
  scale_fill_manual(values = c("flag" = "red", "no flag" = "palegreen")) +
  labs(x = "% Imbalance",
       y = NULL,
       title = "Force Frame Team Testing",
       subtitle = "Pull Weakness < 250 | Squeeze Weakness < 330") +
  facet_grid(~test) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1),
                     limits = c(-0.3, 0.3))

