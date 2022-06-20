

library(tidyverse)
library(gghalves)
theme_set(theme_light())

set.seed(1234)
dat <- tibble(
  subject = LETTERS[1:26],
  pre = rnorm(n = 26, mean = 10, sd = 3)
) %>%
  mutate(post = pre + rnorm(n = 26, mean = 0, sd = 2))

dat_long <- dat %>%
  pivot_longer(cols = -subject) %>%
  mutate(name = factor(name, levels = c("pre", "post"))) 

dat_long %>%
  ggplot(aes(x = name, y = value)) +
  geom_line(aes(group = subject),
            color = "light grey",
            size = 0.7) +
  geom_point(aes(group = subject,
                 color = name),
             alpha = 0.7,
             size = 2) +
  geom_half_violin(aes(x = name),
                   data = dat_long %>% filter(name == 'pre'),
                   fill = "light grey",
                   color = "light grey",
                   side = 'l',
                   alpha = 0.7) +
  geom_half_violin(aes(x = name),
                   data = dat_long %>% filter(name == 'post'),
                   fill = "palegreen",
                   color = "palegreen",
                   side = 'r',
                   alpha = 0.7) +
  scale_color_manual(values = c("pre" = "light grey", "post" = "palegreen")) +
  labs(x = "Test Time",
       y = NULL,
       title = "Changes from Pre to Post") +
  theme(axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 15),
        plot.title = element_text(size = 18),
        legend.position = "none")



