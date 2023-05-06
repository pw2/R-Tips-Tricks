
## Displaying Tables & Plots Together

### Load libraries
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(patchwork)
library(broom)
library(palmerpenguins)

## set plot theme
theme_set(theme_classic() +
            theme(axis.text = element_text(size = 11, face = "bold"),
                  axis.title = element_text(size = 13, face = "bold"),
                  plot.title = element_text(size = 15),
                  legend.position = "top"))


## load data
data("penguins")
d <- penguins %>%
  na.omit()

d %>%
  head()


## Create Plots
plt1 <- d %>%
  ggplot(aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point(aes(fill = sex),
             size = 4,
             shape = 21,
             color = "black",
             alpha = 0.5) +
  geom_smooth(method = "lm",
              aes(color = sex)) +
  scale_fill_manual(values = c("female" = "green", "male" = "blue")) +
  scale_color_manual(values = c("female" = "green", "male" = "blue")) +
  labs(x = "Flipper Length (mm)",
       y = "Bill Length (mm)",
       title = "Bill Length ~ Flipper Length")


plt2 <- d %>%
  ggplot(aes(x = sex, y = bill_length_mm)) +
  geom_violin(alpha = 0.5,
              aes(fill = sex)) +
  geom_boxplot(width = 0.2) +
  geom_jitter(alpha = 0.5) +
  labs(x = "Sex",
       y = "Bill Length (mm)",
       title = "Bill Length Conditional on Penguin Gender")


## Create table
fit <- d %>%
  lm(bill_length_mm ~ flipper_length_mm + sex, data = .) %>%
  tidy() %>%
  mutate(across(.cols = estimate:statistic,
                ~round(.x, 3)),
         term = case_when(term == "(Intercept)" ~ "Intercept",
                          term == "flipper_length_mm" ~ "Flipper Length (mm)",
                          term == "sexmale" ~ "Male"))

## Build table into a nice ggtextable() to visualize it
tbl <- ggtexttable(fit, rows = NULL, theme = ttheme("blank")) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
  tab_add_hline(at.row = 4, row.side = "bottom", linewidth = 3, linetype = 1)


## Plots & Table together using ggarange()
ggarrange(plt1, plt2, tbl,
          ncol = 2, nrow = 2)


## Plots & Table together using patchwork
# Need to build the table as a tableGrob() instead of ggtextable
# to make it work with patch work
tbl2 <- tableGrob(fit, rows = NULL, theme = ttheme("blank")) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
  tab_add_hline(at.row = 4, row.side = "bottom", linewidth = 3, linetype = 1)

# now visualize together
wrap_plots(plt1, plt2, tbl2, 
           ncol = 2,
           nrow = 2)



