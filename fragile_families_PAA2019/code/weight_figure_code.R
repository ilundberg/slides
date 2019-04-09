library(tidyverse)
library(haven)

d <- read_dta("/Users/iandl/Downloads/FF_wave1_2019.dta")
forplot <- d %>%
  filter(!is.na(m1natwt)) %>%
  arrange(-m1natwt) %>%
  mutate(prop_weight = cumsum(m1natwt) / sum(m1natwt),
         prop_sample = (1:n()) / n()) %>%
  select(prop_weight, prop_sample)

p25 <- forplot %>%
  filter(prop_sample > .25) %>%
  filter((1:n()) == 1)

forplot %>%
  ggplot(aes(x = prop_sample, y = prop_weight)) +
  #ylab("Proportion of weight\n") +
  scale_x_continuous(breaks = c(0,1), minor_breaks = NULL,
                     labels = function(x) format(round(x,2), nsmall = 2, digits = 2)) +
  scale_y_continuous(breaks = c(0,1), minor_breaks = NULL,
                     labels = function(x) format(round(x,2), nsmall = 2, digits = 2)) +
  theme_bw() +
  coord_fixed() +
  theme(axis.text = element_text(face = "bold"),
        axis.title = element_blank()) +
  ggsave("/Users/iandl/Dropbox/slides_for_FF_workshop/figures/weight_issue_0.pdf",
         height = 1.8, width = 1.9)

forplot %>%
  ggplot(aes(x = prop_sample, y = prop_weight)) +
  annotate(geom = "segment", x = 0, xend = 1, y = 0, yend = 1, linetype = "dashed",
           color = "darkgray") +
  annotate(geom = "text", x = .65, y = .55,
           label = "If simple random sample",
           size = 2.5,
           angle = 45, color = "darkgray", fontface = "bold") +
  #xlab("\nProportion of sample") +
  #ylab("Proportion of weight\n") +
  scale_x_continuous(breaks = c(0,1), minor_breaks = NULL,
                     labels = function(x) format(round(x,2), nsmall = 2, digits = 2)) +
  scale_y_continuous(breaks = c(0,1), minor_breaks = NULL,
                     labels = function(x) format(round(x,2), nsmall = 2, digits = 2)) +
  theme_bw() +
  coord_fixed() +
  theme(axis.text = element_text(face = "bold"),
        axis.title = element_blank()) +
  ggsave("/Users/iandl/Dropbox/slides_for_FF_workshop/figures/weight_issue_1.pdf",
         height = 1.8, width = 1.9)

forplot %>%
  ggplot(aes(x = prop_sample, y = prop_weight)) +
  annotate(geom = "segment", x = 0, xend = 1, y = 0, yend = 1, linetype = "dashed",
           color = "darkgray") +
  geom_line(color = "blue", size = 1.3) +
  annotate(geom = "text", x = .65, y = .55,
           label = "If simple random sample",
           size = 2.5,
           angle = 45, color = "darkgray", fontface = "bold") +
  #xlab("\nProportion of sample") +
  #ylab("Proportion of weight\n") +
  scale_x_continuous(breaks = c(0,1), minor_breaks = NULL,
                     labels = function(x) format(round(x,2), nsmall = 2, digits = 2)) +
  scale_y_continuous(breaks = c(0,1), minor_breaks = NULL,
                     labels = function(x) format(round(x,2), nsmall = 2, digits = 2)) +
  theme_bw() +
  coord_fixed() +
  theme(axis.text = element_text(face = "bold"),
        axis.title = element_blank()) +
  ggsave("/Users/iandl/Dropbox/slides_for_FF_workshop/figures/weight_issue_2.pdf",
         height = 1.8, width = 1.9)

forplot %>%
  ggplot(aes(x = prop_sample, y = prop_weight)) +
  annotate(geom = "segment", x = .25, xend = .25, y = 0, yend = p25$prop_weight,
           color = "seagreen4", size = 1.15) +
  annotate(geom = "segment", x = .25, xend = 0, y = p25$prop_weight, yend = p25$prop_weight,
           color = "seagreen4", size = 1.15) +
  annotate(geom = "segment", x = 0, xend = 1, y = 0, yend = 1, linetype = "dashed",
           color = "darkgray") +
  geom_line(color = "blue", size = 1.3) +
  annotate(geom = "text", x = .65, y = .55,
           label = "If simple random sample",
           size = 2.5,
           angle = 45, color = "darkgray", fontface = "bold") +
  #xlab("\nProportion of sample") +
  #ylab("Proportion of weight\n") +
  scale_x_continuous(breaks = c(0,.25,1), minor_breaks = NULL,
                     labels = function(x) format(round(x,2), digits = 2)) +
  scale_y_continuous(breaks = c(0,p25$prop_weight,1), minor_breaks = NULL,
                     labels = function(x) format(round(x,2), digits = 2)) +
  theme_bw() +
  coord_fixed() +
  theme(axis.text = element_text(face = "bold"),
        axis.title = element_blank()) +
  ggsave("/Users/iandl/Dropbox/slides_for_FF_workshop/figures/weight_issue_3.pdf",
         height = 1.8, width = 1.9)

  