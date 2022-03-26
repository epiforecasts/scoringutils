library(ggplot2)
library(patchwork)
library(scoringutils)

p1 <-
  ggplot(data.frame(x = seq(-8, 8, 0.01),
                    x_example = rnorm(n = 1601, mean = 0, sd = 0.45)),
         aes(x = x)) +
  # geom_histogram(aes(x = x_example, y = ..density..),
  #                colour = "white", fill = "grey50", bins = 50) +
  geom_function(fun = dnorm, colour = "black",
                args = list(sd = 0.45)) +
  expand_limits(y = c(0, 1.0), x = c(-3, 3)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  ggtitle("More sharp") +
  theme_scoringutils()

p2 <-
  ggplot(data.frame(x = seq(-8, 8, 0.01),
                    x_example = rnorm(n = 1601, mean = 0, sd = 1.25)),
         aes(x = x)) +
  # geom_histogram(aes(x = x_example, y = ..density..), colour = "white", fill = "grey50") +
  geom_function(fun = dnorm, colour = "black",
                args = list(sd = 1.25)) +
  expand_limits(y = c(0, 1.0), x = c(-3, 3)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  ggtitle("Less sharp") +
  theme_scoringutils()

p21 <- ggplot(data.frame(x = seq(-8, 8, 0.01),
                        x_example = rnorm(n = 1601, mean = 0, sd = 1.05)),
             aes(x = x)) +
  geom_histogram(aes(x = x_example, y = ..density..), colour = "white", fill = "grey50") +
  geom_function(fun = dnorm, colour = "black",
                args = list(sd = 1)) +
  ggtitle("Well calibrated") +
  labs(y = "Density") +
  theme_scoringutils()

p22 <- ggplot(data.frame(x = seq(-8, 8, 0.01),
                        x_example = rnorm(n = 1601, mean = 1, sd = 1.05)),
             aes(x = x)) +
  geom_histogram(aes(x = x_example, y = ..density..), colour = "white", fill = "grey50") +
  geom_function(fun = dnorm, colour = "black",
                args = list(mean = 2, sd = 1)) +
  ggtitle("Badly calibrated") +
  labs(y = "Density") +
  theme_scoringutils()

p23 <- ggplot(data.frame(x = seq(-8, 8, 0.01),
                        x_example = rnorm(n = 1601, mean = 0, sd = 1.05)),
             aes(x = x)) +
  geom_histogram(aes(x = x_example, y = ..density..), colour = "white", fill = "grey50") +
  geom_function(fun = dnorm, colour = "black",
                args = list(mean = 0, sd = 2.05)) +
  ggtitle("Badly calibrated") +
  labs(y = "Density") +
  theme_scoringutils()

(p1 + p2) /
  (p21 + p22 + p23) &
  plot_annotation(tag_levels = "A")

ggsave("inst/manuscript/output/calibration-sharpness-illustration.png",
       width = 8, height = 3.8)
