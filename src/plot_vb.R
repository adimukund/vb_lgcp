library(plyr)
library(grid)
library(gridExtra)
library(latex2exp)
library(patchwork)
library(tidyverse)
library(ggExtra)
library(scales)
library(gtable)

# read in data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# The 5 x 5 basic plot test ----------------------------------------------------
df = read_csv(('../data/test_vb_5x5.csv'))
df$Source = factor(df$Source, levels = c("Real", "Computed"))

p = ggplot(data = df)
p = p + facet_wrap( ~ Source)
p = p + geom_tile(aes(x = X,
                      y = Y,
                      fill = Rate))
p = p + coord_cartesian(xlim = c(min(df$X) - 0.5, max(df$X) + 0.5),
                        ylim = c(min(df$Y) - 0.5, max(df$Y) + 0.5))
p = p + scale_fill_viridis(
    TeX('$\\mu$'),
    option = "cividis",
    # trans="log",
    limits = c(-10, 110),
    breaks = c(0, 50, 100)
)
p = p + theme_bw()
p = p + theme(strip.background = element_rect(fill = "grey85"))
p = p + theme(strip.text = element_text(colour = "grey10", face = "bold"))
p = p + theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
p

ggsave(
    "../figs/test_vb_5x5.pdf",
    p,
    device = "pdf",
    width = 8,
    height = 4
)

p = ggplot(data = dplyr::filter(df, Source == "Real"))
p = p + facet_wrap( ~ Source)
p = p + geom_tile(aes(x = X,
                      y = Y,
                      fill = Rate))
p = p + coord_cartesian(xlim = c(min(df$X) - 0.5, max(df$X) + 0.5),
                        ylim = c(min(df$Y) - 0.5, max(df$Y) + 0.5))
p = p + scale_fill_viridis(
    TeX('$\\mu$'),
    option = "cividis",
    # trans="log",
    limits = c(-10, 110),
    breaks = c(0, 50, 100)
)
p = p + theme_bw()
p = p + theme(strip.background = element_rect(fill = "grey85"))
p = p + theme(strip.text = element_text(colour = "grey10", face = "bold"))
p = p + theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
p

ggsave(
    "../figs/real_vb_5x5.pdf",
    p,
    device = "pdf",
    width = 4,
    height =3.25 
)

# The 2 hills plot test ----------------------------------------------------
df = read_csv('../data/test_vb_2hills.csv')
df$Source = factor(df$Source, levels = c("Real", "Computed"))

p = ggplot(data = df)
p = p + facet_wrap( ~ Source)
p = p + geom_tile(aes(x = X,
                      y = Y,
                      fill = Rate))
p = p + coord_cartesian(xlim = c(min(df$X) - 0.5, max(df$X) + 0.5),
                        ylim = c(min(df$Y) - 0.5, max(df$Y) + 0.5))
p = p + scale_x_continuous(breaks = min(df$X):max(df$X))
p = p + scale_y_continuous(breaks = min(df$Y):max(df$Y))
p = p + scale_fill_viridis(
    TeX('$\\mu$'),
    option = "cividis",
    # trans="log",
    limits = c(-10, 110),
    breaks = c(0, 50, 100)
)
p = p + theme_bw()
p = p + theme(strip.background = element_rect(fill = "grey85"))
p = p + theme(strip.text = element_text(colour = "grey10", face = "bold"))
p = p + theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
p

ggsave(
    "../figs/test_vb_2hill.pdf",
    p,
    device = "pdf",
    width = 8,
    height = 4
)


# The pacman test ----------------------------------------------------
df = read_csv('../data/test_vb_pacman.csv')
df$Source = factor(df$Source, levels = c("Real", "Computed"))

p = ggplot(data = df)
p = p + facet_wrap( ~ Source)
p = p + geom_tile(aes(x = X,
                      y = Y,
                      fill = Rate))
p = p + coord_cartesian(xlim = c(min(df$X) - 0.5, max(df$X) + 0.5),
                        ylim = c(min(df$Y) - 0.5, max(df$Y) + 0.5))
p = p + scale_x_continuous(breaks = min(df$X):max(df$X))
p = p + scale_y_continuous(breaks = min(df$Y):max(df$Y))
p = p + scale_fill_viridis(
    TeX('$\\mu$'),
    option = "cividis",
    # trans="log",
    limits = c(-10, 110),
    breaks = c(0, 50, 100)
)
p = p + theme_bw()
p = p + theme(strip.background = element_rect(fill = "grey85"))
p = p + theme(strip.text = element_text(colour = "grey10", face = "bold"))
p = p + theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
p

ggsave(
    "../figs/test_vb_pacman.pdf",
    p,
    device = "pdf",
    width = 8,
    height = 4
)

# The repeat pacman test ----------------------------------------------------
df = read_csv('../data/repeat_test_vb_pacman_mean.csv')
df$Source = factor(df$Source, levels = c("Real", "Computed"))

p = ggplot(data = df)
p = p + facet_wrap( ~ Source)
p = p + geom_tile(aes(x = X,
                      y = Y,
                      fill = Rate))
p = p + coord_cartesian(xlim = c(min(df$X) - 0.5, max(df$X) + 0.5),
                        ylim = c(min(df$Y) - 0.5, max(df$Y) + 0.5))
p = p + scale_x_continuous(breaks = min(df$X):max(df$X))
p = p + scale_y_continuous(breaks = min(df$Y):max(df$Y))
p = p + scale_fill_viridis(
    TeX('$\\mu$'),
    option = "cividis",
    # trans="log",
    limits = c(-10, 110),
    breaks = c(0, 50, 100)
)
p = p + theme_bw()
p = p + theme(strip.background = element_rect(fill = "grey85"))
p = p + theme(strip.text = element_text(colour = "grey10", face = "bold"))
p = p + theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
p

ggsave(
    "../figs/repeat_vb_pacman.pdf",
    p,
    device = "pdf",
    width = 8,
    height = 4
)


# The repeat grf test ----------------------------------------------------
df = read_csv('../data/repeat_test_lgrf_mean.csv')
df$Source = factor(df$Source, levels = c("Real", "Computed"))

p = ggplot(data = df)
p = p + facet_wrap( ~ Source)
p = p + geom_tile(aes(x = X,
                      y = Y,
                      fill = Rate))
p = p + coord_cartesian(xlim = c(min(df$X) - 0.5, max(df$X) + 0.5),
                        ylim = c(min(df$Y) - 0.5, max(df$Y) + 0.5))
p = p + scale_x_continuous(breaks = min(df$X):max(df$X))
p = p + scale_y_continuous(breaks = min(df$Y):max(df$Y))
p = p + scale_fill_viridis(
    TeX('$\\mu$'),
    option = "cividis",
    # trans="log",
    limits = c(-10, plyr::round_any(max(df$Rate), 100, f=ceiling)+10),
    breaks = c(0, plyr::round_any(max(df$Rate), 100, f=ceiling)/2, plyr::round_any(max(df$Rate), 100, f=ceiling))
)
p = p + theme_bw()
p = p + theme(strip.background = element_rect(fill = "grey85"))
p = p + theme(strip.text = element_text(colour = "grey10", face = "bold"))
p = p + theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
p

ggsave(
    "../figs/repeat_vb_lgrf.pdf",
    p,
    device = "pdf",
    width = 8,
    height = 4
)