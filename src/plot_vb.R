library(plyr)
library(grid)
library(gridExtra)
library(latex2exp)
library(patchwork)
library(tidyverse)
library(ggExtra)
library(scales)
library(gtable)
library(lognorm)
library(gamlss.dist)
library(gamlss)

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


# The repeat grf stats ----------------------------------------------------
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999") 

df = read_csv('../data/repeat_test_lgrf_mean.csv')
df$Source = factor(df$Source, levels = c("Real", "Computed"))

p = ggplot()
p = p + geom_freqpoly(
    data = df,
    aes(
        x = Rate,
        color = Source,
        fill = Source,
    ),
    size = 1
)
p = p + ylab("Num. Cells")
p = p + scale_color_manual(values=cbPalette) + scale_fill_manual(values=cbPalette)
p = p + theme_cowplot()
p

ggsave(
    "../figs/repeat_vb_lgrf_histogram.pdf",
    p,
    device = "pdf",
    width = 7,
    height = 4
)


df %>%
    dplyr::group_by(Source) %>%
    dplyr::do(
        setNames(data.frame(t(estimateParmsLognormFromSample(.$Rate))),
                 c("mu", "sigma"))) -> dfz

logNormDist = function(x, mu, sigma) {
    return(dLOGNO(x, mu, sigma))
}

p = ggplot()
p = p + geom_line(
    aes(
        x = seq(0, 400, by=0.1),
        y = logNormDist(seq(0, 400, by=0.1), 3.97, 0.990),
        color = "Real",
    ),
    size = 1,
)
p = p + geom_line(
    aes(
        x = seq(0, 400, by=0.1),
        y = logNormDist(seq(0, 400, by=0.1), 3.99, 0.943),
        color = "Computed",
    ),
    size = 1,
)
p = p + scale_color_manual(
    name = "Source",
    breaks = c("Real", "Computed"),
    values = c(cbPalette[1], cbPalette[2]),
#     values = c(Real=cbPalette[1], Computed=cbPalette[2])
)
p = p + coord_cartesian(
    xlim=c(0, 350)
)
p = p + xlab("Rate") + ylab("Density of Log-Normal PDF")
p = p + theme_cowplot()
p

ggsave(
    "../figs/density_fit_lgrf.pdf",
    p,
    device = "pdf",
    width = 8,
    height = 4
)

