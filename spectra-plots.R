library(tidyverse)
library(glue)
library(RCurl)
library(gtools)
library(GGally)
library(ggbiplot)
library(showtext)
library(patchwork)
library(viridis)

delta_gr_mag <- runif(n = 1, min = -2, max = +2)
delta_ri_mag <- runif(n = 1, min = -2, max = +2)
bands_min <- 15
bands_max <- 20
delta <- 0.1
urlBase <- "http://skyserver.sdss.org/dr15/SkyserverWS/SearchTools/SqlSearch?"

master_targets <- read_csv("data/dec-10to-8-clean.csv") %>% 
  select(ra:fiberid, cor) %>% 
  arrange(cor)

font_add("Fuzzy Bubbles", regular = "fonts/ABeeZee-Regular.ttf")
showtext_auto()
theme_clean <- function() {
  theme_minimal(base_family = "Fuzzy Bubbles") +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 16, family = "Fuzzy Bubbles"),
          plot.background = element_rect(fill = "white", color = NA),
          axis.text = element_text(size = 16),
          axis.title = element_text(face = "bold", size = 20),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.text = element_text(size = 16))
}

get_spectrum <- function(object = match[1,], 
                         wavelength_lower_limit = 3000, 
                         wavelength_upper_limit = 10000){
  plate <- object$plate
  mjd <- object$mjd
  fiber <- object$fiberid
  url_spect <- glue("http://dr12.sdss.org/csvSpectrum?plateid={plate}", 
                    "&mjd={mjd}&fiber={fiber}&reduction2d=v5_7_0")
  spectrum <- read_csv(file = url_spect)
  spectrum %>% 
    filter(between(Wavelength, wavelength_lower_limit, wavelength_upper_limit)) %>% 
    select(Wavelength, BestFit)
}

index <- 200
delta_gr_mag <- master_targets$g[index] - master_targets$r[index]
delta_ri_mag <- master_targets$r[index] - master_targets$i[index]
plate <- master_targets$plate[index]

match_SqlQuery <- glue("SELECT top 4 p.ra, p.dec, ",
                       "p.u, p.g, p.r, p.i, p.z, p.objid, ", 
                       "s.specobjid, s.class, s.subclass, s.survey, ", 
                       "s.plate, s.mjd, s.fiberid ", 
                       "FROM photoObj AS p ", 
                       "JOIN SpecObj AS s ON s.bestobjid = p.objid ",
                       "WHERE p.g BETWEEN {bands_min} AND {bands_max} ",
                       "AND p.r BETWEEN {bands_min} AND {bands_max} ", 
                       "AND p.i BETWEEN {bands_min} AND {bands_max} ", 
                       "AND s.class = 'STAR' ",
                       "AND p.clean = 1 AND (p.calibStatus_r & 1) != 0",
                       "AND (p.g-p.r) BETWEEN {delta_gr_mag} AND {delta_gr_mag+delta} ",
                       "AND (p.r-p.i) BETWEEN {delta_ri_mag} AND {delta_ri_mag+delta} ",
                       "AND s.survey != 'eboss' ", 
                       "AND s.plate != {plate}" )
match_SqlQuery <- str_squish(match_SqlQuery)
X <- getForm(urlBase, cmd = match_SqlQuery, format = "csv")
match <- read.table(text = X, header = TRUE, sep = ",", dec = ".", comment.char = "#")

match_SqlQuery <- glue("SELECT top 4 p.ra, p.dec, ",
                       "p.u, p.g, p.r, p.i, p.z, p.objid, ", 
                       "s.specobjid, s.class, s.subclass, s.survey, ", 
                       "s.plate, s.mjd, s.fiberid ", 
                       "FROM photoObj AS p ", 
                       "JOIN SpecObj AS s ON s.bestobjid = p.objid ",
                       "WHERE s.specobjid = {specobj}",
                       "OR s.specobjid = {specobj1}")
match_SqlQuery <- str_squish(match_SqlQuery)
X <- getForm(urlBase, cmd = match_SqlQuery, format = "csv")
match1 <- read.table(text = X, header = TRUE, sep = ",", dec = ".", comment.char = "#")




spect1 <- get_spectrum(match[1,])
spect2 <- get_spectrum(match[2,])
spect3 <- get_spectrum(match1[1,])
spect4 <- get_spectrum(match1[2,])

x <- spect1 %>% filter(between(Wavelength, 5500, 7000)) %>% pull(BestFit)
y <- spect2 %>% filter(between(Wavelength, 5500, 7000)) %>% pull(BestFit)
cor_value <- cor(x, y)
bind_cols(star_1=match, star_2=master_targets[index,], cor = cor_value)

spectrum_plot <- function(spectrum, object) {
  spectrum %>% 
    ggplot(aes(Wavelength/10, BestFit)) +
    geom_line(colour = "gray70") +
    geom_line(data = spectrum %>% filter(between(Wavelength, 5500, 7000)),
              aes(Wavelength/10, BestFit),
              colour = "black",
              size = 1.5) +
    scale_x_continuous(breaks = seq(400, 1000, by = 100)) +
    labs(x = "Wavelength (nm)",
         y = "Flux (erg/cm²/s Å)") +
    annotate("text", 
             x = 800,
             y = max(spectrum$BestFit) * 0.9,
             family = "Fuzzy Bubbles",
             size = 8,
             label = glue("specobjid\n{as.character(object$specobjid)}")) +
    annotate("text", 
             x = 620,
             y = min(spectrum$BestFit) * 0.9 + max(spectrum$BestFit) * 0.1,
             family = "Fuzzy Bubbles",
             size = 10,
             label = glue("u = {round(object$u, 2)}, g = {round(object$g, 2)}, r = {round(object$r, 2)}, i = {round(object$i, 2)}, z = {round(object$z, 2)}")) +
    theme_clean() +
    theme(axis.title.y = element_text(size = 20))
}

plot1 <- spectrum_plot(spect1, match[1,]) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
plot2 <- spectrum_plot(spect2, match[2,])
plot3 <- spectrum_plot(spect3, match[3,]) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
plot4 <- spectrum_plot(spect4, match[4,])

(plot1 / plot2)  + 
  plot_annotation(tag_levels = 'A',
                  tag_prefix = "(",
                  tag_suffix = ")")
(plot3 / plot4)  + 
  plot_annotation(tag_levels = list(c('C', 'D')),
                  tag_prefix = "(",
                  tag_suffix = ")")


#plot from z1_extra data

my_index <- 159
first_star <- z1_extra[my_index, 1:15]
second_star <- z1_extra[my_index, 16:30]
names(second_star) <- names(first_star)

spectrum_plot <- function(spectrum, object) {
  spectrum %>% 
    ggplot(aes(Wavelength/10, BestFit)) +
    geom_line(colour = "gray70") +
    geom_line(data = spectrum %>% filter(between(Wavelength, 5500, 7000)),
              aes(Wavelength/10, BestFit),
              colour = "black",
              size = 1.5) +
    scale_x_continuous(breaks = seq(400, 1000, by = 100)) +
    labs(x = "Wavelength (nm)",
         y = "Flux (erg/cm²/s Å)") +
    annotate("text", 
             x = 800,
             y = max(spectrum$BestFit) * 0.85,
             family = "Fuzzy Bubbles",
             size = 8,
             label = glue("specobjid\n{as.character(object$specobjid)}")) +
    annotate("text", 
             x = 550,
             y = max(spectrum$BestFit) * 0.85,
             family = "Fuzzy Bubbles",
             size = 8,
             label = glue("subclass\n{as.character(object$subclass)}")) +
    annotate("text", 
             x = 620,
             y = min(spectrum$BestFit) * 0.9 + max(spectrum$BestFit) * 0.1,
             family = "Fuzzy Bubbles",
             size = 10,
             label = glue("u = {round(object$u, 2)}, g = {round(object$g, 2)}, r = {round(object$r, 2)}, i = {round(object$i, 2)}, z = {round(object$z, 2)}")) +
    theme_clean() +
    theme(axis.title.y = element_text(size = 20))
}



spect1 <- get_spectrum(first_star)
spect2 <- get_spectrum(second_star)
plot1 <- spectrum_plot(spect1, first_star) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
plot2 <- spectrum_plot(spect2, second_star)
(plot1 / plot2)  + 
  plot_annotation(tag_levels = 'A',
                  tag_prefix = "(",
                  tag_suffix = ")")
