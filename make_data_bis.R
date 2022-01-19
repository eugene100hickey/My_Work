library(tidyverse)
library(glue)
library(RCurl)
library(gtools)
library(xml2)


# SQL that downloads some info on the chosen target from SDSS.
# ObjID from SDSS specifies the target
# set search parameters
N <- 50
sub_N <- 10
delta <- 0.1
bands_min <- 15
bands_max <- 20


master_target_SqlQuery <- glue("SELECT top {N} p.ra, p.dec, ",
                               "p.u, p.g, p.r, p.i, p.z, p.objid, ", 
                               "s.specobjid, s.class, s.subclass, s.survey, ", 
                               "s.plate, s.mjd, s.fiberid ", 
                               "FROM photoObj AS p ", 
                               "JOIN SpecObj AS s ON s.bestobjid = p.objid ",
                               "WHERE p.g BETWEEN {bands_min} AND {bands_max} ",
                               "AND p.r BETWEEN {bands_min} AND {bands_max} ", 
                               "AND p.i BETWEEN {bands_min} AND {bands_max} ", 
                               "AND s.class = 'STAR' ",
                               "AND s.survey != 'eboss'" )
# downloads target data
# dataframe target has necessary info
master_target_SqlQuery <- str_squish(master_target_SqlQuery)
urlBase <- "http://skyserver.sdss.org/dr17/SkyserverWS/SearchTools/SqlSearch?"
X <- getForm(urlBase, cmd = master_target_SqlQuery, format = "csv")
master_targets <- read.table(text = X, header = TRUE, sep = ",", dec = ".", comment.char = "#") %>% 
  mutate(across(where(is.numeric), round, 4),
         objid = as.character(objid),
         specobjid = as.character(specobjid))

get_spectrum <- function(object, wavelength_lower_limit = 5500, wavelength_upper_limit = 7000){
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

index <- 1

radial_url_root <- "http://skyserver.sdss.org/dr17/SkyServerWS/SearchTools/SqlSearch?cmd="
radial_url_core <- glue("SELECT top 1 p.objid, ", 
                        "s.specobjid, s.bestobjid,", 
                        "s.plate, s.mjd, s.fiberid ", 
                        "FROM photoObj AS p ", 
                        "JOIN SpecObj AS s ON s.bestobjid = p.objid ",
                        "WHERE s.specobjid IN (7220539695150946304)" ) %>% 
  str_replace_all(" ", "%20") %>% 
  str_replace_all("\n", "")
w <- rvest::read_html(glue::glue(radial_url_root, radial_url_core, "&format=csv"))
X <- as_list(w)$html$body$p[[1]] %>% 
  as.character() %>% 
  str_remove("#Table1\n")
new_master_targets <- read.table(text = X, header = TRUE, sep = ",", dec = ".", comment.char = "#") %>% 
  mutate(across(where(is.numeric), round, 2),
         objid = as.character(objid),
         bestobjid = as.character(bestobjid),
         specobjid = as.character(specobjid))

spect1 <- get_spectrum(master_targets[index,], 
                       wavelength_lower_limit = 3500, 
                       wavelength_upper_limit = 8000)
spect1 %>% 
  mutate(Wavelength = Wavelength / 10) %>% 
  ggplot(aes(Wavelength, BestFit)) +
  geom_line() +
  scale_x_continuous(name = "Wavelength (nm)")
