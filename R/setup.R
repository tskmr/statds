setup = function(is.setwd_here = FALSE, is.rm = TRUE){
  if(is.setwd_here) setwd_here(is.rm)
  if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
  if(!require(magrittr)) instatll.packages("magrittr"); library(magrittr)
  if(!require(ggridges)) install.packages("ggridges"); library(ggridges)
  if(!require(directlabels)) install.packages("directlabels"); library(directlabels)
  #
  if(!require(scales)) install.packages("scales"); library(scales);
  if(!require(lubridate)) install.packages("lubridate"); library(lubridate)
  # install.packages("systemfonts", "miniUI", "DT")
  if(!require(systemfonts)) install.packages(systemfonts); library(systemfonts)
  if(!require(miniUI)) install.packages("miniUI"); library(miniUI)
  if(!require(DT)) install.packages("DT"); library(DT)
  capabilities()[c("cairo", "X11")]
  if(!require(fontregisterer)) remotes::install_github("Gedevan-Aleksizde/fontregisterer", repos = NULL, type = "source"); library(fontregisterer)
  sans = fontregisterer::get_standard_ja_fonts()[1]
  theme_set(theme(text = element_text(family = sans),
                  title = element_text(family = sans),
                  legend.title = element_text(family = sans),
                  legend.text = element_text(family = sans),
                  axis.title = element_text(family = sans),
                  axis.text = element_text(family = sans)))
  update_geom_defaults("text", list(family = sans))
  # theme_set(mytheme_bw(36)+theme(axis.title.x = element_text(family = "serif"),axis.title.y = element_text(family = "serif")))
  if(!require(rstan)) install.packages("rstan"); library(rstan)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  rstan_options(javascript = FALSE)
}
