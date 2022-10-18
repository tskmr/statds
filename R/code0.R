setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); rm(list = ls())
library(tidyverse); library(statds); library(scales); library(rstan); rstan_options(auto_write=TRUE); options(mc.cores=parallel::detectCores()); rstan_options(javascript=FALSE); library(lubridate); library(ggridges);
# install.packages("systemfonts", "miniUI", "DT")# remotes::install_github("Gedevan-Aleksizde/fontregisterer", repos = NULL, type = "source")
capabilities()[c("cairo", "X11")]; require(fontregisterer); sans = fontregisterer::get_standard_ja_fonts()[1]; theme_set(theme(text = element_text(family = sans))); update_geom_defaults("text", list(family = sans))
# theme_set(mytheme_bw(36)+theme(axis.title.x = element_text(family = "serif"),axis.title.y = element_text(family = "serif")))
