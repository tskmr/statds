#
setwd_here = function(is.rm = TRUE){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  if(is.rm) rm(list = ls())
}
