kaplan.meier = function(t, cens=NULL, dp=0){
  n = length(t)
  if(!is.null(cens)) {
    idx = which(cens==1)
  }else{
    idx = 1:n
  }
  t.obs = t[idx]
  nj = rev(seq_along(t))[idx]
  data.frame(t.obs, nj) %>% 
    mutate(p = (1-dp)/nj) %>% 
    mutate(q = 1-p) %>% 
    mutate(SF = cumprod(q)) %>% 
    mutate(CDF = 1-SF)
}
