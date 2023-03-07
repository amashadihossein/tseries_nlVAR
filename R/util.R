yspline_get <- function(){
  knots <- c(0.25, 0.5, 0.75)
  theta <-cbind(
    grp0 = c(0.1, 0.15, 0.3, 0.2, 0.1, 0.1, 0.1),
    grp1 = c(0.1, 0.8, 0.4, 0.2, 0.1, 0.1, 0.1))
  t_obs <- 1:30
  t_obs.normalize <- (t_obs - min(t_obs))/(max(t_obs) - min(t_obs))
  
  basis <- splines::bs(x = t_obs.normalize, knots = knots, degree = 3, 
                       Boundary.knots = c(0, 1), intercept = TRUE)
  
  
  y_spline <- basis %*% theta
  colnames(y_spline) = c("0", "1")
  rownames(y_spline) = as.character(t_obs)
  return(y_spline)
}

aveProfile<- function(n, time, allergic, unitnoise, pk){
  # browser()
  time = as.character(time)
  allergic = as.character(allergic)
  ap <- sapply(1:n, function(ii) y_spline[time[ii], allergic[ii]] + unitnoise[ii])
  
  ap/exp(.2*pk)
}

aveProfile2<- function(n, time, allergic, unitnoise, x1bar, responder){
  # browser()
  
  time = as.character(time)
  allergic = as.character(allergic)
  ap <- sapply(1:n, function(ii) y_spline[time[ii], allergic[ii]] + unitnoise[ii])
  
  ap2 <- responder*x1bar + (1-responder)* ap
}


getPk <- function(n, pkm1, rx){
  # browser()
  rx + pkm1/exp(0.05)
}


format_long <- function(sim_xbar){
  dplot <- sim_xbar %>% 
    tidyr::pivot_longer(cols = trt_1:rxn_30, values_to = "value") %>% 
    tidyr::separate(col = name,into = c("name","t")) %>% 
    tidyr::pivot_wider(names_from = name ,values_from = value) %>% 
    mutate(t = as.numeric(t)) %>%
    mutate(x1 = x1bar + rnorm(n=nrow(.), sd = .01)) %>% 
    mutate(x2 = x2bar +rnorm(n=nrow(.), sd = .01)) %>%
    mutate(allergic = if_else(allergic == 0, "N","Y")) %>%
    mutate(responder = if_else(responder == 0, "N", "Y")) %>%
    mutate(trt = tidyr::replace_na(trt,0))
  
  dplot <- dplot %>% group_by(ID) %>% 
    summarise(treated_early = if_else(sum( (t<5) * trt)>0,"Y","N")) %>%
    ungroup() %>%
    left_join(dplot)
  
  return(dplot)
}
