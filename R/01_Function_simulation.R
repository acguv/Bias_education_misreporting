
# ---------------------------------------------------------------------------- #
# Project:  Estimating bias in educational inequalities in mortality
# Author: Ana C. Gomez-Ugarte
# Title: Functions
# ---------------------------------------------------------------------------- #

### Simulation function

# Nx: g*n x g*n diagonal matrix with the population exposures by education
# gamma: g*n x 1 matrix with real education-specific mortality rates
# coverage: g*n x g*n diagonal matrix with age-education-specific coverage rates
# age_mis: g*n x g*n matrix with information on age misreporting
# edu_mis: g*n x g*n matrix with education age-specific education misreporting
# ages: vector of ages included in the analysis. All education groups have the same number of ages
# g: number of education groups

scenario_func <- function(Nx, gamma, coverage, age_mis, edu_mis, ages, g, edu_cat){
  n <- length(ages)
  
  # Distorted mortality rates
  d_hat <- edu_mis %*% age_mis %*% coverage %*% Nx %*% gamma
  
  result <- data.frame(education = edu_cat, age = rep(ages, g), 
                       mu = d_hat/diag(Nx))
  
  return(result)
}

### Simulation function with misreporting in the census
simulation_w_exposures <- function(Nx, gamma, coverage, age_mis, edu_mis, 
                                   ages, g, coverage_exp, age_mis_exp, edu_mis_exp, edu_cat){
  n <- length(ages)
  
  # Distorted mortality rates
  d_hat <- edu_mis %*% age_mis %*% coverage %*% Nx %*% gamma
  
  N_hat <- edu_mis_exp %*% age_mis_exp %*% coverage_exp %*% diag(Nx) 
  
  result <- data.frame(education = edu_cat, age = rep(ages, g), 
                       mu = d_hat/N_hat)
  
  return(result)
}

### Life table function
lifetable.mx <- function(x, mx, sex="M", ax=NULL){
  m <- length(x)
  n <- c(diff(x), NA)
  if(is.null(ax)){
    ax <- rep(0,m)
    if(x[1]!=0 | x[2]!=1){
      ax <- n/2
      ax[m] <- 1 / mx[m]
    }else{    
      if(sex=="F"){
        if(mx[1]>=0.107){
          ax[1] <- 0.350
        }else{
          ax[1] <- 0.053 + 2.800*mx[1]
        }
      }
      if(sex=="M"){
        if(mx[1]>=0.107){
          ax[1] <- 0.330
        }else{
          ax[1] <- 0.045 + 2.684*mx[1]
        }
      }
      ax[-1] <- n[-1]/2
      ax[m] <- 1 / mx[m]
    }
  }
  qx  <- n*mx / (1 + (n - ax) * mx)
  qx[m] <- 1
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*1
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]/mx[m]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  return.df <- data.frame(x, n, mx, ax, qx, px, lx, dx, Lx, Tx, ex)
  return(return.df)
}

# ------Standard deviation of ages at deatch------#
sdv.func <- function(lt){
  nages <- dim(lt)[1]
  ex.age <- lt$x + lt$ex
  ax.age <- lt$x + lt$ax
  
  V <- rev(cumsum(rev(lt$dx*(ax.age-ex.age)^2)))/lt$lx
  S <- sqrt(V)
  
  return(S[1])
}

# ------E-dagger------#
e_dagger.func <- function(lt){
  m <- length(lt$ex)
  edag <- (sum(lt$dx[-m]* (lt$ex[-m] + lt$ax[-m]*(lt$ex[-1]-lt$ex[-m]) )) + lt$ex[m])
  return(edag)
}

# ------Entropy------#
entropy.func <- function(lt){
  m <- length(lt$ex)
  edag <- (sum(lt$dx[-m]* (lt$ex[-m] + lt$ax[-m]*(lt$ex[-1]-lt$ex[-m]) )) + lt$ex[m])
  H <- edag/lt$ex[1]
  return(H)
}

# ------Non-overlap between two distributions------#
noi_func <- function(a,b){
  FX <- cbind(a,b)
  fmin <- apply(FX,1,min)
  fmax <- apply(FX,1,max)
  
  out <- 1-(sum(fmin)/sum(fmax))
  return(out)
}

noi_pair_func_dx <- function(dx_sim){
  
  w <- matrix(dx_sim$edu_weights, ncol = g)
  w_dpi <- colSums(w)/sum(w)
  dx <- matrix(dx_sim$dx, ncol = g)
  dx.d <- dx/colSums(dx)
  comb_dx <- combn(ncol(dx),2)
  
  Sij <- rep(NA,ncol(comb_dx))
  weights <- rep(NA,ncol(comb_dx))
  i <- 1
  for (i in 1:ncol(comb_dx)){
    Sij[i] <- noi_func(dx[,comb_dx[1,i]],dx[,comb_dx[2,i]])
    weights[i] <- w_dpi[comb_dx[1,i]]*w_dpi[comb_dx[2,i]]
  }
  
  noi_pair <- sum(Sij*weights)/sum(weights)
  
  return(list(noi_pair = noi_pair))
}

# ------AID: Inter-group difference------# 
aid_func <- function(asmr_sim){
  asmr <- asmr_sim[asmr_sim$education != "Total",]
  edu = unique(asmr$education)
  total_sum = 0
  
  for (i in edu) {
    for (j in edu){
      temp = (abs(asmr_sim$asmr[asmr_sim$education == i]-asmr_sim$asmr[asmr_sim$education == j])*
        asmr_sim$edu_weights[asmr_sim$education == i]*asmr_sim$edu_weights[asmr_sim$education == j])*1/2
      
      total_sum = total_sum + temp
    }
  }
  total_sum
  pg = total_sum/asmr_sim$asmr[asmr_sim$education == "Total"]
  
  return(list(aid = total_sum, pseudo_gini = pg))
}

# ------SII: Slope index of inequality------# 
sii_func <- function(ind_sim, indicator){
  ind <- ind_sim[ind_sim$education != "Total",]
  reg <- lm(ind[[indicator]] ~ ind$Edu_ranks)$coefficient
  rii = (reg[1] + reg[2])/(reg[1])
  sii = reg[2]
  return(list(sii = unname(sii), rii = unname(rii)))
}

# ------PAF: Population attributable fraction------# 
paf_func <- function(asmr_sim){
  par = asmr_sim$asmr[asmr_sim$education == "Total"]-asmr_sim$asmr[asmr_sim$education == "High"]
  paf = par/asmr_sim$asmr[asmr_sim$education == "Total"]
  return(list(par = par, paf = paf))
}

# ------PALL: Population attributable life lost------# 
pall_func <- function(ex_sim){
  pall_abs = ex_sim$ex[ex_sim$education == "High"]-ex_sim$ex[ex_sim$education == "Total"]
  
  pall_rel = sum(((ex_sim$ex[ex_sim$education == "High"]-ex_sim$ex[ex_sim$education != "Total"])*
    ex_sim$edu_weights[ex_sim$education != "Total"])/ex_sim$ex[ex_sim$education == "Total"])
  
  cii_abs = sum((ex_sim$ex[ex_sim$education == "High"]-ex_sim$ex[ex_sim$education != "Total"])*
                    ex_sim$edu_weights[ex_sim$education != "Total"])
  
  idll_abs = sum(abs(ex_sim$ex[ex_sim$education == "Total"]-ex_sim$ex[ex_sim$education != "Total"])*
                    ex_sim$edu_weights[ex_sim$education != "Total"])
  
  idll_rel = idll_abs/ex_sim$ex[ex_sim$education == "Total"]
  
  return(list(pall_abs = pall_abs, pall_rel = pall_rel, cii_abs = cii_abs, 
              cii_rel = pall_rel, idll_rel = idll_rel, idll_abs = idll_abs))
}

# ------Theil Index------# 
theil_func <- function(ex_sim){
  theil = sum(ex_sim$edu_weights[ex_sim$education != "Total"]*
                ex_sim$ex[ex_sim$education != "Total"]/
                ex_sim$ex[ex_sim$education == "Total"]*
                log(ex_sim$ex[ex_sim$education != "Total"]/
                      ex_sim$ex[ex_sim$education == "Total"]))
  
  return(list(theil = theil))
}

# ------Range ratio------#
rr_func <- function(ind_sim, indicator){
  ind <- ind_sim[ind_sim$education != "Total",]
  if(indicator %in% c("ex", "sdv")){
    range <- ind_sim[[indicator]][ind_sim$education == "High"]-ind_sim[[indicator]][ind_sim$education == "Low"]
    ratio <- ind_sim[[indicator]][ind_sim$education == "High"]/ind_sim[[indicator]][ind_sim$education == "Low"]
  } else{
    range <- ind_sim[[indicator]][ind_sim$education == "Low"]-ind_sim[[indicator]][ind_sim$education == "High"]
    ratio <- ind_sim[[indicator]][ind_sim$education == "Low"]/ind_sim[[indicator]][ind_sim$education == "High"]
  }
  return(list(range = range, ratio = ratio))
}

#### Estimating inequality measures
ineq_measures <- function(sim_result, g, edu_ranks, edu_weights, mu_tot) {
  n = dim(sim_result)[1]/g 
  ages = unique(sim_result$age)
  
  lt_sim <- sim_result %>%
    group_by(education) %>%
    mutate(LT = lifetable.mx(x = ages, mx = mu, ax = NULL)) 
  
  lt_tot <- mu_tot %>%
    mutate(LT = lifetable.mx(x = age, mx = mu, ax = NULL)) 
  
  asmr_sim <- lt_sim %>%
    left_join(who_std, by = "age") %>%
    mutate(mx_w = mu*Prop) %>%
    group_by(education) %>%
    summarise(asmr = sum(mx_w)*100000) %>%
    arrange(education) %>%
    left_join(edu_ranks, by = c("education")) %>%
    left_join(edu_weights, by = c("education")) %>%
    rbind(lt_tot %>%
            left_join(who_std, by = "age") %>%
            mutate(mx_w = mu*Prop) %>%
            summarise(asmr = sum(mx_w)*100000) %>%
            mutate(education = "Total", Edu_ranks = NA, edu_weights = NA))
      
  ex_sim <- lt_sim %>%
    # dplyr::select(-i, -j, -k, -l) %>%
    rbind(lt_tot %>% mutate(education = "Total")) %>%
    group_by(education) %>%
    mutate(e_dagger = e_dagger.func(LT),
           entropy = entropy.func(LT),
           ex = LT$ex,
           dx = LT$dx,
           sdv = sdv.func(LT)) %>%
    filter(age == 30) %>%
    dplyr::select(education, age, ex, e_dagger, entropy, sdv) %>%
    left_join(edu_ranks, by = c("education")) %>%
    left_join(edu_weights, by = c("education"))
  
  dx_sim <- lt_sim %>%
    left_join(edu_weights, by = c("education")) %>%
    mutate(dx = unlist(LT$dx)) %>%
    dplyr::select(education, age, dx, edu_weights) 
    
  range_ex = rr_func(ex_sim, "ex")$range
  
  ratio_ex = rr_func(ex_sim, "ex")$ratio
  
  range_sdv = rr_func(ex_sim, "sdv")$range
  
  ratio_sdv = rr_func(ex_sim, "sdv")$ratio
  
  range_asmr = rr_func(asmr_sim, "asmr")$range
  
  ratio_asmr = rr_func(asmr_sim, "asmr")$ratio

  sii_ex = ifelse(g <= 2, NA, sii_func(ex_sim, "ex")$sii)
  
  rii_ex = ifelse(g <= 2, NA, sii_func(ex_sim, "ex")$rii)
  
  sii_asmr = ifelse(g <= 2, NA, sii_func(asmr_sim, "asmr")$sii)
  
  rii_asmr = ifelse(g <= 2, NA, sii_func(asmr_sim, "asmr")$rii)
  
  paf = paf_func(asmr_sim)$paf
  
  par = paf_func(asmr_sim)$par
    
  pall_abs = pall_func(ex_sim)$pall_abs
  
  pall_rel = pall_func(ex_sim)$pall_rel
  
  cii_abs = pall_func(ex_sim)$cii_abs
  
  idll_rel = pall_func(ex_sim)$idll_rel
  
  idll_abs = pall_func(ex_sim)$idll_abs
  
  theil = theil_func(ex_sim)$theil
  
  aid = aid_func(asmr_sim)$aid
  
  pseudo_gini = aid_func(asmr_sim)$pseudo_gini
  
  noi_pair =  noi_pair_func_dx(dx_sim)$noi_pair
    
  all_ineq <- cbind(range_asmr, ratio_asmr, range_ex, ratio_ex, range_sdv, ratio_sdv, 
                    sii_ex, rii_ex, sii_asmr, rii_asmr, paf, par, pall_abs, pall_rel,
                    cii_abs, idll_abs, idll_rel, theil, aid, pseudo_gini, noi_pair)
  
  ex <- ex_sim %>%
    dplyr::select(-Edu_ranks, -edu_weights, -age) %>%
    left_join(asmr_sim %>%
                dplyr::select(-Edu_ranks, -edu_weights), by = "education")
  
  return(list(all_ineq = all_ineq, ex = ex))
}    

### Estimate inequality measures for the 2 group scenario
run_ineq_measures <- function(ex, seq_i, seq_j, edu_ranks, edu_weights){
  ineq_ex <- setNames(data.frame(matrix(ncol = 23, nrow = 0)), 
                      c("i", "j", "range_asmr", "ratio_asmr", "range_ex", "ratio_ex",
                      "range_sdv", "ratio_sdv",  "sii_ex", "rii_ex", "sii_asmr", "rii_asmr",
                      "paf", "par", "pall_abs", "pall_rel", "cii_abs", "idll_abs", "idll_rel",
                      "theil", "aid", "pseudo_gini", "noi_pair"))
  
  bias_ex <- setNames(data.frame(matrix(ncol = 34, nrow = 0)), 
                      c("i", "j", "range_asmr", "ratio_asmr", "range_ex", "ratio_ex",
                        "range_sdv", "ratio_sdv",  "sii_ex", "rii_ex", "sii_asmr", "rii_asmr",
                        "paf", "par", "pall_abs", "pall_rel", "cii_abs", "idll_abs", "idll_rel",
                        "theil", "aid", "pseudo_gini", "noi_pair", "ratio_asmr_log", 
                        "ratio_ex_log", "ratio_sdv_log", "rii_log", "rii_asmr_log",
                        "paf_logit", "theil_logit", "psuedo_gini_logit", "noi_pair_logit"))
  
  for (i in seq_i) {
    for (j in seq_j) {
      temp <- data.frame(ineq_measures(ex[ex$i == i & ex$j == j,],
                                       g, edu_ranks, edu_weights, mu_tot)$all_ineq)
      temp$i <- i
      temp$j <- j
      ineq_ex <- rbind(ineq_ex, temp) 
    }
  }

    ineq <- ineq_ex %>%
      mutate(ratio_asmr_log = log(ratio_asmr),
             ratio_ex_log = log(ratio_ex),
             ratio_sdv_log = log(ratio_sdv),
             rii_ex_log = log(rii_ex),
             rii_asmr_log = log(rii_asmr),
             paf_logit = log(paf/(1-paf)),
             theil_logit = log(theil/(1-theil)),
             psuedo_gini_logit = log(pseudo_gini/(1-pseudo_gini)),
             noi_pair_logit = log(noi_pair/(1-noi_pair)))
    
    true_ineq <- ineq[ineq$i == 0 & ineq$j == 0,]
    
    ineq_std <- ineq %>%
      # mutate(across(!c(i, j), ~ as.numeric(scale(.))))
      mutate(across(!c(i, j), ~ (. - true_ineq[[cur_column()]])/sd(.)))
    
    true_ineq_std <- ineq_std[ineq_std$i == 0 & ineq_std$j == 0,]
    
    bias_ex <- ineq %>%
      mutate(across(c(-i, -j), ~ (. - true_ineq[[cur_column()]])))
    
    bias_ex_std <- ineq_std %>%
      mutate(across(c(-i, -j), ~ (. - true_ineq_std[[cur_column()]])))
    
    rel_bias_ex <- ineq %>%
      mutate(across(c(-i, -j), ~ (. - true_ineq[[cur_column()]])/true_ineq[[cur_column()]]))
    
    rel_bias_ex_std <- ineq_std %>%
      mutate(across(c(-i, -j), ~ (. - true_ineq_std[[cur_column()]])/true_ineq_std[[cur_column()]]))
  
  return(list(ineq = ineq, bias_ex = bias_ex, rel_bias_ex = rel_bias_ex, 
              rel_bias_ex_std = rel_bias_ex_std, ineq_std = ineq_std, 
              bias_ex_std = bias_ex_std))
}

### Estimate inequality measures for the 3 group scenario
run_ineq_measures_3 <- function(ex, seq_i, seq_j, seq_k, seq_l, edu_ranks, edu_weights){
  ineq_ex <- setNames(data.frame(matrix(ncol = 25, nrow = 0)), 
                      c("i", "j", "k", "l", "range_asmr", "ratio_asmr", "range_ex", "ratio_ex",
                        "range_sdv", "ratio_sdv",  "sii_ex", "rii_ex", "sii_asmr", "rii_asmr",
                        "paf", "par", "pall_abs", "pall_rel", "cii_abs", "idll_abs", "idll_rel",
                        "theil", "aid", "pseudo_gini", "noi_pair"))
  
  bias_ex <- setNames(data.frame(matrix(ncol = 34, nrow = 0)), 
                      c("i", "j", "k", "l", "range_asmr", "ratio_asmr", "range_ex", "ratio_ex",
                        "range_sdv", "ratio_sdv",  "sii_ex", "rii_ex", "sii_asmr", "rii_asmr",
                        "paf", "par", "pall_abs", "pall_rel", "cii_abs", "idll_abs", "idll_rel",
                        "theil", "aid", "pseudo_gini", "noi_pair", "ratio_asmr_log", 
                        "ratio_ex_log", "ratio_sdv_log", "rii_log", "rii_asmr_log",
                        "paf_logit", "theil_logit", "psuedo_gini_logit", "noi_pair_logit"))
  
  param_grid <- expand.grid(i = seq_i, j = seq_j, k = seq_k, l = seq_l)
  
  results <- param_grid %>%
    pmap(function(i, j, k, l) {
      res <- ineq_measures(ex[ex$i == i & ex$j == j & ex$k == k & ex$l == l, ], 
                           g, edu_ranks, edu_weights, mu_tot)
      
      # Extract both elements, add grouping columns
      list(ineq_res = data.frame(res$all_ineq) %>% mutate(i = i, j = j, k = k, l = l),
           ex = res$ex %>% mutate(i = i, j = j, k = k, l = l))
    }
    )
  
  ineq_res <- map_dfr(results, "ineq_res")
  ex <- map_dfr(results, "ex")
    
  ineq <- ineq_res %>%
      mutate(ratio_asmr_log = log(ratio_asmr),
             ratio_ex_log = log(ratio_ex),
             ratio_sdv_log = log(ratio_sdv),
             rii_ex_log = log(rii_ex),
             rii_asmr_log = log(rii_asmr),
             paf_logit = log(paf/(1-paf)),
             theil_logit = log(theil/(1-theil)),
             psuedo_gini_logit = log(pseudo_gini/(1-pseudo_gini)),
             noi_pair_logit = log(noi_pair/(1-noi_pair)))
    
    ineq_std <- ineq %>%
      mutate(across(!c(i, j, k, l), ~ as.numeric(scale(.))))
    
    true_ineq <- ineq[ineq$i == 0 & ineq$j == 0 & ineq$k == 0 & ineq$l == 0,]
    
    true_ineq_std <- ineq_std[ineq_std$i == 0 & ineq_std$j == 0 & ineq$k == 0 & ineq$l == 0,]
    
    bias_ex <- ineq %>%
      mutate(across(!c(i, j, k, l), ~ (. - true_ineq[[cur_column()]])))
    
    bias_ex_std <- ineq_std %>%
      mutate(across(!c(i, j, k, l), ~ (. - true_ineq_std[[cur_column()]])))
    
    rel_bias_ex <- ineq %>%
      mutate(across(!c(i, j, k, l), ~ (. - true_ineq[[cur_column()]])/true_ineq[[cur_column()]]))
    
    rel_bias_ex_std <- ineq_std %>%
      mutate(across(!c(i, j, k, l), ~ (. - true_ineq_std[[cur_column()]])/true_ineq_std[[cur_column()]]))
        
  return(list(ineq = ineq, bias_ex = bias_ex, rel_bias_ex = rel_bias_ex, 
              rel_bias_ex_std = rel_bias_ex_std, ineq_std = ineq_std, 
              bias_ex_std = bias_ex_std, ex = ex))
}
