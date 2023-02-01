library(tidyverse)

Rcpp::cppFunction('NumericVector rcpp_clip( NumericVector x, double a, double b){
    return clamp( a, x, b ) ;
}')


calc_logER <- function(pre,post){
  qlogis(post) - qlogis(pre)
}


calc_Z <- function(pre, post){
  z <-  ifelse(pre >= post, (post-pre)/(1-pre), (post-pre)/(pre))
  # if (pre >= post) {
  #   z <- (post-pre)/(1-pre)
  # } else {
  #   z <- (post-pre)/(pre)
  # }
  
  return(z)
}



df_raw <- read_csv("../data/study1.csv")

df <- df_raw %>% 
  filter(check_33==33) %>% 
  rename(
    stadium_pre1 = `1_prob_pre1`,
    climate_pre1 = `2_prob_pre1`,
    fentanyl_pre1 = `3_prob_pre1`,
    learning_pre1 = `4_prob_pre1`,
    stadium_pre2 = `1_prob_pre2`,
    climate_pre2 = `2_prob_pre2`,
    fentanyl_pre2 = `3_prob_pre2`,
    learning_pre2 = `4_prob_pre2`,
    stadium_post2 = `1_prob_post2`,
    climate_post2 = `2_prob_post2`,
    fentanyl_post2 = `3_prob_post2`,
    learning_post2 = `4_prob_post2`,
    stadium_persuasive = `1_persuasive`,
    climate_persuasive = `2_persuasive`,
    fentanyl_persuasive = `3_persuasive`,
    learning_persuasive = `4_persuasive`
  ) %>% 
  pivot_longer(
    matches("(_pre|_post|_pers)"),
    names_to = c("topic", ".value"),
    names_sep = "_",
    values_to = "value",
    values_transform = list(value = as.numeric)
  ) %>% 
  mutate(
    pre = (pre1 + pre2)/2,
    post = (post1 + post2)/2,
  ) %>% 
  select(subj_id, pre, post, topic, persuasive, contains("trust"),Political_Party) %>% 
  gather(phase, prob, pre, post) %>% 
  mutate(
    prob = rcpp_clip(prob/100, .005, .995),
    phase = ordered(phase, levels=c("pre","post"))
  ) %>% 
  rename(
    climate_sci_trust = `1_trust`,
    sci_trust = `2_trust`,
    psych_trust = `3_trust`,
    journalists_trust = `4_trust`,
    economists_trust = `5_trust`,
    medicalexperts_trust = `6_trust`,
    people_trust = `7_trust`
  ) %>% 
  mutate(
    persuasive = case_when(
      persuasive == "Extremely persuasive" ~ 5,
      persuasive == "Very persuasive" ~ 4,
      persuasive == "Moderately persuasive" ~ 3,
      persuasive == "Slightly persuasive" ~ 2,
      persuasive == "Not at all persuasive" ~ 1,
    )
  ) %>% 
  pivot_longer(
    contains("trust"),
    names_to = "speaker_type",
    values_to = "trust"
  ) %>% 
  mutate(
    trust = case_when(
      trust == "Completely trustworthy" ~ 6,
      trust == "Largely trustworthy" ~ 5,
      trust == "Somewhat trustworthy" ~ 4,
      trust == "Somewhat untrustworthy" ~ 3,
      trust == "Largely untrustworthy" ~ 2,
      trust == "Completely untrustworthy" ~ 1
    )
  ) %>% 
  pivot_wider(
    names_from = speaker_type,
    values_from = trust
  )

df_demo <-  df_raw %>% 
  filter(check_33==33) %>% 
  select(subj_id, Age, Gender, Race, Education_level, Political_Party, Political_Views)
  


df_conf <- df %>% 
  mutate(
    persuasive_fct = as.factor(persuasive),
    persuasive_ord = as.ordered(persuasive)
  ) %>% 
  spread(phase, prob) %>% 
  # filter(Political_Party %in% c("Democratic Party","Republican Party","Independent")) %>% 
  mutate(
    logER = calc_logER(pre, post),
    diff = (post - pre),
    ratio = log(post/pre),
    Z = map2_dbl(pre, post, calc_Z)
  )


winsor_mean <- function(x, trim=.1){
  ll <- trim/2
  ul <- 1-trim/2
  rank <- percent_rank(x)
  return(mean(x[!(rank < ll) & !(rank >ul)]))
}

winsor_sd <- function(x, trim=.1){
  ll <- trim/2
  ul <- 1-trim/2
  rank <- percent_rank(x)
  return(sd(x[!(rank < ll) & !(rank >ul)]))
}
