library(tidyverse)
library(skimr)
library(gganimate)
library(gifski)
library(lubridate)
library(ggthemes)

raw_data <- read_csv("mrc_table3.csv")

college_and_parent_quintile <- raw_data %>%
  drop_na() %>%
  pivot_longer(
    names_to = c(".value", "quintile"),
    names_pattern = "([a-z0-9_]*)(parq.)",
    cols = c(k_married_cond_parq1:k_married_cond_parq5, k_rank_cond_parq1:k_rank_cond_parq5, ktop1pc_cond_parq1:ktop1pc_cond_parq5) ) %>%
  mutate(quintile = str_sub(quintile, 5, 5)) %>%
  rename("prob_kid_top_one_pct" = ktop1pc_cond_) %>%
  rename("mean_kid_income_ranking" = k_rank_cond_) %>%
  rename("k_married_2014" = k_married_cond_) 

mobility_data <- raw_data %>%
  drop_na() %>%
  pivot_longer(
    names_to = "parent_and_kid_quintile",
    values_to = "prob_based_on_k_par_quintile",
    cols = kq1_cond_parq1:kq5_cond_parq5 ) %>%
  mutate("kid_quintile" = str_sub(parent_and_kid_quintile, 3, 3)) %>%
  mutate("quintile" = str_sub(parent_and_kid_quintile, 14, 14)) 

long_p_quintiles <- raw_data %>%
  drop_na() %>%
  pivot_longer(
    names_to = "quintile",
    values_to = "par_quint_distribution",
    cols = par_q1:par_q5 ) %>%
  mutate(quintile = str_sub(quintile, 6, 6)) 

long_k_quintiles <- raw_data %>%
  drop_na() %>%
  pivot_longer(
    names_to = "kid_quintile",
    values_to = "kid_quint_distribution",
    cols = k_q1:k_q5 ) %>%
  mutate(kid_quintile = str_sub(kid_quintile, 4, 4)) 

long_p_and_q_quints <- inner_join(long_p_quintiles, long_k_quintiles)

combined_data <- inner_join(mobility_data, college_and_parent_quintile) 

long_data <- inner_join(combined_data, long_p_and_q_quints) %>%
  select(-c(kq1_cond_parq1:kq5_cond_parq5)) %>%
  select(-c(k_married_cond_parq1:k_married_cond_parq5)) %>%
  select(-c(k_rank_cond_parq1:k_rank_cond_parq5)) %>%
  select(-c(ktop1pc_cond_parq1:ktop1pc_cond_parq5)) %>%
  select(-parent_and_kid_quintile) %>%
  select(-c(par_q1:par_q5, k_q1:k_q5))

clean_data <- long_data %>%
  mutate(tier = as.factor(tier)) %>%
  mutate(quintile = as.factor(quintile)) %>%
  mutate(kid_quintile = as.factor(kid_quintile)) %>%
  mutate(type = as.factor(type)) %>%
  mutate(kid_quintile = as.factor(kid_quintile)) %>%
  mutate(iclevel = as.factor(iclevel)) %>%
  mutate(state = as.factor(state)) 



exclude_geo <- combined_data %>%
  select(-c(tier_name, region:multi))


