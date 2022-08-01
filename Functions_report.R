
import_packages_report <- function() {
  library(rio)
  library(janitor)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
  library(ggridges)
  library(openxlsx)
  library(purrr)
  library(tidyselect)
  library(comprehenr)
  library(stringr)
  library(readxl)
  library("gridExtra")
  library(kknn)
  library(htmlwidgets)
  library(ggpubr)
  library(ggplot2)
  library(htmltools)
  library(grid)
  library(gtable)
  library(ggrepel)
  library(weights)
  library(ggcorrplot)
  library(tidymodels)
  library(aplot)
  library(scales)
  library(ggbreak)
  library(patchwork)
  library(shiny)
  library(ranger)
  library(plotly)
  options(scipen=5)
  theme_set(theme_bw())
}

not_all_na <- function(x) any(!is.na(x))

remove_non_duplicated_columns <- function(cases) {
  must_have_cols <- c("case_id", "reg_date", "default_dt", "balance", "source")
  cases_test <- cases %>% mutate_all(as.character)
  
  for (must_have_col in must_have_cols) {
    for (rest_col in (cases_test %>% select(-all_of(must_have_cols)) %>% colnames())) {
      if (all(cases_test[[rest_col]] == cases_test[[must_have_col]])) { 
        print(paste("removed", rest_col)) 
        cases <- cases %>%  select(-rest_col) }    } } 
  return (cases)}

is.date <- function(x) { 
  any(inherits(x, 'Date'), inherits(x, 'POSIXt'), inherits(x, 'POSIXct')) }

npl_summary <- function(NPL_obj, group_vector =c()) {
  NPL_obj$cases %>% group_by_at(c('source', group_vector)) %>% 
    summarise(n = n(), capital = as_eur(sum(original_capital)), 
              avg_cap = as_eur(mean(original_capital)), 
              avg_age_y = mean(as.numeric(cutoff_dt - default_dt)/365), gini = ineq::Gini(original_capital), 
              across(matches("^l[0-9]+m_pmts$"), ~sum(.x > 0)/n(),  .names = "{str_sub(.col,1,3)}_payers")) %>% 
    left_join( NPL_obj$cases %>% group_by_at(c('source',group_vector)) %>% top_frac(0.2, wt=original_capital) %>% summarise(sum_20_perc=sum(original_capital))) %>% 
    mutate(cap20perc = sum_20_perc/capital) %>% select(-source, -sum_20_perc)
}

cases_distribution <- function(NPL_obj, col_variable, output_variable, group_vector =c()) {
  group_vector <- c(group_vector, "portfolio")
  df1 <-   NPL_obj$cases %>% 
    mutate(year=case_when(year(!!sym(col_variable))<1990 ~ "-----Older than 1990",
                          year(!!sym(col_variable))<2000 ~ "----1990-2000",
                          year(!!sym(col_variable))<2006 ~ "---2000-2005",
                          year(!!sym(col_variable))<2011 ~ "--2006-2010",
                          year(!!sym(col_variable))<2013 ~ "-2010-2012",
                          TRUE ~ year(!!sym(col_variable)) %>%  as.character())) 
  df1 %>% 
    group_by(year, !!!syms(group_vector)) %>% 
    summarise(n_cases=n(), sum_FV = sum(balance), 
              perc=percent(sum_FV/(df1$balance) %>%  sum(), accuracy = 0.3)) %>% 
    rowwise() %>% 
    mutate(group = paste(!!!syms(group_vector))) %>%  select(year, !!sym(output_variable), group) %>% 
    pivot_wider(names_from=year, values_from=!!sym(output_variable))
}

fv_distribution_chart_per_year <- function(NPL_obj, col_variable) {
  p1<- 
    NPL_obj$cases %>% mutate(year=as.factor(year(!!sym(col_variable)))) %>% 
    ggplot(aes(year, balance)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    stat_summary(fun=mean, geom="point", shape=20, size=1, color="red", fill="red") +
    ggtitle("Balance vs year")
  
  p2 <-
    NPL_obj$cases %>% mutate(year=as.factor(year(!!sym(col_variable)))) %>% 
    ggplot(aes(year, balance)) +
    geom_boxplot() +
    scale_y_log10() +
    theme_bw() +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    stat_summary(fun=mean, geom="point", shape=20, size=1, color="red", fill="red") +
    ggtitle("Balance vs year (log)")
  
  return (plot_grid(p1, p2, labels=first(NPL_obj$cases$source)))
  }

debt_age_distribution_chart <- function(NPL_obj, groups_vector=c()) {
  cases <- list(NPL_obj) %>% map("cases") %>% bind_rows() 
  pmts <- list(NPL_obj) %>% map("pmts") %>% bind_rows() %>% filter(case_id %in% cases$case_id)
  
  if (!is.null(groups_vector)) { data <-  cases %>% mutate(group = paste(!!!syms(groups_vector)))  }
  else { data <- cases %>% mutate(group='All') }
  
  data %>% 
    ggplot(aes(debt_age_m, fill=source)) + geom_bar(alpha=0.5) +
    facet_wrap(~group, scale='free_y', ncol=1) +
    scale_y_log10()
}

fv_distribution_chart <- function(NPL_obj, groups_vector=c(), scale='log') {
  cases <- list(NPL_obj) %>% map("cases") %>% bind_rows() 
  
  if (!is.null(groups_vector)) { data <-  cases %>% mutate(group = paste(!!!syms(groups_vector)))  }
  else { data <- cases %>% mutate(group='All') }
  
  p1 <-   data %>% 
    ggplot(aes(x=balance, fill=source, height(stat(density)))) + 
    geom_histogram(aes(y = ..count..), alpha=0.7, position = "dodge", bins =200)  + 
    ggtitle("Balance distribution") +
    coord_cartesian(xlim = c(1, pmin(cases$balance %>% max(), 700000))) +
    labs(x = "Balance", y = 'No. of claims') +
    theme(legend.position = 'none')+
    facet_wrap(~ group, ncol=1)
  
    if (scale=='log') { p1 <- p1  +  scale_x_continuous(trans='log10', breaks = c(0,10,100,500,1000, 5e3, 1e4, 1e4, 1e5,1e6)) }
  
  (ggplotly(p1) %>% layout(height = length(unique(data$group))*220))
}

return_liq_rate_from_default_grouped <- function(NPL_obj, groups_vector = c()){
  cases <- list(NPL_obj) %>% map("cases") %>% bind_rows() 
  pmts <- list(NPL_obj) %>% map("pmts") %>% bind_rows() %>% filter(case_id %in% cases$case_id)
  liq_rates_df <- 
    pmts %>%  inner_join(cases, by='case_id') %>%
    mutate(age =  interval(default_dt, month)%/%months(1) ) %>% 
    group_by_at(c(groups_vector, "source", "age")) %>% summarise(collections_sum = sum(amount)) %>% 
    left_join(cases %>%  group_by_at(c(groups_vector, "source")) %>% summarise(balance_sum = sum(balance)), by=c("source", groups_vector)) %>% 
    mutate(liq_rate = collections_sum/balance_sum) %>% 
    group_by_at(c(groups_vector, "source")) %>% mutate(liq_ratec=cumsum(liq_rate)) 
  
  if (is.null(groups_vector)) {
    liq_rates_df <- liq_rates_df %>% mutate(group_col='All')
  } else {
    liq_rates_df <- liq_rates_df %>%  rowwise() %>% mutate(group_col=paste(!!!syms(c(groups_vector))))
  } 
  liq_rates_df
}

negative_pmts_table_total <- function(NPL_obj, groups_vector=c()) {
  NPL_obj$pmts %>% inner_join(NPL_obj$cases, by='case_id') %>% 
    mutate(flag=if_else(amount<0, 'negative', 'positive')) %>% 
    group_by_at(c(groups_vector, "source", 'flag')) %>% 
    summarise(n_pmts =n(), sum=sum(amount)) %>%  arrange(flag)
}

negative_pmts_table_per_case <- function(NPL_obj) {
  NPL_obj$pmts %>% inner_join(NPL_obj$cases, by='case_id') %>% 
    group_by(case_id, source) %>% summarise(sum=sum(amount)) %>%
    mutate(flag=if_else(sum<0, 'negative', 'positive')) %>% 
    tabyl(flag, source)
}
