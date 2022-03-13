library(tidyverse)
library(kamaken)
library(gt)



# データ作成 ---------------------------------------------------------------------------------------

dat <- read_csv('dat.csv')

dat %>% as.table()

dat_ind <- 
  dat$n %>% 
  array(dim = c(2, 3, 2, 3), 
        dimnames = list(D = dat$D %>% unique(), 
                        C = dat$C %>% unique(), 
                        B = dat$B %>% unique(),
                        A = dat$A %>% unique())
  ) %>% 
  epitools::expand.table() %>% 
  as_tibble() %>% 
  mutate(across(.fns = as.numeric))



# 結果のまとめ --------------------------------------------------------------------------------------

extract_gof <- function(tab = 'tab3.2') {
  filepath <- list.files(path = str_c('再現/', tab, '/'), 
                         pattern = '.out', 
                         full.names = T)
  Model <- str_extract(filepath, '(?<=_).+(?=_)')
  
  tibble(
    Model = Model,
    filepath = filepath,
    output = map_chr(filepath, read_file)
  ) %>% 
    tibble(
      Lsq = str_extract(output, 
                        '(?<=L-squared            = ).+(?=\\(.+\\))') %>% 
        parse_double(),
      Xsq = str_extract(output, 
                        '(?<=X-squared            = ).+(?=\\(.+\\))') %>% 
        parse_double(),
      df = str_extract(output, '(?<=Degrees of freedom   = ).+') %>% 
        parse_double()
    ) %>% 
    select(-filepath, -output) %>% 
    arrange(df)
}


# tab3.2

extract_gof(tab = 'tab3.2') %>% 
  arrange(desc(df)) %>% 
  gt()

# tab3.3

bind_rows(
  read_table('再現/tab3.3/tab3.3_out.txt', skip = 193, n_max = 10, 
             col_names = F) ,
  read_table('再現/tab3.3/tab3.3_out.txt', skip = 192, n_max = 1, 
             col_names = F) %>% 
    rename(X3 = X1, X4 = X2, X5 = X3)
) %>% 
  select(manifest_var = X1, categories = X2, 
         Ideal = X3, Believers = X5, Skeptics = X4) %>% 
  gt()



# tab3.4
extract_gof(tab = 'tab3.4') %>% 
  gt()

# tab3.5

bind_rows(
  read_table('再現/tab3.4/tab3.4_Specific Value and Equality Restriction_out.txt', skip = 195, n_max = 10, 
             col_names = F) ,
  read_table('再現/tab3.4/tab3.4_Specific Value and Equality Restriction_out.txt', skip = 194, n_max = 1, 
             col_names = F) %>% 
    rename(X3 = X1, X4 = X2, X5 = X3)
) %>% 
  select(manifest_var = X1, categories = X2, 
         Ideal = X3, Believers = X4, Skeptics = X5) %>% 
  gt()

# tab4.4
extract_gof(tab = 'tab4.4') %>% 
  arrange(desc(df)) %>% 
  gt()

# tab5.2

bind_rows(
  read_delim('再現/tab5.2/tab5.2_out.txt', delim = ' ', col_names = F) %>% 
    select(X2, X3, X5, X6, X9) %>% 
    slice(181:300) %>% 
    slice(1:64) %>% 
    fill(X2) %>% 
    filter(!is.na(X9)) %>% 
    mutate(X9 = str_trim(X9)) %>% 
    rename(variable = X2, outcome = X3, 潜在クラス = X5, group = X6, 
           probability = X9),
  
  read_delim('再現/tab5.2/tab5.2_out.txt', delim = ' ', col_names = F) %>% 
    select(X2, X3, X5, X6, X9) %>% 
    slice(174:300) %>% 
    slice(1:7) %>% 
    fill(X2) %>% 
    filter(!is.na(X9)) %>% 
    mutate(X9 = str_trim(X9)) %>% 
    select(variable = X2, 潜在クラス = X3, group = X5, 
           probability = X9) 
)  %>% 
  
  mutate(variable = str_extract(variable, '(?<=P\\().(?=|)'),
         group = case_when(group == 1 ~ 'White', 
                           group == 2 ~ 'Black')) %>% 
  pivot_wider(names_from = c(group, 潜在クラス), names_sep = '_', 
              values_from = probability) %>% 
  gt() %>% 
  tab_spanner_delim(delim = '_')


# tab5.3

extract_gof('tab5.3') %>% 
  arrange(df) %>% 
  gt()

# tab5.4

bind_rows(
  read_delim('再現/tab5.3/tab5.3_H10_out.txt', delim = ' ', col_names = F) %>% 
    select(X2, X3, X5, X6, X9) %>% 
    slice(169:300) %>% 
    slice(1:35) %>% 
    fill(X2) %>% 
    filter(!is.na(X9)) %>% 
    mutate(X9 = str_trim(X9)) %>% 
    rename(variable = X2, outcome = X3, 潜在クラス = X5, group = X6, 
           probability = X9),
  
  read_delim('再現/tab5.3/tab5.3_H10_out.txt', delim = ' ', col_names = F) %>% 
    select(X2, X3, X5, X6, X9) %>% 
    slice(162:300) %>% 
    slice(1:7) %>% 
    fill(X2) %>% 
    filter(!is.na(X9)) %>% 
    mutate(X9 = str_trim(X9)) %>% 
    select(variable = X2, 潜在クラス = X3, group = X5, 
           probability = X9) 
)  %>% 
  
  mutate(variable = str_extract(variable, '(?<=P\\().(?=|)'),
         group = case_when(group == 1 ~ 'White', 
                           group == 2 ~ 'Black')) %>%
  pivot_wider(names_from = c(潜在クラス),
              values_from = probability) %>%
  gt() %>% 
  tab_spanner_delim(delim = '_')






