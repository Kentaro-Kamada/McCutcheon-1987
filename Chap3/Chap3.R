library(tidyverse)
library(kamaken)
library(gt)



# データ作成 ---------------------------------------------------------------------------------------

data <- read_csv('Chap3/tab3.1.csv')

data_person <- 
  pmap_dfr(list(data$A, data$B, data$C, data$D, data$n),
           ~tibble(A = rep(..1, ..5), B = rep(..2, ..5), C = rep(..3, ..5), D = rep(..4, ..5)))



res3.2 <- 
  poLCA_result(as.matrix(data_person) ~ 1, 
               data = data_person, nclass = 1:3, maxiter = 10000, nrep = 10)


# tab3.2
res3.2 %>% poLCA_BLRT()

# tab3.3
res3.2 %>% poLCA_check_class(3)




lem(lat = 'lat 1', 
    man = 'man 4', 
    dim = 'dim 3 3 2 3 2', 
    lab = 'lab X P A C U', 
    mod = 'mod X P|X A|X C|X U|X', 
    dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'), 
    ite = 'ite 10000', 
    see = str_c('see ', rdunif(10, 9999, 1111)), 
    path = 'lem/tab3.3/'
    )

lem_max_loglik('lem/tab3.3/out/')

lem_gof('lem/tab3.3/out/see3512.out')



# tab3.3
bind_rows(
  read_table('lem/tab3.3/out/see3512.out', skip = 261, n_max = 1, 
             col_names = F, col_types = 'ccc') %>% 
    rename(X3 = X1, X4 = X2, X5 = X3),
  read_table('lem/tab3.3/out/see3512.out', skip = 262, n_max = 10,
             col_names = F, col_types = 'ccccc')
) %>% 
  select(manifest_var = X1, categories = X2, 
         Ideal = X3, Believers = X5, Skeptics = X4) %>% 
  gt()


# specific value restriction
lem(lat = 'lat 1', 
    man = 'man 4', 
    dim = 'dim 3 3 2 3 2', 
    lab = 'lab X P A C U', 
    mod = 'mod X 
               P|X 
               A|X eq2
               C|X eq2
               U|X eq2', 
    des = 'des [0 0  0 0 -1 0
                0 0 -1 0  0 0 0 0 0
               -1 0  0 0  0 0]', 
    sta = 'sta A|X [.5 .5 .5 .5 0 1.0]
           sta C|X [.9 .1 0  .3 .3 .3 .3 .3 .3]
           sta U|X [1.0 0 .5 .5 .5 .5]',
    dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'), 
    ite = 'ite 10000', 
    see = str_c('see ', rdunif(10, 9999, 1111)), 
    path = 'lem/tab3.4_speval/'
)


# specific value and equality restriction
lem(lat = 'lat 1', 
    man = 'man 4', 
    dim = 'dim 3 3 2 3 2', 
    lab = 'lab X P A C U', 
    mod = 'mod X 
               P|X eq2
               A|X eq2
               C|X eq2
               U|X eq2', 
    des = 'des [1 2  0 1  2 0 0 0 0
                3 0  3 0 -1 0
                0 0 -1 0  0 0 0 0 0
               -1 0  0 0  0 0]', 
    sta = 'sta A|X [.5 .5 .5 .5 0 1.0]
           sta C|X [.9 .1 0  .3 .3 .3 .3 .3 .3]
           sta U|X [1.0 0 .5 .5 .5 .5]',
    dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'), 
    ite = 'ite 10000', 
    see = str_c('see ', rdunif(10, 9999, 1111)), 
    path = 'lem/tab3.4_speval_and_equ/'
)


# tab3.4
c('lem/tab3.3/out/', 'lem/tab3.4_speval/out/', 'lem/tab3.4_speval_and_equ/out/') %>% 
  map(lem_max_loglik) %>% 
  map(~pluck(., 'filepath', 1)) %>% 
  map_dfr(lem_gof)


# tab3.5
bind_rows(
  read_table('lem/tab3.4_speval_and_equ/out/see1273.out', skip = 201, n_max = 10, 
             col_names = F),
  read_table('lem/tab3.4_speval_and_equ/out/see1273.out', skip = 200, n_max = 1, 
             col_names = F) %>% 
    rename(X3 = X1, X4 = X2, X5 = X3)
) %>% 
  select(manifest_var = X1, categories = X2, 
         Ideal = X3, Believers = X4, Skeptics = X5) %>% 
  gt()



