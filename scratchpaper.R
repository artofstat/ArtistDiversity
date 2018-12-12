library(dplyr)
gen <- df %>%
  dplyr::filter(gender != 'Not Inferred') %>%
  count(museum,gender) %>%
  group_by(museum) %>%
  mutate(relfreq=n/sum(n)) #%>%
  #dplyr::dplyr::filter(gender == 'Woman')

gen <- gen %>%
  select(gender,relfreq) %>%
  spread(gender,relfreq) 

eth <- df %>%
  dplyr::filter(ethnicity != 'Not Inferred') %>%
  count(museum,ethnicity) %>%
  group_by(museum) %>%
  mutate(relfreq=n/sum(n)) #%>%
  #dplyr::dplyr::filter(ethnicity != 'White')

eth <- eth %>%
  select(ethnicity,relfreq) %>%
  spread(ethnicity,relfreq) %>%
  replace_na(list(Asian = 0,Black = 0,`Hispanice or Latino/a` = 0,Other = 0))



geo <- df %>%
  dplyr::filter(nationality != 'Not Inferred') %>%
  count(museum,nationality) %>%
  group_by(museum) %>%
  mutate(relfreq=n/sum(n)) #%>%
  #dplyr::dplyr::filter(nationality != 'North American')

geo <- geo %>%
  select(nationality,relfreq) %>%
  spread(nationality,relfreq) %>%
  replace_na(list(Africa = 0,`Asia/Pacific` = 0,`West Asia` = 0,`Latin American/Caribbean` = 0,Europe = 0))

byr <- df %>% 
  dplyr::filter(birthyear != 'Not Inferred') %>%
  count(museum,birthyear) %>%
  group_by(museum) %>%
  mutate(relfreq=n/sum(n)) #%>%
  #dplyr::dplyr::filter(birthyear != 'Before 500')

byr <- byr %>%
  select(birthyear,relfreq) %>%
spread(birthyear,relfreq) %>%
  replace_na(list(`500-1500` = 0,`1500's` = 0,`1600's` = 0,`1700's` = 0,`1800's` = 0,`1900's` = 0))

x <- bind_cols(eth,gen) %>%
  select(-starts_with("museu")) %>%
  ungroup() %>%
  select(-1)

divOrd <- x %>%
  dist(.) %>%
  hclust(.) %>% as.dendrogram() %>% rotate_DendSer(ser_weight = dist(x)) %>% labels()



x2 <- bind_cols(byr,geo) %>%
  select(-starts_with("museu")) %>%
  ungroup() %>%
  select(-1)

colOrd <-  x2 %>%
  dist(.) %>%
  hclust(.) %>% as.dendrogram() %>% rotate_DendSer(ser_weight = dist(x2)) %>% labels()
