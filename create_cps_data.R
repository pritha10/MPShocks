## CPS 1979-2008 ##
# clean data

## Download Current Population Survey, Merged Outgoing Rotation Group files from NBER
# link : https://data.nber.org/data/morg.html

datasets <- paste0("morg",c(0:16,79:99))

# esr, grateat, class, occ70
group1 <- paste0("morg",c(79:82))
list1 <- list()
for (i in group1) {
  list1[[i]] <- read.dta(paste0("data/",i,".dta"), convert.dates = TRUE, convert.factors = FALSE,
                         missing.type = FALSE,
                         convert.underscore = FALSE, 
                         warn.missing.labels = TRUE) %>% 
    # select("year", "intmonth", "age", "sex", "race", "weight",
    #        "earnwt" , "gradeat", "earnwke", "earnhre", "paidhre",
    #        "class", "esr", "uhourse", "hourslw", "occ70", "activlwr") %>% 
    filter(age > 16, age < 65) %>% 
    mutate(empl = case_when(esr %in% c(1,2) ~ "E", 
                            esr %in% c(4:7) ~ "NILF",
                            TRUE ~ "U"),
           skill = case_when(gradeat >= 16 ~ "HS",
                             TRUE ~ "LS"),
           worker = case_when(class %in% c(5,6) ~ "self",
                              TRUE ~ "paid"))%>% 
    select("year", "intmonth", "age", "sex", "race", "weight",
           "earnwt" , "skill", "earnwke", "earnhre", "paidhre",
           "worker", "empl", "uhourse", "hourslw", "occ"="occ70")
  cat(i)
}

# esr, gradeat, class, occ80
group2 <- paste0("morg",c(83:88))
list2 <- list()
for (i in group2) {
  list2[[i]] <- read.dta(paste0("data/",i,".dta"), convert.dates = TRUE, convert.factors = FALSE,
                         missing.type = FALSE,
                         convert.underscore = FALSE, 
                         warn.missing.labels = TRUE) %>% 
    # select("year", "intmonth", "age", "sex", "race", "weight",
    #        "earnwt" , "gradeat", "earnwke", "earnhre", "paidhre",
    #        "class", "esr", "uhourse", "hourslw", "occ80", "activlwr") %>% 
    filter(age > 16, age < 65) %>% 
    mutate(empl = case_when(esr %in% c(1,2) ~ "E", 
                            esr %in% c(4:7) ~ "NILF",
                            TRUE ~ "U"),
           skill = case_when(gradeat >= 16 ~ "HS",
                             TRUE ~ "LS"),
           worker = case_when(class %in% c(5,6) ~ "self",
                              TRUE ~ "paid")) %>% 
    select("year", "intmonth", "age", "sex", "race", "weight",
           "earnwt" , "skill", "earnwke", "earnhre", "paidhre",
           "worker", "empl", "uhourse", "hourslw", "occ"="occ80")
  cat(i)
}

# lfsr89, gradeat, class, occ80
group3 <- paste0("morg",c(89:91))
list3 <- list()
for (i in group3) {
  list3[[i]] <- read.dta(paste0("data/",i,".dta"), convert.dates = TRUE, convert.factors = FALSE,
                         missing.type = FALSE,
                         convert.underscore = FALSE, 
                         warn.missing.labels = TRUE) %>% 
    # select("year", "intmonth", "age", "sex", "race", "weight",
    #        "earnwt" , "gradeat", "earnwke", "earnhre", "paidhre",
    #        "class", "esr", "uhourse", "hourslw", "occ80", "activlwr") %>% 
    filter(age > 16, age < 65) %>% 
    mutate(empl = case_when(lfsr89 %in% c(1,2) ~ "E", 
                            lfsr89 %in% c(3:4) ~ "U",
                            TRUE ~ "NILF"),
           skill = case_when(gradeat >= 16 ~ "HS",
                             TRUE ~ "LS"),
           worker = case_when(class %in% c(5,6) ~ "self",
                              TRUE ~ "paid")) %>% 
    select("year", "intmonth", "age", "sex", "race", "weight",
           "earnwt" , "skill", "earnwke", "earnhre", "paidhre",
           "worker", "empl", "uhourse", "hourslw", "occ"="occ80")
  cat(i)
}

# lfsr89, grade92, class, occ80
group4 <- paste0("morg",c(92:93))
list4 <- list()
for (i in group4) {
  list4[[i]] <- read.dta(paste0("data/",i,".dta"), convert.dates = TRUE, convert.factors = FALSE,
                         missing.type = FALSE,
                         convert.underscore = FALSE, 
                         warn.missing.labels = TRUE) %>% 
    # select("year", "intmonth", "age", "sex", "race", "weight",
    #        "earnwt" , "gradeat", "earnwke", "earnhre", "paidhre",
    #        "class", "esr", "uhourse", "hourslw", "occ80", "activlwr") %>% 
    filter(age > 16, age < 65) %>% 
    mutate(empl = case_when(lfsr89 %in% c(1,2) ~ "E", 
                            lfsr89 %in% c(3:4) ~ "U",
                            TRUE ~ "NILF"),
           skill = case_when(grade92 >= 43 ~ "HS",
                             TRUE ~ "LS"),
           worker = case_when(class %in% c(5,6) ~ "self",
                              TRUE ~ "paid")) %>% 
    select("year", "intmonth", "age", "sex", "race", "weight",
           "earnwt" , "skill", "earnwke", "earnhre", "paidhre",
           "worker", "empl", "uhourse", "hourslw", "occ"="occ80")
  cat(i)
}

# lfsr94, grade92, class94, occ80 
group5 <- paste0("morg",c(94:99))
list5 <- list()
for (i in group5) {
  list5[[i]] <- read.dta(paste0("data/",i,".dta"), convert.dates = TRUE, convert.factors = FALSE,
                         missing.type = FALSE,
                         convert.underscore = FALSE, 
                         warn.missing.labels = TRUE) %>% 
    # select("year", "intmonth", "age", "sex", "race", "weight",
    #        "earnwt" , "gradeat", "earnwke", "earnhre", "paidhre",
    #        "class", "esr", "uhourse", "hourslw", "occ80", "activlwr") %>% 
    filter(age > 16, age < 65) %>% 
    mutate(empl = case_when(lfsr94 %in% c(1,2) ~ "E", 
                            lfsr94 %in% c(3:4) ~ "U",
                            TRUE ~ "NILF"),
           skill = case_when(grade92 >= 43 ~ "HS",
                             TRUE ~ "LS"),
           worker = case_when(class94 %in% c(6,7) ~ "self",
                              TRUE ~ "paid")) %>% 
    select("year", "intmonth", "age", "sex", "race", "weight",
           "earnwt" , "skill", "earnwke", "earnhre", "paidhre",
           "worker", "empl", "uhourse", "hourslw", "occ"="occ80")
  cat(i)
}

# lfsr94, grade92, class94, docc00
group6 <- paste0("morg",c(0:18))
list6 <- list()
for (i in group6) {
  list6[[i]] <- read.dta(paste0("data/",i,".dta"), convert.dates = TRUE, convert.factors = FALSE,
                         missing.type = FALSE,
                         convert.underscore = FALSE, 
                         warn.missing.labels = TRUE) %>% 
    # select("year", "intmonth", "age", "sex", "race", "weight",
    #        "earnwt" , "gradeat", "earnwke", "earnhre", "paidhre",
    #        "class", "esr", "uhourse", "hourslw", "occ80", "activlwr") %>% 
    filter(age > 16, age < 65) %>% 
    mutate(empl = case_when(lfsr94 %in% c(1,2) ~ "E", 
                            lfsr94 %in% c(3:4) ~ "U",
                            TRUE ~ "NILF"),
           skill = case_when(grade92 >= 43 ~ "HS",
                             TRUE ~ "LS"),
           worker = case_when(class94 %in% c(6,7) ~ "self",
                              TRUE ~ "paid")) %>% 
    select("year", "intmonth", "age", "sex", "race", "weight",
           "earnwt" , "skill", "earnwke", "earnhre", "paidhre",
           "worker", "empl", "uhourse", "hourslw", "occ"="docc00")
  cat(i)
}

dataset_1979_2018 <- bind_rows(list1,list2,list3,list4,list5,list6) %>% 
  mutate(race = case_when(race == 1 ~ "W",
                          race == 2 ~ "B",
                          TRUE ~ "O"),
         skillgender = case_when(skill == "HS" & sex == 1 ~ "HSM",
                                 skill == "HS" & sex == 2 ~ "HSF",
                                 skill == "LS" & sex == 1 ~ "LSM",
                                 skill == "LS" & sex == 2 ~ "LSF"),
         skillrace = case_when(skill == "HS" & race == "W" ~ "HSW",
                               skill == "HS" & race == "B" ~ "HSB",
                               skill == "HS" & race == "O" ~ "HSO",
                               skill == "LS" & race == "W" ~ "LSW",
                               skill == "LS" & race == "B" ~ "LSB",
                               skill == "LS" & race == "O" ~ "LSO"))

write.csv(dataset_1979_2018,file="cps1979_2018.csv")

dataset_1979_2018 <- read.csv("data/cps1979_2018.csv")

###########################################################
# Create data extract for 1979-1996, 1979-2018
###########################################################

## High-skill and Low-skill demographic group
# Unemployment rate, labor force participation, wages, hours

vardata_skill <- dataset_1979_2018 %>%
  filter(empl!="NILF")%>%
  group_by(skill,empl,year,intmonth)%>%
  summarise(n = sum(weight))%>%
  group_by(skill,year,intmonth)%>%
  mutate(unemp = 100*(n/sum(n)))%>%
  filter(empl == "U")%>%
  select(skill,year,intmonth,unemp)%>%
  ungroup()%>%
  mutate(skill =if_else(skill == "HS","unemp_HS","unemp_LS"))%>%
  spread(skill,unemp) %>% 
  left_join(dataset_1979_2018 %>%
              drop_na(weight)%>%
              mutate(ilf = if_else(empl != "NILF",1,0))%>%
              group_by(skill,ilf,year,intmonth)%>%
              summarise(n = sum(weight))%>%
              group_by(skill,year,intmonth)%>%
              mutate(lfp = 100*(n/sum(n)))%>%
              ungroup()%>%
              filter(ilf == 1)%>%
              select(skill,lfp,year,intmonth)%>%
              mutate(skill = if_else(skill == "HS","lfp_HS","lfp_LS")) %>% 
              spread(skill,lfp)) %>% 
  left_join(dataset_1979_2018 %>%
              filter(empl!="NILF", worker!="self", !is.na(uhourse), uhourse!=0, !is.na(earnwke))%>%
              mutate(hourlywage = earnwt*(earnwke/uhourse)) %>%
              group_by(skill,empl,year,intmonth)%>%
              summarise(n = sum(earnwt), h = sum(hourlywage))%>%
              group_by(skill,year,intmonth)%>%
              mutate(wages = (h/sum(n)))%>%
              filter(empl!="U")%>%
              select(skill,year,intmonth,wages)%>%
              ungroup()%>%
              mutate(skill = if_else(skill == "HS","wages_HS","wages_LS")) %>%
              spread(skill,wages)) %>% 
  left_join(dataset_1979_2018 %>%
              filter(empl!="NILF")%>%
              mutate(uhourse = replace_na(uhourse,0), wthours=weight*uhourse) %>% 
              group_by(skill,empl,year,intmonth)%>%
              summarise(n = sum(weight), h = sum(wthours))%>%
              group_by(skill,year,intmonth)%>%
              mutate(hours = (h/sum(n)))%>%
              filter(empl!="U")%>%
              select(skill,year,intmonth,hours)%>%
              ungroup()%>%
              mutate(skill = if_else(skill == "HS","hours_HS","hours_LS")) %>%
              spread(skill,hours))

vardata_skill_gender <- dataset_1979_2018 %>% 
  filter(empl!="NILF")%>%
  group_by(skillgender,empl,year,intmonth)%>%
  summarise(n = sum(weight))%>%
  group_by(skillgender,year,intmonth)%>%
  mutate(unemp = 100*(n/sum(n)))%>%
  filter(empl == "U")%>%
  select(skillgender,year,intmonth,unemp)%>%
  ungroup()%>%
  mutate(skillgender = case_when(skillgender == "HSM" ~ "unemp_HSM",
                                 skillgender == "HSF" ~ "unemp_HSF",
                                 skillgender == "LSM" ~ "unemp_LSM",
                                 skillgender == "LSF" ~ "unemp_LSF")) %>%
  spread(skillgender,unemp) %>% 
  left_join(dataset_1979_2018 %>%
              drop_na(weight)%>%
              mutate(ilf = if_else(empl != "NILF",1,0))%>%
              group_by(skillgender,ilf,year,intmonth)%>%
              summarise(n = sum(weight))%>%
              group_by(skillgender,year,intmonth)%>%
              mutate(lfp = 100*(n/sum(n)))%>%
              ungroup()%>%
              filter(ilf == 1)%>%
              select(skillgender,lfp,year,intmonth)%>%
              mutate(skillgender = case_when(skillgender == "HSM" ~ "lfp_HSM",
                                             skillgender == "HSF" ~ "lfp_HSF",
                                             skillgender == "LSM" ~ "lfp_LSM",
                                             skillgender == "LSF" ~ "lfp_LSF")) %>% 
              spread(skillgender,lfp)) %>% 
  left_join(dataset_1979_2018 %>%
              filter(empl!="NILF", worker!="self", !is.na(uhourse), uhourse!=0, !is.na(earnwke))%>%
              mutate(hourlywage = earnwt*(earnwke/uhourse)) %>%
              group_by(skillgender,empl,year,intmonth)%>%
              summarise(n = sum(earnwt), h = sum(hourlywage))%>%
              group_by(skillgender,year,intmonth)%>%
              mutate(wages = (h/sum(n)))%>%
              filter(empl!="U")%>%
              select(skillgender,year,intmonth,wages)%>%
              ungroup()%>%
              mutate(skillgender = case_when(skillgender == "HSM" ~ "wages_HSM",
                                             skillgender == "HSF" ~ "wages_HSF",
                                             skillgender == "LSM" ~ "wages_LSM",
                                             skillgender == "LSF" ~ "wages_LSF")) %>%
              spread(skillgender,wages)) %>% 
  left_join(dataset_1979_2018 %>%
              filter(empl!="NILF")%>%
              mutate(uhourse = replace_na(uhourse,0), wthours=weight*uhourse) %>% 
              group_by(skillgender,empl,year,intmonth)%>%
              summarise(n = sum(weight), h = sum(wthours))%>%
              group_by(skillgender,year,intmonth)%>%
              mutate(hours = (h/sum(n)))%>%
              filter(empl!="U")%>%
              select(skillgender,year,intmonth,hours)%>%
              ungroup()%>%
              mutate(skillgender = case_when(skillgender == "HSM" ~ "hours_HSM",
                                             skillgender == "HSF" ~ "hours_HSF",
                                             skillgender == "LSM" ~ "hours_LSM",
                                             skillgender == "LSF" ~ "hours_LSF")) %>%
              spread(skillgender,hours))

vardata_skill_race <- dataset_1979_2018 %>% 
  filter(empl!="NILF")%>%
  group_by(skillrace,empl,year,intmonth)%>%
  summarise(n = sum(weight))%>%
  group_by(skillrace,year,intmonth)%>%
  mutate(unemp = 100*(n/sum(n)))%>%
  filter(empl == "U")%>%
  select(skillrace,year,intmonth,unemp)%>%
  ungroup()%>%
  mutate(skillrace = case_when(skillrace == "HSW" ~ "unemp_HSW",
                               skillrace == "HSB" ~ "unemp_HSB",
                               skillrace == "HSO" ~ "unemp_HSO",
                               skillrace == "LSW" ~ "unemp_LSW",
                               skillrace == "LSB" ~ "unemp_LSB",
                               skillrace == "LSO" ~ "unemp_LSO")) %>%
  spread(skillrace,unemp) %>% 
  left_join(dataset_1979_2018 %>%
              drop_na(weight)%>%
              mutate(ilf = if_else(empl != "NILF",1,0))%>%
              group_by(skillrace,ilf,year,intmonth)%>%
              summarise(n = sum(weight))%>%
              group_by(skillrace,year,intmonth)%>%
              mutate(lfp = 100*(n/sum(n)))%>%
              ungroup()%>%
              filter(ilf == 1)%>%
              select(skillrace,lfp,year,intmonth)%>%
              mutate(skillrace = case_when(skillrace == "HSW" ~ "lfp_HSW",
                                           skillrace == "HSB" ~ "lfp_HSB",
                                           skillrace == "HSO" ~ "lfp_HSO",
                                           skillrace == "LSW" ~ "lfp_LSW",
                                           skillrace == "LSB" ~ "lfp_LSB",
                                           skillrace == "LSO" ~ "lfp_LSO")) %>% 
              spread(skillrace,lfp)) %>% 
  left_join(dataset_1979_2018 %>%
              filter(empl!="NILF", worker!="self", !is.na(uhourse), uhourse!=0, !is.na(earnwke))%>%
              mutate(hourlywage = earnwt*(earnwke/uhourse)) %>%
              group_by(skillrace,empl,year,intmonth)%>%
              summarise(n = sum(earnwt), h = sum(hourlywage))%>%
              group_by(skillrace,year,intmonth)%>%
              mutate(wages = (h/sum(n)))%>%
              filter(empl!="U")%>%
              select(skillrace,year,intmonth,wages)%>%
              ungroup()%>%
              mutate(skillrace = case_when(skillrace == "HSW" ~ "wages_HSW",
                                           skillrace == "HSB" ~ "wages_HSB",
                                           skillrace == "HSO" ~ "wages_HSO",
                                           skillrace == "LSW" ~ "wages_LSW",
                                           skillrace == "LSB" ~ "wages_LSB",
                                           skillrace == "LSO" ~ "wages_LSO")) %>%
              spread(skillrace,wages)) %>% 
  left_join(dataset_1979_2018 %>%
              filter(empl!="NILF")%>%
              mutate(uhourse = replace_na(uhourse,0), wthours=weight*uhourse) %>% 
              group_by(skillrace,empl,year,intmonth)%>%
              summarise(n = sum(weight), h = sum(wthours))%>%
              group_by(skillrace,year,intmonth)%>%
              mutate(hours = (h/sum(n)))%>%
              filter(empl!="U")%>%
              select(skillrace,year,intmonth,hours)%>%
              ungroup()%>%
              mutate(skillrace = case_when(skillrace == "HSW" ~ "hours_HSW",
                                           skillrace == "HSB" ~ "hours_HSB",
                                           skillrace == "HSO" ~ "hours_HSO",
                                           skillrace == "LSW" ~ "hours_LSW",
                                           skillrace == "LSB" ~ "hours_LSB",
                                           skillrace == "LSO" ~ "hours_LSO")) %>%
              spread(skillrace,hours))


# labor force participation
# nacount <- dataset_1979_2018%>%
#   group_by(year,empl)%>%
#   summarise(count = sum(is.na(weight)))

## Gender
# Unemployment rate, labor force participation, wages, hours

vardata_gender_18 <- dataset_1979_2018 %>%
  filter(empl!= "NILF")%>%
  group_by(sex,empl,year,intmonth)%>%
  summarise(n = 4*sum(weight))%>%
  group_by(sex,year,intmonth)%>%
  mutate(unemp = 100*(n/sum(n)))%>%
  filter(empl == "U")%>%
  select(sex,year,intmonth,unemp)%>%
  ungroup()%>%
  mutate(sex=if_else(sex == 1,"unemp_m","unemp_f"))%>%
  spread(sex,unemp) %>% 
  left_join(dataset_1979_2018 %>%
              drop_na(weight)%>%
              mutate(ilf = if_else(empl != "NILF",1,0))%>%
              group_by(sex,ilf,year,intmonth)%>%
              summarise(n = sum(weight))%>%
              group_by(sex,year,intmonth)%>%
              mutate(lfp = 100*(n/sum(n)))%>%
              ungroup()%>%
              filter(ilf == 1)%>%
              select(sex,lfp,year,intmonth)%>%
              mutate(sex = if_else(sex == 1,"lfp_m","lfp_f")) %>% 
              spread(sex,lfp)) %>% 
  left_join(dataset_1979_2018 %>%
              filter(empl!="NILF", worker!="self", !is.na(uhourse), uhourse!=0, !is.na(earnwke))%>%
              mutate(hourlywage = earnwt*(earnwke/uhourse)) %>%
              group_by(sex,empl,year,intmonth)%>%
              summarise(n = sum(earnwt), h = sum(hourlywage))%>%
              group_by(sex,year,intmonth)%>%
              mutate(wages = (h/sum(n)))%>%
              filter(empl!="U")%>%
              select(sex,year,intmonth,wages)%>%
              ungroup()%>%
              mutate(sex = if_else(sex == 1,"wages_m","wages_f")) %>%
              spread(sex,wages)) %>% 
  left_join(dataset_1979_2018 %>%
              filter(empl!="NILF")%>%
              mutate(uhourse = replace_na(uhourse,0), wthours=weight*uhourse) %>% 
              group_by(sex,empl,year,intmonth)%>%
              summarise(n = sum(weight), h = sum(wthours))%>%
              group_by(sex,year,intmonth)%>%
              mutate(hours = (h/sum(n)))%>%
              filter(empl!="U")%>%
              select(sex,year,intmonth,hours)%>%
              ungroup()%>%
              mutate(sex = if_else(sex == 1,"hours_m","hours_f")) %>%
              spread(sex,hours))

vardata_gender_96 <- vardata_gender_18 %>% 
  filter(year < 1997)

## Race
# Unemployment rate, labor force participation, wages, hours

# nrow(test[is.na(test$hours_o),])

vardata_race_18 <- dataset_1979_2018 %>%
  filter(empl!= "NILF")%>%
  group_by(race,empl,year,intmonth)%>%
  summarise(n = 4*sum(weight))%>%
  group_by(race,year,intmonth)%>%
  mutate(unemp = 100*(n/sum(n)))%>%
  filter(empl == "U")%>%
  select(race,year,intmonth,unemp)%>%
  ungroup()%>%
  mutate(race = case_when(race == "W" ~ "unemp_w",
                          race == "B" ~ "unemp_b",
                          race == "O" ~ "unemp_o"))%>%
  spread(race,unemp) %>% 
  left_join(dataset_1979_2018 %>%
              drop_na(weight)%>%
              mutate(ilf = if_else(empl != "NILF",1,0))%>%
              group_by(race,ilf,year,intmonth)%>%
              summarise(n = sum(weight))%>%
              group_by(race,year,intmonth)%>%
              mutate(lfp = 100*(n/sum(n)))%>%
              ungroup()%>%
              filter(ilf == 1)%>%
              select(race,lfp,year,intmonth)%>%
              mutate(race = case_when(race == "W" ~ "lfp_w",
                                      race == "B" ~ "lfp_b",
                                      race == "O" ~ "lfp_o"))%>%
              spread(race,lfp)) %>% 
  left_join(dataset_1979_2018 %>%
              filter(empl!="NILF", worker!="self", !is.na(uhourse), uhourse!=0, !is.na(earnwke))%>%
              mutate(hourlywage = earnwt*(earnwke/uhourse)) %>%
              group_by(race,empl,year,intmonth)%>%
              summarise(n = sum(earnwt), h = sum(hourlywage))%>%
              group_by(race,year,intmonth)%>%
              mutate(wages = (h/sum(n)))%>%
              filter(empl!="U")%>%
              select(race,year,intmonth,wages)%>%
              ungroup()%>%
              mutate(race = case_when(race == "W" ~ "wages_w",
                                      race == "B" ~ "wages_b",
                                      race == "O" ~ "wages_o"))%>%
              spread(race,wages)) %>% 
  left_join(dataset_1979_2018 %>%
              filter(empl!="NILF")%>%
              mutate(uhourse = replace_na(uhourse,0), wthours=weight*uhourse) %>% 
              group_by(race,empl,year,intmonth)%>%
              summarise(n = sum(weight), h = sum(wthours))%>%
              group_by(race,year,intmonth)%>%
              mutate(hours = (h/sum(n)))%>%
              filter(empl!="U")%>%
              select(race,year,intmonth,hours)%>%
              ungroup()%>%
              mutate(race = case_when(race == "W" ~ "hours_w",
                                      race == "B" ~ "hours_b",
                                      race == "O" ~ "hours_o"))%>%
              spread(race,hours))

vardata_race_96 <- vardata_race_18 %>% 
  filter(year < 1997)

