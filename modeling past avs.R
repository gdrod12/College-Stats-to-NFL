library(tidyverse)
passer_avs <- read_csv("passer_avs.csv") %>%
  mutate(year=as.numeric(substr(year, 1, 4)))
draft_data <- read_csv("draft_data.csv") %>%
  filter(pos=="QB") %>%
  summarise(player_id, season)
split_passers <- split(passer_avs, passer_avs$player_id)
#model to predict fifth season based on first four

first_four <- function(x){
  print(x)
  player_id <- names(split_passers[x])
  data<-data.frame(split_passers[x])
  names(data) <- names(passer_avs)
  data <- merge(data, draft_data, by="player_id")
  draft_year <- data$season[1]
  first <- data %>%
    filter(year < draft_year+4) %>%
    summarise(g=sum(g), gs=sum(gs), av=sum(av))
  ifelse(nrow(first)>0,
         first <- first,
         first <- data.frame(g=0, gs=0, av=0)
  )
  output<-data %>%
    filter(year == draft_year+4) %>%
    summarise(g5=sum(g), gs5=sum(gs), av5=sum(av))
  ifelse(nrow(output)>=1, output<-output, output<-data.frame(g5=0, gs5=0, av5=0))
  data.frame(player_id, first, output, draft_year)
}
first_four_seasons<-reduce(lapply(1:length(split_passers), first_four), rbind) %>%
  filter(player_id!="EricCr00") %>%
  filter(draft_year<=2018)
four_model <- lm(av5~av, data=first_four_seasons)
get_four_qbs <- function(x){
  print(x)
  player_id <- names(split_passers[x])
  data<-data.frame(split_passers[x])
  names(data) <- names(passer_avs)
  data <- merge(data, draft_data, by="player_id")
  draft_year <- data$season[1]
  data<-data %>%
    filter(season==2019, year<=2022) %>%
    group_by(player_id) %>%
    summarise(g=sum(g), gs=sum(gs), av=sum(av), draft_year=draft_year)
  data
}

four_qbs <- drop_na(reduce(lapply(1:length(split_passers), get_four_qbs), rbind))
four_qbs$fifth_year <- predict(four_model, four_qbs)
four_qbs <- four_qbs %>%
  mutate(five_av=av+fifth_year)
#model to predict fourth+fifth season based on first three
first_three <- function(x){
  print(x)
  player_id <- names(split_passers[x])
  data<-data.frame(split_passers[x])
  names(data) <- names(passer_avs)
  data <- merge(data, draft_data, by="player_id")
  draft_year <- data$season[1]
  first <- data %>%
    filter(year < draft_year+3) %>%
    summarise(g=sum(g), gs=sum(gs), av=sum(av))
  ifelse(nrow(first)>0,
         first <- first,
         first <- data.frame(g=0, gs=0, av=0)
  )
  output<-data %>%
    filter(year == draft_year+4|year== draft_year+3) %>%
    summarise(g45=sum(g), gs45=sum(gs), av45=sum(av))
  ifelse(nrow(output)>=1, output<-output, output<-data.frame(g45=0, gs45=0, av45=0))
  data.frame(player_id, first, output, draft_year)
}
first_three_seasons<-reduce(lapply(1:length(split_passers), first_three), rbind) %>%
  filter(player_id!="EricCr00") %>%
  filter(draft_year<=2018)
three_model <- lm(av45~av, data=first_three_seasons)
get_three_qbs <- function(x){
  print(x)
  player_id <- names(split_passers[x])
  data<-data.frame(split_passers[x])
  names(data) <- names(passer_avs)
  data <- merge(data, draft_data, by="player_id")
  draft_year <- data$season[1]
  data<-data %>%
    filter(season==2020, year<=2022) %>%
    group_by(player_id) %>%
    summarise(g=sum(g), gs=sum(gs), av=sum(av), draft_year=draft_year)
  data
}
three_qbs <- reduce(lapply(1:length(split_passers), get_three_qbs), rbind)
three_qbs$fifth_year <- predict(three_model, three_qbs)
three_qbs <- three_qbs %>%
  mutate(five_av=av+fifth_year)
#model to predict third+fourth+fifht season based on first two
first_two <- function(x){
  print(x)
  player_id <- names(split_passers[x])
  data<-data.frame(split_passers[x])
  names(data) <- names(passer_avs)
  data <- merge(data, draft_data, by="player_id")
  draft_year <- data$season[1]
  first <- data %>%
    filter(year < draft_year+2) %>%
    summarise(g=sum(g), gs=sum(gs), av=sum(av))
  ifelse(nrow(first)>0,
         first <- first,
         first <- data.frame(g=0, gs=0, av=0)
  )
  output<-data %>%
    filter(year == draft_year+4|year== draft_year+3| year== draft_year+2) %>%
    summarise(g345=sum(g), gs345=sum(gs), av345=sum(av))
  ifelse(nrow(output)>=1, output<-output, output<-data.frame(g345=0, gs345=0, av345=0))
  data.frame(player_id, first, output, draft_year)
}
first_two_seasons<-reduce(lapply(1:length(split_passers), first_two), rbind) %>%
  filter(player_id!="EricCr00") %>%
  filter(draft_year<=2018)
two_model <- lm(av345~av, data=first_two_seasons)
get_two_qbs <- function(x){
  print(x)
  player_id <- names(split_passers[x])
  data<-data.frame(split_passers[x])
  names(data) <- names(passer_avs)
  data <- merge(data, draft_data, by="player_id")
  draft_year <- data$season[1]
  data<-data %>%
    filter(season==2021, year<=2022) %>%
    group_by(player_id) %>%
    summarise(g=sum(g), gs=sum(gs), av=sum(av), draft_year=draft_year)
  data
}
two_qbs <- reduce(lapply(1:length(split_passers), get_two_qbs), rbind)
two_qbs$fifth_year <- predict(two_model, two_qbs)
two_qbs <- two_qbs %>%
  mutate(five_av=av+fifth_year)
#model to predict second+third+fourth+fifht season based on rookie year
first_one <- function(x){
  print(x)
  player_id <- names(split_passers[x])
  data<-data.frame(split_passers[x])
  names(data) <- names(passer_avs)
  data <- merge(data, draft_data, by="player_id")
  draft_year <- data$season[1]
  first <- data %>%
    filter(year < draft_year+1) %>%
    summarise(g=sum(g), gs=sum(gs), av=sum(av))
  ifelse(nrow(first)>0,
         first <- first,
         first <- data.frame(g=0, gs=0, av=0)
  )
  output<-data %>%
    filter(year == draft_year+4|year== draft_year+3| year== draft_year+2|year==draft_year+1) %>%
    summarise(g2345=sum(g), gs2345=sum(gs), av2345=sum(av))
  ifelse(nrow(output)>=1, output<-output, output<-data.frame(g2345=0, gs2345=0, av2345=0))
  data.frame(player_id, first, output, draft_year)
}
first_one_seasons<-reduce(lapply(1:length(split_passers), first_one), rbind) %>%
  filter(player_id!="EricCr00") %>%
  filter(draft_year<=2018)
one_model <- lm(av2345~av, data=first_one_seasons)
get_one_qbs <- function(x){
  print(x)
  player_id <- names(split_passers[x])
  data<-data.frame(split_passers[x])
  names(data) <- names(passer_avs)
  data <- merge(data, draft_data, by="player_id")
  draft_year <- data$season[1]
  data<-data %>%
    filter(season==2022, year<=2022) %>%
    group_by(player_id) %>%
    summarise(g=sum(g), gs=sum(gs), av=sum(av), draft_year=draft_year)
  data 
}
one_qbs <- reduce(lapply(1:length(split_passers), get_one_qbs), rbind)
one_qbs$fifth_year <- predict(one_model, one_qbs)
one_qbs <- one_qbs %>%
  mutate(five_av=av+fifth_year)
modeled_qbs <- bind_rows(one_qbs, two_qbs, three_qbs, four_qbs) %>%
  summarise(player_id, av=five_av, draft_year)
#getting past performance for pre 2019 QBs
get_vets <- function(x){
  print(x)
  data<-data.frame(split_passers[x])
  names(data) <- names(passer_avs)
  data<-merge(data, draft_data, by="player_id")
  data %>%
    filter(year<=season+4) %>%
    group_by(player_id) %>%
    summarise(av=sum(av), draft_year = data$season[1])
}
veterans<-reduce(lapply(1:length(split_passers), get_vets), rbind) %>%
  filter(player_id!="EricCr00") %>%
  filter(draft_year <= 2018)
qbs<-rbind(modeled_qbs, veterans)
qbs$draft_year <- NULL
qb_avs<-merge(draft_data, qbs, by="player_id", all.x = T)
write_csv(qb_avs, "modeled_passer_avs.csv")
