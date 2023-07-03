library(tidyverse)
library(nflreadr)
combine_data<-read_csv("draft_data_imputed.csv")
running_backs<-read_csv("running_backs.csv") %>%
  mutate(cfbref_id=player_id)
running_backs$player_id <- NULL
running_backs[is.na(running_backs)] = 0
running_backs <- running_backs %>%
  mutate(srs_total=srs*g, sos_total=sos*g, off_plus_total=off_plus*g,
         rush_yards=rush_att*rush_yards_att_plus,
         rush_tds=rush_tds_att_plus*rush_att,
         rec_yards=rec*rec_yards_att_plus,
         rec_tds=rec*td_rec_att_plus) %>%
  group_by(cfbref_id) %>%
  summarise(g=sum(g), rush_att=sum(rush_att),
            srs=sum(srs_total)/sum(g),
            sos=sum(sos_total)/sum(g), 
            off_plus=sum(off_plus_total)/sum(g),
            rec=sum(rec),
            rush_yards_att_plus=sum(rush_yards)/sum(rush_att),
            rush_tds_att_plus=sum(rush_tds)/sum(rush_att),
            rec_yards_att_plus=sum(rec_yards)/sum(rec),
            td_rec_att_plus=sum(rec_tds)/sum(rec))
avs<-read_csv("rushing_avs.csv") %>%
  filter(pos=="RB") %>%
  summarise(pfr_id=player_id, av)
avs <- merge(avs, combine_data, by.x="pfr_id", by.y="pfr_player_id") %>%
  summarise(pfr_id, av, season, pick, age, 
            height_inches, weight, forty, vertical, 
            broad_jump, shuttle, bench, cone,
            cfbref_id=cfb_player_id)
rm(combine_data)
running_backs <- merge(avs, running_backs, by="cfbref_id") %>%
  filter(av>0) %>%
  filter(rush_att>0) %>%
  filter(!duplicated(cfbref_id))
x_data <- running_backs[6:24]
x_data[is.na(x_data)] = 0
x_stdv <- apply(x_data, 2, sd)
x_scaled <- scale(x_data,center=T, x_stdv)
y_data <- as.vector(running_backs[3])
get_correlations<-function(index) {
  r_value=as.numeric(cor(x_data[index], y_data))
  variable_names=as.vector(names(x_data[index]))
  data<-data.frame(variable_names,
                   r_value, 
                   abs(r_value))
  names(data) <- c("variable", "r", "r_abs_value")
  data
}
correlations<-reduce(lapply(1:ncol(x_scaled), get_correlations), rbind)
weighted_x_scaled <- data.frame(matrix(nrow=nrow(x_scaled)))
for (index in 1:ncol(x_scaled)){
  column<-x_scaled[,index]*correlations[index,3]*100
  weighted_x_scaled<-cbind(weighted_x_scaled, column)
}

weighted_x_scaled[,1] <- NULL
colnames(weighted_x_scaled) <- colnames(x_scaled)
project_rbs<-function(index){
  print(running_backs[index,][1])
  player_info <- running_backs[index,][1:3]
  train<-as.matrix(weighted_x_scaled[-index,])
  test<-as.matrix(weighted_x_scaled[index,])
  k_value<- 15
  model<-KODAMA::knn_Armadillo(Xtrain=train, Xtest=test, k=k_value)
  knn_data<-data.frame()
  for (player_index in 1:k_value){
    id <- running_backs[model$nn_index[player_index],][1:3]
    weight <- 1/model$distances[player_index]
    new_data <- data.frame(id, weight)
    knn_data <- rbind(knn_data, new_data)
    
  }
  comp_1 <- knn_data[1,1]
  comp_2<-knn_data[2,1]
  comp_3<-knn_data[3,1]
  predicted_av<-as.numeric(knn_data %>%
                             mutate(av_sum=av*weight) %>%
                             summarise(predicted_av=sum(av_sum)/sum(weight)))
  data.frame(player_info, predicted_av, comp_1, comp_2, comp_3)
}
rb_projections<-drop_na(reduce(lapply(1:nrow(running_backs), project_rbs), 
                               rbind))
write_csv(rb_projections, "running_back_example.csv")
