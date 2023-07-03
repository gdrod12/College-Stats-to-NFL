library(tidyverse)
library(nflreadr)
library(keras)
library(tensorflow)

combine_data<-read_csv("draft_data_imputed.csv")
quarterbacks<-read_csv("quarterbacks.csv") %>%
  mutate(cfbref_id=player_id)
quarterbacks$player_id <- NULL
quarterbacks[is.na(quarterbacks)] = 0
quarterbacks<-quarterbacks %>%
  mutate(srs_total=srs*G, sos_total=sos*G, off_plus_total=off_plus*G,
         yards_att_total=yards_att_plus*Att, cmp_pct_total=cmp_pct_plus*Att,
         td_att_total=td_pct_plus*Att, int_att_total=int_pct_plus*Att,
         rush_yards_att_total=rush_yards_att_plus*rush_att, 
         rush_tds_att_total=rush_tds_att_plus*rush_att) %>%
  group_by(cfbref_id) %>%
  summarise(g=sum(G), att=sum(Att), rush_att=sum(rush_att), 
            srs=sum(srs_total)/sum(G),
            sos=sum(sos_total)/sum(G), 
            off_plus=sum(off_plus_total)/sum(G),
            yards_att_plus=sum(yards_att_total)/sum(Att),
            cmp_pct_plus=sum(cmp_pct_total)/sum(Att),
            td_pct_plus=sum(td_att_total)/sum(Att),
            int_pct_plus=sum(int_att_total)/sum(Att),
            rush_yards_att_plus=sum(rush_yards_att_total)/sum(rush_att),
            rush_tds_att_plus=sum(rush_tds_att_total)/sum(rush_att)) 
avs<-read_csv("passer_avs.csv") %>%
  filter(pos=="QB") %>%
  summarise(pfr_id=player_id, av)
avs <- merge(avs, combine_data, by.x="pfr_id", by.y="pfr_player_id") %>%
  summarise(pfr_id, av, season, pick, age, 
            height_inches, weight, forty, vertical, 
            broad_jump, shuttle, bench, cone,
            cfbref_id=cfb_player_id)
rm(combine_data)
quarterbacks <- merge(avs, quarterbacks, by="cfbref_id") %>%
  filter(av>=0) %>%
  filter(rush_att>0) %>%
  filter(!duplicated(cfbref_id))

x_data <- quarterbacks[6:26]
x_maxs <- apply(x_data, 2, max)
x_mins<-apply(x_data, 2, min)
x_scaled <- scale(x_data,center=x_mins, x_maxs-x_mins)
y_data <- unlist(quarterbacks[3])
y_scaled <- scale(y_data, center=min(y_data), max(y_data)-min(y_data))
#setting reproducable rng
set.seed(1432)
passing_x <- as.matrix(data.frame(x_scaled) %>%
  summarise(att, g, yards_att_plus, cmp_pct_plus, td_pct_plus, int_pct_plus))
passing_model <- keras_model_sequential() %>% 
  layer_dense(units = 2, input_shape = ncol(passing_x)) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units = 2) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units = 1, activation="relu")
summary(passing_model)
# Compile the model
passing_model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam()
)
# Train the model with a validation split of 0.1
passing_modelhistory <- passing_model %>% fit(
  x = passing_x, y = y_scaled,
  epochs = 500, batch_size = 32,
  lr=0.01
)
quarterbacks$passer_rating<-passing_model %>% predict(passing_x)

rushing_x <- as.matrix(data.frame(x_scaled) %>%
                         summarise(rush_att,rush_yards_att_plus,
                                   rush_tds_att_plus, g))
rushing_model <- keras_model_sequential() %>% 
  layer_dense(units = 2, input_shape = ncol(rushing_x)) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units = 2) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units = 1, activation="relu")
summary(rushing_model)
# Compile the model
rushing_model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam()
)
# Train the model with a validation split of 0.1
rushing_modelhistory <- rushing_model %>% fit(
  x = rushing_x, y = y_scaled,
  epochs = 500, batch_size = 32,
  lr=0.01
)
quarterbacks$rusher_rating<- rushing_model %>% predict(rushing_x)


athleticism_x <- as.matrix(data.frame(x_scaled) %>%
                         summarise(height_inches, weight, forty,
                                   vertical, broad_jump, shuttle,
                                   bench, cone))
athleticism_model <- keras_model_sequential() %>% 
  layer_dense(units = 2, input_shape = ncol(athleticism_x)) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units = 2) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units = 1, activation="relu")
summary(athleticism_model)
# Compile the model
athleticism_model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam()
)
# Train the model with a validation split of 0.1
athleticism_modelhistory <- athleticism_model %>% fit(
  x = athleticism_x, y = y_scaled,
  epochs = 500, batch_size = 32,
  lr=0.01
)
quarterbacks$athleticism_rating<- athleticism_model %>% predict(athleticism_x)

success_x <- as.matrix(data.frame(x_scaled) %>%
                             summarise( srs, sos, off_plus))
success_model <- keras_model_sequential() %>% 
  layer_dense(units = 2, input_shape = ncol(success_x)) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units = 2) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units = 1, activation="relu")
summary(success_model)
# Compile the model
success_model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam()
)
# Train the model with a validation split of 0.1
success_modelhistory <- success_model %>% fit(
  x = success_x, y = y_scaled,
  epochs = 500, batch_size = 32,
  lr=0.01
)

quarterbacks$success_rating<- success_model %>% predict(success_x)
quarterbacks$age_rating <- scale(quarterbacks$age, center=min(quarterbacks$age), 
                                 max(quarterbacks$age)-min(quarterbacks$age))
predictive_ratings <- quarterbacks[27:31]
project_qbs<-function(index){
  print(quarterbacks[index,][1])
  player_info <- quarterbacks[index,][1:5]
  train<-as.matrix(predictive_ratings[-index,])
  test<-as.matrix(predictive_ratings[index,])
  k_value<- 3
  model<-KODAMA::knn_Armadillo(Xtrain=train, Xtest=test, k=k_value)
  knn_data<-data.frame()
  for (player_index in 1:k_value){
    id <- quarterbacks[model$nn_index[player_index],][1:5]
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
qb_projections<-(reduce(lapply(1:nrow(quarterbacks), project_qbs), 
                               rbind))
overall_model <- keras_model_sequential() %>% 
  layer_dense(units = 5, input_shape = ncol(predictive_ratings)) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units = 5) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units = 1, activation="relu")
summary(overall_model)
# Compile the model
overall_model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam()
)
# Train the model with a validation split of 0.1
overall_modelhistory <- overall_model %>% fit(
  x = as.matrix(predictive_ratings), y = y_scaled,
  epochs = 500, batch_size = 32,
  lr=0.01
)
qb_projections$regression_predicted_av<-min(y_data)+(max(y_data)-min(y_data))*
  overall_model %>% predict(as.matrix(predictive_ratings))

our_rankings <- qb_projections %>%
  arrange(-regression_predicted_av) %>%
  summarise(cfbref_id, av, pick, our_rank=1:nrow(qb_projections)) %>%
  arrange(pick) %>%
  mutate(nfl_rank=1:nrow(qb_projections)) %>%
  arrange(-av) %>%
  mutate(actual_rank=1:nrow(qb_projections))

cor.test(our_rankings$our_rank, our_rankings$actual_rank, method="kendall")
cor.test(our_rankings$nfl_rank, our_rankings$actual_rank, method="kendall")

