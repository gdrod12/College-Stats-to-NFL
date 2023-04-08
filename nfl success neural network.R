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
  mutate(srs_total=srs*g, sos_total=sos*g, off_plus_total=off_plus*g) %>%
  group_by(cfbref_id) %>%
  summarise(g=sum(g), att=sum(att), rush_att=sum(rush_att), 
            srs=sum(srs_total)/sum(g),
            sos=sum(sos_total)/sum(g), 
            off_plus=sum(off_plus_total)/sum(g),
            yards_plus=sum(yards_plus),
            cmp_plus=sum(cmp_plus),
            td_plus=sum(td_plus),
            int_plus=sum(int_plus),
            rush_yards_plus=sum(rush_yards_plus),
            rush_tds_plus=sum(rush_tds_plus)) 
avs<-read_csv("modeled_passer_avs.csv") %>%
  summarise(pfr_id=player_id, av)

avs[is.na(avs)]=0
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
correlations <- data.frame()
for (x in 6:ncol(quarterbacks)){
  name <- names(quarterbacks[x])
  cor <- cor(quarterbacks[x], quarterbacks[3])
  data <- data.frame(measure = name, r_value = cor[1], r_squared = cor[1]^2)
  correlations <- rbind(correlations, data)
}
predict_draft_class <- function(test_year){
set.seed(43*test_year)
testqbs <- quarterbacks %>%
  filter(season==test_year)
trainqbs <- quarterbacks %>%
  filter(season!=test_year)

x_data <- trainqbs[6:26]
#removing team success data from the sample due to lack of significant importance
#retaining strength of schedule for context
x_maxs <- apply(x_data, 2, max)
x_mins<-apply(x_data, 2, min)
x_scaled <- scale(x_data,center=x_mins, x_maxs-x_mins)
forty_weight <- (correlations %>%
  filter(measure == "forty"))[,3]
shuttle_weight <- (correlations %>%
                   filter(measure == "shuttle"))[,3]
cone_weight <- (correlations %>%
                   filter(measure == "cone"))[,3]
vertical_weight <- (correlations %>%
                  filter(measure == "vertical"))[,3]
broad_jump_weight <- (correlations %>%
                        filter(measure == "broad_jump"))[,3]
x_scaled <- as.matrix((data.frame(x_scaled) %>%
  summarise(age, height_inches, weight, 
            run=(forty*forty_weight+shuttle*shuttle_weight+cone*cone_weight)/
              (forty_weight+shuttle_weight+cone_weight),
            jump=(vertical*vertical_weight+broad_jump*broad_jump_weight)/
              (vertical_weight+broad_jump_weight),
            g, att, rush_att, srs, sos, off_plus,
            yards_plus, cmp_plus, td_plus, int_plus,
            rush_yards_plus, rush_tds_plus)))
y_data <- unlist(trainqbs[3])
y_scaled <- scale(y_data, center=min(y_data), max(y_data)-min(y_data))
test_scaled <- as.matrix((data.frame(scale(testqbs[c(6:26)], center=x_mins, x_maxs-x_mins)) %>%
                         summarise(age, height_inches, weight, 
                                   run=(forty*forty_weight+shuttle*shuttle_weight+cone*cone_weight)/
                                     (forty_weight+shuttle_weight+cone_weight),
                                   jump=(vertical*vertical_weight+broad_jump*broad_jump_weight)/
                                     (vertical_weight+broad_jump_weight),
                                   g, att, rush_att, srs, sos, off_plus,
                                   yards_plus, cmp_plus, td_plus, int_plus,
                                   rush_yards_plus, rush_tds_plus)))
y_test <- unlist(testqbs[3])
sample <- sample(c(TRUE, FALSE), nrow(x_scaled), replace=TRUE, prob=c(1,0))
x_train  <- x_scaled[sample, ]
x_test   <- x_scaled[!sample, ]
y_train <- y_scaled[sample, ]
y_test <- y_scaled[!sample, ]
qb_model <- keras_model_sequential() %>% 
  layer_dense(units = 4, input_shape = ncol(x_train)) %>%
  layer_activation_parametric_relu() %>%
  layer_dense(units = 2) %>%
  layer_activation_parametric_relu() %>%
  layer_dense(units = 1) %>%
  layer_activation_parametric_relu()
summary(qb_model)
# Compile the model
qb_model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam()
)
# Train the model with a validation split of 0.1
passing_modelhistory <- qb_model %>% fit(
  x = x_train, y = y_train,
  epochs = 500, batch_size = 32,
  lr=0.01
)
testqbs$predicted_av <- qb_model %>% predict(test_scaled)*(max(y_data)-min(y_data))+min(y_data)
testqbs %>%
  arrange(-predicted_av)
}
drafT_classes<-lapply(1987:2022, predict_draft_class)
draft_classes <- reduce(drafT_classes, rbind)
draft2016 <- predict_draft_class(2016)
draft_classes<-rbind(draft_classes %>%
  filter(season!=2016), draft2016)
rm(drafT_classes)
seasons <- split(draft_classes, draft_classes$season)
draft_data <- data.frame()
success_rates <- data.frame()
for (class in 1:length(seasons)){
  data <- data.frame(seasons[class])
  print(data)
  names(data) <- names(draft_classes)
  data <- data %>%
    summarise(cfbref_id, pfr_id, av, season, pick, age, predicted_av) %>%
    arrange(-predicted_av) %>%
    mutate(our_ranking=1:nrow(data)) %>%
    arrange(-av) %>%
    mutate(real_ranking=1:nrow(data)) %>%
    arrange(pick) %>%
    mutate(nfl_ranking=1:nrow(data))
  our_success<-cor.test(data$our_ranking, data$real_ranking, method="kendall")
  print(our_success)
  nfl_success<-cor.test(data$nfl_ranking, data$real_ranking, method="kendall")
  success_rate <- data.frame(our_success=our_success[4], nfl_success=nfl_success[4],
                             our_pvalue=our_success[3], nfl_pvalue=nfl_success[3])
  draft_data<-rbind(draft_data, data)
  success_rates <- rbind(success_rates, success_rate)
}
our_rankings <- train_qbs %>%
  arrange(-predicted_av) %>%
  summarise(cfbref_id, av, pick, our_rank=1:nrow(train_qbs)) %>%
  arrange(pick) %>%
  mutate(nfl_rank=1:nrow(train_qbs)) %>%
  arrange(-av) %>%
  mutate(actual_rank=1:nrow(train_qbs))
nfl_success[4]
cor.test(draft_data$our_ranking, draft_data$real_ranking, method="kendall")
cor.test(draft_data$nfl_ranking, draft_data$real_ranking, method="kendall")



write_csv(data.frame(as.matrix(draft_data)), "draft_rankings.csv")
write_csv(data.frame(as.matrix(success_rates)), "tau_values.csv")

