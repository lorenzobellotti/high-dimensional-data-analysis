#import librerie
library(modeldata)
library(tidyverse)
library(tidymodels)
library(patchwork)
library(splines)
library(themis)
library(dplyr)
library(tidytext)
library(broom)  
library(cowplot)
library(ggplot2)
library(corrplot)



#import dataset
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))


###plot esplorativi###
#sale_price
ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, fill = "#0A537D", col = "black" ) +
  scale_x_log10()

#fence
ggplot(ames, aes(y = Fence)) + 
  geom_bar(fill = "#0A537D", col = "black") + 
  labs(y = NULL)

#pool_qc
ggplot(ames, aes(y = Pool_QC)) + 
  geom_bar(fill = "#0A537D", col = "black") + 
  labs(y = NULL)

#neighborhood
ggplot(ames, aes(y = Neighborhood)) + 
  geom_bar(fill = "#0A537D", col = "black") + 
  labs(y = NULL)


## Riduzione delle modalità delle variabili categoriche:

# riduzione delle modalità di Fence
ames$Fence <- gsub(pattern = "Minimum_Wood_Wire",ames$Fence ,replacement ="Fence" )
ames$Fence <- gsub(pattern = "Minimum_Privacy",ames$Fence ,replacement ="Fence" )
ames$Fence <- gsub(pattern = "Good_Wood",ames$Fence ,replacement ="Fence" )
ames$Fence <- gsub(pattern = "Good_Privacy",ames$Fence ,replacement ="Fence" )

# riduzione delle modalità di Pool_QC
ames$Pool_QC <- gsub(pattern = "Typical",ames$Pool_QC ,replacement ="Pool" )
ames$Pool_QC <- gsub(pattern = "Good",ames$Pool_QC ,replacement ="Pool" )
ames$Pool_QC <- gsub(pattern = "Fair",ames$Pool_QC ,replacement ="Pool" )
ames$Pool_QC <- gsub(pattern = "Excellent",ames$Pool_QC ,replacement ="Pool" )

#grafici di fence, pool_QC e mszoning dopo la riduzione delle modalità

#fence
ggplot(ames, aes(y = Fence)) + 
  geom_bar(fill = "#0A537D", col = "black") + 
  labs(y = NULL)

#pool_qc
ggplot(ames, aes(y = Pool_QC)) + 
  geom_bar(fill = "#0A537D", col = "black") + 
  labs(y = NULL)





#train(80%) e test(20%) con random sampling 
set.seed(123)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)





###interaction terms###
ggplot(ames_train, aes(x = Gr_Liv_Area, y = 10^Sale_Price)) + 
  geom_point(alpha = .2) + 
  facet_wrap(~ Bldg_Type) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, col = "red") + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Gross Living Area", y = "Sale Price (USD)")



###spline functions###

#latitude
plot_smoother <- function(deg_free) {
  ggplot(ames_train, aes(x = Latitude, y = Sale_Price)) + 
    geom_point(alpha = .2) + 
    scale_y_log10() +
    geom_smooth(
      method = lm,
      formula = y ~ ns(x, df = deg_free),
      col = "red",
      se = FALSE
    ) +
    ggtitle(paste(deg_free, "Spline Terms"))
}

( plot_smoother(2) + plot_smoother(5) ) / ( plot_smoother(20) + plot_smoother(100) )

#longitude
plot_smoother <- function(deg_free) {
  ggplot(ames_train, aes(x = Longitude, y = Sale_Price)) + 
    geom_point(alpha = .2) + 
    scale_y_log10() +
    geom_smooth(
      method = lm,
      formula = y ~ ns(x, df = deg_free),
      col = "red",
      se = FALSE
    ) +
    ggtitle(paste(deg_free, "Spline Terms"))
}

( plot_smoother(2) + plot_smoother(5) ) / ( plot_smoother(20) + plot_smoother(100) )


###PCA###
tmwr_cols <- colorRampPalette(c("#91CBD765", "#CA225E"))
ames_train %>% 
  select(matches("(SF$)|(Gr_Liv)")) %>% 
  cor() %>% 
  corrplot(col = tmwr_cols(200), tl.col = "black")





pca_recipe <- 
  recipe( ~ ., data = ames_train) %>% 
  step_center(matches("(SF$)|(Gr_Liv)")) %>% 
  step_pca(matches("(SF$)|(Gr_Liv)"),id = "pca")  %>% 
  prep()

pca <- 
  pca_recipe %>% 
  tidy(id = "pca") 
pca

pca_recipe %>% 
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#0A537D") + 
  xlim(c(0, 7)) + 
  ylab("% of total variance")

 
pca %>%
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  ) 





###recipe###
ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude   , data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)
  




#summary del recipe
tidy(ames_rec)



#workflow
lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

lm_fit %>% 
  extract_recipe(estimated = TRUE)
lm_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()


#rmse e rsq train vs test
estimate_perf <- function(model, dat) {

  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- gsub("ames_", "", data_name)
  
  reg_metrics <- metric_set(rmse, rsq)
  
  model %>% 
    predict(dat) %>% 
    bind_cols(dat %>% select(Sale_Price)) %>% 
    reg_metrics(Sale_Price, .pred) %>% 
    select(-.estimator) %>% 
    mutate(object = obj_name, data = data_name)
}

estimate_perf(lm_fit, ames_train)
estimate_perf(lm_fit, ames_test)



#impatto degli step del recipe su RMSE
basic_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude , data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

interaction_rec <- 
  basic_rec %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) 

spline_rec <- 
  interaction_rec %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

preproc <- 
  list(basic = basic_rec, 
       interact = interaction_rec, 
       splines = spline_rec
  )

lm_models <- workflow_set(preproc, list(lm = lm_model), cross = FALSE)
lm_models



#Cross validation per valutare rmse
set.seed(123)
ames_folds <- vfold_cv(ames_train, v = 10)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

lm_models <- 
  lm_models %>% 
  workflow_map("fit_resamples", 
                
               seed = 123, verbose = TRUE,
                
               resamples = ames_folds, control = keep_pred)

collect_metrics(lm_models) %>% 
  filter(.metric == "rmse")

autoplot(lm_models, metric = "rmse")

lm_models


###RECIPE + FENCE + POOL ###
ames_rec2 <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude + Fence + Pool_QC  , data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow2 <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec2)

lm_fit2 <- fit(lm_wflow2, ames_train)

#rmse e rsq train vs test
estimate_perf <- function(model, dat) {
  
  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- gsub("ames_", "", data_name)
  
  reg_metrics <- metric_set(rmse, rsq)
  
  model %>% 
    predict(dat) %>% 
    bind_cols(dat %>% select(Sale_Price)) %>% 
    reg_metrics(Sale_Price, .pred) %>% 
    select(-.estimator) %>% 
    mutate(object = obj_name, data = data_name)
}

estimate_perf(lm_fit2, ames_train)
estimate_perf(lm_fit2, ames_test)

