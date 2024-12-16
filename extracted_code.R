## ----setup, message=FALSE, warning=FALSE, include=FALSE-------------------------------------------------------------------------------------------------------------------
#chunks
knitr::opts_chunk$set(eval=TRUE, message=FALSE, warning=FALSE, echo=TRUE, fig.height=5, fig.align='center')

#libraries
library(tidyverse)
library(DMwR)
library(xgboost)
library(vip)
library(summarytools)
library(fpp3)
library(readxl)
library(curl)
library(latex2exp)
library(seasonal)
library(GGally)
library(gridExtra)
library(doParallel)
library(reshape2)
library(Hmisc)
library(corrplot)
library(e1071)
library(caret)
library(VIM)
library(rpart)
library(forecast)
library(urca)
library(earth)
library(glmnet)
library(cluster)
library(kernlab)
library(aTSA)
library(AppliedPredictiveModeling)
library(mlbench)
library(randomForest)
library(party)
library(gbm)
library(Cubist)
library(partykit)
library(kableExtra)
library(factoextra)
library(FactoMineR)
library(naniar)
library(mice)
library(janitor)
library(writexl)


## ----load_data------------------------------------------------------------------------------------------------------------------------------------------------------------
#Load train data
#URL the raw .xlsx file
url <- "https://raw.githubusercontent.com/ex-pr/project_2_DATA624/main/StudentData.xlsx"
#Temporary path
temp_file <- tempfile(fileext = ".xlsx")
#Download file
curl_download(url, temp_file)
#Read file
data_beverage <- read_excel(temp_file)
#Copy data
beverage_df <- data_beverage
#Clean temp file
unlink(temp_file)

#Load test data
#URL the raw .xlsx file
url <- "https://raw.githubusercontent.com/ex-pr/project_2_DATA624/main/StudentEvaluation.xlsx"
#Temporary path
temp_file <- tempfile(fileext = ".xlsx")
#Download file
curl_download(url, temp_file)
#Read file
data_eval <- read_excel(temp_file)
#Copy data
eval_df <- data_eval


## ----summary_statistics---------------------------------------------------------------------------------------------------------------------------------------------------
#Check first rows of data
DT::datatable(
      beverage_df[1:10,],
      options = list(scrollX = TRUE,
                     deferRender = TRUE,
                     dom = 'lBfrtip',
                     fixedColumns = TRUE, 
                     info = FALSE,  
                     paging=FALSE,
                     searching = FALSE), 
      rownames = FALSE,
      caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
    'Table 1: First 10 Rows of Beverage Data'
  )) 

DT::datatable(
      eval_df[1:10,],
      options = list(scrollX = TRUE,
                     deferRender = TRUE,
                     dom = 'lBfrtip',
                     fixedColumns = TRUE, 
                     info = FALSE,      
                     paging=FALSE,
                     searching = FALSE), 
      rownames = FALSE,
      caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
    'Table 2: First 10 Rows of Evaluation Data'
  )) 

#Summary statistics
DT::datatable(
      dfSummary(beverage_df, text.graph.col = FALSE, graph.col = FALSE, style = "grid", valid.col = FALSE),
      extensions = c('Scroller'),
      options = list(scrollY = 350,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     dom = 'lBfrtip',
                     fixedColumns = TRUE, 
                     searching = FALSE), 
      rownames = FALSE,
      caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
    'Table 3: Summary Statistics for Beverage Data'
  )) 
 

#Summary statistics
DT::datatable(
      dfSummary(eval_df, text.graph.col = FALSE, graph.col = FALSE, style = "grid", valid.col = FALSE),
      extensions = c('Scroller'),
      options = list(scrollY = 350,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     dom = 'lBfrtip',
                     fixedColumns = TRUE, 
                     searching = FALSE), 
      rownames = FALSE,
      caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
    'Table 4: Summary Statistics for Evaluation Data'
  )) 

stat <- beverage_df %>%
  group_by(`Brand Code`) %>%
  filter(!is.na(`Brand Code`)) %>% 
  dplyr::summarize(
    Min = min(PH, na.rm = TRUE),
    Q1 = quantile(PH, 0.25, na.rm = TRUE),
    Median = median(PH, na.rm = TRUE),
    Mean = mean(PH, na.rm = TRUE),
    Q3 = quantile(PH, 0.75, na.rm = TRUE),
    Max = max(PH, na.rm = TRUE),
    StdDev = sd(PH, na.rm = TRUE),
    Count = n(),
    Missing = sum(is.na(PH)) 
  )

#Summary statistics by code
DT::datatable(
      stat,
      options = list(dom = 'lBfrtip',
                     fixedColumns = TRUE, 
                     searching = FALSE,
                     paging=FALSE,
                     info = FALSE), 
      rownames = FALSE,
      caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
    'Table 5: Summary Statistics PH for Each Brand Code'
  )) %>%
  DT::formatRound(columns = c("Min", "Q1", "Median", "Mean", "Q3", "Max", "StdDev"), digits = 3)


## ----ph_plots-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Distribution PH plot
ggplot(beverage_df, aes(x = PH)) +
  geom_histogram(binwidth = 0.1, fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of PH", x = "PH", y = "Frequency")

#Distribution Brand Code plot
ggplot(beverage_df, aes(x = `Brand Code`)) +
  geom_bar(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Count by Brand Code", x = "Brand Code", y = "Count")

filtered_df <- beverage_df %>%
  filter(!is.na(`Brand Code`))

#Boxplot brand code vs ph
ggplot(filtered_df, aes(x = `Brand Code`, y = PH, fill = `Brand Code`)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of PH by Brand Code", x = "Brand Code", y = "PH")


## ----group_plots----------------------------------------------------------------------------------------------------------------------------------------------------------
group_1 <- c("Carb Pressure", "Carb Pressure1", "Carb Temp", "Temperature", 
             "Usage cont", "Fill Pressure", "Air Pressurer", "Alch Rel", 
             "Balling", "Balling Lvl")

group_2 <- c("Carb Volume", "Carb Rel", "Fill Ounces", "Oxygen Filler", "Density", 
             "PC Volume", "PSC", "PSC CO2", 
             "PSC Fill", "Pressure Setpoint", "Pressure Vacuum")

group_3 <- c("Mnf Flow", "Carb Flow", "Filler Level", "Filler Speed", "Hyd Pressure1", 
             "Hyd Pressure2", "Hyd Pressure3", "Hyd Pressure4", "MFR", "Bowl Setpoint")

#Group 1 plot
beverage_df %>%
  select(all_of(group_1)) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.3, fill = "lightgreen", color = "black") +
  facet_wrap(~name, scales = "free", ncol = 5) +
  theme_minimal() +
  labs(title = "Distribution of Variables: Group 1")

#Group 2 Plot
beverage_df %>%
  select(all_of(group_2)) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.05, fill = "lightgreen", color = "black") +
  facet_wrap(~name, scales = "free", ncol = 5) +
  theme_minimal() +
  labs(title = "Distribution of Variables: Group 2")

#Group 3 Plot
beverage_df %>%
  select(all_of(group_3)) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  facet_wrap(~name, scales = "free", ncol = 5) +
  theme_minimal() +
  labs(title = "Distribution of Variables: Group 3")


## ----na_scatterplt_ph_vs_features-----------------------------------------------------------------------------------------------------------------------------------------
#NA
gg_miss_var(beverage_df, show_pct = TRUE)

#Choose numeric vars
numeric_vars <- beverage_df %>% 
  select(where(is.numeric)) %>% 
  names()

# Choose numeric variables, remove PH
numeric_vars <- beverage_df %>% 
  select(where(is.numeric)) %>%
  select(-PH) %>% 
  names()

#Pivot longer for faceting
beverage_long <- beverage_df %>%
  pivot_longer(cols = all_of(numeric_vars), names_to = "variable", values_to = "value")

#All scatter plots faceted by variable
ggplot(beverage_long, aes(x = PH, y = value)) +
  geom_point(alpha = 0.6, color = "blue") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(y = "Value", x = "PH", title = "Relationship between Variables and PH")


## ----corr-----------------------------------------------------------------------------------------------------------------------------------------------------------------
tst <- beverage_df %>% 
  select(where(is.numeric))
#Correlation with PH
cor_table <- cor(drop_na(tst))[, "PH"] %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  rename(Coefficient = ".") %>%
  arrange(desc(Coefficient))

kable(cor_table, "html", escape = F, col.names = c('Variable', 'Coefficient')) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(1, bold = T) %>%
  add_header_above(c("Table 6: Correlation with PH" = 2))

#Corr matrix
rcore <- rcorr(as.matrix(tst))
coeff <- rcore$r
#Filter to include only |r| > 0.1, the rest are 0
filtered_coeff <- coeff
filtered_coeff[abs(filtered_coeff) < 0.1] <- 0 
coeff <- rcore$r
corrplot(filtered_coeff, tl.cex = .6, tl.col="black", method = 'color', addCoef.col = "black",
         type="upper", order="hclust", number.cex=0.45, diag=FALSE)


## ----colnames_zeroVar-----------------------------------------------------------------------------------------------------------------------------------------------------
#Fix column names
colnames(beverage_df) <- make_clean_names(colnames(beverage_df))
#Apply the same to eval_df 
colnames(eval_df) <- make_clean_names(colnames(eval_df))
#Check new column names
colnames(beverage_df)

# Identify zero-variance predictors
zero_var <- nearZeroVar(beverage_df, saveMetrics = TRUE)
print(zero_var[zero_var$nzv, ])

beverage_df <- beverage_df[, !zero_var$nzv]
eval_df <- eval_df[, !zero_var$nzv]


## ----impute_numeric-------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(547)

#KNN imputation
beverage_imputed <- as.data.frame(beverage_df)
beverage_imputed <- knnImputation(beverage_imputed[, !names(beverage_imputed) %in% c("brand_code", "mnf_flow")])
beverage_imputed$brand_code <- beverage_df$brand_code
beverage_imputed$mnf_flow <- beverage_df$mnf_flow

#KNN imputation mnf_flow
beverage_imputed$mnf_flow[beverage_imputed$mnf_flow == -100] <- NA
beverage_imputed <-  knnImputation(beverage_imputed[, !names(beverage_imputed) %in% c("brand_code")], k = 5, scale = TRUE, meth = 'median')  
beverage_imputed$brand_code <- beverage_df$brand_code

eval_imputed <- as.data.frame(eval_df)
eval_imputed <- knnImputation(eval_imputed[, !names(eval_imputed) %in% c("brand_code", "ph")])
eval_imputed$brand_code <- eval_df$brand_code
eval_imputed$ph <- eval_df$ph


## ----brand_Code_impute----------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(547)
impute_rf <- function(data, target_var, exclude_vars = NULL) {
  #Ensure target_var is a factor
  data[[target_var]] <- factor(data[[target_var]])
  
  #Exclude specified variables from the model input
  if (!is.null(exclude_vars)) {
    model_data <- data[, !(names(data) %in% exclude_vars)]
  } else {
    model_data <- data
  }
  
  #Train model on non-NA data for the target variable
  rf_model <- randomForest(reformulate(".", target_var), data = model_data[!is.na(model_data[[target_var]]),], na.action = na.omit)
  
  #Predict NAs
  na_indices <- is.na(data[[target_var]])
  if (sum(na_indices) > 0) {
    predicted_values <- predict(rf_model, newdata = model_data[na_indices,])
    data[[target_var]][na_indices] <- predicted_values
  }
  
  return(data)
}

beverage_imputed <- impute_rf(beverage_imputed, "brand_code")
eval_imputed <- impute_rf(eval_imputed, "brand_code", exclude_vars = "ph")

#Verify
colSums(is.na(beverage_imputed))
colSums(is.na(eval_imputed))


## ----encode_categorical---------------------------------------------------------------------------------------------------------------------------------------------------
#One-hot Encoding for Brand Code
beverage_imputed$brand_code <- as.factor(beverage_imputed$brand_code)
eval_imputed$brand_code <- as.factor(eval_imputed$brand_code)
#Create dummies
dummy_vars <- dummyVars(~ brand_code, data = beverage_imputed, fullRank = TRUE)
beverage_imputed <- cbind(beverage_imputed, predict(dummy_vars, newdata = beverage_imputed))
beverage_imputed <- beverage_imputed %>% select(-brand_code) 

eval_imputed <- cbind(eval_imputed, predict(dummy_vars, newdata = eval_imputed))
eval_imputed <- eval_imputed %>% select(-brand_code) 


## ----new_features---------------------------------------------------------------------------------------------------------------------------------------------------------
#Creating Interaction Terms
beverage_revised <- beverage_imputed %>%
  select(-contains('_interaction'), -contains('composite'), -contains('efficiency'), -contains('index')) %>%
  mutate(
    setpoint_diff = bowl_setpoint - pressure_setpoint,  #feature engineering
    carb_volume2 = carb_volume^2,  #Polynomial feature for carb_volume
    pc_volume_pressure = pc_volume * carb_pressure  #Interaction feature
  )

eval_revised <- eval_imputed %>%
  select(-contains('_interaction'), -contains('composite'), -contains('efficiency'), -contains('index')) %>%
  mutate(
    setpoint_diff = bowl_setpoint - pressure_setpoint,  #feature engineering
    carb_volume2 = carb_volume^2,  #Polynomial feature for carb_volume
    pc_volume_pressure = pc_volume * carb_pressure  #Interaction feature
  )

#Remove original columns to reduce collinearity
beverage_revised <- beverage_revised %>%
  select(-c(bowl_setpoint, pressure_setpoint, carb_volume, pc_volume, carb_pressure))
eval_revised <- eval_revised %>%
  select(-c(bowl_setpoint, pressure_setpoint, carb_volume, pc_volume, carb_pressure))

#Creating a binned feature for carb_temp
beverage_revised$carb_temp_binned <- cut(beverage_revised$carb_temp,
                                         breaks=quantile(beverage_revised$carb_temp, probs=seq(0, 1, 0.25)),
                                         include.lowest=TRUE,
                                         labels=FALSE)

eval_revised$carb_temp_binned <- cut(eval_revised$carb_temp,
                                         breaks=quantile(eval_revised$carb_temp, probs=seq(0, 1, 0.25)),
                                         include.lowest=TRUE,
                                         labels=FALSE)

#Remove original carb_temp if the binned version is preferred
beverage_revised <- beverage_revised %>%
  select(-carb_temp)

eval_revised <- eval_revised %>%
  select(-carb_temp)


## ----transform_vars-------------------------------------------------------------------------------------------------------------------------------------------------------
#Apply transformations
set.seed(123)
# Log transformations
beverage_transformed <- beverage_imputed
beverage_transformed$filler_speed <- log(beverage_transformed$filler_speed)
beverage_transformed$mfr <- log(beverage_transformed$mfr)

#Square root transformations
beverage_transformed$temperature <- sqrt(beverage_transformed$temperature)
beverage_transformed$air_pressurer <- sqrt(beverage_transformed$air_pressurer)

#Shifted log transformations
beverage_transformed$oxygen_filler <- log(beverage_transformed$oxygen_filler + 1)
beverage_transformed$psc_co2 <- log(beverage_transformed$psc_co2 + 1)

#Log transformations
eval_transformed <- eval_imputed
eval_transformed$filler_speed <- log(eval_imputed$filler_speed)
eval_transformed$mfr <- log(eval_imputed$mfr)

#Square root transformations
eval_transformed$temperature <- sqrt(eval_transformed$temperature)
eval_transformed$air_pressurer <- sqrt(eval_transformed$air_pressurer)

#Shifted log transformations
eval_transformed$oxygen_filler <- log(eval_transformed$oxygen_filler + 1)
eval_transformed$psc_co2 <- log(eval_transformed$psc_co2 + 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Summary statistics
DT::datatable(
      dfSummary(beverage_revised, text.graph.col = FALSE, graph.col = FALSE, style = "grid", valid.col = FALSE),
      extensions = c('Scroller'),
      options = list(scrollY = 350,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     dom = 'lBfrtip',
                     fixedColumns = TRUE, 
                     searching = FALSE), 
      rownames = FALSE,
      caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
    'Table 7: Summary Statistics for Transformed Beverage Data'
  )) 
 

#Summary statistics
DT::datatable(
      dfSummary(eval_revised, text.graph.col = FALSE, graph.col = FALSE, style = "grid", valid.col = FALSE),
      extensions = c('Scroller'),
      options = list(scrollY = 350,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     dom = 'lBfrtip',
                     fixedColumns = TRUE, 
                     searching = FALSE), 
      rownames = FALSE,
      caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
    'Table 8: Summary Statistics for Transformed Evaluation Data'
  )) 


## ----parallel_core--------------------------------------------------------------------------------------------------------------------------------------------------------
#Detect the number of available cores
numCores <- detectCores()

#Register parallel backend (use numCores - 1 to leave 1 core free for system tasks)
cl <- makeCluster(numCores - 1)
registerDoParallel(cl)


## ----split_data_revised---------------------------------------------------------------------------------------------------------------------------------------------------
#Data with new features
set.seed(123)
trainIndex <- createDataPartition(beverage_revised$ph, p = 0.8, list = FALSE)
train_revised <- beverage_revised[trainIndex, ]
test_revised <- beverage_revised[-trainIndex, ]
eval_revised <- subset(eval_revised, select = -ph)


## ----split_data_transf----------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
trainIndex <- createDataPartition(beverage_transformed$ph, p = 0.8, list = FALSE)
train_transformed <- beverage_transformed[trainIndex, ]
test_transformed <- beverage_transformed[-trainIndex, ]
eval_transformed <- subset(eval_transformed, select = -ph)
#Setup cv
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)


## ----mars_model-----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
#Grid with parameters
marsGrid <- expand.grid(degree = 1:2, nprune = 2:20)

#Train MARS
marsModel <- train(ph ~ ., data = train_transformed, 
                   method = "earth",
                   preProc = c("center", "scale"),
                   tuneGrid = marsGrid,
                   trControl = control
)


#degree nprune      RMSE   Rsquared       MAE   
# 2     20      0.1269369   0.459168    0.0960094 
mars_tune <- marsModel$results[marsModel$results$nprune == marsModel$bestTune$nprune & marsModel$results$degree == marsModel$bestTune$degree, ]
kable(mars_tune, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
  row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
  add_header_above(c("MARS Model Tuning Results"=9), bold = TRUE)

#Predict
marsPred <- predict(marsModel, newdata = test_transformed)
#RMSE      Rsquared        MAE 
#0.12208106 0.51031566 0.09219265
mars_results <- postResample(pred = marsPred, obs = test_transformed$ph)
kable(mars_results, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
  row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
  add_header_above(c("MARS Model Performance Metrics"=2), bold = TRUE)


## ----mars_varimpt---------------------------------------------------------------------------------------------------------------------------------------------------------
importance_mars <- varImp(marsModel, scale = FALSE)
importance_mars_sorted <- as.data.frame(importance_mars$importance) %>%
  arrange(desc(Overall))
kable(importance_mars_sorted, "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
    row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
    add_header_above(c("MARS Model Variable Importance" = 2), bold = TRUE)
plot(importance_mars, top = 10, main = "Top 10 predictors, MARS Model") 


## ----svm_model------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
#Grid with parameters
svmGrid <- expand.grid(sigma = c(0.001, 0.01, 0.1), C = c(0.1, 1, 10, 100))
#Train SVM
svmModel <- train(ph ~ ., data = train_transformed, 
                  method = "svmRadial", 
                  preProc = c("center", "scale"),
                  tuneGrid = svmGrid,
                  trControl = control
                  )

#sigma  C      RMSE   Rsquared        MAE   
# 0.01 10  0.1193689  0.5235771   0.08771225
svm_tune <- svmModel$results[svmModel$results$sigma == svmModel$bestTune$sigma & svmModel$results$C == svmModel$bestTune$C, ]
kable(svm_tune , "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
  row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
  add_header_above(c("SVM Model Tuning Results"=9), bold = TRUE)

#Predict
svmPred <- predict(svmModel, newdata = test_transformed)
#RMSE   Rsquared        MAE 
#0.11787203 0.54495184 0.08445738 
svm_results <- postResample(pred = svmPred, obs = test_transformed$ph)
kable(svm_results, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
  row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
  add_header_above(c("SVM Model Performance Metrics"=2), bold = TRUE)


## ----svm_varimpt----------------------------------------------------------------------------------------------------------------------------------------------------------
importance_svm <- varImp(svmModel, scale = FALSE)
importance_svm_sorted <- as.data.frame(importance_svm$importance) %>%
  arrange(desc(Overall))
kable(importance_svm_sorted, "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
    row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
    add_header_above(c("SVM Model Variable Importance" = 2), bold = TRUE)
plot(importance_svm, top = 10, main = "Top 10 predictors, SVM Model")


## ----nnet_model-----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
#Grid with parameters
nnetGrid <- expand.grid(.decay = c(0, 0.01, 0.1), .size = 1:10, .bag = FALSE)

#Train avNNet
nnetModel <- train(ph ~ ., data = train_transformed, 
                   method = "avNNet",
                   preProc = c("center", "scale"),
                   tuneGrid = nnetGrid,
                   trControl = control,
                   linout = TRUE,
                   trace = FALSE)  

#decay size   bag      RMSE    Rsquared       MAE
#  0   10   FALSE   0.1145209   0.5585798 0.0855949 
nnet_tune <- nnetModel$results[nnetModel$results$size == nnetModel$bestTune$size & nnetModel$results$decay == nnetModel$bestTune$decay, ]
kable(nnet_tune, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
  row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
  add_header_above(c("avNNet Model Tuning Results"=10), bold = TRUE)

#Predict
nnetPred <- predict(nnetModel, newdata = test_transformed)
#RMSE        Rsquared        MAE 
#0.11073852 0.59788239 0.08266435
nnet_results <- postResample(pred = nnetPred, obs = test_transformed$ph)
kable(nnet_results, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
  row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
  add_header_above(c("avNNet Model Performance Metrics"=2), bold = TRUE)


## ----nnet_varimpt---------------------------------------------------------------------------------------------------------------------------------------------------------
importance_nnet <- varImp(nnetModel, scale = FALSE)
importance_nnet_sorted <- as.data.frame(importance_nnet$importance) %>%
  arrange(desc(Overall))
kable(importance_nnet_sorted, "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
    row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
    add_header_above(c("avNNet Model Variable Importance" = 2), bold = TRUE)
plot(importance_nnet, top = 10, main = "Top 10 predictors, avNNet Model")


## ----gbm_model------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
#Grid with parameters
gbmGrid <- expand.grid(.n.trees = seq(100, 1000, by = 100), .interaction.depth = seq(1, 7, by = 2), .shrinkage = 0.01, .n.minobsinnode = c(5, 10, 15))
#Train GBM
gbm_model <- train(ph ~ ., data = train_revised, 
                   method = "gbm", 
                   preProc = c("center", "scale"),
                   tuneGrid = gbmGrid, 
                   trControl = control,
                   verbose = FALSE)

#shrinkage interaction.depth n.minobsinnode n.trees      RMSE     Rsquared        MAE   
# 0.01                 7             10    1000       0.1101819    0.5955535 0.08304377
gbm_tune <- gbm_model$results[gbm_model$results$n.trees == gbm_model$bestTune$n.trees & gbm_model$results$interaction.depth == gbm_model$bestTune$interaction.depth & gbm_model$results$n.minobsinnode == gbm_model$bestTune$n.minobsinnode,]
kable(gbm_tune, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
  row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
  add_header_above(c("GBM Model Tuning Results"=11), bold = TRUE)

#Predict
gbm_predictions <- predict(gbm_model, newdata = test_revised)
#      RMSE   Rsquared        MAE 
#0.10548388 0.64448254 0.07928038 
gbm_results <- postResample(pred = gbm_predictions, obs = test_revised$ph)
kable(gbm_results, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
  row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
  add_header_above(c("GBM Model Performance Metrics"=2), bold = TRUE)


## ----gbm_varimpt----------------------------------------------------------------------------------------------------------------------------------------------------------
importance_gbm <- varImp(gbm_model, scale = FALSE)
importance_gbm_sorted <- as.data.frame(importance_gbm$importance) %>%
  arrange(desc(Overall))
kable(importance_gbm_sorted, "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
    row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
    add_header_above(c("GBM Model Variable Importance" = 2), bold = TRUE)
plot(importance_gbm, top = 10, main = "Top 10 predictors, GBM")


## ----rf_model-------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
#Grid with parameters
rfGrid <- expand.grid(.mtry = c(2, 4, 6, 8, 10))

#Train rf
rf_model <- train(ph ~ ., data = train_revised, 
                  method = "rf",
                  preProc = c("center", "scale"),
                  trControl = control,
                  tuneGrid = rfGrid,
                  importance = TRUE) 

#mtry      RMSE   Rsquared        MAE   
# 10  0.1007928     0.6741332   0.07271081 
rf_tune <- rf_model$results[rf_model$results$mtry == rf_model$bestTune$mtry, ]
kable(rf_tune, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
  row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
  add_header_above(c("RF Model Tuning Results"=8), bold = TRUE)
#Predict
rf_pred <- predict(rf_model, newdata = test_revised)
#RMSE      Rsquared        MAE 
#0.09630017 0.71303966 0.06989033 
rf_results <- postResample(pred = rf_pred, obs = test_revised$ph)
kable(rf_results, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
  row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
  add_header_above(c("RF Model Performance Metrics"=2), bold = TRUE)

# Correct data frame for actual vs predicted values
res<- data.frame(Actual = test_revised$ph, Predicted = rf_pred)
# Scatter plot: Actual vs Predicted Values
ggplot(res, aes(x = Actual, y = Predicted)) +
    geom_point(color = "blue", alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
    labs(title = "Actual vs Predicted pH Values (RF Model)", 
         x = "Actual pH", y = "Predicted pH") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


## ----rf_variimpt----------------------------------------------------------------------------------------------------------------------------------------------------------
importance_rf <- varImp(rf_model, scale = FALSE)
importance_rf_sorted <- as.data.frame(importance_rf$importance) %>%
  arrange(desc(Overall))
kable(importance_rf_sorted, "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center") %>%
    row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
    add_header_above(c("RF Model Variable Importance" = 2), bold = TRUE)
plot(importance_rf, top = 10, main = "Top 10 predictors, Random Forest")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Stop parallel once you're done with models
stopCluster(cl)


## ----compare_models-------------------------------------------------------------------------------------------------------------------------------------------------------
#Create empty df
results <- data.frame(
  Model = character(),
  Resample_RMSE = numeric(),
  Resample_R2 = numeric(),
  Resample_MAE = numeric(),
  Test_RMSE = numeric(),
  Test_R2 = numeric(),
  Test_MAE = numeric(),
  stringsAsFactors = FALSE
)

#Fill df with results
results <- rbind(results, data.frame(Model = "MARS", Resample_RMSE = mars_tune$RMSE, Resample_R2 = mars_tune$Rsquared, Resample_MAE = mars_tune$MAE, Test_RMSE = mars_results[1], Test_R2 = mars_results[2], Test_MAE = mars_results[3]))
results <- rbind(results, data.frame(Model = "SVM", Resample_RMSE = svm_tune$RMSE, Resample_R2 = svm_tune$Rsquared, Resample_MAE = svm_tune$MAE, Test_RMSE = svm_results[1], Test_R2 = svm_results[2], Test_MAE = svm_results[3]))
results <- rbind(results, data.frame(Model = "NNet", Resample_RMSE = nnet_tune$RMSE, Resample_R2 = nnet_tune$Rsquared, Resample_MAE = nnet_tune$MAE, Test_RMSE = nnet_results[1], Test_R2 = nnet_results[2], Test_MAE = nnet_results[3]))
results <- rbind(results, data.frame(Model = "GBM", Resample_RMSE = gbm_tune$RMSE, Resample_R2 = gbm_tune$Rsquared, Resample_MAE = gbm_tune$MAE, Test_RMSE = gbm_results[1], Test_R2 = gbm_results[2], Test_MAE = gbm_results[3]))
results <- rbind(results, data.frame(Model = "RF", Resample_RMSE = rf_tune$RMSE, Resample_R2 = rf_tune$Rsquared, Resample_MAE = rf_tune$MAE, Test_RMSE = rf_results[1], Test_R2 = rf_results[2], Test_MAE = rf_results[3]))
row.names(results) <- NULL

kable(results, "html", escape = FALSE, align = c('l', rep('c', 6)), col.names = c("Model", "Resample RMSE", "Resample R²", "Resample MAE", "Test RMSE", "Test R²", "Test MAE")) %>%
  kable_styling("striped", full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c(" " = 1, "Resample Metrics" = 3, "Test Metrics" = 3), 
                   bold = TRUE, background = "#D3D3D3", color = "black")


## ----save_pred------------------------------------------------------------------------------------------------------------------------------------------------------------
#Predictions for the evaluation dataset
eval_pred <- predict(rf_model, newdata = eval_revised)

#Create df
eval_results <- data.frame(
  ph_pred = eval_pred
)

eval_df_pred <- eval_df
eval_df_pred$ph <- eval_pred

#Save to Excel
write_xlsx(eval_df_pred , "eval_df_pred .xlsx")
write_xlsx(eval_results, "ph_predictions.xlsx")

par(mfrow = c(1, 2))
hist(beverage_revised$ph, main = "Original Data Distribution", xlab = "pH", col = "skyblue", border = "white")
hist(eval_pred, main = "Predicted pH Distribution", xlab = "pH", col = "lightgreen", border = "white")
par(mfrow = c(1, 1))


## ----code, eval=FALSE, include=TRUE, echo=TRUE----------------------------------------------------------------------------------------------------------------------------
## ## ----setup, message=FALSE, warning=FALSE, include=FALSE-------------------------------------------------------------------------------------------------------------------
## #chunks
## knitr::opts_chunk$set(eval=TRUE, message=FALSE, warning=FALSE, echo=TRUE, fig.height=5, fig.align='center')
## 
## #libraries
## library(tidyverse)
## library(DMwR)
## library(xgboost)
## library(vip)
## library(summarytools)
## library(fpp3)
## library(readxl)
## library(curl)
## library(latex2exp)
## library(seasonal)
## library(GGally)
## library(gridExtra)
## library(doParallel)
## library(reshape2)
## library(Hmisc)
## library(corrplot)
## library(e1071)
## library(caret)
## library(VIM)
## library(rpart)
## library(forecast)
## library(urca)
## library(earth)
## library(glmnet)
## library(cluster)
## library(kernlab)
## library(aTSA)
## library(AppliedPredictiveModeling)
## library(mlbench)
## library(randomForest)
## library(party)
## library(gbm)
## library(Cubist)
## library(partykit)
## library(kableExtra)
## library(factoextra)
## library(FactoMineR)
## library(naniar)
## library(mice)
## library(janitor)
## library(writexl)
## 
## 
## ## ----load_data------------------------------------------------------------------------------------------------------------------------------------------------------------
## #Load train data
## #URL the raw .xlsx file
## url <- "https://raw.githubusercontent.com/ex-pr/project_2_DATA624/main/StudentData.xlsx"
## #Temporary path
## temp_file <- tempfile(fileext = ".xlsx")
## #Download file
## curl_download(url, temp_file)
## #Read file
## data_beverage <- read_excel(temp_file)
## #Copy data
## beverage_df <- data_beverage
## #Clean temp file
## unlink(temp_file)
## 
## #Load test data
## #URL the raw .xlsx file
## url <- "https://raw.githubusercontent.com/ex-pr/project_2_DATA624/main/StudentEvaluation.xlsx"
## #Temporary path
## temp_file <- tempfile(fileext = ".xlsx")
## #Download file
## curl_download(url, temp_file)
## #Read file
## data_eval <- read_excel(temp_file)
## #Copy data
## eval_df <- data_eval
## 
## 
## ## ----summary_statistics---------------------------------------------------------------------------------------------------------------------------------------------------
## #Check first rows of data
## DT::datatable(
##       beverage_df[1:10,],
##       options = list(scrollX = TRUE,
##                      deferRender = TRUE,
##                      dom = 'lBfrtip',
##                      fixedColumns = TRUE,
##                      info = FALSE,
##                      paging=FALSE,
##                      searching = FALSE),
##       rownames = FALSE,
##       caption = htmltools::tags$caption(
##     style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
##     'Table 1: First 10 Rows of Beverage Data'
##   ))
## 
## DT::datatable(
##       eval_df[1:10,],
##       options = list(scrollX = TRUE,
##                      deferRender = TRUE,
##                      dom = 'lBfrtip',
##                      fixedColumns = TRUE,
##                      info = FALSE,
##                      paging=FALSE,
##                      searching = FALSE),
##       rownames = FALSE,
##       caption = htmltools::tags$caption(
##     style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
##     'Table 2: First 10 Rows of Evaluation Data'
##   ))
## 
## #Summary statistics
## DT::datatable(
##       dfSummary(beverage_df, text.graph.col = FALSE, graph.col = FALSE, style = "grid", valid.col = FALSE),
##       extensions = c('Scroller'),
##       options = list(scrollY = 350,
##                      scrollX = 500,
##                      deferRender = TRUE,
##                      scroller = TRUE,
##                      dom = 'lBfrtip',
##                      fixedColumns = TRUE,
##                      searching = FALSE),
##       rownames = FALSE,
##       caption = htmltools::tags$caption(
##     style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
##     'Table 3: Summary Statistics for Beverage Data'
##   ))
## 
## 
## #Summary statistics
## DT::datatable(
##       dfSummary(eval_df, text.graph.col = FALSE, graph.col = FALSE, style = "grid", valid.col = FALSE),
##       extensions = c('Scroller'),
##       options = list(scrollY = 350,
##                      scrollX = 500,
##                      deferRender = TRUE,
##                      scroller = TRUE,
##                      dom = 'lBfrtip',
##                      fixedColumns = TRUE,
##                      searching = FALSE),
##       rownames = FALSE,
##       caption = htmltools::tags$caption(
##     style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
##     'Table 4: Summary Statistics for Evaluation Data'
##   ))
## 
## stat <- beverage_df %>%
##   group_by(`Brand Code`) %>%
##   filter(!is.na(`Brand Code`)) %>%
##   dplyr::summarize(
##     Min = min(PH, na.rm = TRUE),
##     Q1 = quantile(PH, 0.25, na.rm = TRUE),
##     Median = median(PH, na.rm = TRUE),
##     Mean = mean(PH, na.rm = TRUE),
##     Q3 = quantile(PH, 0.75, na.rm = TRUE),
##     Max = max(PH, na.rm = TRUE),
##     StdDev = sd(PH, na.rm = TRUE),
##     Count = n(),
##     Missing = sum(is.na(PH))
##   )
## 
## #Summary statistics by code
## DT::datatable(
##       stat,
##       options = list(dom = 'lBfrtip',
##                      fixedColumns = TRUE,
##                      searching = FALSE,
##                      paging=FALSE,
##                      info = FALSE),
##       rownames = FALSE,
##       caption = htmltools::tags$caption(
##     style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
##     'Table 5: Summary Statistics PH for Each Brand Code'
##   )) %>%
##   DT::formatRound(columns = c("Min", "Q1", "Median", "Mean", "Q3", "Max", "StdDev"), digits = 3)
## 
## 
## ## ----ph_plots-------------------------------------------------------------------------------------------------------------------------------------------------------------
## #Distribution PH plot
## ggplot(beverage_df, aes(x = PH)) +
##   geom_histogram(binwidth = 0.1, fill = "lightgreen", color = "black") +
##   theme_minimal() +
##   labs(title = "Distribution of PH", x = "PH", y = "Frequency")
## 
## #Distribution Brand Code plot
## ggplot(beverage_df, aes(x = `Brand Code`)) +
##   geom_bar(fill = "lightgreen") +
##   theme_minimal() +
##   labs(title = "Count by Brand Code", x = "Brand Code", y = "Count")
## 
## filtered_df <- beverage_df %>%
##   filter(!is.na(`Brand Code`))
## 
## #Boxplot brand code vs ph
## ggplot(filtered_df, aes(x = `Brand Code`, y = PH, fill = `Brand Code`)) +
##   geom_boxplot() +
##   theme_minimal() +
##   labs(title = "Boxplot of PH by Brand Code", x = "Brand Code", y = "PH")
## 
## 
## ## ----group_plots----------------------------------------------------------------------------------------------------------------------------------------------------------
## group_1 <- c("Carb Pressure", "Carb Pressure1", "Carb Temp", "Temperature",
##              "Usage cont", "Fill Pressure", "Air Pressurer", "Alch Rel",
##              "Balling", "Balling Lvl")
## 
## group_2 <- c("Carb Volume", "Carb Rel", "Fill Ounces", "Oxygen Filler", "Density",
##              "PC Volume", "PSC", "PSC CO2",
##              "PSC Fill", "Pressure Setpoint", "Pressure Vacuum")
## 
## group_3 <- c("Mnf Flow", "Carb Flow", "Filler Level", "Filler Speed", "Hyd Pressure1",
##              "Hyd Pressure2", "Hyd Pressure3", "Hyd Pressure4", "MFR", "Bowl Setpoint")
## 
## #Group 1 plot
## beverage_df %>%
##   select(all_of(group_1)) %>%
##   pivot_longer(cols = everything()) %>%
##   ggplot(aes(value)) +
##   geom_histogram(binwidth = 0.3, fill = "lightgreen", color = "black") +
##   facet_wrap(~name, scales = "free", ncol = 5) +
##   theme_minimal() +
##   labs(title = "Distribution of Variables: Group 1")
## 
## #Group 2 Plot
## beverage_df %>%
##   select(all_of(group_2)) %>%
##   pivot_longer(cols = everything()) %>%
##   ggplot(aes(value)) +
##   geom_histogram(binwidth = 0.05, fill = "lightgreen", color = "black") +
##   facet_wrap(~name, scales = "free", ncol = 5) +
##   theme_minimal() +
##   labs(title = "Distribution of Variables: Group 2")
## 
## #Group 3 Plot
## beverage_df %>%
##   select(all_of(group_3)) %>%
##   pivot_longer(cols = everything()) %>%
##   ggplot(aes(value)) +
##   geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
##   facet_wrap(~name, scales = "free", ncol = 5) +
##   theme_minimal() +
##   labs(title = "Distribution of Variables: Group 3")
## 
## 
## ## ----na_scatterplt_ph_vs_features-----------------------------------------------------------------------------------------------------------------------------------------
## #NA
## gg_miss_var(beverage_df, show_pct = TRUE)
## 
## #Choose numeric vars
## numeric_vars <- beverage_df %>%
##   select(where(is.numeric)) %>%
##   names()
## 
## # Choose numeric variables, remove PH
## numeric_vars <- beverage_df %>%
##   select(where(is.numeric)) %>%
##   select(-PH) %>%
##   names()
## 
## #Pivot longer for faceting
## beverage_long <- beverage_df %>%
##   pivot_longer(cols = all_of(numeric_vars), names_to = "variable", values_to = "value")
## 
## #All scatter plots faceted by variable
## ggplot(beverage_long, aes(x = PH, y = value)) +
##   geom_point(alpha = 0.6, color = "blue") +
##   facet_wrap(~ variable, scales = "free_y") +
##   theme_minimal() +
##   labs(y = "Value", x = "PH", title = "Relationship between Variables and PH")
## 
## 
## ## ----corr-----------------------------------------------------------------------------------------------------------------------------------------------------------------
## tst <- beverage_df %>%
##   select(where(is.numeric))
## #Correlation with PH
## cor_table <- cor(drop_na(tst))[, "PH"] %>%
##   as.data.frame() %>%
##   rownames_to_column(var = "Variable") %>%
##   rename(Coefficient = ".") %>%
##   arrange(desc(Coefficient))
## 
## kable(cor_table, "html", escape = F, col.names = c('Variable', 'Coefficient')) %>%
##   kable_styling("striped", full_width = F) %>%
##   column_spec(1, bold = T) %>%
##   add_header_above(c("Table 6: Correlation with PH" = 2))
## 
## #Corr matrix
## rcore <- rcorr(as.matrix(tst))
## coeff <- rcore$r
## #Filter to include only |r| > 0.1, the rest are 0
## filtered_coeff <- coeff
## filtered_coeff[abs(filtered_coeff) < 0.1] <- 0
## coeff <- rcore$r
## corrplot(filtered_coeff, tl.cex = .6, tl.col="black", method = 'color', addCoef.col = "black",
##          type="upper", order="hclust", number.cex=0.45, diag=FALSE)
## 
## 
## ## ----colnames_zeroVar-----------------------------------------------------------------------------------------------------------------------------------------------------
## #Fix column names
## colnames(beverage_df) <- make_clean_names(colnames(beverage_df))
## #Apply the same to eval_df
## colnames(eval_df) <- make_clean_names(colnames(eval_df))
## #Check new column names
## colnames(beverage_df)
## 
## # Identify zero-variance predictors
## zero_var <- nearZeroVar(beverage_df, saveMetrics = TRUE)
## print(zero_var[zero_var$nzv, ])
## 
## beverage_df <- beverage_df[, !zero_var$nzv]
## eval_df <- eval_df[, !zero_var$nzv]
## 
## 
## ## ----impute_numeric-------------------------------------------------------------------------------------------------------------------------------------------------------
## set.seed(547)
## 
## #KNN imputation
## beverage_imputed <- as.data.frame(beverage_df)
## beverage_imputed <- knnImputation(beverage_imputed[, !names(beverage_imputed) %in% c("brand_code", "mnf_flow")])
## beverage_imputed$brand_code <- beverage_df$brand_code
## beverage_imputed$mnf_flow <- beverage_df$mnf_flow
## 
## #KNN imputation mnf_flow
## beverage_imputed$mnf_flow[beverage_imputed$mnf_flow == -100] <- NA
## beverage_imputed <-  knnImputation(beverage_imputed[, !names(beverage_imputed) %in% c("brand_code")], k = 5, scale = TRUE, meth = 'median')
## beverage_imputed$brand_code <- beverage_df$brand_code
## 
## eval_imputed <- as.data.frame(eval_df)
## eval_imputed <- knnImputation(eval_imputed[, !names(eval_imputed) %in% c("brand_code", "ph")])
## eval_imputed$brand_code <- eval_df$brand_code
## eval_imputed$ph <- eval_df$ph
## 
## 
## ## ----brand_Code_impute----------------------------------------------------------------------------------------------------------------------------------------------------
## set.seed(547)
## impute_rf <- function(data, target_var, exclude_vars = NULL) {
##   #Ensure target_var is a factor
##   data[[target_var]] <- factor(data[[target_var]])
## 
##   #Exclude specified variables from the model input
##   if (!is.null(exclude_vars)) {
##     model_data <- data[, !(names(data) %in% exclude_vars)]
##   } else {
##     model_data <- data
##   }
## 
##   #Train model on non-NA data for the target variable
##   rf_model <- randomForest(reformulate(".", target_var), data = model_data[!is.na(model_data[[target_var]]),], na.action = na.omit)
## 
##   #Predict NAs
##   na_indices <- is.na(data[[target_var]])
##   if (sum(na_indices) > 0) {
##     predicted_values <- predict(rf_model, newdata = model_data[na_indices,])
##     data[[target_var]][na_indices] <- predicted_values
##   }
## 
##   return(data)
## }
## 
## beverage_imputed <- impute_rf(beverage_imputed, "brand_code")
## eval_imputed <- impute_rf(eval_imputed, "brand_code", exclude_vars = "ph")
## 
## #Verify
## colSums(is.na(beverage_imputed))
## colSums(is.na(eval_imputed))
## 
## 
## ## ----encode_categorical---------------------------------------------------------------------------------------------------------------------------------------------------
## #One-hot Encoding for Brand Code
## beverage_imputed$brand_code <- as.factor(beverage_imputed$brand_code)
## eval_imputed$brand_code <- as.factor(eval_imputed$brand_code)
## #Create dummies
## dummy_vars <- dummyVars(~ brand_code, data = beverage_imputed, fullRank = TRUE)
## beverage_imputed <- cbind(beverage_imputed, predict(dummy_vars, newdata = beverage_imputed))
## beverage_imputed <- beverage_imputed %>% select(-brand_code)
## 
## eval_imputed <- cbind(eval_imputed, predict(dummy_vars, newdata = eval_imputed))
## eval_imputed <- eval_imputed %>% select(-brand_code)
## 
## 
## ## ----new_features---------------------------------------------------------------------------------------------------------------------------------------------------------
## #Creating Interaction Terms
## beverage_revised <- beverage_imputed %>%
##   select(-contains('_interaction'), -contains('composite'), -contains('efficiency'), -contains('index')) %>%
##   mutate(
##     setpoint_diff = bowl_setpoint - pressure_setpoint,  #feature engineering
##     carb_volume2 = carb_volume^2,  #Polynomial feature for carb_volume
##     pc_volume_pressure = pc_volume * carb_pressure  #Interaction feature
##   )
## 
## eval_revised <- eval_imputed %>%
##   select(-contains('_interaction'), -contains('composite'), -contains('efficiency'), -contains('index')) %>%
##   mutate(
##     setpoint_diff = bowl_setpoint - pressure_setpoint,  #feature engineering
##     carb_volume2 = carb_volume^2,  #Polynomial feature for carb_volume
##     pc_volume_pressure = pc_volume * carb_pressure  #Interaction feature
##   )
## 
## #Remove original columns to reduce collinearity
## beverage_revised <- beverage_revised %>%
##   select(-c(bowl_setpoint, pressure_setpoint, carb_volume, pc_volume, carb_pressure))
## eval_revised <- eval_revised %>%
##   select(-c(bowl_setpoint, pressure_setpoint, carb_volume, pc_volume, carb_pressure))
## 
## #Creating a binned feature for carb_temp
## beverage_revised$carb_temp_binned <- cut(beverage_revised$carb_temp,
##                                          breaks=quantile(beverage_revised$carb_temp, probs=seq(0, 1, 0.25)),
##                                          include.lowest=TRUE,
##                                          labels=FALSE)
## 
## eval_revised$carb_temp_binned <- cut(eval_revised$carb_temp,
##                                          breaks=quantile(eval_revised$carb_temp, probs=seq(0, 1, 0.25)),
##                                          include.lowest=TRUE,
##                                          labels=FALSE)
## 
## #Remove original carb_temp if the binned version is preferred
## beverage_revised <- beverage_revised %>%
##   select(-carb_temp)
## 
## eval_revised <- eval_revised %>%
##   select(-carb_temp)
## 
## 
## ## ----transform_vars-------------------------------------------------------------------------------------------------------------------------------------------------------
## #Apply transformations
## set.seed(123)
## # Log transformations
## beverage_transformed <- beverage_imputed
## beverage_transformed$filler_speed <- log(beverage_transformed$filler_speed)
## beverage_transformed$mfr <- log(beverage_transformed$mfr)
## 
## #Square root transformations
## beverage_transformed$temperature <- sqrt(beverage_transformed$temperature)
## beverage_transformed$air_pressurer <- sqrt(beverage_transformed$air_pressurer)
## 
## #Shifted log transformations
## beverage_transformed$oxygen_filler <- log(beverage_transformed$oxygen_filler + 1)
## beverage_transformed$psc_co2 <- log(beverage_transformed$psc_co2 + 1)
## 
## #Log transformations
## eval_transformed <- eval_imputed
## eval_transformed$filler_speed <- log(eval_imputed$filler_speed)
## eval_transformed$mfr <- log(eval_imputed$mfr)
## 
## #Square root transformations
## eval_transformed$temperature <- sqrt(eval_transformed$temperature)
## eval_transformed$air_pressurer <- sqrt(eval_transformed$air_pressurer)
## 
## #Shifted log transformations
## eval_transformed$oxygen_filler <- log(eval_transformed$oxygen_filler + 1)
## eval_transformed$psc_co2 <- log(eval_transformed$psc_co2 + 1)
## 
## 
## ## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## #Summary statistics
## DT::datatable(
##       dfSummary(beverage_revised, text.graph.col = FALSE, graph.col = FALSE, style = "grid", valid.col = FALSE),
##       extensions = c('Scroller'),
##       options = list(scrollY = 350,
##                      scrollX = 500,
##                      deferRender = TRUE,
##                      scroller = TRUE,
##                      dom = 'lBfrtip',
##                      fixedColumns = TRUE,
##                      searching = FALSE),
##       rownames = FALSE,
##       caption = htmltools::tags$caption(
##     style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
##     'Table 7: Summary Statistics for Transformed Beverage Data'
##   ))
## 
## 
## #Summary statistics
## DT::datatable(
##       dfSummary(eval_revised, text.graph.col = FALSE, graph.col = FALSE, style = "grid", valid.col = FALSE),
##       extensions = c('Scroller'),
##       options = list(scrollY = 350,
##                      scrollX = 500,
##                      deferRender = TRUE,
##                      scroller = TRUE,
##                      dom = 'lBfrtip',
##                      fixedColumns = TRUE,
##                      searching = FALSE),
##       rownames = FALSE,
##       caption = htmltools::tags$caption(
##     style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold;',
##     'Table 8: Summary Statistics for Transformed Evaluation Data'
##   ))
## 
## 
## ## ----parallel_core--------------------------------------------------------------------------------------------------------------------------------------------------------
## #Detect the number of available cores
## numCores <- detectCores()
## 
## #Register parallel backend (use numCores - 1 to leave 1 core free for system tasks)
## cl <- makeCluster(numCores - 1)
## registerDoParallel(cl)
## 
## 
## ## ----split_data_revised---------------------------------------------------------------------------------------------------------------------------------------------------
## #Data with new features
## set.seed(123)
## trainIndex <- createDataPartition(beverage_revised$ph, p = 0.8, list = FALSE)
## train_revised <- beverage_revised[trainIndex, ]
## test_revised <- beverage_revised[-trainIndex, ]
## eval_revised <- subset(eval_revised, select = -ph)
## 
## 
## ## ----split_data_transf----------------------------------------------------------------------------------------------------------------------------------------------------
## set.seed(123)
## trainIndex <- createDataPartition(beverage_transformed$ph, p = 0.8, list = FALSE)
## train_transformed <- beverage_transformed[trainIndex, ]
## test_transformed <- beverage_transformed[-trainIndex, ]
## eval_transformed <- subset(eval_transformed, select = -ph)
## #Setup cv
## control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)
## 
## 
## ## ----mars_model-----------------------------------------------------------------------------------------------------------------------------------------------------------
## set.seed(123)
## #Grid with parameters
## marsGrid <- expand.grid(degree = 1:2, nprune = 2:20)
## 
## #Train MARS
## marsModel <- train(ph ~ ., data = train_transformed,
##                    method = "earth",
##                    preProc = c("center", "scale"),
##                    tuneGrid = marsGrid,
##                    trControl = control
## )
## 
## 
## #degree nprune      RMSE   Rsquared       MAE
## # 2     20      0.1269369   0.459168    0.0960094
## mars_tune <- marsModel$results[marsModel$results$nprune == marsModel$bestTune$nprune & marsModel$results$degree == marsModel$bestTune$degree, ]
## kable(mars_tune, "html", caption = "MARS Model Tuning Results") %>%
##   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## 
## #Predict
## marsPred <- predict(marsModel, newdata = test_transformed)
## #RMSE      Rsquared        MAE
## #0.12208106 0.51031566 0.09219265
## mars_results <- postResample(pred = marsPred, obs = test_transformed$ph)
## kable(mars_results, "html", caption = "MARS Model Performance Metrics") %>%
##     kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## 
## 
## ## ----mars_varimpt---------------------------------------------------------------------------------------------------------------------------------------------------------
## importance_mars <- varImp(marsModel, scale = FALSE)
## kable(as.data.frame(importance_mars$importance), "html", escape = FALSE, caption = "MARS Model Variable Importance") %>%
##   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## plot(importance_mars, top = 10, main = "Top 10 predictors, MARS Model")
## 
## 
## ## ----svm_model------------------------------------------------------------------------------------------------------------------------------------------------------------
## set.seed(123)
## #Grid with parameters
## svmGrid <- expand.grid(sigma = c(0.001, 0.01, 0.1), C = c(0.1, 1, 10, 100))
## #Train SVM
## svmModel <- train(ph ~ ., data = train_transformed,
##                   method = "svmRadial",
##                   preProc = c("center", "scale"),
##                   tuneGrid = svmGrid,
##                   trControl = control
##                   )
## 
## #sigma  C      RMSE   Rsquared        MAE
## # 0.01 10  0.1193689  0.5235771   0.08771225
## svm_tune <- svmModel$results[svmModel$results$sigma == svmModel$bestTune$sigma & svmModel$results$C == svmModel$bestTune$C, ]
## kable(svm_tune, "html", caption = "SVM Model Tuning Results") %>%
##   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## 
## #Predict
## svmPred <- predict(svmModel, newdata = test_transformed)
## #RMSE   Rsquared        MAE
## #0.11787203 0.54495184 0.08445738
## svm_results <- postResample(pred = svmPred, obs = test_transformed$ph)
## kable(svm_results, "html", caption = "SVM Model Performance Metrics") %>%
##     kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## 
## 
## ## ----svm_varimpt----------------------------------------------------------------------------------------------------------------------------------------------------------
## importance_svm <- varImp(svmModel, scale = FALSE)
## kable(as.data.frame(importance_svm$importance), "html", escape = FALSE, caption = "MARS Model Variable Importance") %>%
##   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## plot(importance_svm, top = 10, main = "Top 10 predictors, SVM Model")
## 
## 
## ## ----nnet_model-----------------------------------------------------------------------------------------------------------------------------------------------------------
## set.seed(123)
## #Grid with parameters
## nnetGrid <- expand.grid(.decay = c(0, 0.01, 0.1), .size = 1:10, .bag = FALSE)
## 
## #Train avNNet
## nnetModel <- train(ph ~ ., data = train_transformed,
##                    method = "avNNet",
##                    preProc = c("center", "scale"),
##                    tuneGrid = nnetGrid,
##                    trControl = control,
##                    linout = TRUE,
##                    trace = FALSE)
## 
## #decay size   bag      RMSE    Rsquared       MAE
## #  0   10   FALSE   0.1145209   0.5585798 0.0855949
## nnet_tune <- nnetModel$results[nnetModel$results$size == nnetModel$bestTune$size & nnetModel$results$decay == nnetModel$bestTune$decay, ]
## kable(nnet_tune, "html", caption = "avNNet Model Tuning Results") %>%
##   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## 
## #Predict
## nnetPred <- predict(nnetModel, newdata = test_transformed)
## #RMSE        Rsquared        MAE
## #0.11073852 0.59788239 0.08266435
## nnet_results <- postResample(pred = nnetPred, obs = test_transformed$ph)
## kable(nnet_results, "html", caption = "avNNet Model Performance Metrics") %>%
##     kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## 
## 
## ## ----nnet_varimpt---------------------------------------------------------------------------------------------------------------------------------------------------------
## importance_nnet <- varImp(nnetModel, scale = FALSE)
## kable(as.data.frame(importance_nnet$importance), "html", escape = FALSE, caption = "avNNet Model Variable Importance") %>%
##   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## plot(importance_nnet, top = 10, main = "Top 10 predictors, avNNet Model")
## 
## 
## ## ----gbm_model------------------------------------------------------------------------------------------------------------------------------------------------------------
## set.seed(123)
## #Grid with parameters
## gbmGrid <- expand.grid(.n.trees = seq(100, 1000, by = 100), .interaction.depth = seq(1, 7, by = 2), .shrinkage = 0.01, .n.minobsinnode = c(5, 10, 15))
## #Train GBM
## gbm_model <- train(ph ~ ., data = train_revised,
##                    method = "gbm",
##                    preProc = c("center", "scale"),
##                    tuneGrid = gbmGrid,
##                    trControl = control,
##                    verbose = FALSE)
## 
## #shrinkage interaction.depth n.minobsinnode n.trees      RMSE     Rsquared        MAE
## # 0.01                 7             10    1000       0.1101819    0.5955535 0.08304377
## gbm_tune <- gbm_model$results[gbm_model$results$n.trees == gbm_model$bestTune$n.trees & gbm_model$results$interaction.depth == gbm_model$bestTune$interaction.depth & gbm_model$results$n.minobsinnode == gbm_model$bestTune$n.minobsinnode,]
## kable(gbm_tune, "html", caption = "GBM Model Tuning Results") %>%
##   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## 
## #Predict
## gbm_predictions <- predict(gbm_model, newdata = test_revised)
## #      RMSE   Rsquared        MAE
## #0.10548388 0.64448254 0.07928038
## gbm_results <- postResample(pred = gbm_predictions, obs = test_revised$ph)
## kable(gbm_results, "html", caption = "GBM Model Performance Metrics") %>%
##     kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## 
## 
## ## ----gbm_varimpt----------------------------------------------------------------------------------------------------------------------------------------------------------
## importance_gbm <- varImp(gbm_model, scale = FALSE)
## kable(as.data.frame(importance_gbm$importance), "html", escape = FALSE, caption = "GBM Model Variable Importance") %>%
##   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## plot(importance_gbm, top = 10, main = "Top 10 predictors, GBM")
## 
## 
## ## ----rf_model-------------------------------------------------------------------------------------------------------------------------------------------------------------
## set.seed(123)
## #Grid with parameters
## rfGrid <- expand.grid(.mtry = c(2, 4, 6, 8, 10))
## 
## #Train rf
## rf_model <- train(ph ~ ., data = train_revised,
##                   method = "rf",
##                   preProc = c("center", "scale"),
##                   trControl = control,
##                   tuneGrid = rfGrid,
##                   importance = TRUE)
## 
## #mtry      RMSE   Rsquared        MAE
## # 10  0.1007928     0.6741332   0.07271081
## rf_tune <- rf_model$results[rf_model$results$mtry == rf_model$bestTune$mtry, ]
## kable(rf_tune, "html", caption = "RF Model Tuning Results") %>%
##   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## 
## #Predict
## rf_pred <- predict(rf_model, newdata = test_revised)
## #RMSE      Rsquared        MAE
## #0.09630017 0.71303966 0.06989033
## rf_results <- postResample(pred = rf_pred, obs = test_revised$ph)
## kable(rf_results, "html", caption = "RF Model Performance Metrics") %>%
##     kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## 
## # Correct data frame for actual vs predicted values
## results <- data.frame(Actual = test_revised$ph, Predicted = rf_pred)
## # Scatter plot: Actual vs Predicted Values
## ggplot(results, aes(x = Actual, y = y)) +
##     geom_point(color = "blue", alpha = 0.7) +
##     geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
##     labs(title = "Actual vs Predicted pH Values (RF Model)",
##          x = "Actual pH", y = "Predicted pH") +
##     theme_minimal() +
##     theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
## 
## 
## ## ----rf_variimpt----------------------------------------------------------------------------------------------------------------------------------------------------------
## importance_rf <- varImp(rf_model, scale = FALSE)
## kable(as.data.frame(importance_rf$importance), "html", escape = FALSE, caption = "RF Model Variable Importance") %>%
##   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
## plot(importance_rf, top = 10, main = "Top 10 predictors, Random Forest")
## 
## 
## ## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## #Stop parallel once you're done with models
## stopCluster(cl)
## 
## 
## ## ----compare_models-------------------------------------------------------------------------------------------------------------------------------------------------------
## #Create empty df
## results <- data.frame(
##   Model = character(),
##   Resample_RMSE = numeric(),
##   Resample_R2 = numeric(),
##   Resample_MAE = numeric(),
##   Test_RMSE = numeric(),
##   Test_R2 = numeric(),
##   Test_MAE = numeric(),
##   stringsAsFactors = FALSE
## )
## 
## #Fill df with results
## results <- rbind(results, data.frame(Model = "MARS", Resample_RMSE = mars_tune$RMSE, Resample_R2 = mars_tune$Rsquared, Resample_MAE = mars_tune$MAE, Test_RMSE = mars_results[1], Test_R2 = mars_results[2], Test_MAE = mars_results[3]))
## results <- rbind(results, data.frame(Model = "SVM", Resample_RMSE = svm_tune$RMSE, Resample_R2 = svm_tune$Rsquared, Resample_MAE = svm_tune$MAE, Test_RMSE = svm_results[1], Test_R2 = svm_results[2], Test_MAE = svm_results[3]))
## results <- rbind(results, data.frame(Model = "NNet", Resample_RMSE = nnet_tune$RMSE, Resample_R2 = nnet_tune$Rsquared, Resample_MAE = nnet_tune$MAE, Test_RMSE = nnet_results[1], Test_R2 = nnet_results[2], Test_MAE = nnet_results[3]))
## results <- rbind(results, data.frame(Model = "GBM", Resample_RMSE = gbm_tune$RMSE, Resample_R2 = gbm_tune$Rsquared, Resample_MAE = gbm_tune$MAE, Test_RMSE = gbm_results[1], Test_R2 = gbm_results[2], Test_MAE = gbm_results[3]))
## results <- rbind(results, data.frame(Model = "RF", Resample_RMSE = rf_tune$RMSE, Resample_R2 = rf_tune$Rsquared, Resample_MAE = rf_tune$MAE, Test_RMSE = rf_results[1], Test_R2 = rf_results[2], Test_MAE = rf_results[3]))
## row.names(results) <- NULL
## 
## kable(results, "html", escape = FALSE, align = c('l', rep('c', 6)), col.names = c("Model", "Resample RMSE", "Resample R²", "Resample MAE", "Test RMSE", "Test R²", "Test MAE")) %>%
##   kable_styling("striped", full_width = FALSE) %>%
##   column_spec(1, bold = TRUE) %>%
##   add_header_above(c(" " = 1, "Resample Metrics" = 3, "Test Metrics" = 3),
##                    bold = TRUE, background = "#D3D3D3", color = "black")
## 
## 
## ## ----save_pred------------------------------------------------------------------------------------------------------------------------------------------------------------
## #Predictions for the evaluation dataset
## eval_pred <- predict(rf_model, newdata = eval_revised)
## 
## #Create df
## eval_results <- data.frame(
##   ph_pred = eval_pred
## )
## 
## eval_revised_pred <- eval_revised
## eval_revised_pred$ph_pred <- eval_pred
## 
## #Save to Excel
## write_xlsx(eval_revised_pred, "eval_revised_pred.xlsx")
## write_xlsx(eval_results, "ph_predictions.xlsx")
## 
## par(mfrow = c(1, 2))
## hist(beverage_revised$ph, main = "Original Data Distribution", xlab = "pH", col = "skyblue", border = "white")
## hist(eval_pred, main = "Predicted pH Distribution", xlab = "pH", col = "lightgreen", border = "white")
## par(mfrow = c(1, 1))
## 

