library(h2o)
library(dplyr)
library(tidyr)
library(ggplot2)

library(repr)

load("data_ps_days_train.dat")
cat("Press Metadata\n")
data_ps_days_train %>% select(-data) %>% glimpse()


cat("Press Data\n")
(data_ps_days_train %>% select(data))[[1]][[1]] %>% glimpse()



load("data_ps_days_test.dat")
cat("Press Metadata\n")
data_ps_days_train %>% select(-data) %>% glimpse()



cat("Press Data\n")
(data_ps_days_train %>% select(data))[[1]][[1]] %>% glimpse()


seed <- 100
set.seed(seed)

h2o.init(ip = "15.8.169.215", port = 54321, startH2O = F)



training_data <- data_ps_days_train %>% unnest() %>%
  #filter entries where one of these features are 0
  filter(scaled_PIP_Temperature_mean != 0 &
           scaled_IO_temperature_mean != 0 &
           scaled_CS_Voltage_mean != 0 &
           scaled_vessel_flow_mean != 0) %>%
  #select only the scaled values aggregated with 'mean, median, min, max, sd and IQR'
  select(event_press, time_slice, Product_Line, events, days, event_slice_count,
         ends_with("_mean"), ends_with("_median"), ends_with("_min"), ends_with("_max"),
         ends_with("_sd"), ends_with("_IQR"), -contains("interp")) %>%
  select(event_press, time_slice, Product_Line, events, days, event_slice_count, contains("scaled")) %>%
  #remove entries with missing values
  na.omit() 

cat("All observations: ", data_ps_days_train %>% unnest() %>% nrow(), "\n")
training_data %>% glimpse()


training_data %>% group_by(event_press) %>%
  summarise(n_events = n()) %>% 
  summarise(n_presses = n(), n_events = sum(n_events))
training_data %>% group_by(Product_Line, event_press) %>%
  summarise(n_events = n()) %>% 
  summarise(n_presses = n(), n_events = sum(n_events))


test_data <- data_ps_days_test %>% unnest() %>%
  #filter entries where one of these features are 0
  filter(scaled_PIP_Temperature_mean != 0 &
           scaled_IO_temperature_mean != 0 &
           scaled_CS_Voltage_mean != 0 &
           scaled_vessel_flow_mean != 0) %>%
  #select only the scaled values aggregated with 'mean, median, min, max, sd and IQR'
  select(event_press, time_slice, Product_Line, events, days, event_slice_count,
         ends_with("_mean"), ends_with("_median"), ends_with("_min"), ends_with("_max"),
         ends_with("_sd"), ends_with("_IQR"), -contains("interp")) %>%
  select(event_press, time_slice, Product_Line, events, days, event_slice_count, contains("scaled")) %>%
  #remove entries with missing values
  na.omit() 

cat("All observations: ", data_ps_days_test %>% unnest() %>% nrow(), "\n")
test_data %>% glimpse()


test_data %>% group_by(event_press) %>%
  summarise(n_events = n()) %>% 
  summarise(n_presses = n(), n_events = sum(n_events))
test_data %>% group_by(Product_Line, event_press) %>%
  summarise(n_events = n()) %>% 
  summarise(n_presses = n(), n_events = sum(n_events))


h2o_train <- as.h2o(select(training_data, -time_slice))
glimpse(h2o_train)

h2o_test <- as.h2o(select(test_data, -time_slice))
glimpse(h2o_test)


h2o_model <- h2o.deeplearning(
  x = 5:185, 
  training_frame = h2o_train,  # data is automatically normalized by h2o with range(-0.5, 0.5)
  autoencoder = TRUE, 
  hidden = c(30, 10, 30), #c(20, 10, 20),  #c(10, 30, 50, 30, 10), #read documentation to know what it means 
  activation = "Tanh", # default activation for autoencoder
  epochs = 5,
  seed = seed) #Setting a seed value for reproducibility purposes


anom_scores_train <- h2o.anomaly(h2o_model, h2o_train)
anom_scores_test <- h2o.anomaly(h2o_model, h2o_test)
anom_scores_train %>% glimpse()
anom_scores_test %>% glimpse()


anom_scores_per_feature_train <- h2o.anomaly(h2o_model, h2o_train, per_feature = T)
anom_scores_per_feature_test <- h2o.anomaly(h2o_model, h2o_test, per_feature = T)
anom_scores_per_feature_train %>% glimpse()
anom_scores_per_feature_test %>% glimpse()

anom_scores_data_train <- anom_scores_per_feature_train %>% as.data.frame() %>%
  mutate(Reconstruction.MSE = as.vector(anom_scores_train))

anom_scores_data_train %>% glimpse()

anom_scores_data_test <- anom_scores_per_feature_test %>% as.data.frame() %>%
  mutate(Reconstruction.MSE = as.vector(anom_scores_test))

anom_scores_data_test %>% glimpse()


training_anom <- cbind(training_data, anom_scores_data_train) %>%
  arrange(desc(Reconstruction.MSE))

training_anom %>% glimpse()

test_anom <- cbind(test_data, anom_scores_data_test) %>%
  arrange(desc(Reconstruction.MSE))

test_anom %>% glimpse()


options(repr.plot.width=10, repr.plot.height = 3)
training_anom %>% ggplot(aes(x = Reconstruction.MSE, fill = Product_Line)) +
  geom_histogram(bins = 40) +
  ggtitle("Histogram of Anomaly Scores (Training Data)") +
  xlab("Anomaly Score (Reconstruction.MSE)") +
  scale_x_continuous(breaks = seq(0, 1, 0.05), limits = c(0,0.3)) +
  geom_vline(aes(xintercept = max(Reconstruction.MSE)), size = 0.1)


test_anom %>% ggplot(aes(x = Reconstruction.MSE, fill = Product_Line)) +
  geom_histogram(bins = 40) +
  ggtitle("Histogram of Anomaly Scores (Test Data)") +
  xlab("Anomaly Score (Reconstruction.MSE)") +
  scale_x_continuous(breaks = seq(0, 1, 0.05), limits = c(0,0.3)) +
  geom_vline(aes(xintercept = max(Reconstruction.MSE)), size = 0.1)



display_data_train <- training_anom %>% select(event_press, Product_Line, starts_with("reconstr_scaled_")) %>%
  gather(key = Feature, value = Value, -event_press, -Product_Line) %>%
  mutate(Feature = gsub("_mean.SE", ".mean_SE", Feature)) %>%
  mutate(Feature = gsub("_median.SE", ".median_SE", Feature)) %>%
  mutate(Feature = gsub("_min.SE", ".min_SE", Feature)) %>%
  mutate(Feature = gsub("_max.SE", ".max_SE", Feature)) %>%
  mutate(Feature = gsub("_IQR.SE", ".IQR_SE", Feature)) %>%
  mutate(Feature = gsub("_sd.SE", ".sd_SE", Feature)) %>%
  separate(Feature, c("Feature", "Method"), "[[.]]")

display_data_test <- test_anom %>% select(event_press, Product_Line, starts_with("reconstr_scaled_")) %>%
  gather(key = Feature, value = Value, -event_press, -Product_Line) %>%
  mutate(Feature = gsub("_mean.SE", ".mean_SE", Feature)) %>%
  mutate(Feature = gsub("_median.SE", ".median_SE", Feature)) %>%
  mutate(Feature = gsub("_min.SE", ".min_SE", Feature)) %>%
  mutate(Feature = gsub("_max.SE", ".max_SE", Feature)) %>%
  mutate(Feature = gsub("_IQR.SE", ".IQR_SE", Feature)) %>%
  mutate(Feature = gsub("_sd.SE", ".sd_SE", Feature)) %>%
  separate(Feature, c("Feature", "Method"), "[[.]]")


options(repr.plot.width=10, repr.plot.height = 12)
display_data_train %>% filter(Method == "mean_SE" | Method == "median_SE" | Method == "IQR_SE") %>%
  ggplot(aes(x = Value, fill = Product_Line)) +
  geom_histogram(bins = 60) +
  facet_wrap(~Feature + Method, nrow = 5, ncol = 3, scales = "free_x") +
  ggtitle("Histogram of Mean/Median/IQR SE Values per Feature (Training)") +
  xlab("Reconstruction.MSE")

display_data_test %>% filter(Method == "mean_SE" | Method == "median_SE" | Method == "IQR_SE") %>%
  ggplot(aes(x = Value, fill = Product_Line)) +
  geom_histogram(bins = 60) +
  facet_wrap(~Feature + Method, nrow = 5, ncol = 3, scales = "free_x") +
  ggtitle("Histogram of Mean/Median/IQR SE Values per Feature (Test)") +
  xlab("Reconstruction.MSE")


options(repr.plot.width=10, repr.plot.height = 12)
display_data_train %>% filter(Method == "min_SE" | Method == "max_SE" | Method == "sd_SE") %>%
  ggplot(aes(x = Value, fill = Product_Line)) +
  geom_histogram(bins = 60) +
  facet_wrap(~Feature + Method, nrow = 5, ncol = 3, scales = "free_x") +
  ggtitle("Histogram of Min/Max/sd SE Values per Feature (Training)") +
  xlab("Reconstruction.MSE")

display_data_test %>% filter(Method == "min_SE" | Method == "max_SE" | Method == "sd_SE") %>%
  ggplot(aes(x = Value, fill = Product_Line)) +
  geom_histogram(bins = 60) +
  facet_wrap(~Feature + Method, nrow = 5, ncol = 3, scales = "free_x") +
  ggtitle("Histogram of Min/Max/sd SE Values per Feature (Test)") +
  xlab("Reconstruction.MSE")



cat("Training data\n")
training_anom %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom)) %>% 
  summarise(n_presses = n(),
            max_anom = max(max_anom),
            min_anom = min(min_anom),
            range_anom = max_anom - min_anom,
            events = sum(n_events))

training_anom %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(training_anom %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom)) %>% 
  summarise(n_presses = n(),
            max_anom = max(max_anom),
            min_anom = min(min_anom),
            range_anom = max_anom - min_anom,
            events = sum(n_events))
cat("Testing data\n")
test_anom %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom)) %>% 
  summarise(n_presses = n(),
            max_anom = max(max_anom),
            min_anom = min(min_anom),
            range_anom = max_anom - min_anom,
            events = sum(n_events))

test_anom %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(test_anom %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom)) %>% 
  summarise(n_presses = n(),
            max_anom = max(max_anom),
            min_anom = min(min_anom),
            range_anom = max_anom - min_anom,
            events = sum(n_events))



cat("Training data\n")
training_anom %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(Product_Line) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom), desc(n_events))
training_anom %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(training_anom %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(Product_Line) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom), desc(n_events))

cat("Testing data\n")
test_anom %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(Product_Line) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom), desc(n_events))
test_anom %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(test_anom %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(Product_Line) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom), desc(n_events))



training_data_7000 <- training_data %>% filter(Product_Line == "HP Indigo 7000")
training_data_7500 <- training_data %>% filter(Product_Line == "HP Indigo 7500")
training_data_7600 <- training_data %>% filter(Product_Line == "HP Indigo 7600")

test_data_7000 <- test_data %>% filter(Product_Line == "HP Indigo 7000")
test_data_7500 <- test_data %>% filter(Product_Line == "HP Indigo 7500")
test_data_7600 <- test_data %>% filter(Product_Line == "HP Indigo 7600")


h2o_train_7000 <- as.h2o(select(training_data_7000, -time_slice))
h2o_train_7500 <- as.h2o(select(training_data_7500, -time_slice))
h2o_train_7600 <- as.h2o(select(training_data_7600, -time_slice))

h2o_test_7000 <- as.h2o(select(test_data_7000, -time_slice))
h2o_test_7500 <- as.h2o(select(test_data_7500, -time_slice))
h2o_test_7600 <- as.h2o(select(test_data_7600, -time_slice))



h2o_model_7000 <- h2o.deeplearning(
  x = 5:185, 
  training_frame = h2o_train_7000,  # data is automatically normalized by h2o with range(-0.5, 0.5)
  autoencoder = TRUE, 
  hidden = c(30, 10, 30), #c(20, 10, 20),  #c(10, 30, 50, 30, 10), #read documentation to know what it means 
  activation = "Tanh", # default activation for autoencoder
  epochs = 5,
  seed = seed) #Setting a seed value for reproducibility purposes


h2o_model_7500 <- h2o.deeplearning(
  x = 5:185, 
  training_frame = h2o_train_7500,  # data is automatically normalized by h2o with range(-0.5, 0.5)
  autoencoder = TRUE, 
  hidden = c(30, 10, 30), #c(20, 10, 20),  #c(10, 30, 50, 30, 10), #read documentation to know what it means 
  activation = "Tanh", # default activation for autoencoder
  epochs = 5,
  seed = seed) #Setting a seed value for reproducibility purposes



h2o_model_7600 <- h2o.deeplearning(
  x = 5:185, 
  training_frame = h2o_train_7600,  # data is automatically normalized by h2o with range(-0.5, 0.5)
  autoencoder = TRUE, 
  hidden = c(30, 10, 30), #c(20, 10, 20),  #c(10, 30, 50, 30, 10), #read documentation to know what it means 
  activation = "Tanh", # default activation for autoencoder
  epochs = 5,
  seed = seed) #Setting a seed value for reproducibility purposes


anom_scores_train_7000 <- h2o.anomaly(h2o_model_7000, h2o_train_7000)
anom_scores_train_7500 <- h2o.anomaly(h2o_model_7500, h2o_train_7500)
anom_scores_train_7600 <- h2o.anomaly(h2o_model_7600, h2o_train_7600)

anom_scores_test_7000 <- h2o.anomaly(h2o_model_7000, h2o_test_7000)
anom_scores_test_7500 <- h2o.anomaly(h2o_model_7500, h2o_test_7500)
anom_scores_test_7600 <- h2o.anomaly(h2o_model_7600, h2o_test_7600)


anom_scores_per_feature_train_7000 <- h2o.anomaly(h2o_model_7000, h2o_train_7000, per_feature = T)
anom_scores_per_feature_train_7500 <- h2o.anomaly(h2o_model_7500, h2o_train_7500, per_feature = T)
anom_scores_per_feature_train_7600 <- h2o.anomaly(h2o_model_7600, h2o_train_7600, per_feature = T)

anom_scores_per_feature_test_7000 <- h2o.anomaly(h2o_model_7000, h2o_test_7000, per_feature = T)
anom_scores_per_feature_test_7500 <- h2o.anomaly(h2o_model_7500, h2o_test_7500, per_feature = T)
anom_scores_per_feature_test_7600 <- h2o.anomaly(h2o_model_7600, h2o_test_7600, per_feature = T)


anom_scores_data_train_7000 <- anom_scores_per_feature_train_7000 %>% as.data.frame() %>%
  mutate(Reconstruction.MSE = as.vector(anom_scores_train_7000))
anom_scores_data_train_7500 <- anom_scores_per_feature_train_7500 %>% as.data.frame() %>%
  mutate(Reconstruction.MSE = as.vector(anom_scores_train_7500))
anom_scores_data_train_7600 <- anom_scores_per_feature_train_7600 %>% as.data.frame() %>%
  mutate(Reconstruction.MSE = as.vector(anom_scores_train_7600))

anom_scores_data_test_7000 <- anom_scores_per_feature_test_7000 %>% as.data.frame() %>%
  mutate(Reconstruction.MSE = as.vector(anom_scores_test_7000))
anom_scores_data_test_7500 <- anom_scores_per_feature_test_7500 %>% as.data.frame() %>%
  mutate(Reconstruction.MSE = as.vector(anom_scores_test_7500))
anom_scores_data_test_7600 <- anom_scores_per_feature_test_7600 %>% as.data.frame() %>%
  mutate(Reconstruction.MSE = as.vector(anom_scores_test_7600))


training_anom_7000 <- cbind(training_data_7000, anom_scores_data_train_7000) 
training_anom_7500 <- cbind(training_data_7500, anom_scores_data_train_7500) 
training_anom_7600 <- cbind(training_data_7600, anom_scores_data_train_7600)
training_anom_all <- rbind(training_anom_7000, training_anom_7500, training_anom_7600)

test_anom_7000 <- cbind(test_data_7000, anom_scores_data_test_7000) 
test_anom_7500 <- cbind(test_data_7500, anom_scores_data_test_7500) 
test_anom_7600 <- cbind(test_data_7600, anom_scores_data_test_7600)
test_anom_all <- rbind(test_anom_7000, test_anom_7500, test_anom_7600)


options(repr.plot.width=10, repr.plot.height = 5)
training_anom_all %>% ggplot(aes(x = Reconstruction.MSE, fill = Product_Line)) +
  geom_histogram(bins = 40) +
  facet_wrap(~Product_Line, nrow = 3) +
  ggtitle("Histogram of Anomaly Scores by Product Line") +
  xlab("Anomaly Score (Reconstruction.MSE)")
test_anom_all %>% ggplot(aes(x = Reconstruction.MSE, fill = Product_Line)) +
  geom_histogram(bins = 40) +
  facet_wrap(~Product_Line, nrow = 3) +
  ggtitle("Histogram of Anomaly Scores by Product Line") +
  xlab("Anomaly Score (Reconstruction.MSE)")



cat("Product Line: All\n")
training_anom_all %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))


training_anom_all %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(training_anom_all %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))

cat("Product Line: HP Indigo 7000\n")
training_anom_7000 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))
training_anom_7000 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(training_anom_7000 %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))

cat("Product Line: HP Indigo 7500\n")
training_anom_7500 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))
training_anom_7500 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(training_anom_7500 %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))

cat("Product Line: HP Indigo 7600\n")
training_anom_7600 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))
training_anom_7600 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(training_anom_7600 %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))



cat("Product Line: All\n")
test_anom_all %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))


test_anom_all %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(test_anom_all %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))

cat("Product Line: HP Indigo 7000\n")
test_anom_7000 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))
test_anom_7000 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(test_anom_7000 %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))

cat("Product Line: HP Indigo 7500\n")
test_anom_7500 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))
test_anom_7500 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(test_anom_7500 %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))

cat("Product Line: HP Indigo 7600\n")
test_anom_7600 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))
test_anom_7600 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(test_anom_7600 %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(event_press) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  summarise(n_presses = n(),
            max_anom = round(max(max_anom), 4),
            mean_anom = round(mean(mean_anom), 4),
            min_anom = round(min(min_anom), 4),
            range_anom = round(max_anom - min_anom, 4),
            events = sum(n_events))




cat("All anomalous events by product line\n")
training_anom_all %>% arrange(desc(Reconstruction.MSE)) %>%
  #Grouping by press, summarise the anomaly score
  group_by(Product_Line) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom), desc(n_events))

cat("10% of most anomalous events with product lines combined separated by product line\n")
training_anom_all %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(training_anom_all %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(Product_Line) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom), desc(n_events))

cat("10% of most anomalous events with product lines separated\n")
training_anom_7000 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(training_anom_7000 %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(Product_Line) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom), desc(n_events))


training_anom_7500 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(training_anom_7500 %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(Product_Line) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom), desc(n_events))


training_anom_7600 %>% arrange(desc(Reconstruction.MSE)) %>%
  #Select the 10% more anomalous entries
  head(training_anom_7600 %>% nrow()/10) %>% 
  #Grouping by press, summarise the anomaly score
  group_by(Product_Line) %>% 
  summarise(max_anom = round(max(Reconstruction.MSE), 4),
            min_anom = round(min(Reconstruction.MSE), 4),
            mean_anom = round(mean(Reconstruction.MSE), 4),
            median_anom = round(median(Reconstruction.MSE), 4),
            sd_anom = round(sd(Reconstruction.MSE), 4),
            n_events = n()) %>%
  arrange(desc(max_anom), desc(n_events))


library(clValid)
library(factoextra)

cluster_validation  <- training_data %>%
  select(6:186) %>% as.data.frame() %>%
  clValid(2:10, clMethods = "kmeans", 
          validation = "internal", maxitems = nrow(training_data))

summary(cluster_validation)
optimalScores(cluster_validation)

cluster_validation_test  <- test_data %>%
  select(6:186) %>% as.data.frame() %>%
  clValid(2:10, clMethods = "kmeans", 
          validation = "internal", maxitems = nrow(test_data))

summary(cluster_validation_test)
optimalScores(cluster_validation_test)



