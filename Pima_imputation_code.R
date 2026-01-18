library(randomForest) 
library(naniar) 
library(mice) 
library(ggplot2)
library(ggpubr) 
library(rstatix) 
library(Hmisc) 
library(tidyverse)
library(patchwork) 
library(visdat)
library(pROC)

#reading the datest
diabetes <- read.csv("../PRIMA/diabetes_data.csv", header=T)

#initial inspection of the data
head(diabetes) 
str(diabetes) 

#changing class to factor for Outcome variable
diabetes$Outcome<- as.factor(diabetes$Outcome)

#checking for simple statistical summary and number of missing values
summary(diabetes)
na_counts <- colSums(is.na(diabetes)) 
print(na_counts)

#initial exploration of data distribution
hist(diabetes)

#Analysis of the impact of biollogical impossible zero values on the data structure

diabetes_exploration<-diabetes 
diabetes_exploration_total_n <- as.data.frame(colSums(diabetes_exploration[c(2:7)]==0))
colnames(diabetes_exploration_total_n) <-"Count"


number_zero <- ggplot(data=diabetes_exploration_total_n,
                      aes(y=Count,x=rownames(diabetes_exploration_total_n), fill =
                            rownames(diabetes_exploration_total_n))) + 
  geom_bar(stat ="identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  scale_fill_manual(values=c("lightblue", "skyblue4", "deepskyblue4","dodgerblue4", "skyblue3", "skyblue2"))+ 
  scale_y_continuous(limits = c(0, 410))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Visualistion of amout of zeros for numeric variable",
       fill="Variables",x="") 
print(number_zero)

# Replacing zeros in all numeric columns (with exception of pregancy) with NAs

diabetes_imp <- diabetes %>%
  mutate(across(2:8, ~ na_if(.x, 0)))


vis_miss_p<-vis_miss(diabetes_imp, cluster = T, sort_miss = T, show_perc = T, show_perc_col = T)
vis_miss_p +  # move axis below panel
  theme(axis.text.x = element_text(angle = 45, hjust = 0,vjust=0.1),
        plot.margin = margin(t = 1, r = 20, b = 10, l = 5))

#Exploration of missingness patterns and their co-occurence.

gg_miss_upset(diabetes_imp,main.bar.color = "deepskyblue4", sets.bar.color = "deepskyblue4")

# Mcar_test to evaluate if missingnes is or not completly at random 

mcar<-mcar_test(diabetes_imp[2:8])
print(mcar)

#MCAR assumption is rejected;MAR mechanism assesment with logical regression. 

vars <- c("Insulin", "Glucose","BMI","SkinThickness","BloodPressure")

pvals <- sapply(vars, function(v) { 
  preds <- setdiff(c("Insulin","Glucose","BMI","SkinThickness","BloodPressure"), v) 
  f <-as.formula(paste0("is.na(", v, ") ~", paste(preds, collapse = " + ")))
  fit <- glm(f, data = diabetes_imp, family = binomial)
  coef(summary(fit))[-1, 4] })

print(pvals)

# As results are sugessting MAR missingness, multiple imputation using MICE will be performed.As the next step involves predictive modeling, the data will first be split into training and testing sets.

set.seed(123) 
n <- nrow(diabetes_imp) 
train_idx <- sample(seq_len(n),size = 0.7 * n)

training_dataset <- diabetes_imp[train_idx, ]
dim(training_dataset)

testing_dataset <- diabetes_imp[-train_idx, ] 
dim(testing_dataset)


# MICE algorithm imputation and plotting of distibutions of imputed data

imp_train <- mice(training_dataset, m = 20, method = "pmm",seed = 230)
imp_test <- mice(testing_dataset,m = 20,method = "pmm",seed = 233)

bwplot(imp_test)
densityplot(imp_test)

bwplot(imp_train)
densityplot(imp_train)

# Logistic regression and a random forest models generation

fit_glm <- with(imp_train, glm(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, family = binomial))

completed_train <- lapply(1:imp_train$m, function(i) complete(imp_train, i))


glm_preds <- sapply(1:imp_train$m, function(i) {
  predict(
    fit_glm$analyses[[i]],         
    newdata = complete(imp_test, i),type = "response"
  )
})

final_pred_glm <- rowMeans(glm_preds)

rf_preds <- sapply(1:imp_train$m, function(i) {
  train_complete <- complete(imp_train, i) 
  test_complete <- complete(imp_test, i) 
  rf <- randomForest(Outcome ~ Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, data = train_complete, ntree = 500)
  
  predict(rf, newdata = test_complete, type = "prob")[,2]
})

final_pred_rf <- rowMeans(rf_preds)

# Predictive performance evaluation

roc_obj <- roc(testing_dataset$Outcome, final_pred_glm)
auc(roc_obj)
plot(roc_obj, col = "blue", main = "GLM ROC Curve", print.auc = TRUE)

roc_rf <- roc(testing_dataset$Outcome, final_pred_rf)
auc(roc_rf)
plot(roc_rf, col = "blue", main = "RF ROC Curve", print.auc = TRUE)


