#Recall commands
#------------------------------
library('tidyverse')
#(ggplot, tibble, tidyr, dplyr)
library(nycflights13)
library(ggcorrplot)
library(GGally)
library(mclust)
library(summarytools)
library(gridExtra)
library(ggpubr)
library(Rmixmod)
#----------------------------------

#Import, exploration and manipulation Dataset
#-------------------------------------
data<-read.csv('C:\\Users\\bassi\\Desktop\\Wholesale customers data.csv')

str(data)
data$Channel<-as.factor(data$Channel)
data$Region<-as.factor(data$Region)
data<-as_tibble(data)
range(data$Grocery)
#since we are considering sales, the spending volume is very variable. a transformation could be appropriate

#let's remove region, as it is not useful for the purpose of the analysis and it is not a well-constructed variable, since it gives us little information only on two cities that obviously collect only part of the information

data<-data|>
  select(-Region)#(done after having performed the exploratory analyses)

#Descriptive Statistics
#------------------------------------
dfSummary(data)
table_plot <- ggtexttable(statistiche_descrittive, 
                          rows = NULL, 
                          theme = ttheme("light"))

ggarrange(table_plot)
#the calculation of descriptive statistics has not been included to avoid making the code heavier, but they have been saved in statistiche_descrittive. the purpose was only to create the table

#spending volume by channel
spesa_totale_per_canale <- data %>%
  mutate(Totale_Spesa = Fresh + Milk + Grocery + Frozen + Detergents_Paper + Delicassen) %>% 
  group_by(Channel) %>%
  summarise(Totale_Spesa = sum(Totale_Spesa)) 
#---------------------------------

#let's see distribution of quantitative variables

# conversion to long format
data_long <- data %>%
  pivot_longer(cols = c(Fresh, Milk, Grocery, Frozen, Detergents_Paper, Delicassen),
               names_to = "Variabile", values_to = "Valore")

# histograms and density
ggplot(data_long, aes(x = Valore)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  facet_wrap(~Variabile, scales = "free") +
  labs(title = "Distributions of quantitative variables", x = "Value", y = "Density") +
  theme_minimal()
# Boxplot
ggplot(data_long, aes(y = Valore)) +
  geom_boxplot(fill = "pink", color = "black", alpha = 0.7) +
  facet_wrap(~Variabile, scales = "free") +
  labs(title = "Boxplot of quantitative variables", x = "", y = "Value") +
  theme_minimal()

print(boxplot_plot)


#--------------------------------------
#Log transformation
#--------------------------------
data.log <- data %>%
  mutate(
    log.Fresh = log(Fresh + 1),
    log.Milk = log(Milk + 1),
    log.Grocery = log(Grocery + 1),
    log.Frozen = log(Frozen + 1),
    log.Detergents_Paper = log(Detergents_Paper + 1),
    log.Delicassen = log(Delicassen + 1)
  ) %>%
  select(Channel, starts_with("log."))

#visualization of distributions

# log dataset in long format
data_log_long <- data.log %>%
  pivot_longer(cols = starts_with("log."),
               names_to = "Variabile",   
               values_to = "Valore")    

# Hist. with density
ggplot(data_log_long, aes(x = Valore)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  facet_wrap(~Variabile, scales = "free") +
  labs(title = "Distributions of logarithmic variables", x = "Log(Spending)", y = "Density") +
  theme_minimal()

# Boxplot
ggplot(data_log_long, aes(y = Valore)) +
  geom_boxplot(fill = "pink", color = "black", alpha = 0.7) +
  facet_wrap(~Variabile, scales = "free") + 
  labs(title = "Boxplot of logarithmic variables", x = "", y = "Log(Value)") +
  theme_minimal()

#now the variables follow a much more centered distribution, and the boxplots have a reduced number of outliers. it is considered appropriate to continue the analyses on the log dataset.

#in particular, some distributions such as:
#log(Detergents_Paper)
#log(Frozen)
#log(Grocery)
#log(Milk)
#seem to present a bimodal distribution. 
#----------------------------------
#let's analyze the statistics related to qualitative variables
#---------------------------------
# Descriptive statistics for Region
region_summary <- data.log %>%
  count(Region) %>%
  mutate(Proporzione = n / sum(n) * 100)

print(region_summary)

# Descriptive statistics for Channel
channel_summary <- data.log %>%
  count(Channel) %>%
  mutate(Proporzione = n / sum(n) * 100)

print(channel_summary)

#results 
#Region     n     Proportion
#1         77        17.5
#2         47        10.7
#3        316        71.8


#Channel     n    Proportion
#1          298        67.7
#2          142        32.3

cor(data.log[,-1])

# Bar plot with proportions for Region
ggplot(data.log, aes(x = as.factor(Region), y = ..prop.., group = 1)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7, stat = "count") +
  labs(title = "Proportion of regions", x = "Region", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

# Bar plot with proportions for Channel
ggplot(data.log, aes(x = as.factor(Channel), fill = as.factor(Channel))) +
  geom_bar(
    aes(y = ..count.. / sum(..count..)),
    color = "black", 
    alpha = 0.7,
    stat = "count",
    size = 1 
  ) +
  geom_text(
    stat = "count",
    aes(
      y = ..count.. / sum(..count..),
      label = scales::percent(..count.. / sum(..count..), accuracy = 0.1)
    ),
    position = position_stack(vjust = 0.5), 
    size = 4
  ) +
  labs(
    title = "Proportion of Channels",
    x = "Channel",
    y = "Proportion",
    fill = "Channel"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("lightblue", "lightgreen")) + 
  theme_minimal()

#----------------------------------------------------------------
# how the Channel categories are distributed within each Region.

# dataset log in long format with region
data_log_long_region <- data.log %>%
  pivot_longer(cols = starts_with("log."), 
               names_to = "Variabile",      
               values_to = "Valore") %>%  
  mutate(Region = as.factor(data$Region)) 

ggplot(data_log_long_region, aes(x = Region, y = Valore, fill = Region)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  facet_wrap(~Variabile, scales = "free") +  
  labs(x = "Region", y = "Log(Value)") +
  theme_minimal() 

#region is distributed uniformly across all spending variables, let's try with Channel. not useful for the purpose of the analysis

#------------------------------------------------------------------
# Boxplot of spending log(Fresh) by Channel
boxplot_fresh_channel<- ggplot(data.log, aes(x = as.factor(Channel), y = log.Fresh, fill = as.factor(Channel))) +
  geom_boxplot(alpha = 0.7, color = "black") +
  labs(title = "Spending (log(Fresh)) by Channel", x = "Channel", y = "log(Fresh)") +
  theme_minimal()

print(boxplot_fresh_channel)
#also for channel there does not seem to be a difference in the distribution. let's try to compare them all with faceting

# long format
data_long <- data.log %>%
  pivot_longer(
    cols = starts_with("log."), 
    names_to = "Variable",      
    values_to = "Value"   )

ggplot(data_long, aes(x = Value, fill = as.factor(Channel))) +
  geom_density(alpha = 0.7, color = "black") + 
  facet_wrap(~Variable, scales = "free") +    
  labs(
    y = "Density",
    fill = "Channel"
  ) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "lightgreen")) + 
  theme_minimal()

#let's try with the distributions that we have seen to be bimodal
#log(Detergents_Paper)
#log(Grocery)
#log(Milk)
#log(Frozen)

ggplot(data.log, aes(x = as.factor(Channel), y = log.Detergents_Paper, fill = as.factor(Channel))) +
  geom_boxplot(alpha = 0.7, color = "black") +
  labs(title = "Spending (log(Detergents_Paper)) by Channel", x = "Channel", y = "log(Detergents_Paper)") +
  theme_minimal()

#now it is evident that the distributions of log spending for detergents_paper have very distinct patterns depending on the purchase channel. 
ggpairs(data.log[,-1],aes(col=data.log$Channel))

#as we expected, there seem to be different purchasing behaviors depending on the distribution channel, regarding the variables:
#log(Detergents_Paper)
#log(Grocery)
#log(Milk)
#log(Frozen)

#in particular, it seems useful to relate Frozen and Detergent graphically
ggplot(data.log, aes(x = log.Frozen, y = log.Detergents_Paper, color = as.factor(Channel))) +
  geom_point(alpha = 0.7, size = 2) + 
  geom_smooth(method = "lm", se = TRUE, linetype = "solid", size = 1) + 
  scale_color_manual(values = c("1" = "blue", "2" = "red")) + 
  labs(
    title = "Relationship between Frozen and Detergents_Paper",
    subtitle = "With linear regressions for each Channel",
    x = "log(Frozen)",
    y = "log(Detergents_Paper)",
    color = "Channel"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    legend.position = "top"
  )


ggplot(data.log, aes(x = log.Frozen, y = log.Grocery, color = as.factor(Channel))) +
  geom_point(alpha = 0.7, size = 2) + 
  geom_smooth(method = "lm", se = TRUE, linetype = "solid", size = 1) + 
  scale_color_manual(values = c("1" = "blue", "2" = "red")) + 
  labs(
    title = "Relationship between Frozen and Grocery",
    subtitle = "With linear regressions for each Channel",
    x = "log(Frozen)",
    y = "log(Grocery)",
    color = "Channel"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    legend.position = "top"
  )

#first approach to variable selection:

#methods  
library(rpart)
tree <- rpart(Channel ~ log.Milk + log.Grocery + log.Detergents_Paper + log.Fresh + log.Frozen + log.Delicassen, data = data.log)
plot(tree)
text(tree, use.n = TRUE)

library(randomForest)
rf <- randomForest(as.factor(Channel) ~ log.Milk + log.Grocery + log.Detergents_Paper + log.Fresh + log.Frozen + log.Delicassen, data = data.log)
importance(rf) 
varImpPlot(rf)

pca<-princomp(data.log[,-1])
pca$loadings#does not work (it could be due to normalization?)
pca2<-princomp(data[,-1])
pca2$loadings#no


#MODEL-BASED CLUSTER
#----------------------------------------------
#Comparison of clustering results with all variables and with a subset

#I split into data and labels:
data.num<-data.log|>
  select(where(is.numeric))

true.label <- unlist (data.log [1])
#

#first we do not add any parameter inside the function
Mclust <- Mclust(data.num)
summary(Mclust)
#Mclust VVE (ellipsoidal, equal orientation) model with 5 components
#  BIC            ICL
#-7609.118     -7736.55

#Clustering table:
#1   2   3   4   5 
#45 102 130 106  57

#let's try to do it with ICL as selection criterion (more complete and gives an index of goodness also for the goodness of clustering, it does not consider only the fit on the data like BIC)

mclustICL <- mclustICL (data.num)
#let's see the first three results for both:
Mclust$BIC #BIC based. VVE,5 VVE,3 VVE,6
mclustICL #ICL based  VVE,3     VVE,5     VVV,3 

#considering ICL, we get closer to the model we are looking for
#ICL proposes in first place VVE,3 with ICL=-7728.688

#now let's try forcing the number of classes for the purposes of our analysis.

mICL2<-mclustICL(data.num,G=2)

#VEE,2 ICL=-7792.280
#the BIC favors the more complex model
plot(Mclust , what = "classification")

#for our purposes this approach seems useless. 

plot(mclustICL , what = "classification")

#we insert in the clustering model the criteria that emerge as optimal according to ICL2
mclust2<-Mclust(data.num,G=2,modelNames='VEE')
plot(mclust2 , what = "classification")
summary(mclust2)
#in this way the cluster does not seem to work to cluster the channels



mclust3<-Mclust(data.num,G=3,modelNames='VVE')
plot(mclust3 , what = "classification")
summary(mclust3)
#observation
#forcing the clusters to 2, the produced results neither join with the natural label Channel, nor seem functional in general. it divides the observations into a "big group" with almost all the u.s. (362 out of 440, as much as 82%) and puts the more distant observations in another group. #it is evidently not functional. 
#instead, considering mclust3, with G=3, VVE, we get much closer to the natural labels (Horeca and Retail), and the aggregation of the groups seems much more sensible at a graphical level. an evident problem remains with the observations that deviate the most from the others, even if this could mean the existence of a further subclass of channel that distinguishes the observations, which would seem to be particularly influenced by the spending of Fresh (fresh products), where it seems to be distinct. 



#comparison of predicted labels with the true labels

pred.label.3<-as.factor(mclust3$classification)
pred.label.2<-as.factor(mclust2$classification)
#str(true.label)

#plot(s) to save
par(mfrow = c(1, 3)) 
data.log.frame<-as.data.frame(data.log)
# True classification
coordProj(data.log.frame, dimens = c(2, 4),
          what = "classification",
          classification = true.label,
          col = c("green", "purple"), 
          symbols = c(16,17), 
          sub = "True Classification")

# Predicted classification (3 clusters), VVE,3 model
coordProj(data.log.frame, dimens = c(2, 4), 
          what = "classification",
          classification = pred.label.3,
          col = c("red2", "purple", "green"), 
          symbols = c(10,17,16),
          sub = "M-B Clustering, VVE,3")


# Predicted classification (2 clusters), VEE,2 model
coordProj(data.log.frame, dimens = c(2, 4), 
          what = "classification",
          classification = pred.label.2,
          col = c("green", "purple"), 
          symbols = c(16,17),
          sub = "M-B Clustering, VEE,2")


par(mfrow = c(1, 1)) 

#let's see the goodness of the cluster

#we consider mclust3 as the best one.

#uncertainty
plot(mclust3, what = "uncertainty")
#high level of uncertainty for almost all the observations. 


Inc<-ggplot(data.log, aes(x = log.Fresh, y = log.Grocery)) +
  geom_point(aes(color = Uncertainty, size = Uncertainty), alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_size(range = c(1, 5)) +                 
  labs(       x = "log(Fresh)",
              y = "log(Grocery)",
              color = "Uncertainty",
              size = "Uncertainty") +
  theme_minimal() +
  theme(legend.position = "right")


#CER 3
cer<-classError(pred.label.3, class=true.label)
#$errorRate 0.2840909. #28% was not classified correctly. 
missclass.lab<-cer$misclassified 
str(missclass.lab)#125 out of 440 misclassified. 
#consequently Accuracy = 1-CER= 1- 0.2840909 =0.7159091 

#ARI 3
adjustedRandIndex (pred.label.3, true.label)
#0.359895, to comment

#confusion matrix
#in this case we cannot calculate it since we are comparing two models with different k. 

#CER 2
classError(pred.label.2, class=true.label)#0.6045455
#ARI 2
adjustedRandIndex (pred.label.2, true.label)#0.01036691
#CONFUSION MATRIX 


#final comment on the goodness of the cluster.



# ClustVarSel (variable selection for clustering)
library(clustvarsel)
selected_vars <- clustvarsel(data.log[, c("log.Milk", "log.Grocery", "log.Detergents_Paper", "log.Fresh", "log.Frozen", "log.Delicassen")])

print(selected_vars$subset)
summary(selected_vars)
#search greedy, direction forward, model VVE, 
plot(selected_vars)

#clustvarsel recommends removing the Frozen variable. let's try to do the clustering without it.
#NOTE: the models cannot be compared with ICL since they do not have the same structure in terms of variables. 

mclust.no.frozen<-Mclust(data.num[,-4])
summary(mclust.no.frozen)
#VvE,3. BIC=-6212.487, ICL= -6317.685

mclust.no.frozen.ICL<-mclustICL(data.num[,-4])
#VVV,3 with ICL=-6309.799

mclust.no.frozen.3<-Mclust(data.num[,-4],G=3,modelNames='VVV')
summary(mclust.no.frozen.3) #BIC=-6226.906


par(mfrow = c(1, 2)) 
pred.label.3.no.frozen<-as.factor(mclust.no.frozen.3$classification)
# True classification
coordProj(data.log.frame, dimens = c(2, 4),
          what = "classification",
          classification = true.label,
          col = c("green", "purple"), 
          symbols = c(16,17), 
          sub = "True Classification") 

# Predicted classification (3 clusters), VVV,3 model, no Frozen
coordProj(data.log.frame, dimens = c(2, 4), 
          what = "classification",
          classification = pred.label.3.no.frozen,
          col = c("purple", "red2", "green"), 
          symbols = c(17,10,16),
          sub = "M-B Clustering no Frozen, VVE,3") 
par(mfrow = c(1, 1)) 
#more or less it gives us the same results as the model with Frozen, let's try to see some index to compare them


#uncertainty
plot(mclust.no.frozen.3, what = "uncertainty")
#high level of uncertainty for almost all the observations. #as before

#CER
cer.no.frozen<-classError(pred.label.3.no.frozen, class=true.label)
#$errorRate 0.2113636 #21% was not classified correctly. it seems to have improved compared to the model with Frozen
missclass.lab.3.no.frozen<-cer.no.frozen$misclassified 
str(missclass.lab.3.no.frozen)#93 out of 440 misclassified. 
#consequently Accuracy = 1-CER= 1- 0.2113636 =0.7886364. quite good 

#ARI
adjustedRandIndex (pred.label.3.no.frozen, true.label)
#0.4792814, to comment, also ARI improved clearly.

#we could consider not keeping Frozen in the classification to simplify. 

#let's try to remove all the variables that do not have difference between channels (Fresh, Frozen, Delicassen)

mclust.iii<-Mclust(data.num[,-c(1,4,6)])
summary(mclust.iii)
#VVV,3. BIC=-3328.653, ICL= -3414.547

mclust.iii.ICL<-mclustICL(data.num[,-c(1,4,6)])
#VVV,3 with ICL=-3414.547, same for ICL
par(mfrow=c(1,3))
plot(mclust.iii,what='BIC')
plot(mclust.iii,what='classification')

pred.label.iii<-as.factor(mclust.iii$classification)
par(mfrow = c(1, 2)) 
# True classification
coordProj(data.log.frame, dimens = c(2, 4),
          what = "classification",
          classification = true.label,
          col = c("green", "purple"), 
          symbols = c(16,17), 
          sub = "True Classification") 

# Predicted classification (3 clusters), VVV,3 model, no Frozen
coordProj(data.log.frame, dimens = c(2, 4), 
          what = "classification",
          classification = pred.label.iii,
          col = c("purple", "green", "red2"), 
          symbols = c(17,16,20),
          sub = "M-B Clustering no Frozen, VVE,3") 
par(mfrow = c(1, 1)) 

#it is evident that by removing the variables Frozen, Fresh and Delicassen, the model fits much better. 

#uncertainty
plot(mclust.iii, what = "uncertainty")
#uncertainty remains high for each u.s.

#CER, proportion of misclassified observations
cer.iii<-classError(pred.label.iii, class=true.label)
#$errorRate 0.1545455 #15% was not classified correctly. it seems to have further improved 
missclass.lab.iii<-cer.iii$misclassified 
str(missclass.lab.iii)#68 out of 440 misclassified. 
#consequently Accuracy = 1-CER= 1- 0.1545455 =0.8454545 very good.  

#ARI, measure of closeness between partitions
adjustedRandIndex (pred.label.iii, true.label)
#0.5470469, to comment, also ARI improved clearly.

#uncertainty already tells us that surely the goodness of clustering is not the best. let's try to evaluate other goodness indices

KL_S<-function(mu1,mu2,sigma1,sigma2) #means and variances of the two groups, you find them in the mclust parameters
  t(mu1-mu2)%*%(solve(sigma1)+solve(sigma2))%*%(mu1-mu2)/2+
  sum(diag(sigma1%*%solve(sigma2)+solve(sigma1)%*%sigma2))/2-length(mu1)

mclust.iii$parameters

mu1<-mclust.iii$parameters$mean[,1]
mu2<-mclust.iii$parameters$mean[,2]
mu3<-mclust.iii$parameters$mean[,3]

sigma1<-mclust.iii$parameters$variance$sigma[,,1]
sigma2<-mclust.iii$parameters$variance$sigma[,,2]
sigma3<-mclust.iii$parameters$variance$sigma[,,3]

#KLs between cluster 1 and cluster 2
KL_S(mu1,mu2,sigma1,sigma2)#17.75809

#KLs between cluster 1 and cluster 3
KL_S(mu1,mu3,sigma1,sigma3)#52.78115

#KLs between cluster 2 and cluster 3
KL_S(mu2,mu3,sigma2,sigma3)#6.339742


#following these analyses, it is decided to build the classifier excluding the variables. 

#------------------------------------------------
#CLASSIFICATION

#first of all, we split the dataset into train set and test set, we remove the variables that we excluded. 
set.seed(888)
train <- data.log %>% sample_frac(0.7,replace=F) #(70%)
test <- anti_join(data.log, train)#30%

true.channel<-unlist(test[1])

train<-train|> select(-log.Fresh,-log.Frozen,-log.Delicassen)
test<-test|>select(-log.Fresh,-log.Frozen,-log.Delicassen,,-Channel)#we also remove Channel

channel <- unlist (train [1])
data.train<-train[,-1]


#training the model, with EDDA models
set.seed(888)
n_iterations <- 20 
results <- list()

for (i in 1:n_iterations) {
  model <- mixmodLearn(
    data.train,
    channel, 
    models = mixmodGaussianModel(family = "all", equal.proportions = FALSE),
    criterion = c("CV", "BIC")
  )
  
  
  cv_value <- model@results[[1]]@criterionValue[1] 
  
  #save model and cv value
  results[[i]] <- list(
    model = model,
    CV = cv_value 
  )
  
  # execution check
  cat("Iteration:", i, "- CV:", cv_value, "\n")
}

#we identify the best model
best_model_index <- which.min(sapply(results, function(x) x$CV))
best_model <- results[[best_model_index]]$model 


cat("Iteration with the best model:", best_model_index, "\n")
cat("Lowest CV value:", results[[best_model_index]]$CV, "\n")

summary(best_model)

#we implement a nested loop to evaluate more v-subsets for cross validation, in order to obtain the most efficient model assessment possible. 
#never run it again it takes a lifetime!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! but it works
set.seed(999)
n <- nrow(data.train) 
n_iterations <- 10     # number of iterations for each v
results <- list()    

for (v in n:2) {
  cat("Trying nbCVBlocks =", v, "\n")
  #mixmodLearn for n_iterations
  for (i in 1:n_iterations) {
    #  mixmodLearn with the current value of v
    model <- tryCatch(
      {
        mixmodLearn(
          data.train,                      
          channel,                        
          models = mixmodGaussianModel(family = "all", equal.proportions = FALSE),
          criterion = c("CV", "BIC"),    
          nbCVBlocks = v                  
        )
      },
      error = function(e) {
        cat("Error with nbCVBlocks =", v, "iteration =", i, "\n", e$message, "\n")
        return(NULL)
      }
    )
    
    if (!is.null(model)) {
      
      cv_value <- model@results[[1]]@criterionValue[1]
      
      
      results[[paste0("v", v, "_iter", i)]] <- list(
        nbCVBlocks = v, 
        iteration = i,  
        CV = cv_value,   
        model = model   
      )
      
      cat("nbCVBlocks =", v, "- Iteration:", i, "- CV:", cv_value, "\n")
    }
  }
}
#never run it again it takes a lifetime
#  model with the lowest CV
best_result <- results[[which.min(sapply(results, function(x) x$CV))]]

best_nbCVBlocks <- best_result$nbCVBlocks
best_iteration <- best_result$iteration
best_cv_value <- best_result$CV
best_model <- best_result$model

cat("\nBest model:\n")
cat("nbCVBlocks:", best_nbCVBlocks, "\n")
cat("Iteration:", best_iteration, "\n")
cat("CV value:", best_cv_value, "\n")

summary(best_model)
#the best model seems to be
#Gaussian_pk_L_C
#Criterion =  CV(0.0584) BIC(2679.4989)
#with the lowest MER estimate. the optimal v turned out to be 36. 

#p vector to understand whether the clusters are well balanced or whether there are particularly predominant or minority clusters.
###################
#group 1
##################
#Proportion =  0.6916 
#Means =  7.6483 7.9089 5.9263 
#################
#group 2
################
#Proportion =  0.3084 
#Means =  9.0611 9.5777 8.6752 

#mean to describe the profiles of customers or observations within each group.


#PREDICTION on the test set, with the best model
test<-as.data.frame(test)

prediction <- mixmodPredict(test, classificationRule=best_model@bestResult)

# Prediction results
predicted_clusters_test <- prediction@partition
head(predicted_clusters_test)


#true.channel, true labels of the test
true.channel<-as.factor(true.channel)
predicted_clusters_test<-as.factor(predicted_clusters_test)
#CER
cc<-classError(predicted_clusters_test, class=true.channel)
#$errorRate 0.1136364 #11% was not classified correctly.
missclass<-cc$misclassified

#ARI, measure of closeness between partitions
adjustedRandIndex (predicted_clusters_test, true.channel)#0.5914

#confusion Matrix
confusionMatrix(predicted_clusters_test,true.channel)

#the classification works well.

#PLOT true vs predicted with missclasse level curves

#True classification (True Labels)
plot_true <- ggplot(test, aes(x = log.Detergents_Paper, y = log.Grocery, color = true.channel)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "True classification (True Labels)",
       x = "log(Detergents_Paper)",
       y = "log(Grocery)",
       color = "True Labels") +
  theme_minimal() +
  theme(legend.position = "right")

# Predicted Clusters with missclassified
plot_predicted <- ggplot(test, aes(x = log.Detergents_Paper, y = log.Grocery)) +
  geom_point(aes(color = predicted_clusters_test), size = 2, alpha = 0.7) +
  geom_point(data = test[missclass, ], aes(x = log.Detergents_Paper, y = log.Grocery), 
             color = "red", size = 2, shape = 20) + 
  labs(title = "Predicted Clusters with Missclassified",
       x = "log(Detergents_Paper)",
       y = "log(Grocery)",
       color = "Predicted Clusters") +
  theme_minimal() +
  theme(legend.position = "right")

uncertainty <- 1 - apply(prediction@proba, 1, max)


#  Classification uncertainty
plot_uncertainty <- ggplot(test, aes(x = log.Detergents_Paper, y = log.Grocery)) +
  geom_point(aes(color = uncertainty), size = 2, alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Uncertainty on the variables",
       x = "log(Detergents_Paper)",
       y = "log(Grocery)",
       color = "Uncertainty") +
  theme_minimal() +
  theme(legend.position = "right")


grid.arrange(plot_true, plot_predicted, plot_uncertainty, ncol = 3)

#final plot:

plot(best_model)