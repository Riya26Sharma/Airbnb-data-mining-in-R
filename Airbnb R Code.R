
#import necessary packages
library(tidyverse)
library(caret)
library(tree)
library(class)
#install.packages('xgbplot')
library(randomForest)
library(data.table)
library(xgbplot)

#install.packages("syuzhet")
library("syuzhet")
library(tidytext)
library(dplyr)
library(ggplot2) # visualization
library('ggthemes') # visualization
library('scales')
library(pROC)
library(ROCR)
library(xgboost)
library(caret)


train_x <- read_csv("airbnb_train_x_2023.csv")
train_y <- read_csv("airbnb_train_y_2023.csv")
test_x <- read_csv("airbnb_test_x_2023.csv")


train_old <- cbind(train_x, train_y) %>%
mutate(perfect_rating_score = as.factor(perfect_rating_score),
        high_booking_rate = as.factor(high_booking_rate)) 



#subset of the data
train <- train_old
train$extra_people <- as.numeric(parse_number(train$extra_people))
train$cleaning_fee <- ifelse(is.na(train$cleaning_fee),0, train$cleaning_fee)
train$price <- ifelse(is.na(train$price),0, train$price)

train <- train %>% 
  mutate_if(is.numeric,~replace_na(.,mean(., na.rm=TRUE)))

train <- mutate(train,
                price_per_person = price/accommodates,
                has_cleaning_fee = ifelse(cleaning_fee >0,TRUE,FALSE),
                bed_category = ifelse(bed_type %in% c("Real Bed") ,"bed","other"),
                property_category = as.factor(case_when(
                  property_type %in% c("Apartment","Serviced apartment","Loft") ~ "apartment",
                  property_type %in% c("Bed & Breakfast","Boutique hotel","Hostel") ~ "hotel",
                  property_type %in% c("Townhouse","Condominium") ~ "condo",
                  property_type %in% c("Bungalow","House") ~ "house",
                  TRUE ~ "other"))
)

##################################### FEATURE ENGINEERING #############################################################
train$apartment <- ifelse(train$property_category == "apartment",1, 0)
train$hotel <- ifelse(train$property_category == "hotel",1, 0)
train$condo <- ifelse(train$property_category == "condo",1, 0)
train$house <- ifelse(train$property_category == "house",1, 0)
train$other <- ifelse(train$property_category == "other",1, 0)
train$realBed <- ifelse(train$bed_category == "bed",1, 0)
train$otherBed <- ifelse(train$bed_category == "other",1, 0)
train$cancellation_strict <- ifelse(train$cancellation_policy == "strict",1, 0)
train$cancellation_moderate <- ifelse(train$cancellation_policy == "moderate",1, 0)
train$cancellation_flexible <- ifelse(train$cancellation_policy == "flexible",1, 0)
train$cancellation_no_refunds <- ifelse(train$cancellation_policy == "no_refunds",1, 0)
train$cancellation_super_strict_30 <- ifelse(train$cancellation_policy == "super_strict_30",1, 0)
train$cancellation_super_strict_60 <- ifelse(train$cancellation_policy == "super_strict_60",1, 0)
train$cancellation_no_policy <- ifelse(is.na(train$cancellation_policy),1, 0)
train$host_acceptance_rate <- ifelse(is.na(train$host_acceptance_rate),0, train$host_acceptance_rate)
train$host_response_rate <- ifelse(is.na(train$host_response_rate),0, train$host_response_rate)

train <- train %>% 
  group_by(property_category) %>%
  mutate(med = median(price_per_person, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ppp_ind = ifelse(price_per_person > med, 1, 0))
train$room_type <- as.factor(train$room_type)
train$ppp_ind <- as.factor(train$ppp_ind)
train$zipcode <- as.numeric(train$zipcode)
train$zipcode <- ifelse(is.na(train$zipcode),0, train$zipcode)
train$host_has_profile_pic <- ifelse(is.na(train$host_has_profile_pic),FALSE, train$host_has_profile_pic)
train$host_identity_verified <- ifelse(is.na(train$host_identity_verified),FALSE, train$host_identity_verified)
train$bathrooms <- ifelse(is.na(train$bathrooms),median(bathrooms), train$bathrooms)
train$host_is_superhost <- ifelse(is.na(train$host_is_superhost),FALSE, train$host_is_superhost)
train$high_booking_rate <- as.factor(train$high_booking_rate)
train$security_deposit <- ifelse(is.na(train$security_deposit),0, train$security_deposit)
train$extra_people <- ifelse(is.na(train$extra_people),0, train$extra_people)
train$has_min_nights <- ifelse(train$minimum_nights > 1,FALSE,TRUE)
train$has_rules = ifelse(is.na(train$house_rules),FALSE,TRUE)
train$easy_transit =ifelse(is.na(train$transit),FALSE,TRUE)
train$notes <- ifelse(is.na(train$notes),FALSE, TRUE)
train$license <- ifelse(is.na(train$notes),0, 1)
train$interaction <- ifelse(is.na(train$interaction),FALSE, TRUE)
train$host_response_time <- ifelse(is.na(train$host_response_time),0, 1)
train$host_about <- ifelse(is.na(train$host_about),FALSE, TRUE)
train$access <- ifelse(is.na(train$host_about),0, 1)
train$high_booking_rate <- as.numeric(train$high_booking_rate) - 1
train$price_per_person <- as.numeric(as.character(train$price_per_person))
train$host_is_superhost <- as.numeric(as.logical(train$host_is_superhost))
train$zipcode <- as.numeric(as.character(train$zipcode))
train$security_deposit <- as.numeric(parse_number(train$security_deposit))
train$Description_Length <- as.numeric(train$Description_Length)
train$instant_bookable <- as.numeric(as.logical(train$instant_bookable))
train$availability_60 <- as.numeric(as.character(train$availability_60))
train$availability_30 <- as.numeric(as.character(train$availability_30))
train$availability_90 <- as.numeric(as.character(train$availability_90))
train$availability_365 <- as.numeric(as.character(train$availability_365))
train$price <- as.numeric(as.character(train$price))
train$cleaning_fee <- as.numeric(as.character(train$cleaning_fee))
train$minimum_nights <- as.numeric(as.character(train$minimum_nights))
train$host_total_listings_count <- as.numeric(as.character(train$host_total_listings_count))
train$maximum_nights <- as.numeric(as.character(train$maximum_nights))
train$instant_bookable <- as.numeric(as.logical(train$instant_bookable))
train$accommodates <- as.numeric(as.character(train$accommodates))
train$has_min_nights <- as.numeric(as.logical(train$has_min_nights))
train$beds <- as.numeric(as.character(train$beds))
train$bedrooms <- as.numeric(as.character(train$bedrooms))
train$host_response_rate <- ifelse(is.na(train$host_response_rate),0, train$host_response_rate)
train$host_acceptance_rate <- ifelse(is.na(train$host_acceptance_rate),0, train$host_acceptance_rate)
train$host_response_rate <- as.numeric(as.character(train$host_response_rate))
train$host_acceptance_rate <- as.numeric(as.character(train$host_acceptance_rate))
train$host_identity_verified <- as.numeric(as.logical(train$host_identity_verified))
train$requires_license <- as.numeric(as.logical(train$requires_license))
train$is_location_exact <- as.numeric(as.logical(train$is_location_exact))
train$ppp_ind <- as.numeric(as.character(train$ppp_ind))
train$host_has_profile_pic <- as.numeric(as.logical(train$host_has_profile_pic))
train$has_cleaning_fee <- as.numeric(as.logical(train$has_cleaning_fee))
train$has_rules <- as.numeric(as.logical(train$has_rules))
train$easy_transit <- as.numeric(as.logical(train$easy_transit))
train$notes  <- as.numeric(as.logical(train$notes))
train$host_about  <- as.numeric(as.logical(train$host_about))
train$interaction  <- as.numeric(as.logical(train$interaction))
train$host_response_rate <- ifelse(is.na(train$host_response_rate),0, train$host_response_rate)
train$host_acceptance_rate <- ifelse(is.na(train$host_acceptance_rate),0, train$host_acceptance_rate)
train$description <- train$description %>% replace_na('No Description')
train <- mutate(train,
                description=ifelse(description %in% c("No Description"),"No description","Description"))
#host_verifications
train$host_verifications <-gsub("[^[:alnum:]\\-\\.\\s]", " ", train$host_verifications)
train$total_verifications <- str_count(train$host_verifications, '\\s+')+1
#Amenities
train$amenities <-gsub("[^[:alnum:]\\-\\.\\s]", " ", train$amenities)
train$total_amenities <- str_count(train$amenities, '\\s+')+1

#FINDING SENTIMENT SCORE FOR DESCRIPTION

train$Description_Length <- NA
train$Sentiment_intensity <- NA

for (i in 1:nrow(train)) {
  s <- train$description[i]
  
  ## Generating sentiment intensity score for descriptions
  sentiment <- get_sentiments("afinn") %>%
    inner_join(tibble(word = unlist(strsplit(s, "\\s+")), .name_repair = "unique"), by = "word")
  
  sentiment_score <- sum(sentiment$value)
  num_words <- length(strsplit(s, " ")[[1]])
  
  if (num_words > 0) {
    normalized_score <- sentiment_score / num_words
  } else {
    normalized_score <- 0
  }
  
  train$Sentiment_intensity[i] <- normalized_score
  
  ## Finding the length of the description
  train$Description_Length[i] <- num_words
}

train <- train %>% 
  select(accommodates,bedrooms,beds,has_cleaning_fee,host_total_listings_count,
         price,ppp_ind,bathrooms,extra_people,host_acceptance_rate,
         host_response_rate,has_min_nights,host_is_superhost, cleaning_fee,
         price_per_person, maximum_nights, minimum_nights, availability_30, availability_60, availability_90,
         availability_365,zipcode,instant_bookable,is_location_exact,requires_license,license,host_has_profile_pic,
         host_identity_verified,security_deposit ,has_rules,easy_transit,notes,interaction,
         host_response_time,host_about, apartment, hotel, condo, house, other, realBed, otherBed, high_booking_rate,access,Sentiment_intensity, Description_Length,
         cancellation_strict,cancellation_moderate,cancellation_flexible,cancellation_no_refunds,cancellation_super_strict_30,cancellation_super_strict_60, cancellation_no_policy, total_verifications, total_amenities)


#set.seed(12)

# partition the features into training and validation rows
valid_instn = sample(nrow(train), 0.30*nrow(train))
train_new <- train[-valid_instn,]
valid_new <- train[valid_instn,]



######################### Logistic Regression ######################### 

model1 <- glm(high_booking_rate~.+host_is_superhost:host_acceptance_rate,data = train_new, family = "binomial")

summary(train_new)
predictions1 <- predict(model1, newdata = valid_new, type = "response")  
classifications1 <- ifelse(predictions1 > .5, "YES" , "NO")
correct_classifications1 <-ifelse(classifications1 == valid_new$high_booking_rate, 1, 0)
accuracy1 = sum(correct_classifications1)/length(correct_classifications1) 
accuracy1


######################### Random Forest ######################### 

rf_model <- randomForest(high_booking_rate ~.+host_is_superhost:host_acceptance_rate
                         + zipcode + instant_bookable,
                         data = train_new)
#82.63
print(rf_model)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


r <- resample(learner = rf_model
              ,task = traintask
              ,resampling = rdesc
              ,measures = list(tpr,fpr,fnr,fpr,acc)
              ,show.info = T)

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

rf_p_train <- predict(rf_model, type="prob")[,2]
rf_pr_train <- prediction(rf_p_train, train_new$high_booking_rate)
r_auc_train1 <- performance(rf_pr_train, measure = "auc")@y.values[[1]] 
r_auc_train1
#87.18316


################################### XGBoost ######################################### 

dtrain <- xgb.DMatrix(data = as.matrix(train_new[, c("accommodates","bedrooms","beds","has_cleaning_fee","host_total_listings_count","price","ppp_ind","bathrooms","extra_people",
                                                     "host_response_rate","host_acceptance_rate","has_min_nights","host_is_superhost","cleaning_fee",
                                                     "price_per_person","maximum_nights","minimum_nights","availability_30","availability_60","availability_90",
                                                     "availability_365","zipcode","instant_bookable","is_location_exact","requires_license","license","host_has_profile_pic",
                                                     "host_identity_verified","security_deposit","has_rules","easy_transit","notes","interaction",
                                                     "host_response_time","host_about","apartment","hotel","condo","house","other","realBed","otherBed","access", "Sentiment_intensity", "Description_Length",
                                                     "cancellation_strict","cancellation_moderate","cancellation_flexible","cancellation_no_refunds","cancellation_super_strict_30","cancellation_super_strict_60","cancellation_no_policy","total_verifications","total_amenities")]), label = train_new$high_booking_rate)


# Set the parameters for XGBoost
params <- list(
  objective = "binary:logistic",  # Binary classification objective
  eval_metric = "auc"  # Evaluation metric (AUC)
)

# Train the XGBoost model
xgb_model <- xgb.train(
  params = params,  # XGBoost parameters
  data = dtrain,  # Training data
  nrounds = 100  # Number of boosting rounds
)

# Make predictions on the training data
pred_prob <- predict(xgb_model, newdata = dtrain, type = "prob")

# Create binary response vector for the training data
response <- ifelse(train_new$high_booking_rate == 1, 1, 0)

# Calculate AUC
auc <- roc(response, pred_prob)$auc

classifications_xgboost <- ifelse(pred_prob > .5, 1 , 0)
correct_classifications1_xgboost <-ifelse(classifications_xgboost == valid_new$high_booking_rate, 1, 0)
accuracy_xg = sum(correct_classifications1_xgboost)/length(correct_classifications1_xgboost) 


print(accuracy_xg)

# Print AUC value
print(auc)

# Plot the feature importance
xgb.importance(model = xgb_model)

# Get feature importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# Convert importance matrix to data.table
importance_dt <- data.table(importance_matrix)

# Plot feature importance
xgb.plot.importance(importance_matrix = importance_dt)

# Convert importance_dt to a data frame for ggplot
importance_df <- as.data.frame(importance_dt)

# Add a row index column for fitting curve
importance_df <- importance_df %>% mutate(Rank = row_number())

# Create a ggplot object
p <- ggplot(importance_df, aes(x = Rank, y = Gain)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Feature Rank", y = "Importance", title = "Feature Importance") +
  scale_x_continuous(breaks = importance_df$Rank)

# Display the plot
print(p)


# Print the trained model
print(xgb_model)
predictions1 <- predict(xgb_model, newdata = train, type = "response")  

va_inds <- sample(nrow(train), .3*nrow(train))
tr <- train[-va_inds,]
va <- train[va_inds,]

cv.res <- xgb.cv(data = dtrain, nfold = 3, nrounds = 100, verbose = FALSE,
                 objective = 'binary:logistic', eval_metric = 'auc', prediction = T)


###### PLOT FITTING CURVE ##########

#To obtain cross validation predictions one must specify 
#prediction = T when calling xgb.cv.


it = which.max(cv.res$evaluation_log$test_auc_mean)

#To obtain best iteration:
best.iter = cv.res$evaluation_log$iter[it]

#To plot ROC curve on the cross validation results:
plot(pROC::roc(response = train_new$high_booking_rate,
               predictor = cv.res$pred,
               levels=c(0, 1)),
     lwd=1.5)

########################################## GET OUTPUT IN TEST DATA  ###########################################


as.factor(ifelse(test_x$cancellation_policy %in% c("strict", "super_strict_30"), "strict", test_x$cancellation_policy))

test_x$cleaning_fee <- as.numeric(parse_number(test_x$cleaning_fee))
test_x$price <- as.numeric(parse_number(test_x$price))
test_x$extra_people <- as.numeric(parse_number(test_x$extra_people))

test_x$cleaning_fee <- ifelse(is.na(test_x$cleaning_fee),0, test_x$cleaning_fee)
test_x$price <- ifelse(is.na(test_x$price),0, test_x$price)

test_x <- test_x %>% 
  mutate_if(is.numeric,~replace_na(.,mean(., na.rm=TRUE)))

test_x <- mutate(test_x,
                 price_per_person = price/accommodates,
                 has_cleaning_fee = ifelse(cleaning_fee >0,"YES","NO"),
                 bed_category = ifelse(bed_type %in% c("Real Bed") ,"bed","other"),
                 property_category = as.factor(case_when(
                   property_type %in% c("Apartment","Serviced apartment","Loft") ~ "apartment",
                   property_type %in% c("Bed & Breakfast","Boutique hotel","Hostel") ~ "hotel",
                   property_type %in% c("Townhouse","Condominium") ~ "condo",
                   property_type %in% c("Bungalow","House") ~ "house",
                   TRUE ~ "other"))
)

test_x <- test_x %>% 
  group_by(property_category) %>%
  mutate(med = median(price_per_person, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ppp_ind = ifelse(price_per_person > med, 1, 0))

test_x$room_type <- as.factor(test_x$room_type)
test_x$ppp_ind <- as.factor(test_x$ppp_ind)
test_x$zipcode <- as.numeric(test_x$zipcode)
test_x$zipcode <- ifelse(is.na(test_x$zipcode),0, test_x$zipcode)
test_x$host_has_profile_pic <- ifelse(is.na(test_x$host_has_profile_pic),FALSE, test_x$host_has_profile_pic)
test_x$host_identity_verified <- ifelse(is.na(test_x$host_identity_verified),FALSE, test_x$host_identity_verified)
test_x$bathrooms <- ifelse(is.na(test_x$bathrooms),median(bathrooms), test_x$bathrooms)

test_x$host_is_superhost <- ifelse(is.na(test_x$host_is_superhost),FALSE, test_x$host_is_superhost)

test_x$host_acceptance<- factor(ifelse(is.na(test_x$host_acceptance_rate), "MISSING",
                                       ifelse(test_x$host_acceptance_rate == "100%","ALL","SOME")),
                                levels = c("ALL", "SOME", "MISSING"),
                                labels = c("ALL", "SOME", "MISSING"))

test_x$host_response<- factor(ifelse(is.na(test_x$host_response_rate), "MISSING",
                                     ifelse(test_x$host_response_rate == "100%","ALL","SOME")),
                              levels = c("ALL", "SOME", "MISSING"),
                              labels = c("ALL", "SOME", "MISSING"))

test_x <- mutate(test_x, 
                 has_min_nights= as.factor(ifelse(test_x$minimum_nights > 1,"YES", "NO")))

test_x <- mutate(test_x, charges_for_extra= as.factor(ifelse(test_x$extra_people > 0,"YES", "NO")))

test_x <- mutate(test_x,as.factor(high_booking_rate))
summary(test_x)

test <- test_x %>% 
  select(accommodates,bedrooms,beds,cancellation_policy,has_cleaning_fee,host_total_listings_count,
         price,ppp_ind,property_category,bed_category,bathrooms,charges_for_extra,host_acceptance,
         host_response,has_min_nights,host_is_superhost, cleaning_fee,
         price_per_person, maximum_nights, minimum_nights, availability_30, availability_60, availability_90,
         availability_365,zipcode,instant_bookable,is_location_exact,requires_license,host_has_profile_pic,
         host_identity_verified)

#set.seed(12)

# partition the features into training and validation rows
valid_instn = sample(nrow(train), 0.30*nrow(train))
train_new <- train[-valid_instn,]
valid_new <- train[valid_instn,]

model1 <- glm(high_booking_rate~.+property_category:price_per_person+host_is_superhost:host_acceptance
              + property_category:price ,
              data = test_x, family = "binomial")
summary(test_x)

predictions12 <- predict(model1, newdata = test, type = "response")  

classifications12 <- ifelse(predictions12 > .5, "YES" , "NO")
correct_classifications12 <-ifelse(classifications12 == valid_new$high_booking_rate, 1, 0)
accuracy1 = sum(correct_classifications1)/length(correct_classifications1) 
accuracy1

xyz <- predict(rf_model,test,type = "prob")
xyz


final <- xyz[,-1]
final
write.csv(final, file = 'rf_mod_Solution_win2.csv', row.names = FALSE)
rf_model <- randomForest(factor(high_booking_rate) ~.+property_category:price_per_person+host_is_superhost:host_acceptance
                         + property_category:price + zipcode + instant_bookable + is_location_exact + requires_license,
                         data = train_new)

#82.63

print(rf_model)
print(rf_model)

prediction1234 <- predict(rf_model, test,type = "response")
prediction1234

write.csv(prediction1234, file = 'rf_mod_Solution_2.csv', row.names = FALSE)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


r <- resample(learner = rf_model
              ,task = traintask
              ,resampling = rdesc
              ,measures = list(tpr,fpr,fnr,fpr,acc)
              ,show.info = T)
# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

table(prediction12)
install.packages('ROCR')
library(pROC)
library(ROCR)
rf_p_train <- predict(rf_model, type="prob")[,2]
rf_pr_train1 <- prediction(rf_p_train, test$high_booking_rate)
r_auc_train1 <- performance(rf_pr_train1, measure = "auc")@y.values[[1]] 
r_auc_train1

###################################################################################

as.factor(ifelse(test_x$cancellation_policy %in% c("strict", "super_strict_30"), "strict", test_x$cancellation_policy))

test_x$cleaning_fee <- as.numeric(parse_number(test_x$cleaning_fee))
test_x$price <- as.numeric(parse_number(test_x$price))
test_x$extra_people <- as.numeric(parse_number(test_x$extra_people))

test_x$cleaning_fee <- ifelse(is.na(test_x$cleaning_fee),0, test_x$cleaning_fee)
test_x$price <- ifelse(is.na(test_x$price),0, test_x$price)

test_x <- test_x %>% 
  mutate_if(is.numeric,~replace_na(.,mean(., na.rm=TRUE)))

test_x <- mutate(test_x,
                 price_per_person = price/accommodates,
                 has_cleaning_fee = ifelse(cleaning_fee >0,"YES","NO"),
                 bed_category = ifelse(bed_type %in% c("Real Bed") ,"bed","other"),
                 property_category = as.factor(case_when(
                   property_type %in% c("Apartment","Serviced apartment","Loft") ~ "apartment",
                   property_type %in% c("Bed & Breakfast","Boutique hotel","Hostel") ~ "hotel",
                   property_type %in% c("Townhouse","Condominium") ~ "condo",
                   property_type %in% c("Bungalow","House") ~ "house",
                   TRUE ~ "other"))
)

test_x <- test_x %>% 
  group_by(property_category) %>%
  mutate(med = median(price_per_person, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ppp_ind = ifelse(price_per_person > med, 1, 0))

test_x$room_type <- as.factor(test_x$room_type)
test_x$ppp_ind <- as.factor(test_x$ppp_ind)

test_x$bathrooms <- ifelse(is.na(test_x$bathrooms),median(bathrooms), test_x$bathrooms)
test_x$host_is_superhost <- ifelse(is.na(test_x$host_is_superhost),FALSE, test_x$host_is_superhost)

test_x$host_acceptance<- factor(ifelse(is.na(test_x$host_acceptance_rate), "MISSING",
                                       ifelse(test_x$host_acceptance_rate == "100%","ALL","SOME")),
                                levels = c("ALL", "SOME", "MISSING"),
                                labels = c("ALL", "SOME", "MISSING"))

test_x$host_response<- factor(ifelse(is.na(test_x$host_response_rate), "MISSING",
                                     ifelse(test_x$host_response_rate == "100%","ALL","SOME")),
                              levels = c("ALL", "SOME", "MISSING"),
                              labels = c("ALL", "SOME", "MISSING"))

test_x <- mutate(test_x, 
                 has_min_nights= as.factor(ifelse(test_x$minimum_nights > 1,"YES", "NO")))

test_x <- mutate(test_x, charges_for_extra= as.factor(ifelse(test_x$extra_people > 0,"YES", "NO")))

test_x$high_booking_rate <- as.factor(test_x$high_booking_rate)

test_x <- test_x %>% 
  select(accommodates,bedrooms,beds,cancellation_policy,has_cleaning_fee,host_total_listings_count,
         price,ppp_ind,property_category,bed_category,bathrooms,charges_for_extra,host_acceptance,
         host_response,has_min_nights,host_is_superhost, cleaning_fee,
         price_per_person, maximum_nights, minimum_nights, availability_30, availability_60, availability_90,
         availability_365)

#set.seed(1)


#analysis starts here
log_reg_model <- glm(high_booking_rate~.+property_category:price_per_person+host_is_superhost:host_acceptance
                     + property_category:price,
                     data = tr, family = "binomial")
pred <- predict(log_reg_model, newdata = va, type = "response")

#prediction on test set
pred_test <- predict(log_reg_model, newdata = test_x, type = "response")


classifications1 <- ifelse(pred_test > .5, 1 , 0)
correct_classifications1 <-ifelse(classifications1 == test_x$high_booking_rate, 1, 0)
accuracy1 = sum(correct_classifications1,na.rm = TRUE)/length(correct_classifications1) 
accuracy1

write.table(pred_test, "high_booking_rate_group2.csv", row.names = FALSE)
