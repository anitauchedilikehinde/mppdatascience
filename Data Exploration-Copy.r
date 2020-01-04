
## Import packages
library(ggplot2)
library(repr)
library(dplyr)
library(caret)
library(MASS)
library(GGally)

options(repr.plot.width=4, repr.plot.height=4) # Set the initial plot area dimensions

#import datasets
train_labels <- read.csv('train_labels_DPetPH2.csv', stringsAsFactors = FALSE, header = TRUE)
train_values <- read.csv('train_values_OL27nta.csv', stringsAsFactors = FALSE, header = TRUE)

#import test dataset
test_data = read.csv('test_values_kWyIOM9.csv', stringsAsFactors = FALSE, header = TRUE)
head(test_data)
head(train_labels)
head(train_values)

#merge datasets by row_id
train_data <- merge(train_values, train_labels, by = 'row_id', all = TRUE)
head(train_data)

#find missing values
summary(train_data)

#viewing coreleations 
cor_pop = cor(train_data$gross_rent, train_data$population)
cor_hsd = cor(train_data$gross_rent, train_data$pct_adults_with_high_school_diploma)
cor_boh = cor(train_data$gross_rent, train_data$pct_adults_bachelors_or_higher)
cor_dr = cor(train_data$gross_rent, train_data$death_rate_per_1k)
cor_white = cor(train_data$gross_rent, train_data$pct_white)
cor_asian = cor(train_data$gross_rent, train_data$pct_asian)
cor_his = cor(train_data$gross_rent, train_data$pct_hispanic)
print(cor_white)
print(cor_asian)
print(cor_his)
print(cor_pop)
print(cor_hsd)
print(cor_boh)
print(cor_dr)

# stating machine learning

#remove NAs common to many clomuns
cols = c('pct_uninsured_adults', 'pct_diabetes')
train_data = train_data[complete.cases(train_data[,cols]),]
test_data = test_data[complete.cases(test_data[,cols]), ]
summary(test_data)
summary(train_data)

# delete columns with too many NAs
# remove homicides_per_100k column *947 - too many NAs
# remove pct_excessive_drinking  column *460 - too many NAs
# remove pct_low_birthweight column *114 - seems not to be very informative
# remove pct_adult_smoking column *216
# remove evictions column *325
# remove motor_vehicle_crash_deaths_per_100k  *188
train_data[, 'homicides_per_100k'] = NULL
train_data[, 'pct_excessive_drinking'] = NULL
train_data[, 'pct_low_birthweight'] = NULL
train_data[, 'pct_adult_smoking'] = NULL
train_data[, 'evictions'] = NULL
train_data[, 'motor_vehicle_crash_deaths_per_100k'] = NULL
dim(train_data)
test_data[, 'homicides_per_100k'] = NULL
test_data[, 'pct_excessive_drinking'] = NULL
test_data[, 'pct_low_birthweight'] = NULL
test_data[, 'pct_adult_smoking'] = NULL
test_data[, 'evictions'] = NULL
test_data[, 'motor_vehicle_crash_deaths_per_100k'] = NULL
dim(test_data)

#replace NAs in air_pollution_particulate_matter_value, pop_per_dentist and pop_per_primary_care_physician
# with median as distribution is skewed IN TRAIN_DATA
train_data$air_pollution_particulate_matter_value <- ifelse(is.na(train_data$air_pollution_particulate_matter_value), 
                                                            median(train_data$air_pollution_particulate_matter_value, na.rm = TRUE), 
                                     train_data$air_pollution_particulate_matter_value)
train_data$pop_per_dentist <- ifelse(is.na(train_data$pop_per_dentist), median(train_data$pop_per_dentist, na.rm = TRUE), 
                                     train_data$pop_per_dentist)
train_data$pop_per_primary_care_physician <- ifelse(is.na(train_data$pop_per_primary_care_physician), 
                                                    median(train_data$pop_per_primary_care_physician, na.rm = TRUE), 
                                     train_data$pop_per_primary_care_physician)


#replace NAs in air_pollution_particulate_matter_value, pop_per_dentist and pop_per_primary_care_physician
# with median as distribution is skewed IN TEST_DATA
test_data$air_pollution_particulate_matter_value <- ifelse(is.na(test_data$air_pollution_particulate_matter_value), 
                                                            median(test_data$air_pollution_particulate_matter_value, na.rm = TRUE), 
                                     test_data$air_pollution_particulate_matter_value)
test_data$pop_per_dentist <- ifelse(is.na(test_data$pop_per_dentist), median(test_data$pop_per_dentist, na.rm = TRUE), 
                                     test_data$pop_per_dentist)
test_data$pop_per_primary_care_physician <- ifelse(is.na(test_data$pop_per_primary_care_physician), 
                                                    median(test_data$pop_per_primary_care_physician, na.rm = TRUE), 
                                     test_data$pop_per_primary_care_physician)


# investigate to see that each categorical variable has balanced sample
table(train_data[, 'rucc'])

table(test_data[, 'rucc'])

#merging 2 segments of Nonmetro-Urban population of 20,000 or more to get a more balanced data
rucc_cats = c('Metro - Counties in metro areas of 1 million population or more' = 'Metro above 1 million', 
              'Metro - Counties in metro areas of 250,000 to 1 million population' = 'Metro 250,000 to 1 million', 
              'Metro - Counties in metro areas of fewer than 250,000 population' = 'Metro 250,000 or less', 
        'Nonmetro - Completely rural or less than 2,500 urban population, adjacent to a metro area' = 'Nonmetro, rural below 2,500, adj to metro', 
    'Nonmetro - Completely rural or less than 2,500 urban population, not adjacent to a metro area' = 'Nonmetro, rural below 2,500, not adj to metro',
      'Nonmetro - Urban population of 2,500 to 19,999, adjacent to a metro area' = 'Nonmetro, Urban 2,500 to 19,999, adjacent to metro',
         'Nonmetro - Urban population of 2,500 to 19,999, not adjacent to a metro area' = 'Nonmetro, Urban 2,500 to 19,999, not adj to metro',
        'Nonmetro - Urban population of 20,000 or more, adjacent to a metro area' = 'Nonmetro, Urban above 20,000',
          'Nonmetro - Urban population of 20,000 or more, not adjacent to a metro area' = 'Nonmetro, Urban above 20,000'  )

out = rep('i', length.out = nrow(train_data))
i = 1
for(x in train_data[,'rucc']){
    out[i] = rucc_cats[[x]]
    i = i + 1
}
train_data[,'rucc'] = out

table(train_data[, 'rucc'])

#merging 2 segments of Nonmetro-Urban population of 20,000 or more to get a more balanced data
rucc_cats = c('Metro - Counties in metro areas of 1 million population or more' = 'Metro above 1 million', 
              'Metro - Counties in metro areas of 250,000 to 1 million population' = 'Metro 250,000 to 1 million', 
              'Metro - Counties in metro areas of fewer than 250,000 population' = 'Metro 250,000 or less', 
        'Nonmetro - Completely rural or less than 2,500 urban population, adjacent to a metro area' = 'Nonmetro, rural below 2,500, adj to metro', 
    'Nonmetro - Completely rural or less than 2,500 urban population, not adjacent to a metro area' = 'Nonmetro, rural below 2,500, not adj to metro',
      'Nonmetro - Urban population of 2,500 to 19,999, adjacent to a metro area' = 'Nonmetro, Urban 2,500 to 19,999, adjacent to metro',
         'Nonmetro - Urban population of 2,500 to 19,999, not adjacent to a metro area' = 'Nonmetro, Urban 2,500 to 19,999, not adj to metro',
        'Nonmetro - Urban population of 20,000 or more, adjacent to a metro area' = 'Nonmetro, Urban above 20,000',
          'Nonmetro - Urban population of 20,000 or more, not adjacent to a metro area' = 'Nonmetro, Urban above 20,000'  )

out = rep('i', length.out = nrow(test_data))
i = 1
for(x in test_data[,'rucc']){
    out[i] = rucc_cats[[x]]
    i = i + 1
}
test_data[,'rucc'] = out

table(test_data[, 'rucc'])

table(train_data[, 'urban_influence'])

table(test_data[, 'urban_influence'])

#merging segments of urban_influence to get a balanced dataset
urbinf_cats = c('Large-in a metro area with at least 1 million residents or more' = 'Large-in metro above 1 million residents', 
              'Micropolitan adjacent to a large metro area' = 'Micropolitan adj to a L or S metro', 
              'Micropolitan adjacent to a small metro area' = 'Micropolitan adj to a L or S metro', 
        'Micropolitan not adjacent to a metro area' = 'Micropolitan not adj to a metro', 
    'Noncore adjacent to a large metro area' = 'Noncore adj to large metro',
      'Noncore adjacent to a small metro and does not contain a town of at least 2,500 residents' = 'Noncore adj to S metro',
         'Noncore adjacent to a small metro with town of at least 2,500 residents' = 'Noncore adj to S metro',
        'Noncore adjacent to micro area and contains a town of 2,500-19,999 residents' = 'Noncore adj to micro',
          'Noncore adjacent to micro area and does not contain a town of at least 2,500 residents' = 'Noncore adj to micro',
             'Noncore not adjacent to a metro/micro area and contains a town of 2,500  or more residents' = 'Noncore not adj to a metro/micro',
             'Noncore not adjacent to a metro/micro area and does not contain a town of at least 2,500 residents' = 'Noncore not adj to a metro/micro',
             'Small-in a metro area with fewer than 1 million residents' = 'Small-in a metro area with 1 million and below')

out = rep('i', length.out = nrow(train_data))
i = 1
for(x in train_data[,'urban_influence']){
    out[i] = urbinf_cats[[x]]
    i = i + 1
}
train_data[,'urban_influence'] = out

table(train_data[, 'urban_influence'])

#merging segments of urban_influence to get a balanced dataset
urbinf_cats = c('Large-in a metro area with at least 1 million residents or more' = 'Large-in metro above 1 million residents', 
              'Micropolitan adjacent to a large metro area' = 'Micropolitan adj to a L or S metro', 
              'Micropolitan adjacent to a small metro area' = 'Micropolitan adj to a L or S metro', 
        'Micropolitan not adjacent to a metro area' = 'Micropolitan not adj to a metro', 
    'Noncore adjacent to a large metro area' = 'Noncore adj to large metro',
      'Noncore adjacent to a small metro and does not contain a town of at least 2,500 residents' = 'Noncore adj to S metro',
         'Noncore adjacent to a small metro with town of at least 2,500 residents' = 'Noncore adj to S metro',
        'Noncore adjacent to micro area and contains a town of 2,500-19,999 residents' = 'Noncore adj to micro',
          'Noncore adjacent to micro area and does not contain a town of at least 2,500 residents' = 'Noncore adj to micro',
             'Noncore not adjacent to a metro/micro area and contains a town of 2,500  or more residents' = 'Noncore not adj to a metro/micro',
             'Noncore not adjacent to a metro/micro area and does not contain a town of at least 2,500 residents' = 'Noncore not adj to a metro/micro',
             'Small-in a metro area with fewer than 1 million residents' = 'Small-in a metro area with 1 million and below')

out = rep('i', length.out = nrow(test_data))
i = 1
for(x in test_data[,'urban_influence']){
    out[i] = urbinf_cats[[x]]
    i = i + 1
}
test_data[,'urban_influence'] = out

table(test_data[, 'urban_influence'])

table(train_data[, 'economic_typology'])

table(train_data[, 'state'])

state_cats = c('08f8fb4' = '08f8fb4', '09d8cd0' = '09d8cd0', '0f8930b' = '0f8930b', '105e445' = 'other', '158df01' = '158df01',
               '1646cf6' = '1646cf6', '176f5f0' = '176f5f0', '1b0d913' = '1b0d913', '1dcfd4e' = '1dcfd4e', '20d32fc' = '20d32fc',
               '2b7da97' = 'other', '3337bbb' = 'other', '3745933' = '3745933', '375d4d3' = 'other', '4522abc' = '4522abc', 
               '485e9af' = '485e9af', '4c72956'= '4c72956', '4cd9667' = '4cd9667', '5029ed4' = 'other', '5086a32' = '5086a32', 
             '528ea9f' = '528ea9f', '52acab4' = '52acab4', '64ffe5d' = '64ffe5d', '698ab34' = '698ab34', '6d287d7' = 'other', 
               '7572db1' = '7572db1', '78e8330' = '78e8330', '7dd3518' = '7dd3518', '8036085' = '8036085', '842bd12' = '842bd12',
              '914c15f' = 'other', '9d0874a' = '9d0874a', '9d1e27d'= '9d1e27d', '9dda412' = '9dda412', '9e0007d' = '9e0007d', 
               '9e065a4' = 'other', 'a952566' = 'a952566', 'b44cfe6' = 'other', 'b795815' = 'b795815', 'bc77872' = 'other', 
               'c3dbf0a' = 'c3dbf0a', 'c479f0c' = 'c479f0c', 'd233cec' = 'd233cec', 'dc9ae72' = 'dc9ae72', 'dfc21f3' = 'other',
              'e2f94fa' = 'e2f94fa', 'e74aca3' = 'e74aca3', 'e899d7f' = 'e899d7f', 'fa605d5' = 'other', 'fb8cab1' = 'fb8cab1')

out = rep('i', length.out = nrow(train_data))
i = 1
for(x in train_data[,'state']){
    out[i] = state_cats[[x]]
    i = i + 1
}
train_data[, 'state'] = out

table(train_data[, 'state'])

table(test_data[, 'state'])

state_cats = c('08f8fb4' = '08f8fb4', '09d8cd0' = '09d8cd0', '0f8930b' = '0f8930b', '105e445' = 'other', '158df01' = '158df01',
               '1646cf6' = '1646cf6', '176f5f0' = '176f5f0', '1b0d913' = '1b0d913', '1dcfd4e' = '1dcfd4e', '20d32fc' = '20d32fc',
               '2b7da97' = 'other', '3337bbb' = 'other', '3745933' = '3745933', '375d4d3' = 'other', '4522abc' = '4522abc', 
               '485e9af' = '485e9af', '4c72956'= '4c72956', '4cd9667' = '4cd9667', '5029ed4' = 'other', '5086a32' = '5086a32', 
             '528ea9f' = '528ea9f', '52acab4' = '52acab4', '64ffe5d' = '64ffe5d', '698ab34' = '698ab34', '6d287d7' = 'other', 
               '7572db1' = '7572db1', '78e8330' = '78e8330', '7dd3518' = '7dd3518', '8036085' = '8036085', '842bd12' = '842bd12',
              '914c15f' = 'other', '9d0874a' = '9d0874a', '9d1e27d'= '9d1e27d', '9dda412' = '9dda412', '9e0007d' = '9e0007d', 
               '9e065a4' = 'other', 'a952566' = 'a952566', 'b44cfe6' = 'other', 'b795815' = 'b795815', 'bc77872' = 'other', 
               'c3dbf0a' = 'c3dbf0a', 'c479f0c' = 'c479f0c', 'd233cec' = 'd233cec', 'dc9ae72' = 'dc9ae72', 'dfc21f3' = 'other',
              'e2f94fa' = 'e2f94fa', 'e74aca3' = 'e74aca3', 'e899d7f' = 'e899d7f', 'fa605d5' = 'other', 'fb8cab1' = 'fb8cab1', 
               'a0e0eec' = 'other')

out = rep('i', length.out = nrow(test_data))
i = 1
for(x in test_data[,'state']){
    out[i] = state_cats[[x]]
    i = i + 1
}
test_data[, 'state'] = out

table(test_data[, 'state'])

#view disribution of gross_rent; distribution is skewed
plot_hist = function(df, col = 'gross_rent', bins = 10){
    options(repr.plot.width=4, repr.plot.height=3) # Set the initial plot area dimensions
bw = (max(df[, col]) - min(df[, col]))/(bins +1)
    p = ggplot(df, aes_string(col)) + 
               geom_histogram(binwidth = bw, aes(y=..density..), alpha = 0.5) +
               geom_density(aes(y=..density..), color = 'blue') + 
               geom_rug()
    print(p)
}    
plot_hist(train_data)   

#transforming gross_rent to log as gross_rent is skewed
train_data[, 'gross_rent'] = log(train_data[ , 'gross_rent'])
plot_hist(train_data, col = 'gross_rent') #gross_rent is normal now

#check for duplicate records
print(dim(train_data))
dim(distinct(train_data)) #no duplicate records

#data exploration
#1. barchart of all categorical variables
plot_bars = function(train_data){
    options(repr.plot.width=6, repr.plot.height=6) # Set the initial plot area dimensions
    for(col in colnames(train_data)){
        if(is.character(train_data[,col])){
            p = ggplot(train_data, aes_string(col)) + 
                       geom_bar(alpha = 0.6) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
            print(p)
        }
    }
}

plot_bars(train_data)


plot_hist = function(train_data, bins = 20){
    options(repr.plot.width=4, repr.plot.height=3) # Set the initial plot area dimensions
    for(col in colnames(train_data)){
        if(is.numeric(train_data[,col])){
            bw = (max(train_data[,col]) - min(train_data[,col]))/(bins + 1)
            p = ggplot(train_data, aes_string(col)) + 
                       geom_histogram(alpha = 0.6, binwidth = bw) 
            print(p)
        }
    }
}

plot_hist(train_data)

#2. boxplot all categorical variables
plot_box = function(train_data, cols, col_y = 'gross_rent'){
    options(repr.plot.width=6, repr.plot.height=8) # Set the initial plot area dimensions
    for(col in cols){
        p = ggplot(train_data, aes_string(col, col_y)) + 
                   geom_boxplot() +
                   ggtitle(paste('Box plot of', col, 'vs.', col_y)) +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        print(p)
    }
}

cat_cols = c('rucc', 'state', 'urban_influence', 'economic_typology')
plot_box(train_data, cat_cols)  

#4. pairwise plots
library(GGally)
numcols = c('poverty_rate', 'pct_unemployment', 'rent_burden', 'pct_uninsured_adults', 'pct_uninsured_children', 'gross_rent')
options(repr.plot.width=6, repr.plot.height=6) # Set the initial plot area dimensions
ggpairs(train_data,
      columns = numcols,
      aes(color = economic_typology, alpha = 0.1),
      lower = list(continuous = 'points'),
      upper = list(continuous = ggally_density))

plot_scatter = function(train_data, cols, col_y = 'gross_rent'){
    options(repr.plot.width=4, repr.plot.height=4) # Set the initial plot area dimensions
    for(col in cols){
        p = ggplot(train_data, aes_string(col, col_y)) + 
                   geom_point() +
                   ggtitle(paste('Scatter plot of', col_y, 'vs.', col))
        print(p)
    }
}

numcols = c('poverty_rate', 'pct_unemployment', 'rent_burden', 'pct_uninsured_adults', 'pct_uninsured_children')
plot_scatter(train_data, numcols)

# check for collinearity
# pct_uninsured_adults and pct_uninsured_children are colinear hence pct_uninsured_children will be dropped from the dataset
train_data[, 'pct_uninsured_children'] = NULL
train_data[, 'row_id'] = NULL
str(train_data)
test_data[, 'pct_uninsured_children'] = NULL
str(test_data)

summary(test_data)

#scale numerical fields
num_cols = c('population', 'renter_occupied_households', 'pct_renter_occupied', 'rent_burden', 'pct_white', 'pct_af_am', 'pct_hispanic',
        'pct_am_ind', 'pct_asian', 'pct_nh_pi', 'pct_multiple', 'pct_other', 'poverty_rate', 'pct_civilian_labor', 'pct_unemployment',
        'pct_uninsured_adults', 'pct_adult_obesity', 'pct_diabetes', 'pct_physical_inactivity', 'air_pollution_particulate_matter_value',
     'heart_disease_mortality_per_100k', 'pop_per_dentist', 'pop_per_primary_care_physician', 'pct_female', 'pct_below_18_years_of_age',
     'pct_aged_65_years_and_older', 'pct_adults_less_than_a_high_school_diploma', 
             'pct_adults_with_some_college', 'pct_adults_bachelors_or_higher', 'birth_rate_per_1k', 'death_rate_per_1k') 
            
preProcValues <- preProcess(train_data[,num_cols], method = c("center", "scale"))

train_data[,num_cols] = predict(preProcValues, train_data[,num_cols])
test_data[,num_cols] = predict(preProcValues, test_data[,num_cols])


#ohe on categorical variables
dummies = dummyVars(gross_rent ~ . , data = train_data)
train_dummies = data.frame(predict(dummies, newdata = train_data))
head(train_dummies)
names(train_dummies)
dim(train_dummies)

#get low variance columns
near_zero = nearZeroVar(train_dummies, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE)
low_variance_cols <- near_zero[(near_zero$zeroVar == TRUE) | (near_zero$nzv == TRUE), ]
low_variance_cols

rownames(low_variance_cols)

#drops low variance columns
drops <- rownames(low_variance_cols)
train_dummies <- train_dummies[ , !(names(train_dummies) %in% drops)]
names(train_dummies)
dim(train_dummies)

train_data[, 'county_code'] = NULL
train_data[, 'state'] = NULL
str(train_data)

## define and fit the linear regression model
lin_mod = lm(gross_rent ~ ., data = train_data)

summary(lin_mod)$coefficients

summary(lin_mod)

# model evaluation
pred <- predict(lin_mod, test_data)
summary(lin_mod)

pred = exp(pred)
head(pred)

pred <- exp(pred)
pred <- as.integer(pred)
class(pred)
head(pred)

predicted = cbind(test_data[, 'row_id'] , pred )

colnames(predicted) <- c('row_id', 'gross_rent')
head(predicted)

#export file to my_prediction.csv
write.csv(predicted, file = 'prediction2.csv') #75% get another model



# model 3 using crossvalidation
# get most important variables
gbmImp <- varImp(lin_mod, scale = FALSE)
gbmImp

#find variables that are highly correlated
nums <- sapply(train_data, is.numeric)
data.numeric <- train_data[ , nums]

data.without_na <- na.omit(data.numeric)
cor_matrix <- cor(data.without_na)

findCorrelation(cor_matrix, 0.7)

#delete highly correlated variables
train_data[, c(18, 30, 19, 32, 2, 25)] = NULL

# fit lm again
lin_mod = lm(gross_rent ~ ., data = train_data)
summary(lin_mod)$coefficients

# first model
summary(lin_mod)

library(elasticnet)
fitControl <- trainControl(method = "repeatedcv",   
                           number = 10,     # number of folds
                           repeats = 10)    # repeated ten times

model.cv <- train(gross_rent ~ .,
               data = train_data,
               method = "lasso",  # now we're using the lasso method
               trControl = fitControl)  

model.cv 

install.packages('elasticnet')

ggplot(varImp(model.cv))


# fit lm again
lin_mod = lm(gross_rent ~ pct_asian + pct_physical_inactivity + pct_adults_bachelors_or_higher + rucc + urban_influence
            + pct_adults_with_high_school_diploma + death_rate_per_1k + pct_adult_obesity + pct_aged_65_years_and_older
             + heart_disease_mortality_per_100k + pct_diabetes + population + pct_hispanic + pct_other + pct_below_18_years_of_age
             + pct_nh_pi + economic_typology + pct_white + poverty_rate + rent_burden, data = train_data)
summary(lin_mod)

summary(lin_mod)$coefficients

# fit lm again
lin_mod = lm(gross_rent ~ pct_asian + pct_physical_inactivity + pct_adults_bachelors_or_higher + rucc + urban_influence
            + pct_adults_with_high_school_diploma + death_rate_per_1k  + pct_adult_obesity + pct_aged_65_years_and_older
              + pct_diabetes + population + pct_hispanic + pct_other + pct_below_18_years_of_age
             + pct_nh_pi + economic_typology + pct_white + poverty_rate + rent_burden + pct_renter_occupied, data = train_data)
summary(lin_mod)

# model evaluation
pred <- predict(lin_mod, test_data)
summary(lin_mod)

pred = exp(pred)
head(pred)

class(pred)
pred <- as.integer(pred)
class(pred)
head(pred)

predicted = cbind(test_data[, 'row_id'] , pred )

colnames(predicted) <- c('row_id', 'gross_rent')
head(predicted)

#export file to my_prediction.csv
write.csv(predicted, file = 'prediction.csv')


