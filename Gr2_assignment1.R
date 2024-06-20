Gr2 Github link: https://github.com/SunnyK-creat/4033Gr2

# Load necessary libraries
library(dplyr)
library(ggplot2)

# 1. Print the structure of the dataset
str(winequality_red)
print(winequality_red)

# 2. List the variables in the dataset
name(winequality_red)

# 3. Print the top 15 rows of the dataset
head(winequality_red, 15)

# 4. Write a user-defined function using any of the variables from the dataset 
sum_of_square <- function(x,y){x^2+y^2}
sum_of_square(winequality_red$`residual sugar`, winequality_red$`chlorides`)

# 5. Use data manipulation techniques and filter rows based on any logical criteria that exist in the dataset 
Mydata = as.data.frame(winequality_red %>% filter(`fixed acidity`>10, `residual sugar`>=5))
print(Mydata)

# 6. Identify the dependent & independent variables and use reshaping techniques
dependent_var <- winequality_red$quality
print(dependent_var)

independent_vars <- winequality_red %>% select(-quality)
print(independent_vars)

# 7. Remove missing values in the dataset
cleaned_wine_data <- winequality_red %>% filter_all(all_vars(!is.na(.)))
print(winequality_red)

# 8. Identify and remove duplicated data in the dataset
unique_winequality<-unique(winequality_red)
print(unique_winequality)
unique_winequality<-winequality_red%>%distinct()
print(unique_winequality)

# 9. Reorder multiple rows in descending order
winequality_red %>%arrange(desc(1))
winequality_red %>%arrange(desc(2))
winequality_red %>%arrange(desc(3))

# 10. Rename some of the column names in the datase
colnames(winequality_red)[colnames(winequality_red) == "citric acid"] <- "citric"
print(colnames(winequality_red))

# 11. Add new variables in the data frame by using a mathematical function
winequality_red_with_new<-winequality_red %>% mutate(Citrate_double = citric * 2)
print(winequality_red_with_new)

# 12. Create a training set using a random number generator engine
set.seed(123)
train_indices<-sample(1:nrow(winequality_red),0.7*nrow(winequality_red))
train_set<-winequality_red[train_indices,]
head(train_set)

# 13. Print the summary statistics of the dataset
print(summary(winequality_red))

# 14. Use any of the numerical variables from the dataset and perform statistical functions
mean(winequality_red$alcohol)
median(winequality_red$alcohol)

calculate_mode_alcohol <- function(data) {ux <- unique(data$alcohol)
  tab <- tabulate(match(data$alcohol, ux))
  mode_alcohol <- ux[tab == max(tab)]
  return(mode_alcohol)}
mode_alcohol <- calculate_mode_alcohol(winequality_red)
print(paste("Mode of alcohol content in winequality_red dataset:", mode_alcohol))

mode(winequality_red$alcohol)
range(winequality_red$alcohol)

# 15. Plot a scatter plot for any 2 variables in the dataset
ggplot(winequality_red, aes(x = alcohol, y = quality)) + geom_point()

# 16. Plot a bar plot for any 2 variables in the dataset
ggplot(winequality_red, aes(x = factor(quality), fill = factor(alcohol))) +geom_bar(position = "stack") +
labs(x = "Quality", y = "Count", title = "Bar Plot for Alcohol vs. Quality in Wine") +
theme_minimal()

# 17. Find the correlation between any 2 variables by applying Pearson correlation 
correlation_value<-cor(winequality_red$alcohol,winequality_red$quality,method = "pearson")
correlation_value




                                          
                                          
                                                                             
                                                                             



