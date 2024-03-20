# we have imported the Biodiversity dataset file (V3 version)
data<-read.csv("/Users/gaurav/Desktop/project/Data/proportional_species_richness_V3.csv")
View(data)

# We can see the structure of data (it has 5280 obs. of 17 variables)
str(data)

# Using the important libraries
library(dslabs)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(rvest)
library(readxl)
library(emmeans)
library(pastecs)
library(corrplot)
library(moments)
library(tidyr)
library(corrplot)

# selecting the BD7 assigned from the original dataset
data_7<-data %>% select(c("Bees","Bryophytes","Butterflies","Carabids","Hoverflies","Isopods","Vascular_plants"))
View(data_7)

# All the 11 taxonomical groups from the original dataset
data_11<-data %>% select(c("Bees","Bryophytes","Butterflies","Carabids","Hoverflies","Isopods","Vascular_plants","Ladybirds","Grasshoppers_._Crickets","Macromoths","Bird"))
View(data_11)

# We have added the period variable column in our BD7 group data
data_7$period<-data$period
data_7$Location<- data$Location

# calculation the mean of BD7
data_7$BD7 <- rowMeans(data_7[,1:7])

# Storing the mean of BD&7 according the time period
BD7_period1 <- data_7$BD7[data_7$period == "Y70"]
BD7_period1
BD7_period2 <- data_7$BD7[data_7$period == "Y00"]
BD7_period2

#######################Univariate Analysis###################
# Exploring the change in Bio Div measure from  land classification "SS"
df_y70 <- data_7[grepl("^SS\\d\\d", data_7$Location) & data_7$period == "Y70", c("Location","Bees","Bryophytes","Butterflies","Carabids","Hoverflies","Isopods","Vascular_plants","period","BD7")]
df_y00 <- data_7[grepl("^SS\\d\\d", data_7$Location) & data_7$period == "Y00", c("Location","Bees","Bryophytes","Butterflies","Carabids","Hoverflies","Isopods","Vascular_plants","period","BD7")]
df_70_00<- rbind(df_y70, df_y00)
str(df_y70)
str(df_y00)
###############################################Bees####################################

# box plot of species for period Y70

library(plotly)
library(dplyr)

new_df_y70 <- df_y70 %>% select(c("Bees", "Bryophytes", "Butterflies", "Carabids", "Hoverflies", "Isopods", "Vascular_plants"))

plot_data <- gather(new_df_y70) %>%
  mutate(key = factor(key, levels = c("Bees", "Bryophytes", "Butterflies", "Carabids", "Hoverflies", "Isopods", "Vascular_plants"))) # set order of x-axis labels

plot <- plot_ly(plot_data, x = ~key, y = ~value, type = "box") %>%
  layout(title = "Frequency of species over different hectads in period Y70",
         xaxis = list(title = "Species", tickangle = 45),
         yaxis = list(title = "Frequency"))

# Display plot
plot


# box plot of species for period Y00
new_df_y00<- df_y00 %>% select(c("Bees", "Bryophytes", "Butterflies", "Carabids", "Hoverflies", "Isopods", "Vascular_plants"))
plot_data <- gather(new_df_y00) %>%
  mutate(key = factor(key, levels = c("Bees", "Bryophytes", "Butterflies", "Carabids", "Hoverflies", "Isopods", "Vascular_plants"))) # set order of x-axis labels

plot <- plot_ly(plot_data, x = ~key, y = ~value, type = "box") %>%
  layout(title = "Frequency of species over different hectads in period Y00",
         xaxis = list(title = "Species", tickangle = 45),
         yaxis = list(title = "Frequency"))
plot

# plotting a bar graph of bees through Y70

ggplot(data=df_y70, aes(x = Location, y = Bees, fill = "Bees")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Bees in Y70",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()

# plotting a bar graph of bees through Y00
ggplot(data=df_y00, aes(x = Location, y = Bees, fill = "Bees")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Bees in Y00",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()

################################################Bryophytes######################################
# Bar graph for "Bryophytes" in Y70

ggplot(data=df_y70, aes(x = Location, y = Bryophytes, fill = "Bryophytes")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Bryophytes in Y70",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()
 


# Bar Graph for "Bryophytes" in Y00
ggplot(data=df_y00, aes(x = Location, y = Bryophytes, fill = "Bryophytes")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Bryophytes in Y00",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()
############################################Butterflies######################################

# Bar graph of "Butterflies" in Y70
ggplot(data=df_y70, aes(x = Location, y = Butterflies, fill = "Butterflies")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Butterflies in Y70",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()


# Bar graph of "Butterflies" in Y00
ggplot(data=df_y00, aes(x = Location, y = Butterflies, fill = "Butterflies")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Butterflies in Y00",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()



##########################################Carabids######################################
# Bar graph for "Carabids" in Y70
ggplot(data=df_y70, aes(x = Location, y = Carabids, fill = "Carabids")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Carabids in Y70",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()


# Bar graph for "Carabids" in Y00
ggplot(data=df_y00, aes(x = Location, y = Carabids, fill = "Carabids")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Carabids in Y00",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()


#######################################Hoverflies###############################
# Bar graph for "Hoverflies" in Y70
ggplot(data=df_y70, aes(x = Location, y = Hoverflies, fill = "Hoverflies")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Hoverflies in Y70",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()

# Bar graph for "Hoverflies" in Y00
ggplot(data=df_y00, aes(x = Location, y = Hoverflies, fill = "Hoverflies")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Hoverflies in Y00",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()


##################################Isopods#######################################

# Bar graph for "Isopods" in Y70
ggplot(data=df_y70, aes(x = Location, y = Isopods, fill = "Isopods")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Isopods in Y70",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()

# Bar graph for "Isopods" in Y00
ggplot(data=df_y00, aes(x = Location, y = Isopods, fill = "Isopods")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Isopods in Y00",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()

####################################Vascular Plants#############################


# Bar graph for "vascular Plants" in Y70
ggplot(data=df_y70, aes(x = Location, y = Vascular_plants, fill = "Vascular_plants")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Vascular plants in Y70",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()

# Bar graph for "vascular Plants" in Y00
ggplot(data=df_y00, aes(x = Location, y = Vascular_plants, fill = "Vascular_plants")) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Species Richness for Vascular plants in Y00",x = "Location", y = "Count of Bees", fill = "") +
  theme_minimal()




################################### finding the corelation values between BD7. ##################


#correlation between the variables
data_BD7 <- subset(data_7, select = c(Bees,Bryophytes,Butterflies,Carabids,Hoverflies,Isopods,Vascular_plants))
correlations <- cor(data_BD7, use = "pairwise.complete.obs")
correlations

# Plot the correlation matrix
corrplot(correlations,
         method = "circle",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("#D73027", "#FC8D59", "#FEE090", "#91BFDB", "#4575B4"))(100),
         bg = "white",
         addCoef.col = "black",
         number.cex = 0.8,
         tl.cex = 0.8,
         mar = c(0, 0, 2, 0),
         title = "Correlation Plot"
)


####################################### Hypothesis testing.  ###############################

#  We have added the period variable column in our BD7 group data
data_BD7$period<-data$period

# calculation the mean of BD7
data_BD7$BD7 <- rowMeans(data_7[,1:7])

# Storing the mean of BD&7 according the time period
BD7_period_Y70 <- data_7$BD7[data_7$period == "Y70"]
BD7_period_Y70
BD7_period_Y00 <- data_7$BD7[data_7$period == "Y00"]
BD7_period_Y00

###### performing the Z-test. ####################


# Calculate the mean of numeric vector which has BD 7 for period Y70 and Y00

BD7_period_Y70_mean <- mean(BD7_period_Y70)
BD7_period_Y00_mean <- mean(BD7_period_Y00)

# Calculate the standard deviation of BD 7 for period Y70 and Y00

BD7_period_Y70_sd<-sd(BD7_period_Y70)
BD7_period_Y70_sd

BD7_period_Y00_sd<-sd(BD7_period_Y00)
BD7_period_Y00_sd

# Calculate the sample sizes for each period
n1 <- length(BD7_period_Y70)
n1
n2 <- length(BD7_period_Y00)
n2

# calculating the degree of freedom
df <- ((BD7_period_Y70_sd^2 / n1 + BD7_period_Y00_sd^2 / n2)^2) /
  ((BD7_period_Y70_sd^2 / n1)^2 / (n1 - 1) + (BD7_period_Y00_sd^2 / n2)^2 / (n2 - 1))

df
# Perform the Z-test
z_value <- (BD7_period_Y70_mean - BD7_period_Y00_mean) / sqrt((BD7_period_Y70_sd^2 / n1) + (BD7_period_Y00_sd^2 / n2))
z_value
# Calculate the p-value using the Z-value
p_value <- 2 * pnorm(-abs(z_value))

# Print the results
cat("Z-value:", z_value, "\nP-value:", p_value)


#####  performing the two tail t test ####

# Two tail t-test
t_test <- t.test(BD7_period1, BD7_period2,mu=0)
print(t_test)

# printing the p-value
p_value_t <- t_test$p.value
p_value_t





  ################################ linear regression.  ######################

# calculating the mean for 11 groups 
data_11$BD11 <- rowMeans(data_11[,1:11])

View(data_BD7)

# add BD11 column to our data to data_7 
data_BD7$BD11 <- data_11$BD11

#linear regression on how BD7 matches BD11
linear_model_BD7_BD11 <- lm(BD7 ~ BD11, data=data_BD7)
summary(linear_model_BD7_BD11)

#linear regression for each period
data_period1 <- data_BD7[data_BD7$period == "Y70", ]
data_period2 <- data_BD7[data_BD7$period == "Y00", ]

linear_model_Y70 <- lm(BD7 ~ BD11, data=data_period1)
linear_model_Y00<- lm(BD7 ~ BD11, data=data_period2)

summary(linear_model_Y70 )
summary(linear_model_Y00)

# plotting a scatter plot
plot(data_BD7$BD11, data_BD7$BD7, xlab = "BD11", ylab = "BD7", main = "Linear Regression: BD7 vs BD11")

# plotting a linear regression plot for Y70
plot(data_period1$BD11, data_period1$BD7, xlab = "BD11", ylab = "BD7", main = "Linear Regression: BD7 vs BD11 - Y70 Period")
abline(linear_model_Y70, col = "steelblue")

# plotting a linear regression plot for Y00
plot(data_period2$BD11, data_period2$BD7, xlab = "BD11", ylab = "BD7", main = "Linear Regression: BD7 vs BD11 - Y00 Period")
abline(linear_model_Y00, col = "red")

############################## multiple linear regression.  #################

# Adding the remaining four groups to a new data set
data_BD4<-data %>% select(c("Bird","Ladybirds","Macromoths","Grasshoppers_._Crickets"))
BD4<-(colMeans(data_BD4))
BD4

# calculating the mean of data set by row for Bio diversity measure of four remaining groups
data_BD4$BD4<- rowMeans(data_BD4[,1:4])


# Merging the column of calculated measure of BD4
data_BD7$BD4 <- data_BD4$BD4
View(data_BD7)

# fit the model
multilinear_model <- lm(BD4 ~ Bees + Bryophytes + Butterflies + Carabids +
                          Hoverflies + Isopods + Vascular_plants, data=data_BD7)
summary(multilinear_model)
x
#to perform feature selection based on p-values and AIC.
#First, look at the p-values of each predictor in the summary output. You may choose to remove predictors with p-values greater than a specific threshold (e.g., 0.05).
multilinear_model_reduced <- step(multilinear_model, direction="backward", trace=0)
summary(model_multilinear_reduced)

data_7_new<-data %>% select(c("Bees","Bryophytes","Butterflies","Carabids","Hoverflies","Isopods","Vascular_plants"))
View(data_7_new)
data_7_new$BD7 <- rowMeans(data_7_new[,1:7])


########################OPEN ANALYSIS######################
### Investigate the relationship between the Northing and Easting and proportional species richness values for each taxonomic group in the two periods.
# List of taxonomic group column names
taxonomic_groups <-c("Bees","Bryophytes","Butterflies","Carabids","Hoverflies","Isopods","Vascular_plants")
View(taxonomic_groups)

data_7<-data %>% select(c("Bees","Bryophytes","Butterflies","Carabids","Hoverflies","Isopods","Vascular_plants"))
data_7$period<-data$period
data_7$Location<- data$Location
data_7$Easting<- data$Easting
data_7$Northing <- data$Northing
data_7$BD7 <- rowMeans(data_7[,1:7])
View(data_7)
new_data7<- data_7

# Convert the Period column to numeric
new_data7$period <- as.factor(new_data7$period)

new_data7$period <- factor(new_data7$period, levels = c("Y00", "Y70"))
levels(new_data7$period)
# Loop through taxonomic_groups and perform linear regression for each group
for (group in taxonomic_groups) {
  
  # Prepare the dataset
  new_data7$interaction_term <- as.numeric(new_data7$Easting, na.rm = TRUE) * 
    as.numeric(new_data7$Northing, na.rm = TRUE) * 
    as.numeric(new_data7$period, na.rm = TRUE)
  
  # Perform the linear regression analysis
  model <- lm(data[[group]] ~ Easting + Northing + period + interaction_term, data=new_data7)
  
  
  # Display the summary of the model
  cat("\nLinear Regression Model for", group, "\n")
  print(summary(model))
}



#scatter plot showing the relation between the data points and model accuracy
library(ggplot2)

for (group in taxonomic_groups) {
  # Prepare the dataset
  new_data7$interaction_term <- as.numeric(new_data7$Easting, na.rm = TRUE) * 
    as.numeric(new_data7$Northing, na.rm = TRUE) * 
    as.numeric(new_data7$period, na.rm = TRUE)
  
  # Remove NAs from the variables used in the regression model
  model_data <- na.omit(new_data7[, c("Easting", "Northing", "period", "interaction_term", group)])
  
  # Perform the linear regression analysis
  model <- lm(formula(paste(group, "~ Easting + Northing + period + interaction_term")), data = model_data)
  
  # Get observed and predicted values
  observed <- model_data[[group]]
  predicted <- predict(model)
  
  # Create scatter plot of observed vs predicted values
  plot_data <- data.frame(Observed = observed, Predicted = predicted)
  
  scatter_plot <- ggplot(plot_data, aes(x = Observed, y = Predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(x = "Observed", y = "Predicted", title = paste("Scatter Plot for BD7 for Y70 and Y00")) +
    theme_minimal()
  
  # Display the scatter plot
  print(scatter_plot)
}



# line plot for model accuracy for both the periods.


library(ggplot2)

for (group in taxonomic_groups) {
  # Get observed and predicted values
  observed <- new_data7[[group]]
  predicted <- predict(model)
  
  # Create a data frame with observed and predicted values
  plot_data <- data.frame(Period = new_data7$period, Observed = observed, Predicted = predicted)
  
  # Create line plot of observed and predicted values over the period
  line_plot <- ggplot(plot_data, aes(x = Period)) +
    geom_line(aes(y = Observed, color = "Observed")) +
    geom_line(aes(y = Predicted, color = "Predicted")) +
    labs(x = "Period", y = "Value", title = paste("Line Plot:", group)) +
    scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red")) +
    theme_minimal()
  
  # Display the line plot
  print(line_plot)
}




