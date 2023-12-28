#IMPORT RELEVANT LIBRARIES---------------------------------------------------------------------------------------------------------------------------------
library(tidyverse) 
library(psych);
library("ggpubr");
library(MASS);
library(dplyr)
library(corrplot) 
library(stats)
library(plotly)
library(ggrepel)
library(RColorBrewer)
library(dplyr)
library(viridis)
library(rpart)
library(e1071)
library(magrittr)
library(dplyr) 
library(e1071)
library(nnet)
library(caret)
library(superml)
library(dbscan)
library(tidyr)
library(randomForest)
library(caret)
library(cluster)
library(FactoMineR)  # for PCA on mixed data
library(tree) 
library(rpart) 
library(rpart.plot)
library(MASS)
library(klaR)
library(plotly)
library(tidyverse)
library(data.table)
library(gridExtra)
library(knitr)
library(ggplot2)

#READ DATA------------------------------------------------------------------------------------------------------------------------------------------------

data <- read.csv("E:/MASTERS/Term 2-Fall/MGSC-661-MultiVariate/Final Project/athlete_events.csv", header = TRUE, sep = ",")
View(data)
str(data)

#SECTION 1: DATA PREPROCESSING----------------------------------------------------------------------------------------------------------------------------

#Identifying the types of variables
glimpse(data)
summary(data)

#1.1: Handling Missing Values----------------------------------------------------------------------------------------------------------------------------

#Check for missing values
colSums(is.na(data))

#Drop rows with no height, weight or Medal
sum(which(is.na(data$Height) & is.na(data$Weight) & is.na(data$Medal)))
data <- data[-c(which(is.na(data$Height) & is.na(data$Weight) & is.na(data$Medal))),]

#Handle missing values of Age, Height and Weight
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = TRUE)
data$Height[is.na(data$Height)] <- mean(data$Height, na.rm = TRUE)
data$Weight[is.na(data$Weight)] <- mean(data$Weight, na.rm = TRUE)

#Handle missing values of Medal. NA to No model
data$Medal[is.na(data$Medal)] <- "No Medal"

#1.2: Handling Duplicate Values----------------------------------------------------------------------------------------------------------------------------

# Find duplicate rows (excluding the first occurrence)
duplicate_rows <- duplicated(data)

# Display the duplicate rows
data_duplicates <- data[duplicate_rows, ]
print(data_duplicates)
head(data_duplicates)

# Remove duplicate rows, keep first occurrence
data <- data[!duplicated(data), ]

#find top sport and count
top_sport <- data %>%
  group_by(Sport) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>% # Calculate the percentage
  arrange(desc(count)) %>%
  head(10)
top_sport


#1.3: Feature Engineering----------------------------------------------------------------------------------------------------------------------------
#Check for categorical variables
str(data)

#Convert age, height and weight to int
data$Age <- as.integer(data$Age)
data$Height <- as.integer(data$Height)
data$Weight <- as.integer(data$Weight)

#Handle Categorical Variables

#Medal
data$Medal <- factor(data$Medal, levels = c("No Medal", "Bronze", "Silver", "Gold"))

#Sex
data$Sex <- factor(data$Sex)

#columns of data
names(data)
sapply(data, function(x) length(unique(x)))

#Making region column

data_region<-read.csv("E:/MASTERS/Term 2-Fall/MGSC-661-MultiVariate/Final Project/noc_regions.csv", header = TRUE, sep = ",")
View(data)
# Merging data frames
data_merge <- merge(data, data_region, by = "NOC", all.x = TRUE)

#find null values
result <- data_merge %>%
  filter(is.na(region)) %>%
  select(NOC, Team) %>%
  unique()
result

#Rename
data_merge <- data_merge %>% rename(Country = region)

#Manualy fill 3 null categories
data_merge$Country <- ifelse(data_merge$NOC == 'SGP', 'Singapore', data_merge$Country)
data_merge$Country <- ifelse(data_merge$NOC == 'ROT', 'Refugee Olympic Athletes', data_merge$Country)
data_merge$Country <- ifelse(data_merge$NOC == 'TUV', 'Tuvalu', data_merge$Country)

na_rows <- data_merge[na_indices, ]
na_rows


#Clean data_selected 
data_selected <- NULL

# Selecting relevant columns
data_selected <- data_merge[c("Sex", "Age", "Height", "Weight", "Year", "Season","Country", "Sport", "Medal")]

View(data_selected)
names(data_selected)
summary(data_selected)

#count unique values of each column
sapply(data_selected, function(x) length(unique(x)))

#data encoding
data_selected$Sport <- as.factor(data_selected$Sport)
data_selected$Season <- as.factor(data_selected$Season)
data_selected$Country <- as.factor(data_selected$Country)


#Normalizing Numerical data

# Defining the preprocessing method
preProcValues <- preProcess(data_selected[, c("Age", "Height", "Weight")], method = c("range"))

# Transforming the data
data_norm <- predict(preProcValues, data_selected[, c("Age", "Height", "Weight")])

# Adding the normalized columns back to the original dataframe
data_selected$Age <- data_norm$Age
data_selected$Height <- data_norm$Height
data_selected$Weight <- data_norm$Weight

#Create winner column
data_selected$Winner <- ifelse(data_selected$Medal == "No Medal", 0, 1)

#Drop medal column
data_selected$Medal <- NULL

# Convert 'Season' to numeric (0 for 'Summer', 1 for 'Winter')
data_selected$Season<- ifelse(data_selected$Season == "Summer", 0, 1)


#SECTION 2:.EDA----------------------------------------------------------------------------------------------------------------------------

eda<-data_merge
attach(eda)
##################################GRAPH1##############################################

#Create winner column
eda$Winner <- ifelse(eda$Medal == "No Medal", 0, 1)

# Create a new categorical variable
eda$Category <- ifelse(eda$Medal == "No Medal", "Non-Winner", 
                       ifelse(eda$Medal %in% c("Gold", "Silver", "Bronze"), "Winner", "Participant"))

# Count the number of athletes in each category per year
counts <- eda %>% 
  group_by(Year, Category) %>%
  summarize(Athletes = n_distinct(ID))

# Plot with smooth lines and a solid line for participants
p1 <- ggplot(counts, aes(x=Year, y=Athletes, group=Category, color=Category)) +
  geom_point(size=2)+
  geom_smooth(data=subset(counts, Category != "Participant"), se=FALSE) +  # Smooth lines for winners and non-winners
  geom_line(data=subset(counts, Category == "Participant")) +  # Solid line for participants
  scale_color_manual(values=c("darkorange","darkblue","green")) + # Assign colors to each category
  xlab("") +  
  annotate("text", x=c(1916,1942), y=c(10000,10000),
           label=c("WWI","WWII"), size=4, color="red") +
  geom_segment(mapping=aes(x=1914,y=8000,xend=1918,yend=8000), color="red", size=2) +
  geom_segment(mapping=aes(x=1939,y=8000,xend=1945,yend=8000), color="red", size=2) +
  labs(color = "Category")  # Label for legend
print(p1)


##################################GRAPH2##############################################

# count number of medals awarded in Athletics (excluding No-medal)
medal_counts_athletics <- eda %>% 
  filter(Sport == "Athletics" & !is.na(Medal) & Medal %in% c("Gold", "Silver", "Bronze")) %>%
  group_by(Team, Medal) %>%
  summarize(Count=length(Medal), .groups = 'drop') 

# calculate total medal count per team and get the top 10 teams
top_teams_athletics <- medal_counts_athletics %>%
  group_by(Team) %>%
  summarize(Total=sum(Count), .groups = 'drop') %>%
  arrange(desc(Total)) %>%
  slice(1:10) %>% # Select top 10 teams
  select(Team)

# filter medal_counts_athletics for only the top teams
medal_counts_athletics <- medal_counts_athletics %>%
  filter(Team %in% top_teams_athletics$Team)

# order Team by total medal count
medal_counts_athletics$Team <- factor(medal_counts_athletics$Team, levels=top_teams_athletics$Team)

# plot
ggplot(medal_counts_athletics, aes(x=Team, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold4", "gray70", "gold1")) +
  ggtitle("Historical medal counts in Athletics (Top 10 Teams)") +
  theme(plot.title = element_text(hjust = 0.5))


##################################GRAPH3##############################################

# count number of medals awarded in Gymnastics (excluding No-medal)
medal_counts_gymnastics <- eda %>% 
  filter(Sport == "Gymnastics" & !is.na(Medal) & Medal %in% c("Gold", "Silver", "Bronze")) %>%
  group_by(Team, Medal) %>%
  summarize(Count=length(Medal), .groups = 'drop') 

# calculate total medal count per team and get the top 10 teams
top_teams_gymnastics <- medal_counts_gymnastics %>%
  group_by(Team) %>%
  summarize(Total=sum(Count), .groups = 'drop') %>%
  arrange(desc(Total)) %>%
  slice(1:10) %>% # Select top 10 teams
  select(Team)

# filter medal_counts_gymnastics for only the top teams
medal_counts_gymnastics <- medal_counts_gymnastics %>%
  filter(Team %in% top_teams_gymnastics$Team)

# order Team by total medal count
medal_counts_gymnastics$Team <- factor(medal_counts_gymnastics$Team, levels=top_teams_gymnastics$Team)

# plot
ggplot(medal_counts_gymnastics, aes(x=Team, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold4", "gray70", "gold1")) +
  ggtitle("Historical medal counts in Gymnastics (Top 10 Teams)") +
  theme(plot.title = element_text(hjust = 0.5))

##################################GRAPH4##############################################

# Calculate average height of gymnasts over time
avg_height_a <- eda %>% 
  filter(Sport == "Athletics" & !is.na(Height)) %>%
  group_by(Year) %>%
  summarize(AvgHeight = mean(Height, na.rm = TRUE)) %>%
  ungroup()

# plot
p4<-ggplot(avg_height_a, aes(x=Year, y=AvgHeight)) +
  geom_point(size=2) +
  geom_line() +scale_color_manual(values=c("darkblue"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Average Height of Athletics Over Time", y="Average Height (cm)", x="Year")


# Calculate average weight of gymnasts over time
avg_weight_a <- eda %>% 
  filter(Sport == "Athletics" & !is.na(Weight)) %>%
  group_by(Year) %>%
  summarize(AvgWeight = mean(Weight, na.rm = TRUE)) %>%
  ungroup()


# plot
p5<-ggplot(avg_weight_a, aes(x=Year, y=AvgWeight)) +
  geom_point(size=2) +
  geom_line() +scale_color_manual(values=c("orange"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Average Weight of Athletics Over Time", y="Average Weight (kg)", x="Year")


# Calculate average age of gymnasts over time
avg_age_a <- eda %>% 
  filter(Sport == "Athletics" & !is.na(Age)) %>%
  group_by(Year) %>%
  summarize(AvgAge = mean(Age, na.rm = TRUE)) %>%
  ungroup()


# plot
p6<-ggplot(avg_age_a, aes(x=Year, y=AvgAge)) + 
  geom_point(size=2) +
  geom_line() +scale_color_manual(values=c("purple"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Average Age of Athletics Over Time", y="Average Age (kg)", x="Year")



grid.arrange(p4, p5, p6, ncol=1)
##################################GRAPH5##############################################

library(ggplot2)
library(dplyr)
library(ggplot2)
library(dplyr)
library(grid)


# Prepare the data for winners only
gym_winners <- eda %>%
  filter(Sport == "Gymnastics", Country %in% c("Russia","USA","China", "Romania"), Medal %in% c("Gold", "Silver", "Bronze")) %>%
  group_by(Year, Country) %>%
  summarize(Winners = n(), .groups = 'drop')  # Count the number of medal winners

# Factor the Country variable to control plot order
gym_winners$Country <- factor(gym_winners$Country, levels = c("Russia","USA","China", "Romania"))

# Create the plot
p <- ggplot(gym_winners, aes(x = Year, y = Winners, group = Country, color = Country)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Country, ncol = 2) +  # Use facet_wrap for a fixed number of columns
  scale_color_manual(values = c("Russia" = "blue", "China" = "red", "Romania" = "orange", "USA" = "purple")) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), # Add borders
        strip.background = element_rect(fill = "white"), # Background color for facet labels
        strip.text = element_text(size = 10), # Text size for facet labels
        plot.title = element_text(hjust = 0.5, size = 14), # Center the main title and adjust size
        plot.margin = unit(c(1, 1, 1, 1), "lines")) + # Adjust plot margins
  labs(title = "Number of Gymnastic Medal Winners Over Time",
       y = "Number of Winners",
       x = "Year",
       color = "Country")

# Print the plot
print(p)


#############################Graph 6#############################

# Pivot the data to long format for faceting
long_data <- eda %>%
  filter(Winner == 1) %>%
  select(Age, Height, Weight) %>%
  pivot_longer(cols = c(Age, Height, Weight), names_to = "Attribute", values_to = "Value")

p10 <- ggplot(long_data, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "darkblue", color = "black") + # Adjust bin count as needed
  facet_wrap(~ Attribute, scales = "free", labeller = labeller(Attribute = c(Age = "Age", Height = "Height", Weight = "Weight"))) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), # Add black borders
        strip.text = element_text(face = "bold")) + # Bold facet labels
  labs(x = NULL, y = "Frequency") # Remove x label, as it is redundant with facet labels

# Print the plot
print(p10)



#SECTION 3: MODEL EXPLORATION----------------------------------------------------------------------------------------------------------------------------



#Heirachial Clustering------------------------------------------------------------------------------------------------


######################ATHLETICS DATA############################################
# Sample a subset of  data
#select data with "Athletics" from  sport
athletics_data <- data_selected %>% 
  filter(Sport == "Athletics"& Winner == 1)
athletics_data$Winner<-NULL
athletics_data$Sport<-NULL
athletics_data$Season<-NULL
set.seed(123)  # for reproducibility
data_sample <- athletics_data[sample(nrow(athletics_data), 3000), ]  

# Calculate Gower distance
library(proxy)
library(cluster)
gower_dist <- daisy(data_sample, metric = "gower")

# Perform hierarchical clustering using the complete linkage method
hc <- hclust(gower_dist, method = "complete")

# Plot the dendrogram
plot(hc)

#cut the tree
clusters <- cutree(hc, 2)

# Add the cluster assignments back to your original data
data_sample$cluster <- clusters

# aggregate data by clusters to find mean Age, Height, and Weight for instance
aggregate(cbind(Age, Height, Weight) ~ cluster, data = data_sample, mean)

# Summarize 'Country' by cluster
Country_summary <- aggregate(Country ~ cluster, data = data_sample, function(x) {
  return(table(factor(x, levels = unique(data_sample$Country))))
})


# print the summaries to see the distribution
print(Country_summary)

# For a categorical variable like 'Country'
Country_distribution <- aggregate(Country ~ cluster, data = data_sample, FUN = table)
Country_distribution
# percentages instead of counts
Country_distribution_percent <- aggregate(Country ~ cluster, data = data_sample, FUN = function(x) {
  tab <- table(x)
  return(100 * tab / sum(tab))
})
Country_distribution_percent


# Count the number of athletes in each cluster by Country
athlete_counts <- data_sample %>%
  filter(Country %in% c("USA", "Finland", "Russia","China")) %>%
  group_by(Country, cluster) %>%
  summarise(AthleteCount = n())

# Create the grouped bar chart
ggplot(athlete_counts, aes(x = Country, y = AthleteCount, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Athletics Winners by Cluster in Selected Countries",
       x = "Country",
       y = "Count of Athletes",
       fill = "Cluster") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Calculate summary statistics for continuous variables by cluster
continuous_distribution <- data_sample %>%
  group_by(cluster) %>%
  summarise(
    Age_Mean = mean(Age, na.rm = TRUE),
    Height_Mean = mean(Height, na.rm = TRUE),
    Weight_Mean = mean(Weight, na.rm = TRUE),
    Age_SD = sd(Age, na.rm = TRUE),
    Height_SD = sd(Height, na.rm = TRUE),
    Weight_SD = sd(Weight, na.rm = TRUE)
  )
continuous_distribution
# Count the number of occurrences of each year within each cluster
year_counts <- aggregate(Year ~ cluster, data = data_sample, FUN = function(x) {
  return(table(factor(x, levels = unique(data_sample$Year))))
})
year_counts

# Aggregate data to get the count of athletes per year, per cluster
year_cluster_counts <- data_sample %>%
  group_by(Year, cluster) %>%
  summarise(AthleteCount = n(), .groups = 'drop')

# Create the line chart

ggplot(year_cluster_counts, aes(x = Year, y = AthleteCount, color = as.factor(cluster))) +
  geom_smooth(method = "loess", se = FALSE) + # se = FALSE removes the confidence interval shading
  labs(title = "Athlete Count Over Years by Cluster",
       x = "Year",
       y = "Count of Athletes",
       color = "Cluster") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

#Significant difference in AGE
# Perform Mann-Whitney U Test for Age
wilcox.test(Age ~ cluster, data = data_sample)
# Perform Welch Two Sample t-test for Age
t.test(Age ~ cluster, data = data_sample)

#Significant difference in Height
# Perform Mann-Whitney U Test for Height
wilcox.test(Height ~ cluster, data = data_sample)
# Perform Welch Two Sample t-test for Age
t.test(Height ~ cluster, data = data_sample)

#Significant difference in Weight
# Perform Mann-Whitney U Test for Weight
wilcox.test(Weight ~ cluster, data = data_sample)
# Perform Welch Two Sample t-test for Weight
t.test(Weight ~ cluster, data = data_sample)


# Reshape the data into a long format, excluding 'Age'
long_data <- gather(data_sample, key = "attribute", value = "value", Height, Weight)

# Create the plot
ggplot(long_data, aes(x = factor(cluster), y = value, fill = factor(cluster))) +
  geom_boxplot() +
  facet_wrap(~attribute, scales = "free") +
  scale_fill_manual(values = c("darkblue", "orange")) +
  labs(title = "Distribution of Height and Weight by Cluster",
       x = "Cluster",
       y = "Value") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add black border around panels
  )





######################Gymnastics########################################################################################


#select data with "Athletics" from  sport
gymnastics_data <- data_selected %>% 
  filter(Sport == "Gymnastics" & Winner==1)
gymnastics_data$Season<-NULL
gymnastics_data$Winner<-NULL
gymnastics_data$Sport<-NULL

# Calculate Gower distance

gower_dist <- daisy(gymnastics_data, metric = "gower")

# Perform hierarchical clustering using the complete linkage method
hc <- hclust(gower_dist, method = "complete")

# Plot the dendrogram
plot(hc)

#cut the tree
cutree(hc,2) 

# Let's say you stored the result of cutree in a variable called 'clusters'
clusters <- cutree(hc, 2)

# Add the cluster assignments back to your original data
gymnastics_data$cluster <- clusters

View(gymnastics_data)
# Now, you can aggregate data by clusters to find mean Age, Height, and Weight for instance
aggregate(cbind(Age, Height, Weight) ~ cluster, data = gymnastics_data, mean)

# Summarize 'Country' by cluster
Country_summary <- aggregate(Country ~ cluster, data = gymnastics_data, function(x) {
  return(table(factor(x, levels = unique(gymnastics_data$NOC))))
})

# For a categorical variable like 'Country'
Country_distribution <- aggregate(Country ~ cluster, data = gymnastics_data, FUN = table)
Country_distribution
# If you want percentages instead of counts
Country_distribution_percent <- aggregate(Country ~ cluster, data = gymnastics_data, FUN = function(x) {
  tab <- table(x)
  return(100 * tab / sum(tab))
})
Country_distribution_percent

# Calculate summary statistics for continuous variables by cluster
continuous_distribution <- gymnastics_data %>%
  group_by(cluster) %>%
  summarise(
    Age_Mean = mean(Age, na.rm = TRUE),
    Height_Mean = mean(Height, na.rm = TRUE),
    Weight_Mean = mean(Weight, na.rm = TRUE),
    Age_SD = sd(Age, na.rm = TRUE),
    Height_SD = sd(Height, na.rm = TRUE),
    Weight_SD = sd(Weight, na.rm = TRUE)
  )
continuous_distribution

# Count the number of occurrences of each year within each cluster
year_counts <- aggregate(Year ~ cluster, data = gymnastics_data, FUN = function(x) {
  return(table(factor(x, levels = unique(gymnastics_data$Year))))
})
year_counts

#plot
long_data <- gather(gymnastics_data, key = "attribute", value = "value", Age)

# Create the plot
ggplot(long_data, aes(x = factor(cluster), y = value, fill = factor(cluster))) +
  geom_boxplot() +
  facet_wrap(~attribute, scales = "free") +
  scale_fill_manual(values = c("darkblue", "orange")) +
  labs(title = "Distribution of Age by Cluster",
       x = "Cluster",
       y = "Value") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add black border around panels
  )


# Load ggplot2
library(ggplot2)

# Count the number of athletes in each cluster by Country
athlete_counts <- gymnastics_data %>%
  filter(Country %in% c("Russia", "Romania", "Japan","China")) %>%
  group_by(Country, cluster) %>%
  summarise(AthleteCount = n())

# Create the grouped bar chart
ggplot(athlete_counts, aes(x = Country, y = AthleteCount, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Gymnastics Winners by Cluster in Selected Countries",
       x = "Country",
       y = "Count of Athletes",
       fill = "Cluster") +
  theme_minimal() +
  scale_fill_manual(values = c("darkblue", "orange")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))  # Add panel border here

#Significant difference in AGE
# Perform Mann-Whitney U Test for Age
wilcox.test(Age ~ cluster, data = gymnastics_data)
# Perform Welch Two Sample t-test for Age
t.test(Age ~ cluster, data = gymnastics_data)

#Significant difference in Height
# Perform Mann-Whitney U Test for Height
wilcox.test(Height ~ cluster, data = gymnastics_data)
# Perform Welch Two Sample t-test for Age
t.test(Height ~ cluster, data = gymnastics_data)

#Significant difference in Weight
# Perform Mann-Whitney U Test for Weight
wilcox.test(Weight ~ cluster, data = gymnastics_data)
# Perform Welch Two Sample t-test for Weight
t.test(Weight ~ cluster, data = gymnastics_data)


###PCA---------------------------------------------------------------------------------------------------------------------------------------------------------------

##################################Athletics

#make new data set for PCA
names(data_selected)
View(data_sample)
data_sample_pca<-data_selected

#drop season and cluster columns
data_sample_pca$Season<-NULL
data_sample_pca$Country<-NULL

# Convert 'Sex' to numeric (0 for 'Male', 1 for 'Female')
data_sample_pca$Sex<- ifelse(data_sample_pca$Sex == "M", 0, 1)

#Filtering Aethlatics
# Filtering Athletics and Winners
pca_athletics <- data_sample_pca %>%
  filter(Sport == "Athletics" & Winner == 1)

#remove sport
pca_atheletics$Sport<-NULL
pca_atheletics$Winner<-NULL
#PCA
pca_athelatics=prcomp(pca_atheletics, scale=TRUE) 
pca_athelatics


#Plot PCA
library(ggfortify)
library(ggplot2)

autoplot(pca_athelatics, loadings = TRUE, 
         loadings.label = TRUE, loadings.label.size = 5,  # Increase font size
         colour = 'grey',  # Change points to grey
         alpha = 0.5)  # Adjust transparency of points

#######################Gymnastics

#Filtering Gymnastics
# Filtering Gymnastics and Winners
pca_Gymnastics <- data_sample_pca %>%
  filter(Sport == "Gymnastics" & Winner == 1)

#remove sport
pca_Gymnastics$Sport<-NULL
pca_Gymnastics$Winner<-NULL

#PCA
pca_Gymnastics=prcomp(pca_Gymnastics, scale=TRUE) 
pca_Gymnastics


#Plot PCA

autoplot(pca_Gymnastics, loadings = TRUE, 
         loadings.label = TRUE, loadings.label.size = 5,  # Increase font size
         colour = 'grey',  # Change points to grey
         alpha = 0.5)  # Adjust transparency of points
