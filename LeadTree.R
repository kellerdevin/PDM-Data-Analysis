#------------------------------------------------------------
# Load data and packages
#------------------------------------------------------------
options(scipen = 10)
set.seed(1818)

# please install these if running the first time 
# using install.packages()
library(partykit)
library(tidyverse)
library(titanic)
library(PerformanceAnalytics)
library(rpart)       
library(rpart.plot)  
install.packages('visNetwork')
install.packages('sparkline')


#------------------------------------------------------------
# Regression Trees
#------------------------------------------------------------
leads_train <- read_csv("/Users/apple/Desktop/Business Analytics/Lead_Tree.csv")

# see fancy correlation chart of titanic dataset
chart.Correlation(leads_train %>% select(-Converted) %>% 
                    select_if(is.numeric), 
                  pch = 20,
                  histogram = TRUE)


# view the top of the titanic data frame
head(leads_train)

# clean data by creating a binary variable of "survived" vs "did not survive"
# create factors for sex and class of cabin


lead_df <- leads_train %>% as_tibble() %>%
  mutate(Converted = as.factor(Converted),
         Non_Work_Email = as.factor(Non_Work_Email),
         Title_Blank = as.factor(Title_Blank),
         Website_Blank = as.factor(Website_Blank))

# Use the function ctree in rparty to estimate a 
# single regression tree classification model 
lead_tree <- ctree(Converted ~ Non_Work_Email + Title_Blank + Website_Blank, 
                      data =lead_df )

# print the fitted model object 
print(lead_tree)

# Viewing the fitted model is easier 
plot(lead_tree)



# use the ctree_control parameters to adjust stopping criteria
lead_tree2 <- ctree(Converted ~ Non_Work_Email + Title_Blank + Website_Blank, 
                       data = lead_df,
                   control = partykit::ctree_control(alpha=0.1, 
                                                     minbucket = 50))
plot(lead_tree2)



library('rpart')

lead_tree_rpart <- rpart(Converted ~ Non_Work_Email + Title_Blank + Website_Blank,
                    data = lead_df,
                    method = "class",control = list(cp = 0, 
                                                    minsplit = 10,
                                                    maxdepth = 10))
lead_tree_rpart$cptable

# plot the relationship between tree complexity (depth and cp)
# and CV error
plotcp(lead_tree_rpart)



visNetwork::visTree(lead_tree_rpart,
                    nodesPopSize = TRUE,
                    edgesFontSize = 18, 
                    nodesFontSize = 20, 
                    width = "100%",
                    height = "1200px")


