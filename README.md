---
title: "bayes_reoprt_code_py"
output: html_document
date: "2023-05-10"
---


```{r}
require(ggplot2)
require(dplyr)
require(psych)
require(rstanarm)
require("corrplot")
require(psych)
require(car)
require(loo)
require(bayesplot)
```

```{r}
#Load dataset
data_raw <- read.csv("~/Downloads/HEsegDataviz_CollegeData_4-year_v5.csv")
```

############### Data cleaning:

### response variable:
```{r}
# the minority group
minority_group <- "col_hispa" # col_hispa is the percentage of undergraduate students who identify as Hispanic/Latino at each postsecondary institution

# Calculate the change in representation of the minority group over time
changein_representation <- data_raw %>%
  filter(!is.na(get(minority_group))) %>%
  group_by(unitid) %>%
  summarize(start_representation = mean(get(minority_group)[(year >= min(year)) & (year <= min(year) + 4)]),
            end_representation = mean(get(minority_group)[(year >= max(year) - 4) & (year <= max(year))])) %>%
  mutate(change_in_representation = end_representation - start_representation)
changein_representation <- changein_representation %>% dplyr::select(unitid,start_representation,change_in_representation)


changein_representation$change_in_representation <- (changein_representation$change_in_representation)/100
changein_representation$start_representation <- (changein_representation$start_representation)/100


ggplot(changein_representation, aes(x = (change_in_representation))) +
  geom_density(fill = "blue", alpha = 0.5)
```

```{r}
# Create a new variable called "selectivity"
data_raw$selectivity <- ifelse(data_raw$selective == 1, "2", 
                            ifelse(data_raw$more_selective == 1, "3", 
                                   ifelse(data_raw$non_selective == 1, "1", NA)))

# Drop the original "selective," "more_selective," and "non_selective" variables
data_raw <- data_raw %>% dplyr::select(-selective, -more_selective, -non_selective, -public)
```

```{r}
# Exclude columns containing the words other races except Hispanic
data_new <- data_raw %>% dplyr::select(-contains(c("black", "white","asian","pacis","amind","twora")))
data_new
```


```{r}
# Convert states variable to 4 regions

# Define lookup table
region_lookup <- data.frame(state = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont",
                                      "New Jersey", "New York", "Pennsylvania",
                                      "Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin",
                                      "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota",
                                      "Delaware",  "Florida", "Georgia", "Maryland", "North Carolina", "South                                                                   Carolina", "Virginia", "West Virginia",
                                      "Alabama", "Kentucky", "Mississippi", "Tennessee",
                                      "Arkansas", "Louisiana", "Oklahoma", "Texas",
                                      "Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming",
                                      "Alaska", "California", "Hawaii", "Oregon", "Washington")
                            
                            ,
                           region = c("Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast",
                                      "Northeast", "Northeast", "Northeast",
                                      "Midwest", "Midwest", "Midwest", "Midwest", "Midwest",
                                      "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest",
                                      "South", "South", "South", "South", "South", "South", "South", "South",
                                      "South", "South", "South", "South",
                                      "South", "South", "South", "South",
                                      "West", "West", "West", "West", "West", "West", "West", "West",
                                      "West", "West", "West", "West", "West"))

# Merge lookup table with original data
data_new2 <- merge(data_new, region_lookup, by.x = "fips_ipeds", by.y = "state", all.x = TRUE)
data_new2 <- data_new2%>% dplyr::select(-fips_ipeds,-inst_name,-slevel,-fourcat)
factor_cols <- c(  "private", "forprofit", "selectivity","region")
data_new2[factor_cols] <- lapply(data_new2[factor_cols], as.factor)
```
## Create predictor variables at start and change:
```{r}
enrollment <- data_raw %>%
  dplyr::filter(!is.na(total_enrollment)) %>%
  group_by(unitid) %>%
  dplyr::summarize(enrollment_start = mean(total_enrollment[(year >= min(year)) & (year <= min(year) + 4)]),
             enrollment_end = mean(total_enrollment[(year >= max(year) - 4) & (year <= max(year))])) %>%
  mutate(change_in_enrollment = enrollment_end - enrollment_start)

enrollment<- enrollment %>% dplyr::select(unitid,enrollment_start,change_in_enrollment)
```

```{r}
dif_hispa <- data_raw %>%
  dplyr::filter(!is.na(dif_hispa)) %>%
  group_by(unitid) %>%
  dplyr::summarize(dif_hispa_start = mean(dif_hispa[(year >= min(year)) & (year <= min(year) + 4)]), dif_hispa_end= mean(dif_hispa[(year >= max(year) - 4) & (year <= max(year))]))%>% mutate(change_dif_hispa = dif_hispa_end  - dif_hispa_start) 


dif_hispa <-dif_hispa %>% dplyr::select(unitid,dif_hispa_start,change_dif_hispa)
```

```{r}
mkt_hispa <- data_raw %>%
  dplyr::filter(!is.na(mkt_hispa)) %>%
  group_by(unitid) %>%
  dplyr::summarize(mkt_hispa_start = mean(mkt_hispa[(year >= min(year)) & (year <= min(year) + 4)]), mkt_hispa_end= mean(mkt_hispa[(year >= max(year) - 4) & (year <= max(year))]))%>% mutate(change_mkt_hispa = mkt_hispa_end  - mkt_hispa_start) 
mkt_hispa <-mkt_hispa %>% dplyr::select(unitid, mkt_hispa_start,change_mkt_hispa)
```

```{r}
# data for analysis:

data_2009 <- data_new2[data_new$year == 2009,] %>% dplyr::select(contains(c("selec", "private","forprofit","unitid","region")))


merged_data <- merge(changein_representation,enrollment,  by = "unitid")
merged_data <- merge(merged_data,dif_hispa, by = "unitid")
merged_data <- merge(merged_data,mkt_hispa, by = "unitid")
merged_data <- merge(merged_data, data_2009, by = "unitid")

selected_data <- merged_data[,2:13]
```

```{r}
# don't use this chunk when you run analysis. This is meant for data exploration 
colnames(merged_data) <- c("Institution ID", "Representation (Start)", "Change in Representation", "Enrollment (Start)", 
                       "Change in Enrollment", "Disparity-Hispanic (Start)", 
                       "Change in Disparity-Hispanic", "Market-Hispanic (Start)",
                       "Change in Market-Hispanic", "Selectivity (Start)", "Private (Start)", "For-Profit (Start)","Region")
selected_data <- merged_data[,2:9]
colnames(merged_data)
```


########### Data exploration:

# Figure 1:
```{r}

# Create a new data frame with the percentage of each race by year
race_perc <- data_raw %>%
  group_by(year) %>%
  summarize(col_white = mean(col_white, na.rm = TRUE),
            col_hispa = mean(col_hispa, na.rm = TRUE),
            col_black = mean(col_black, na.rm = TRUE),
            col_asian = mean(col_asian, na.rm = TRUE),
            col_amind = mean(col_amind, na.rm = TRUE),
            col_pacis = mean(col_pacis, na.rm = TRUE),
            col_twora = mean(col_twora, na.rm = TRUE)) %>%
  pivot_longer(cols = -year, names_to = "race", values_to = "percentage")

# Plot the stacked bar chart
ggplot(race_perc, aes(x=year, y=percentage, fill=race)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Percentage", fill="Race") +
  ggtitle("Percentage of Each Race in US Postsecondary Education Institutions by Year") +
  theme_bw()
```

 # Table 1: 
```{r}
require(table1)
# Subset the data for year 2009 and 2017
data_new3 <- subset(data_new2, year == 2009 | year == 2017)
data_new3 <- data_new3 %>% dplyr::select(-unitid)
colnames(data_new3) <- 
  
colnames(data_new3) <- c("Year",  "Private", "For Profit", "Total Enrollment", "Representation- Hispanic", "Marketing - Hispanic ", " Disparity- Hispanic", "Selectivity", "Region")

# Create Table 1 for predictors at year 2009 vs. 2017
table1:: table1(~.|Year,data_new3) 
```

# Figure 2:

```{r}
par(mfrow = c(1, 3))  
data_raw <- read.csv("~/Downloads/HEsegDataviz_CollegeData_4-year_v5.csv") # need to include all races again
# Boxplot 1: Private vs. Change in Representation
boxplot(change_in_representation ~ private, data = merged_data, 
        xlab = "Private Institution", ylab = "Change in Representation",
        main = "Private vs. Change in Representation",
        cex.axis = 1.2)  # Increase the font size of the axes

# Boxplot 2: For profit vs. Change in Representation
boxplot(change_in_representation ~ forprofit, data = merged_data, 
        xlab = "For-profit Institution", ylab = "Change in Representation",
        main = "For-profit vs. Change in Representation",
        cex.axis = 1.2)  # Increase the font size of the axes

# Boxplot 3: Selectivity level vs. Change in Representation
boxplot(change_in_representation ~ selectivity, data = merged_data, 
        xlab = "Selectivity Level of Institution", ylab = "Change in Representation",
        main = "Selectivity Level vs. Change in Representation")
```

# Correlation Matrix:
```{r}
# Correlation matrix

cor <- cor(char2numeric(selected_data), method = "pearson", use = "pairwise.complete.obs")

# Visualizing correlation matrix 
require(ztable)
options(ztable.type="html")
ztable(cor) %>%makeHeatmap()%>%ztable2flextable()
```
# Model selection:

```{r}

# Model 1:
library(rstan)

# Fit the model that includes all variables using stan_glm with gamma distribution and default prior 
model1 <- stan_glm(abs(change_in_representation+ 0.0001)~ start_representation + enrollment_start + change_in_enrollment + dif_hispa_start + change_dif_hispa + mkt_hispa_start + change_mkt_hispa + selectivity + private + forprofit+region+change_dif_hispa*region,
                  data = selected_data,
                  family = Gamma(link = "log"),
                  chains = 4,
                  iter = 10000)

# Summarize the model
as.data.frame(summary(model1))
data.frame(summary(model1, probs = c(0.025, 0.975)))[1:18, c(1, 4:5)]
pp_check(model1, plotfun = "dens_overlay") + xlim(c(0, 1))

```

# VIF 1:
```{r}
vif_values <- vif(model1)
```

```{r}

# Model 2: exclude some of the variables with high VIF
# Fit the model using stan_glm with gamma distribution and default priors 
model2 <- stan_glm(abs(change_in_representation+ 0.0001)~ start_representation + enrollment_start + change_in_enrollment + selectivity + private + forprofit+region+change_dif_hispa*region,
                  data = selected_data,
                  family = Gamma(link = "log"),
                  chains = 4,
                  iter = 10000)

# Summarize the model
summary(model2)

data.frame(summary(model2, probs = c(0.025, 0.975)))[1:18, c(1, 4:5)]
```

# VIF 2:
```{r}
vif_values <- vif(model2)
```

```{r}

# Model 3:
# Fit the model using stan_glm with gamma distribution and default priors 
model3 <- stan_glm(abs(change_in_representation+ 0.0001)~ enrollment_start + change_in_enrollment + selectivity + private + forprofit+region+change_dif_hispa*region,
                  data = selected_data,
                  family = Gamma(link = "log"),
                  chains = 4,
                  iter = 10000)

# Summarize the model
summary(model3)

data.frame(summary(model3, probs = c(0.025, 0.975)))[1:18, c(1, 4:5)]
```

# WAIC and LOOIC:
```{r}

# Model 1
waic1 = waic(model1)
loo1 = loo(model1)
# Model 2
waic2 = waic(model2)
loo2 = loo(model2)
# Model 3
waic3 = waic(model3)
loo3 = loo(model3)

compare_df <- data.frame(
Model = c("Model 1", "Model 2","Model 3"),
WAIC = c(waic1$waic, waic2$waic,waic3$waic),
LOOIC = c(loo1$looic, loo2$looic,loo3$looic))
```


# Convergence:
```{r}
# Create trace plots for each chain
mcmc_trace(model3)

# Effective sample size
mcmc_samples <- as.matrix(model3)
effectiveSize(mcmc_samples)
# Heidelberg-Welch diagnostic
heidel_diag <- coda::heidel.diag(mcmc_samples)
```

# Model Assessing:
```{r}
# density of y vs yrep
pp_check(model3, plotfun = "dens_overlay") + xlim(c(0, 1))
# histograms of y and yrep
pp_check(model3, plotfun = "hist")
# plot of test statistic
pp_check(model3, plotfun = "stat", stat = "sd") + xlim(c(0, 1))
# boxplots of y vs yrep
pp_check(model3, plotfun = "boxplot")
# scatterplot of y vs mean(y_rep)
pp_check(model3, plotfun = "scatter_avg")
# scatterplot of y vs realization of yrep
pp_check(model3, plotfun = "scatter", nreps = 9)
# posterior intervals for yrep vs y
pp_check(model3, plotfun = "intervals")
```


# effect plots:


```{r}
# For "enrollment_start"
ggpredict(model3, terms = "enrollement_start")

# For "change_in_enrollment"
ggpredict(model3, terms = "change_in_enrollement")

# For "selectivity2"
ggpredict(model3, terms = "selectivity2")

# For "selectivity3"
ggpredict(model3, terms = "selectivity3")

# For "private1"
ggpredict(model3, terms = "private1")

# For "forprofit1"
ggpredict(model3, terms = "forprofit1")

# For "regionNortheast"
ggpredict(model3, terms = "regionNortheast")

# For "regionSouth"
ggpredict(model3, terms = "regionSouth")

# For "regionWest"
ggpredict(model3, terms = "regionWest")

# For "change_dif_hispa"
ggpredict(model3, terms = "change_dif_hispa")

# For "regionNortheast:change_dif_hispa"
ggpredict(model3, terms = "regionNortheast:change_dif_hispa")

# For "regionSouth:change_dif_hispa"
ggpredict(model3, terms = "regionSouth:change_dif_hispa")

# For "regionWest:change_dif_hispa"
ggpredict(model3, terms = "regionWest:change_dif_hispa")


```

























```{r}
 # Plot the effect for each variable
plot(effect_enrollment_start)
plot(effect_change_in_enrollment)
plot(effect_change_dif_hispa)
```


