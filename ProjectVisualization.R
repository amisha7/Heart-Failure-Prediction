df <- read.csv("~/Documents/3.Winter2023/600-Data Mining/Week 10 Final/heart_failure_clinical_records_dataset.csv")

library(tidyverse)
library(skimr)      # skimming data frames
library(patchwork)  # combine separate ggplots into the same graphic
library(corrplot)
library(caret)      # confusionMatrix()
library(rpart)      # Recursive Partitioning and Regression Trees
library(rpart.plot)


palette_ro = c("#ee2f35", "#fa7211", "#fbd600", "#75c731", "#1fb86e", "#0488cf", "#7b44ab")


head(df)

glimpse(df)
str(df)

summary(df)

colSums(is.na(df))
df[duplicated(df), ] 

############# Data visualization

f_features = c("anaemia", "diabetes", "high_blood_pressure", "sex", "smoking", "DEATH_EVENT")

df_n <- df
df <- df %>%
  mutate_at(f_features, as.factor)

############# Binary Features

p1 <- ggplot(df, aes(x = anaemia, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)"))+
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Anaemia") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p2 <- ggplot(df, aes(x = diabetes, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Diabetes") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p3 <- ggplot(df, aes(x = high_blood_pressure, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "High blood pressure") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p4 <- ggplot(df, aes(x = sex, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (Female)", "1 (Male)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Sex") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p5 <- ggplot(df, aes(x = smoking, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Smoking") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p6 <- ggplot(df, aes(x = DEATH_EVENT, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

((p1 + p2 + p3) / (p4 + p5 + p6)) +
  plot_annotation(title = "Distribution of the binary features and DEATH_EVENT")


####### Numeric Features
####### 1. Age

p1 <- ggplot(df, aes(x = age)) + 
  geom_histogram(binwidth = 5, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 5))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  # 5 is binwidth of geom_histogram()
  # binwidth can be calculated from "diff(range(df$age))/20"
  scale_x_continuous(breaks = seq(40, 100, 10)) +
  geom_vline(xintercept = median(df$age), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$age)-5, y = 50,
           label = str_c("Min.     : ", min(df$age),
                         "\nMedian : ", median(df$age),
                         "\nMean    : ", round(mean(df$age), 1),
                         "\nMax.    : ", max(df$age))) +
  labs(title = "Age Distribution") +
  theme_minimal(base_size = 12)

p2 <- ggplot(df, aes(x = age, fill = DEATH_EVENT)) + 
  # geom_histogram(aes(y=..density..), binwidth = 5, colour = "white", position = "identity", alpha = 0.5) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  scale_x_continuous(breaks = seq(40, 100, 10)) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$age), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$age), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$age)-10, y = 0.03,
           label = str_c("Survived median: ", median(filter(df, DEATH_EVENT == 0)$age),
                         "\nDead median: ", median(filter(df, DEATH_EVENT == 1)$age))) +
  
  labs(title = "Relationship between age and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p1 / p2


########### 2. creatinine_phosphokinase
p1 <- ggplot(df, aes(x = creatinine_phosphokinase)) + 
  geom_histogram(binwidth = 100, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 100))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  geom_vline(xintercept = median(df$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$creatinine_phosphokinase)-1000, y = 75,
           label = str_c("Min.     : ", min(df$creatinine_phosphokinase),
                         "\nMedian : ", median(df$creatinine_phosphokinase),
                         "\nMean    : ", round(mean(df$creatinine_phosphokinase), 1),
                         "\nMax.    : ", max(df$creatinine_phosphokinase))) +
  labs(title = "Creatinine Phosphokinase Distribution") +
  theme_minimal(base_size = 12)

p3 <- ggplot(df, aes(x = creatinine_phosphokinase, fill = DEATH_EVENT)) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$creatinine_phosphokinase)-4500, y = 0.7,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$creatinine_phosphokinase),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$creatinine_phosphokinase))) +
  
  labs(title = "Relationship between creatinine_phosphokinase and DEATH_EVENT (log scale)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  scale_x_log10() +
  annotation_logticks()

p1  / p3


#### 3. Ejection Fraction

p1 <- ggplot(df, aes(x = ejection_fraction)) + 
  geom_histogram(binwidth = 1, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 1))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  scale_x_continuous(breaks = seq(10, 80, 10)) +
  geom_vline(xintercept = median(df$ejection_fraction), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$ejection_fraction)-6, y = 45,
           label = str_c("Min.     : ", min(df$ejection_fraction),
                         "\nMedian : ", median(df$ejection_fraction),
                         "\nMean    : ", round(mean(df$ejection_fraction), 1),
                         "\nMax.    : ", max(df$ejection_fraction))) +
  labs(title = "Ejection Fraction Distribution") +
  theme_minimal(base_size = 12)

p2 <- ggplot(df, aes(x = ejection_fraction, fill = DEATH_EVENT)) + 
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  scale_x_continuous(breaks = seq(10, 80, 10)) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$ejection_fraction), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$ejection_fraction), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$age)-26, y = 0.045,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$ejection_fraction),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$ejection_fraction))) +
  
  labs(title = "Relationship between ejection_fraction and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p1 / p2


##### 4. Platelets

p1 <- ggplot(df, aes(x = platelets)) + 
  geom_histogram(binwidth = 20000, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 20000))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  geom_vline(xintercept = median(df$platelets), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$platelets)-100000, y = 40,
           label = str_c("Min.     : ", min(df$platelets),
                         "\nMedian : ", median(df$platelets),
                         "\nMean    : ", round(mean(df$platelets), 1),
                         "\nMax.    : ", max(df$platelets))) +
  labs(title = "Platelets Distribution") +
  theme_minimal(base_size = 12)

p2 <- ggplot(df, aes(x = platelets, fill = DEATH_EVENT)) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$platelets), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$platelets), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$platelets)-180000, y = 0.000005,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$platelets),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$platelets))) +
  
  labs(title = "Relationship between platelets and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p1 / p2


### 5.serum_creatinine

p1 <- ggplot(df, aes(x = serum_creatinine)) + 
  geom_histogram(binwidth = 0.2, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 0.2))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  geom_vline(xintercept = median(df$serum_creatinine), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$serum_creatinine)-1, y = 70,
           label = str_c("Min.     : ", min(df$serum_creatinine),
                         "\nMedian : ", median(df$serum_creatinine),
                         "\nMean    : ", round(mean(df$serum_creatinine), 1),
                         "\nMax.    : ", max(df$serum_creatinine))) +
  labs(title = "Serum Creatinine Distribution") +
  theme_minimal(base_size = 12)


p3 <- ggplot(df, aes(x = serum_creatinine, fill = factor(DEATH_EVENT))) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$serum_creatinine), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$serum_creatinine), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$serum_creatinine)-3.2, y = 3,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$serum_creatinine),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$serum_creatinine))) +
  
  labs(title = "Relationship between serum_creatinine and DEATH_EVENT (log scale)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  scale_x_log10()

p1  / p3


##### 6.serum_sodium

p1 <- ggplot(df, aes(x = serum_sodium)) + 
  geom_histogram(binwidth = 1, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 1))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  scale_x_continuous(breaks = seq(110, 150, 10)) +
  geom_vline(xintercept = median(df$serum_sodium), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = min(df$serum_sodium)+4, y = 36,
           label = str_c("Min.     : ", min(df$serum_sodium),
                         "\nMedian : ", median(df$serum_sodium),
                         "\nMean    : ", round(mean(df$serum_sodium), 1),
                         "\nMax.    : ", max(df$serum_sodium))) +
  labs(title = "Serum Sodium Distribution") +
  theme_minimal(base_size = 12)

p2 <- ggplot(df, aes(x = serum_sodium, fill = DEATH_EVENT)) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  scale_x_continuous(breaks = seq(110, 150, 10)) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$serum_sodium), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$serum_sodium), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = min(df$serum_sodium)+5, y = 0.1,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$serum_sodium),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$serum_sodium))) +
  
  labs(title = "Relationship between serum_sodium and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p1 / p2

###### 7. Time

p1 <- ggplot(df, aes(x = time)) + 
  geom_histogram(binwidth = 10, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 10))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  scale_x_continuous(breaks = seq(0, 300, 50)) +
  geom_vline(xintercept = median(df$time), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$time)-30, y = 22,
           label = str_c("Min.     : ", min(df$time),
                         "\nMedian : ", median(df$time),
                         "\nMean    : ", round(mean(df$time), 1),
                         "\nMax.    : ", max(df$time))) +
  labs(title = "Time Distribution") +
  theme_minimal(base_size = 12)

p2 <- ggplot(df, aes(x = time, fill = DEATH_EVENT)) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  scale_x_continuous(breaks = seq(0, 300, 50)) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$time), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$time), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$time)-50, y = 0.008,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$time),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$time))) +
  
  labs(title = "Relationship between time and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p1 / p2


###### 8. serum_creatinine versus ejection_fraction

ggplot(df, aes(x = serum_creatinine, y = ejection_fraction, colour = DEATH_EVENT)) +
  geom_point() +
  geom_abline(intercept=0, slope=15, colour="grey25", linetype="dashed") +
  scale_colour_manual(values = c(palette_ro[2], palette_ro[7]),
                      name = "parient status\n(DEATH_EVENT)",
                      labels = c("survived", "dead")) +
  labs(title = "Scatterplot of serum_creatinine versus ejection_fraction") +
  theme_minimal(base_size = 12)



####. 9. Correlation matrix
cor(df_n) %>%
  corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 45,
           p.mat = cor.mtest(df_n)$p,
           insig = "p-value", sig.level = -1)




