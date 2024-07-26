

#-------------------------------------------------------------------------------
#-------------------------- Read & Install Packages ----------------------------
#-------------------------------------------------------------------------------


install.packages("emmeans")
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("multcomp")
install.packages("gridExtra")
install.packages("patchwork")
install.packages("purrr")

library(ggpubr)
library(ggplot2)
library(dplyr)
library(emmeans)
library(tidyr)
library(multcomp)
library(gridExtra)
library(patchwork)
library(purrr)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXX---------------------- Total Core Data (3 Types) -----------------------XXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

data <-read.csv("Core_Final.csv")
print(data) #Check for errors

#------------------------------ BD Graph (3 types) -----------------------------

# Calculate mean and standard error for each sampling depth and site type
mean_BD_data <- data %>% group_by(SamplingDepth, SpecificType) %>%
  summarize(Mean_BulkDensity = mean(BulkDensity),
            SE_BulkDensity = sd(BulkDensity) / sqrt(n()))  # Calculate standard error

# Graph
p1 <- ggplot(mean_BD_data, aes(x = Mean_BulkDensity, y = SamplingDepth, color = SpecificType)) +
  geom_errorbarh(aes(xmin = Mean_BulkDensity - SE_BulkDensity, xmax = Mean_BulkDensity + 
                       SE_BulkDensity, height = 0), size = 0.5) +  # Error bars
  geom_point(size = 3) +  # Mean points
  labs(x = "Bulk Density (g cm^-3)", y = "Sampling Depth (cm)", color = "Site Type") +  # Axis and legend label
  scale_x_continuous(limits = c(0, 1.6)) +  
  theme_classic2() +  
  scale_y_reverse(limits=c(200,0))+
  guides(color = "none")  # Remove legend for the combination graph


#------------------------------ C Graph (3 types) ------------------------------

# Calculate mean and standard error for each sampling depth and site type
mean_C_Cdata <- Cdata %>% group_by(SamplingDepth, SpecificType) %>%
  summarize(Mean_C = mean(C),
            SE_C = sd(C) / sqrt(n()))  # Calculate standard error

# Graph
p2 <- ggplot(mean_C_Cdata, aes(x = Mean_C, y = SamplingDepth, color = SpecificType)) +
  geom_errorbarh(aes(xmin = Mean_C - SE_C, xmax = Mean_C + SE_C, 
                     height = 0), size = 0.5) +  # Error bars
  geom_point(size = 3) +  # Mean points
  labs(x = "Percent Carbon (%)", y = "", color = "Site Type") +  # Axis and legend label
  scale_x_continuous(limits = c()) +  
  theme_classic2() +  
  scale_y_reverse(limits = c(200,0)) +
  guides(color = "none")  # Remove legend for the combination graph


#------------------------------ CD Graph (3 types) -----------------------------

# Calculate mean and standard error for each sampling depth and site type
mean_se_data <- CDdata %>% group_by(SamplingDepth, SpecificType) %>%
  summarize(Mean_CDensity = mean(CDensity),
            SE_CDensity = sd(CDensity) / sqrt(n()))  # Calculate standard error

# Graph
p3 <- ggplot(mean_se_data, aes(x = Mean_CDensity, y = SamplingDepth, color = SpecificType)) +
  geom_errorbarh(aes(xmin = Mean_CDensity - SE_CDensity, xmax = Mean_CDensity + SE_CDensity, height = 0), size = 0.5) +  # Error bars
  geom_point(size = 3) +  # Mean points
  labs(x = "Carbon Density (Mg ha^-1)", y = "", color = "Site Type") +  # Axis and legend labels
  scale_x_continuous(limits = c(0, 150)) +
  theme_classic2() +  
  scale_y_reverse(limits=c(200,0)) +
  theme(legend.position = c(0.4, 0.4),   #legend inside graph.
      legend.justification = c(0, 0),   # Anchor point thingy?
      legend.box.background = element_rect(color = "black", size = 0.5))  # adds a box around the legend :)


#------------------------------ Combine Graphs ---------------------------------

  #All 3 on one print out     #NICE
grid.arrange(p1, p2, p3, ncol = 3)






#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXX-------------------------------- Anovas --------------------------------XXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#Load in Data

what1 <- read.csv("New.csv")

#//////////////////////////// Bulk Densities Comparisons \\\\\\\\\\\\\\\\\\\\\\\
#---------------------------------- BD Total Core ------------------------------

#anova
resultBD_Total <- aov(BD_Total ~ Type, data = what1)
anova(resultBD_Total)
TukeyHSD(resultBD_Total)

#calculate C_mean and SE for top 50 cm
BDStats_DataTotal <- what1 %>% group_by(Type) %>%
  summarise(BD_Mean = mean(BD_Total), SE = sd(BD_Total) / sqrt(n()))
#Graph
g1 <- ggline(BDStats_DataTotal, x = "Type", y = "BD_Mean") +
  geom_errorbar(aes(ymin = BD_Mean - SE, ymax = BD_Mean + SE), width = 0.2) +
  labs(x = "Site Type", y = expression("Average Bulk Density (g cm"^"-3)"),
       title = "Total Core") +
  scale_x_discrete(limits = c("NonDitched", "DitchedAway", "DitchedNear")) +
  theme_classic2() +
  annotate("text", x = 3, y = 0.60, label = expression(bold("p = 0.0308")), size = 4) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  ylim(0.2, 1)

#-------------------------------- BD Adjusted Core -----------------------------

#anova
resultBD_ADJ <- aov(BD_Adjusted ~ Type, data = what1)
anova(resultBD_ADJ)
TukeyHSD(resultBD_ADJ)

#calculate C_mean and SE for top 50 cm
BDStats_Data50 <- what1 %>% group_by(Type) %>%
  summarise(BD_Mean = mean(BD_Adjusted), SE = sd(BD_Adjusted) / sqrt(n()))
#Graph
g2 <- ggline(BDStats_Data50, x = "Type", y = "BD_Mean") +
  geom_errorbar(aes(ymin = BD_Mean-SE, ymax = BD_Mean+SE), width = 0.2) +
  labs(x = "Site Type", y = expression("Average Bulk Density (g cm"^"-3)"),
       title = "Adjusted Core") +
  scale_x_discrete(limits = c("NonDitched", "DitchedAway", "DitchedNear")) +
  theme_classic2() +
  annotate("text", x = 3, y = 0.55, label = "p = 0.6077", size = 4) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) +
  ylim(0.2, 1)


#----------------------------------- BD Top 50cm -------------------------------

#anova
resultBD_50 <- aov(BD_50 ~ Type, data = what1)
anova(resultBD_50)
TukeyHSD(resultBD_50)

#calculate C_mean and SE for top 50 cm
BDStats_Data50 <- what1 %>% group_by(Type) %>%
  summarise(BD_Mean = mean(BD_50), SE = sd(BD_50) / sqrt(n()))
#Graph
g3 <- ggline(BDStats_Data50, x = "Type", y = "BD_Mean") +
  geom_errorbar(aes(ymin = BD_Mean-SE, ymax = BD_Mean+SE), width = 0.2) +
  labs(x = "Site Type", y = expression("Average Bulk Density (g cm"^"-3)"),
       title = "Top 50cm") +
  scale_x_discrete(limits = c("NonDitched", "DitchedAway", "DitchedNear")) +
  theme_classic2() +
  annotate("text", x = 3, y = 0.55, label = expression(bold("p = 0.0101")), size = 4) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) +
  ylim(0.2, 1)




#//////////////////////////////// C % Comparisons \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#--------------------------------- C% Total Core -------------------------------

#anova
resultCTotal <- lm(C_Total ~ Type, data = what1)
anova(resultCTotal)

# Calculate C_mean and SE
Stats_CTotalData <- what1 %>% group_by(Type) %>%
  summarise(C_TotalMean = mean(C_Total), SE = sd(C_Total) / sqrt(n()))
#Graph
g4 <- ggline(Stats_CTotalData, x = "Type", y = "C_TotalMean") +
  geom_errorbar(aes(ymin = C_TotalMean-SE, ymax = C_TotalMean+SE), width = 0.2) +
  labs(x = "Site Type", y = expression("Average Percent Carbon (%)")) +
  scale_x_discrete(limits = c("NonDitched", "DitchedAway", "DitchedNear")) +
  theme_classic2() +
  annotate("text", x = 3, y = 20, label = "p = 0.1119", size = 4) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  ylim(7.5, 26)

#---------------------------------- C% Adjusted --------------------------------

#anova
resultCADJ <- lm(C_Adjusted ~ Type, data = what1)
anova(resultCADJ)

# Calculate C_mean and SE
Stats_CADJData <- what1 %>% group_by(Type) %>%
  summarise(C_ADJMean = mean(C_Adjusted), SE = sd(C_Adjusted) / sqrt(n()))
#Graph
g5 <- ggline(Stats_CADJData, x = "Type", y = "C_ADJMean") +
  geom_errorbar(aes(ymin = C_ADJMean-SE, ymax = C_ADJMean+SE), width = 0.2) +
  labs(x = "Site Type", y = expression("Average Percent Carbon (%)")) +
  scale_x_discrete(limits = c("NonDitched", "DitchedAway", "DitchedNear")) +
  theme_classic2() +
  annotate("text", x = 3, y = 20, label = "p = 0.6355", size = 4) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ylim(7.5, 26)

#---------------------------------- C% Top 50cm --------------------------------

#anova
resultC50 <- lm(C_50 ~ Type, data = what1)
anova(resultC50)

# Calculate C_mean and SE
Stats_C50Data <- what1 %>% group_by(Type) %>%
  summarise(C_50Mean = mean(C_50), SE = sd(C_50) / sqrt(n()))
#Graph
g6 <- ggline(Stats_C50Data, x = "Type", y = "C_50Mean") +
  geom_errorbar(aes(ymin = C_50Mean-SE, ymax = C_50Mean+SE), width = 0.2) +
  labs(x = "Site Type", y = expression("Average Percent Carbon (%)")) +
  scale_x_discrete(limits = c("NonDitched", "DitchedAway", "DitchedNear")) +
  theme_classic2() +
  annotate("text", x = 3, y = 20, label = expression(bold("p = 0.0049")), size = 4) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.x = element_blank()) +
  ylim(7.5, 26)




#/////////////////////////// Carbon Density Comparisons \\\\\\\\\\\\\\\\\\\\\\\\
#--------------------------------- CD Total Core -------------------------------
#anova
resultCDTC <- aov(CD_Total ~ Type, data = what1)
anova(resultCDTC)
TukeyHSD(resultCDTC)

#calculate C_mean and SE for top 50 cm
CDData <- what1 %>% group_by(Type) %>%
  summarise(CD_Mean = mean(CD_Total), CD_SE = sd(CD_Total) / sqrt(n()))
#Graph
g7 <- ggline(CDData, x = "Type", y = "CD_Mean") +
  geom_errorbar(aes(ymin = CD_Mean-CD_SE, ymax = CD_Mean+CD_SE), width = 0.2) +
  labs(x = "Site Type", y = "Carbon Density (Mg ha^-1)") +
  scale_x_discrete(limits = c("NonDitched", "DitchedAway", "DitchedNear")) +
  theme_classic2() +
  annotate("text", x = 3, y = 750, label = "p = 0.071", size = 4) +
  ylim(50, 1000)


#--------------------------------- CD Total Core -------------------------------
#anova
resultCDADJ <- aov(CD_Adjusted ~ Type, data = what1)
anova(resultCDADJ)
TukeyHSD(resultCDADJ)

#calculate C_mean and SE for top 50 cm
CDData2 <- what1 %>% group_by(Type) %>%
  summarise(CD_Mean = mean(CD_Adjusted), CD_SE = sd(CD_Adjusted) / sqrt(n()))
#Graph
g8 <- ggline(CDData2, x = "Type", y = "CD_Mean") +
  geom_errorbar(aes(ymin = CD_Mean-CD_SE, ymax = CD_Mean+CD_SE), width = 0.2) +
  labs(x = "Site Type", y = "Carbon Density (Mg ha^-1)") +
  scale_x_discrete(limits = c("NonDitched", "DitchedAway", "DitchedNear")) +
  theme_classic2() +
  annotate("text", x = 3, y = 750, label = expression(bold("p = 0.0389")), size = 4) +
  theme(axis.title.y = element_blank())+
  ylim(50, 1000)


#------------------------------- CD Top 50cm Core ------------------------------
#anova
resultCD50 <- aov(CD_50 ~ Type, data = what1)
anova(resultCD50)
TukeyHSD(resultCD50)

#calculate C_mean and SE for top 50 cm
CDData3 <- what1 %>% group_by(Type) %>%
  summarise(CD_Mean = mean(CD_50), CD_SE = sd(CD_50) / sqrt(n()))
#Graph
g9 <- ggline(CDData3, x = "Type", y = "CD_Mean") +
  geom_errorbar(aes(ymin = CD_Mean-CD_SE, ymax = CD_Mean+CD_SE), width = 0.2) +
  labs(x = "Site Type", y = "Carbon Density (Mg ha^-1)") +
  scale_x_discrete(limits = c("NonDitched", "DitchedAway", "DitchedNear")) +
  theme_classic2() +
  annotate("text", x = 3, y = 750, label = "p = 0.0574", size = 4) +
  theme(axis.title.y = element_blank())+
  ylim(50, 1000)

#/////////////////////////////// Combine all Graphs \\\\\\\\\\\\\\\\\\\\\\\\\\\\

g1 + g2 + g3 + g4 + g5 + g6 + g7 + g8 +g9




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXX---------------------------- Kruskal-Wallis ----------------------------XXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#Load in Data
what1 <- read.csv("Final_Stats.csv")

#/////////////////////////// Bulk Densities Comparisons \\\\\\\\\\\\\\\\\\\\\\\\
#---------------------------------- BD Total Core ------------------------------
# Kruskal-Wallis test
kruskal.test(BD_Total ~ Type, data = what1)

#---------------------------------- BD Adjusted Core ---------------------------
# Kruskal-Wallis test
kruskal.test(BD_Adjusted ~ Type, data = what1)

#---------------------------------- BD Top 50cm --------------------------------
# Kruskal-Wallis test
kruskal.test(BD_50 ~ Type, data = what1)



#/////////////////////////////// C % Comparisons \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#---------------------------------- C% Total Core ------------------------------
# Kruskal-Wallis test
kruskal.test(C_Total ~ Type, data = what1)

#---------------------------------- C% Adjusted --------------------------------
# Kruskal-Wallis test
kruskal.test(C_Adjusted ~ Type, data = what1)

#---------------------------------- C% Top 50cm --------------------------------
# Kruskal-Wallis test
kruskal.test(C_50 ~ Type, data = what1)



#////////////////////////// Carbon Density Comparisons \\\\\\\\\\\\\\\\\\\\\\\\\
#---------------------------------- CD Total Core ------------------------------
# Kruskal-Wallis test
kruskal.test(CD_Total ~ Type, data = what1)
pairwise.wilcox.test(what1$CD_Total, what1$Type, p.adjust.method = "bonferroni")

#---------------------------------- CD Adjusted --------------------------------
# Kruskal-Wallis test
kruskal.test(CD_Adjusted ~ Type, data = what1)
pairwise.wilcox.test(what1$CD_Adjusted, what1$Type, p.adjust.method = "bonferroni")

#---------------------------------- CD Top 50cm --------------------------------
# Kruskal-Wallis test
kruskal.test(CD_50 ~ Type, data = what1)
pairwise.wilcox.test(what1$CD_50, what1$Type, p.adjust.method = "bonferroni")





#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXX------------------------- Distance correlation -------------------------XXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#Load in Data

what1 <- read.csv("New.csv")

#///////////////////// Bulk Density Distance correlation \\\\\\\\\\\\\\\\\\\\\\\

#------------------------------ BD Total core ----------------------------------

#Model & Anova
lmodelBD <- lm(Distance_Ditch ~ BD_Total, data = what1)
p_valueBD <- summary(lmodelBD)$coefficients[2,4]
anova(lmodelBD)

#Correlation for just me :) ... IDK if it actually is needed
correlation <- cor(what1$Distance_Ditch, what1$BD_Total, use = "complete.obs")
print(correlation)

#Graph
D1 <- ggplot(what1, aes(x=Distance_Ditch, y=BD_Total)) + 
  geom_point(col="blue") +
  geom_smooth(method="lm", col="black") +
  stat_regline_equation(label.x=80, label.y=1.1) +
  annotate("text", x=80, y=0.95, label=sprintf("p-value = %.3f", p_valueBD), hjust=0, vjust=0) +
  theme_pubr() + 
  labs(title = "Total Core", x = "Distance From Ditch (meters)", y=expression("Average Bulk Density (g cm"^"-3)")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1.2)


#------------------------------- BD Adjusted -----------------------------------

#Model & Anova
lmodelBDADJ <- lm(Distance_Ditch ~ BD_Adjusted, data = what1)
p_valueBDADJ <- summary(lmodelBDADJ)$coefficients[2,4]
anova(lmodelBDADJ)

#Correlation for just me :) ... IDK if it actually is needed
correlation2 <- cor(what1$Distance_Ditch, what1$BD_Adjusted, use = "complete.obs")
print(correlation2)

#Graph
D2 <- ggplot(what1, aes(x=Distance_Ditch, y=BD_Adjusted)) + 
  geom_point(col="blue") +
  geom_smooth(method="lm", col="black") +
  stat_regline_equation(label.x=75, label.y=1.1) +
  annotate("text", x=75, y=0.95, label=sprintf("p-value = %.3f", p_valueBDADJ), hjust=0, vjust=0) +
  theme_pubr() + 
  labs(title = "Adjusted Core", x = "Distance From Ditch (meters)", y=expression("Average Bulk Density (g cm"^"-3)")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1.2)


#------------------------------- BD Top 50cm -----------------------------------

#Model & Anova
lmodelBD50 <- lm(Distance_Ditch ~ BD_50, data = what1)
p_valueBD50 <- summary(lmodelBD50)$coefficients[2,4]
anova(lmodelBD50)

#Correlation for just me :) ... IDK if it actually is needed
correlation3 <- cor(what1$Distance_Ditch, what1$BD_50, use = "complete.obs")
print(correlation3)

D3 <- ggplot(what1, aes(x=Distance_Ditch, y=BD_50)) + 
  geom_point(col="blue") +
  geom_smooth(method="lm", col="black") +
  stat_regline_equation(label.x=80, label.y=1.1) +
  annotate("text", x=80, y=0.95, label=sprintf("p-value = %.3f", p_valueBD50), hjust=0, vjust=0) +
  theme_pubr() + 
  labs(title = "Top 50cm", x = "Distance From Ditch (meters)", y=expression("Average Bulk Density (g cm"^"-3)")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1.2)


#/////////////////////// Carbon % Distance correlation \\\\\\\\\\\\\\\\\\\\\\\\\

#---------------------------------- Total Core ---------------------------------

#Model & Anova
lmodelC <- lm(Distance_Ditch ~ C_Total, data = what1)
p_valueC <- summary(lmodelC)$coefficients[2,4]
anova(lmodelC)

#Correlation for just me :) ... IDK if it actually is needed
correlation4 <- cor(what1$Distance_Ditch, what1$C_Total, use = "complete.obs")
print(correlation4)

#Graph
D4 <- ggplot(what1, aes(x=Distance_Ditch, y=C_Total)) + 
  geom_point(col="blue") +
  geom_smooth(method="lm", col="black") +
  stat_regline_equation(label.x=80, label.y=8) +
  annotate("text", x=80, y=5, label=sprintf("p-value = %.3f", p_valueC), hjust=0, vjust=0) +
  theme_pubr() + 
  labs(x = "Distance from Ditch (meters)", y = "Percent Carbon (%)") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  ylim(5, 35)


#----------------------------------- Adjusted ----------------------------------

#Model & Anova
lmodelCADJ <- lm(Distance_Ditch ~ C_Adjusted, data = what1)
p_valueCADJ <- summary(lmodelCADJ)$coefficients[2,4]
anova(lmodelCADJ)

#Correlation for just me :) ... IDK if it actually is needed
correlation5 <- cor(what1$Distance_Ditch, what1$C_Adjusted, use = "complete.obs")
print(correlation5)

#Graph
D5 <- ggplot(what1, aes(x=Distance_Ditch, y=C_Adjusted)) + 
  geom_point(col="blue") +
  geom_smooth(method="lm", col="black") +
  stat_regline_equation(label.x=80, label.y=11) +
  annotate("text", x=80, y=8, label=sprintf("p-value = %.3f", p_valueCADJ), hjust=0, vjust=0) +
  theme_pubr() + 
  labs(x = "Distance from Ditch (meters)", y = "Percent Carbon (%)") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank()) +
  ylim(5, 35)


#----------------------------------- Top 50cm ----------------------------------

#Model & Anova
lmodelC50 <- lm(Distance_Ditch ~ C_50, data = what1)
p_valueC50 <- summary(lmodelC50)$coefficients[2,4]
anova(lmodelC50)

#Correlation for just me :) ... IDK if it actually is needed
correlation6 <- cor(what1$Distance_Ditch, what1$C_50, use = "complete.obs")
print(correlation6)

#graph
D6 <- ggplot(what1, aes(x=Distance_Ditch, y=C_50)) + 
  geom_point(col="blue") +
  geom_smooth(method="lm", col="black") +
  stat_regline_equation(label.x=80, label.y=11) +
  annotate("text", x=80, y=8, label=sprintf("p-value = %.3f", p_valueC50), hjust=0, vjust=0) +
  theme_pubr() + 
  labs(x = "Distance from Ditch (meters)", y = "Percent Carbon (%)") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank()) +
  ylim(5, 35)


#///////////////////// Carbon Density Distance correlation \\\\\\\\\\\\\\\\\\\\\

#---------------------------------- CD total -----------------------------------

#Model & Anova
lmodelCDtotal <- lm(Distance_Ditch ~ CD_Total, data = what1)
p_valueCDtotal <- summary(lmodelCDtotal)$coefficients[2,4]
anova(lmodelCDtotal)

#Correlation for just me :) ... IDK if it actually is needed
correlation7 <- cor(what1$Distance_Ditch, what1$CD_Total, use = "complete.obs")
print(correlation7)

#Graph
D7 <- ggplot(what1, aes(x=Distance_Ditch, y=CD_Total)) + 
  geom_point(col="blue") +
  geom_smooth(method="lm", col="black") +
  stat_regline_equation(label.x=85, label.y=1500) +
  annotate("text", x=85, y=1300, label=sprintf("p-value = %.3f", p_valueCDtotal), hjust=0, vjust=0) +
  theme_pubr() + 
  labs(x = "Distance From Ditch (meters)", y = "Carbon Density (Mg ha^-1)") +
  ylim(0, 2000)


#-------------------------------- CD Adjusted  ---------------------------------

#Model & Anova
lmodelCDADJ <- lm(Distance_Ditch ~ CD_Adjusted, data = what1)
p_valueCDADJ <- summary(lmodelCDADJ)$coefficients[2,4]
anova(lmodelCDADJ)

#Correlation for just me :) ... IDK if it actually is needed
correlation8 <- cor(what1$Distance_Ditch, what1$CD_Adjusted, use = "complete.obs")
print(correlation8)

#Graph
D8 <- ggplot(what1, aes(x=Distance_Ditch, y=CD_Adjusted)) + 
  geom_point(col="blue") +
  geom_smooth(method="lm", col="black") +
  stat_regline_equation(label.x=85, label.y=1500) +
  annotate("text", x=85, y=1300, label=sprintf("p-value = %.3f", p_valueCDADJ), hjust=0, vjust=0) +
  theme_pubr() + 
  labs(x = "Distance From Ditch (meters)", y = "Carbon Density (Mg ha^-1)") +
  theme(axis.title.y = element_blank()) +
  ylim(0, 2000)

#--------------------------------- CD top 50cm ---------------------------------

#Model & Anova
lmodelCD50 <- lm(Distance_Ditch ~ CD_50, data = what1)
p_valueCD50 <- summary(lmodelCD50)$coefficients[2,4]
anova(lmodelCD50)

#Correlation for just me :) ... IDK if it actually is needed
correlation9 <- cor(what1$Distance_Ditch, what1$CD_50, use = "complete.obs")
print(correlation9)

#Graph
D9 <- ggplot(what1, aes(x=Distance_Ditch, y=CD_50)) + 
  geom_point(col="blue") +
  geom_smooth(method="lm", col="black") +
  stat_regline_equation(label.x=85, label.y=1500) +
  annotate("text", x=85, y=1300, label=sprintf("p-value = %.3f", p_valueCD50), hjust=0, vjust=0) +
  theme_pubr() + 
  labs(x = "Distance From Ditch (meters)", y = "Carbon Density (Mg ha^-1)") +
  theme(axis.title.y = element_blank()) +
  ylim(0, 2000)




#/////////////////////////////// Combine all Graphs \\\\\\\\\\\\\\\\\\\\\\\\\\\\

D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXX-------------------- Ditched comparisons W/ P-values -------------------XXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#--------------------- C% 2 type comparison with p values ----------------------

Cdata <- read.csv("Core_Final.csv")

filtered_data2 <- Cdata %>% filter(SamplingDepth <= 60) %>%
  filter(SpecificType %in% c("DitchedFar", "DitchedNear"))

# Calculate mean and standard error for each sampling depth and site type
yep <- filtered_data2 %>% group_by(SamplingDepth, SpecificType) %>%
  summarize(Mean_C = mean(C),
            SE_C = sd(C) / sqrt(n()))  # Calculate standard error

# Perform t-test for each sampling depth
p_values <- Cdata %>%
  filter(SpecificType %in% c("DitchedFar", "DitchedNear")) %>%
  filter(SamplingDepth < 55) %>%
  group_by(SamplingDepth) %>%
  summarise(p_value = t.test(C ~ SpecificType)$p.value)

# Graph
M2 <- ggplot(yep, aes(x = Mean_C, y = SamplingDepth, color = SpecificType)) +
  geom_errorbarh(aes(xmin = Mean_C - SE_C, 
                     xmax = Mean_C + SE_C, height = 0), size = 1.1) + 
  geom_point(size = 4, shape=19) +  
  labs(x = "Percent Carbon (%)", y = "Sampling Depth (cm)", 
       color = "SpecificType") + 
  scale_x_continuous(limits = c(0, 40)) +  
  scale_y_reverse(limits = c(50, 0)) +
  annotate("text", x = 1.7, y = p_values$SamplingDepth, 
           label = sprintf("%.4f", p_values$p_value), 
           hjust = -7.5, vjust = -0.3, color = ifelse(p_values$p_value < 0.05, "black", "black"), 
           size = 4.1, fontface = ifelse(p_values$p_value < 0.05, "bold", "plain")) +
  theme_bw() +
  theme(axis.title.y = element_blank(), 
        legend.position = "none")



#-------------------- BD of just both ditched types + pvalues ------------------


BDdata <- read.csv("Core_Final.csv")

filtered_data2 <- BDdata %>% filter(SamplingDepth <= 60) %>%
  filter(SpecificType %in% c("DitchedFar", "DitchedNear"))

# Calculate mean and standard error for each sampling depth and site type
yep <- filtered_data2 %>% group_by(SamplingDepth, SpecificType) %>%
  summarize(Mean_BulkDensity = mean(BulkDensity),
            SE_BulkDensity = sd(BulkDensity) / sqrt(n()))  # Calculate standard error

# Perform t-test for each sampling depth
p_values <- BDdata %>%
  filter(SpecificType %in% c("DitchedFar", "DitchedNear")) %>%
  filter(SamplingDepth < 55) %>%
  group_by(SamplingDepth) %>%
  summarise(p_value = t.test(BulkDensity ~ SpecificType)$p.value)

# Graph
M1 <- ggplot(yep, aes(x = Mean_BulkDensity, y = SamplingDepth, color = SpecificType)) +
  geom_errorbarh(aes(xmin = Mean_BulkDensity - SE_BulkDensity, 
                     xmax = Mean_BulkDensity + SE_BulkDensity, height = 0), size = 1.1) + 
  geom_point(size = 4, shape=19) +  
  labs(x = expression("Bulk Density (g cm"^"-3)"), y = "Sampling Depth (cm)", 
       color = "SpecificType") + 
  scale_x_continuous(limits = c(0, 1.7)) +  
  scale_y_reverse(limits = c(50, 0)) +
  annotate("text", x = 1.7, y = p_values$SamplingDepth, 
           label = sprintf("%.4f", p_values$p_value), 
           hjust = 0.7, vjust = -0.3, color = ifelse(p_values$p_value < 0.05, "black", "black"), 
           size = 4.1, fontface = ifelse(p_values$p_value < 0.05, "bold", "plain")) +
  theme_bw() +
  theme(legend.position = "none")

#-------------------- CD of just both ditched types + pvalues ------------------


CDdata <- read.csv("Core_Final.csv")

filtered_data2 <- CDdata %>% filter(SamplingDepth <= 55) %>%
  filter(SpecificType %in% c("DitchedFar", "DitchedNear"))

# Calculate mean and standard error for each sampling depth and site type
yep <- filtered_data2 %>% group_by(SamplingDepth, SpecificType) %>%
  summarize(Mean_CDensity = mean(CDensity),
            SE_CDensity = sd(CDensity) / sqrt(n()))  # Calculate standard error

# Perform t-test for each sampling depth
p_values <- CDdata %>%
  filter(SpecificType %in% c("DitchedFar", "DitchedNear")) %>%
  filter(SamplingDepth < 55) %>%
  group_by(SamplingDepth) %>%
  summarise(p_value = t.test(CDensity ~ SpecificType)$p.value)

# Graph
M3 <- ggplot(yep, aes(x = Mean_CDensity, y = SamplingDepth, color = SpecificType)) +
  geom_errorbarh(aes(xmin = Mean_CDensity - SE_CDensity, 
                     xmax = Mean_CDensity + SE_CDensity, height = 0), size = 1.1) + 
  geom_point(size = 4, shape=19) +  
  labs(x = "Carbon Density (Mg ha^-1)", y = "Sampling Depth (cm)", 
       color = "SpecificType") + 
  scale_x_continuous(limits = c(0, 100)) +  
  scale_y_reverse(limits = c(50, 0)) +
  annotate("text", x = 100, y = p_values$SamplingDepth, 
           label = sprintf("%.4f", p_values$p_value), 
           hjust = 0.7, vjust = -0.3, color = ifelse(p_values$p_value < 0.05, "black", "black"), 
           size = 4.1, fontface = ifelse(p_values$p_value < 0.05, "bold", "plain")) +
  theme_bw() +
  theme(axis.title.y = element_blank(), legend.position = c(0.7, 0.4),
        legend.text = element_text(size = 12)) 

#/////////////////////////////// Combine all Graphs \\\\\\\\\\\\\\\\\\\\\\\\\\\\

M1 + M2 + M3




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXX--------------------------- BD but with lines --------------------------XXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

BDdata <- read.csv("Core_Final.csv")
print(BDdata)

# Calculate mean and standard error for each sampling depth and site type
mean_se_data <- BDdata %>% group_by(SamplingDepth, SpecificType) %>%
  summarize(Mean_BulkDensity = mean(BulkDensity),
            SE_BulkDensity = sd(BulkDensity) / sqrt(n()))  # Calculate standard error

bulk_density_lines <- c(0.25, 0.4, 1.4, 1.63)

# a data frame with all the annotation info
annotation <- data.frame(
  x = c(1.4,1.63,0.31),
  y = c(5,5,90),
  label = c("1.4", "1.63", "Reference range"))

p <- ggplot(mean_se_data, aes(x = Mean_BulkDensity, y = SamplingDepth, color = SpecificType)) +
  geom_errorbarh(aes(xmin = Mean_BulkDensity - SE_BulkDensity, 
                     xmax = Mean_BulkDensity + SE_BulkDensity, 
                     height = 0), size = 0.5) +  
  geom_point(size = 3) +  
  geom_vline(xintercept = bulk_density_lines, 
             linetype = "dashed", color = "black", size = 0.5) +
  labs(x = "Bulk Density (g cm^-3)", y = "Sampling Depth", color = "Site Type") +  
  scale_x_continuous(limits = c(0, 2)) +  
  theme_pubr() +  
  scale_y_reverse(limits=c(200, 0))

# Add text
p + geom_text(data=annotation, aes( x=x, y=y, label=label), 
              color="red", 
              size=6 , angle=90, fontface="bold" )







