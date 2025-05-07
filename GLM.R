#set working directory
setwd("C:/Users/jeann/OneDrive - Universität Zürich UZH/Bureau/Wc_Dc/resultate/capture-recapture")

#Libraries
install.packages("sjPlot")
install.packages("lme4")
install.packages("patchwork")

library(tidyverse)
library(sjPlot)
library(lme4)  
library(ggplot2)
library(patchwork)

#import data 
d <- read.csv("cmr_wc_dc.csv", sep=";")

#Transform the results, here Wc is the reference (=1)
d$Resultat <- factor(d$RESULTAT, levels = c("Dc", "Wc"))

#filter for only wildcat or domestic cat occurences
d <- d[grepl("^Wc|^Dc", d$RESULTAT), ]


#Generalized Linear Model
m1 <- glm(Resultat ~ AltitudeMedian + SettlementProportion + ForestProportion 
          + Distance, data = d, family = binomial)

m2 <- glm(Resultat ~ AltitudeMedian * SettlementProportion * ForestProportion 
          * Distance, data = d, family = binomial)

m3 <- glm(Resultat ~ AltitudeMedian + SettlementProportion + AltitudeMedian * ForestProportion 
          + Distance, data = d, family = binomial)

#evaluate the best model
anova(m1, m2,m3, test="Chisq")

AIC(m1,m2,m3)

#plot m1 
p1 <- plot_model(m1, type = 'pred', terms = 'AltitudeMedian [all]', colors = "blue") +
  labs(x = "Altitude (meters)", y = "") +
  ggtitle(NULL)

p2 <- plot_model(m1, type = 'pred', terms = 'SettlementProportion [all]', colors = "red") +
  labs(x = "Settlement proportion", y = "") +
  ggtitle(NULL)

p3 <- plot_model(m1, type = 'pred', terms = 'ForestProportion [all]', colors = "darkgreen") +
  labs(x = "Forest Proportion", y = "") +
  ggtitle(NULL)

p4 <- plot_model(m1, type = 'pred', terms = 'Distance [all]', colors = "purple") +
  labs(x = "Distance", y = "") +
  ggtitle(NULL)

y_label <- ggplot() +
  theme_void() +
  annotate("text", x = 0, y = 0.5,
           label = "Predicted wildcat detection",
           angle = 90, size = 4.5, hjust = 0.5)

# Combine plots without y-axis labels
p1 <- p1 + theme(axis.title.y = element_blank())
p2 <- p2 + theme(axis.title.y = element_blank())
p3 <- p3 + theme(axis.title.y = element_blank())
p4 <- p4 + theme(axis.title.y = element_blank())

# Combine 2x2 grid
main_grid <- (p1 | p2) / (p3 | p4)

# Now assemble with the left-side y-axis label
final_plot <- (wrap_elements(y_label) + main_grid) +
  plot_layout(widths = c(0.08, 1)) 

# Display
final_plot

summary(m1)
tab_model(m1)

#####
#domestic cat as reference

#Transform the results, here Dc is the reference (=1)
d$Resultat <- factor(d$RESULTAT, levels = c("Wc", "Dc"))

#filter for only wildcat or domestic cat occurences
d <- d[grepl("^Wc|^Dc", d$RESULTAT), ]


#Generalized Linear Model
m1 <- glm(Resultat ~ AltitudeMedian + SettlementProportion + ForestProportion 
          + Distance, data = d, family = binomial)

m2 <- glm(Resultat ~ AltitudeMedian * SettlementProportion * ForestProportion 
          * Distance, data = d, family = binomial)

m3 <- glm(Resultat ~ AltitudeMedian + SettlementProportion + AltitudeMedian * ForestProportion 
          + Distance, data = d, family = binomial)

#evaluate the best model
anova(m1, m2,m3, test="Chisq")

AIC(m1,m2,m3)

#plot m1 
p1 <- plot_model(m1, type = 'pred', terms = 'AltitudeMedian [all]', colors = "blue") +
  labs(x = "Altitude (meters)", y = "") +
  ggtitle(NULL)

p2 <- plot_model(m1, type = 'pred', terms = 'SettlementProportion [all]', colors = "red") +
  labs(x = "Settlement proportion", y = "") +
  ggtitle(NULL)

p3 <- plot_model(m1, type = 'pred', terms = 'ForestProportion [all]', colors = "darkgreen") +
  labs(x = "Forest Proportion", y = "") +
  ggtitle(NULL)

p4 <- plot_model(m1, type = 'pred', terms = 'Distance [all]', colors = "purple") +
  labs(x = "Distance", y = "") +
  ggtitle(NULL)

y_label <- ggplot() +
  theme_void() +
  annotate("text", x = 0, y = 0.5,
           label = "Predicted domestic cat detection",
           angle = 90, size = 4.5, hjust = 0.5)

# Combine plots without y-axis labels
p1 <- p1 + theme(axis.title.y = element_blank())
p2 <- p2 + theme(axis.title.y = element_blank())
p3 <- p3 + theme(axis.title.y = element_blank())
p4 <- p4 + theme(axis.title.y = element_blank())

# Combine 2x2 grid
main_grid <- (p1 | p2) / (p3 | p4)

# Now assemble with the left-side y-axis label
final_plot <- (wrap_elements(y_label) + main_grid) +
  plot_layout(widths = c(0.08, 1)) 

# Display
final_plot

summary(m1)
tab_model(m1)




