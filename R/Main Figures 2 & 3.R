#*******************************************************************************
#*
#*                              Main Figures 2 & 3                                                                                                                                                                                              
#*                  (Systematic review, meta-analysis levels)                                                                             
#*                          
#* Author: Loukia M. Spineli
#* Date: February 2025
#*******************************************************************************


## Load libraries ----
library(ggplot2)


## Load data ----
# Systematic review level
load("./data/data_syst_level.RData")

# Meta-analysis level
load("./data/data_meta_level.RData")


## Main Figure 2 (SYSTEMATIC REVIEW LEVEL) ----
# Paste First_author with Year
data_syst_level$author_year <- factor(paste(data_syst_level$First_author, data_syst_level$Year))

# Add the number of the articles in the reference list
data_syst_level$author_year <- paste(data_syst_level$author_year, c("[7]", "[10]", "[48]", "[49]", "[47]", "[5]", "[50]", "[51]"))

# Re-order the whole dataset by year and alphabetically
data_syst_new <- data_syst_level[order(data_syst_level$Year, data_syst_level$First_author), ]

# Re-construct isolated dataset to create a heatmap-like graph
methods_syst <- data.frame(value = unlist(data_syst_new[, c("I2_assessed", "Q_test_assessed", "tau2_assessed", "Subgroup", "Metareg", "Sensitivity", "Pub_bias_funnel", "Pub_bias_Egger")]),
                           tool = rep(c("I-squared", "Q-statistic", "tau-squared", "Subgroup", "Meta-regression", "Sensitivity", "Funnel plot", "Egger's test"), each = dim(data_syst_new)[1]),
                           review = rep(data_syst_new[, "author_year"], 8),
                           year = rep(data_syst_new[, "Year"], 8),
                           author = rep(data_syst_new[, "First_author"], 8),
                           facet = rep(c("Statistical heterogeneity was assessed", "Statistical heterogeneity was explored", "Publication bias was assessed"), c(8 * 3, 8 * 3, 8 * 2)))

# Re-order the whole dataset by year and alphabetically
methods_syst_new <- methods_syst[order(methods_syst$year, methods_syst$author), ]

# Obtain the heatmap-like graph
tiff("./Figures/Figure 2.tiff", 
     height = 20, 
     width = 39, 
     units = 'cm', 
     compression = "lzw", 
     res = 300)
ggplot(methods_syst_new, 
       aes(x = factor(tool, levels = c("I-squared", "Q-statistic", "tau-squared", "Subgroup", "Meta-regression", "Sensitivity", "Funnel plot", "Egger's test")), 
           y = factor(review, levels = unique(review)),
           fill = value)) +
  geom_point(size = 10,
             shape = 21,
             col = "white") +
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#009E73", "#D55E00")) +
  facet_grid(cols = vars(factor(facet, levels = c("Statistical heterogeneity was assessed", "Statistical heterogeneity was explored", "Publication bias was assessed"))), 
             scales = "free") +
  labs(x = "",
       y = "",
       fill = "") + 
  theme_bw() +
  ggtitle("Systematic review level") +
  guides(fill = guide_legend(override.aes = list(size = 8))) + 
  theme(title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.position = "bottom",
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15, face = "bold"))
dev.off()


## Main Figure 3 (SELECTED META-ANALYSIS LEVEL) ----
# Paste First_author with Year
data_meta_level$author_year <- factor(paste(data_meta_level$First_author, data_meta_level$Year))

# Add the number of the articles in the reference list
data_meta_level$author_year <- paste(data_meta_level$author_year, c("[7]", "[10]", "[48]", "[49]", "[47]", "[5]", "[50]", "[51]"))

# Re-order the whole dataset by year and alphabetically
data_meta_new <- data_meta_level[order(data_meta_level$Year, data_meta_level$First_author), ]

# Mohaghegh 2020 applied a common-effect model in the selected meta-analysis
data_meta_new$tau2_reported[data_meta_new$tau2_reported == "Common-effect model applied"] <- "No"

# Re-construct isolated dataset to create a heatmap-like graph
report_meta <- data.frame(value = unlist(data_meta_new[, c(21, 19, 23, 25, 27)]),
                          tool = rep(c("I-squared", "tau-squared", "I-squared", "tau-squared", "Cochran's Q-statistic"), each = dim(data_meta_new)[1]),
                          review = rep(data_meta_new[, "author_year"], 5),
                          year = rep(data_meta_new[, "Year"], 5),
                          author = rep(data_meta_new[, "First_author"], 5),
                          facet = rep(c("Statistical heterogeneity measured using", "Statistical heterogeneity interpreted using"), c(8 * 2, 8 * 3)))

# Re-order the whole dataset by year and alphabetically
report_meta_new <- report_meta[order(report_meta$year, report_meta$author), ]

# Obtain the heatmap-like graph
tiff("./Figures/Figure 3.tiff", 
     height = 20, 
     width = 35, 
     units = 'cm', 
     compression = "lzw", 
     res = 300)
ggplot(report_meta_new, 
       aes(x = factor(tool, levels = c("I-squared", "tau-squared", "Cochran's Q-statistic")), 
           y = factor(review, levels = unique(review)),
           fill = value)) +
  geom_point(size = 10,
             shape = 21,
             col = "white") +
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#009E73", "#D55E00")) +
  facet_grid(cols = vars(factor(facet, levels = c("Statistical heterogeneity measured using", "Statistical heterogeneity interpreted using"))), 
             scales = "free") +
  labs(x = "",
       y = "",
       fill = "") + 
  theme_bw() +
  ggtitle("Selected meta-analysis level") +
  guides(fill = guide_legend(override.aes = list(size = 8))) + 
  theme(title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.position = "bottom",
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15, face = "bold"))
dev.off()
