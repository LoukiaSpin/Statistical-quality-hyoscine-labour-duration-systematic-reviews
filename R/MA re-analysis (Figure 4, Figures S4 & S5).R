#*******************************************************************************
#*
#*                          Re-analysing selected meta-analyses                                                                                                      
#*              (Systematic review, meta-analysis and trial level)                             
#*                          
#* Author: Loukia M. Spineli
#* Date: February 2025
#*******************************************************************************


## Load libraries ----
list.of.packages <- c("meta", "ggplot2")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load data ----
load("./data/data_meta.RData")
#data_meta <- as.data.frame(read_excel("./41_Extraction Reviews & Trials/Extraction form_Included studies.xlsx", sheet = "Arm-based data"))[, -18]
#colnames(data_meta)[c(3:4, 6)] <- c("First_author_SR", "Year_review", "First_author_trial")


## Split dataset per selected meta-analysis ----
data_set_split <- lapply(1:8, function(x) subset(data_meta_outcome, ID == x))


## Perform RE-MA with DSL ----
#' Wald-based CI for summary MD (no HKSJ adjustments, no prediction intervals) 
# Run RE-MA
meta_orig <- lapply(data_set_split, function(x) metacont(HBB_n, HBB_mean, HBB_sd, ctrl_n, ctrl_mean, ctrl_sd,
                                                         data = x, 
                                                         method.tau = "DL",
                                                         method.tau.ci = "",
                                                         sm = "MD"))

# Extract pooled MD in a vector
MD_orig <- unlist(lapply(meta_orig, function(x) x$TE.random))

# Extract lower bound of pooled MD in a vector
MD_lower_orig <- unlist(lapply(meta_orig, function(x) x$lower.random))

# Extract upper bound of pooled MD in a vector
MD_upper_orig <- unlist(lapply(meta_orig, function(x) x$upper.random))

# Extract tau in a vector
tau_orig <- unlist(lapply(meta_orig, function(x) x$tau))

# Extract lower bound of tau in a vector
tau_lower_orig <- unlist(lapply(meta_orig, function(x) x$lower.tau))

# Extract upper bound of tau in a vector
tau_upper_orig <- unlist(lapply(meta_orig, function(x) x$upper.tau))

# Extract I2 in a vector
I2_orig <- unlist(lapply(meta_orig, function(x) x$I2))

# Extract lower bound of I2 in a vector
I2_lower_orig <- unlist(lapply(meta_orig, function(x) x$lower.I2))

# Extract upper bound of I2 in a vector
I2_upper_orig <- unlist(lapply(meta_orig, function(x) x$upper.I2))

# Bring all in a data-frame
res_meta_orig <- data.frame(review = paste(unique(data_meta_outcome$First_author_SR), unlist(lapply(data_set_split, function(x) unique(x$Year_review)))),
                            MD = MD_orig, MD_lower = MD_lower_orig, MD_upper = MD_upper_orig, 
                            tau = tau_orig, tau_lower = tau_lower_orig, tau_upper = tau_upper_orig, 
                            I2 = I2_orig, I2_lower = I2_lower_orig, I2_upper = I2_upper_orig, 
                            pred_lower = NA, pred_upper = NA)


## Perform RE-MA with REML and HKSJ CI for summary MD and obtain prediction intervals ----
meta_reanal <- lapply(data_set_split, function(x) metacont(HBB_n, HBB_mean, HBB_sd, ctrl_n, ctrl_mean, ctrl_sd,
                                                           studlab = First_author_trial,
                                                           data = x, 
                                                           method.tau = "REML",
                                                           method.random.ci = "HK",
                                                           method.predict = "HK", 
                                                           sm = "MD"))

# Extract pooled MD in a vector
MD_reanal <- unlist(lapply(meta_reanal, function(x) x$TE.random))

# Extract lower bound of pooled MD in a vector
MD_lower_reanal <- unlist(lapply(meta_reanal, function(x) x$lower.random))

# Extract upper bound of pooled MD in a vector
MD_upper_reanal <- unlist(lapply(meta_reanal, function(x) x$upper.random))

# Extract tau in a vector
tau_reanal <- unlist(lapply(meta_reanal, function(x) x$tau))

# Extract lower bound of tau in a vector
tau_lower_reanal <- unlist(lapply(meta_reanal, function(x) x$lower.tau))

# Extract upper bound of tau in a vector
tau_upper_reanal <- unlist(lapply(meta_reanal, function(x) x$upper.tau))

# Extract lower bound of prediction in a vector
pred_lower_reanal <- unlist(lapply(meta_reanal, function(x) x$lower.predict))

# Extract upper bound of prediction in a vector
pred_upper_reanal <- unlist(lapply(meta_reanal, function(x) x$upper.predict))

# Bring all in a data-frame
res_meta_reanal <- data.frame(review = paste(unique(data_meta_outcome$First_author_SR), unlist(lapply(data_set_split, function(x) unique(x$Year_review)))),
                              MD = MD_reanal, MD_lower = MD_lower_reanal, MD_upper = MD_upper_reanal, 
                              tau = tau_reanal, tau_lower = tau_lower_reanal, tau_upper = tau_upper_reanal, 
                              pred_lower = pred_lower_reanal, pred_upper = pred_upper_reanal)

## Interval plot on *Summary Mean difference* ----
# Merge results from original and re-analysis
merged_data <- rbind(res_meta_orig[, c(1:7, 11:12)], res_meta_reanal)

# Add the analysis indicator
merged_data$analysis <- rep(c("Original", "Re-analysis"), c(dim(res_meta_orig)[1], dim(res_meta_reanal)[1]))

# Add author as a separate column
merged_data$First_author <- rep(unique(data_meta_outcome$First_author_SR), 2)

# Add year as a separate column
merged_data$Year <- rep(unlist(lapply(data_set_split, function(x) unique(x$Year_review))), 2)

# Re-order the whole dataset by year and alphabetically
merged_data_new <- merged_data[order(merged_data$Year, merged_data$First_author), ]

# Add order for the analysis
merged_data_new$order <- rep(1:8, each = 2)

# Create the grouped interval plot and save as .tiff
tiff("./Figures/Figure 4.tiff", 
     height = 25, 
     width = 42, 
     units = 'cm', 
     compression = "lzw", 
     res = 300)
ggplot(merged_data_new,
       aes(x = MD,
           y = factor(order),
           group = factor(analysis, levels = c("Re-analysis", "Original")))) +
  geom_rect(aes(xmin = -60, xmax = 60, ymin = 0, ymax = Inf),
            fill = "#D55E00",
            alpha = 0.005) +
  geom_vline(xintercept = 0,
             lty = 2,
             linewidth = 1,
             col = "grey60") +
  geom_linerange(aes(xmin = pred_lower, 
                     xmax = pred_upper),
                 colour = "#56B4E9",
                 alpha = 0.7, 
                 position = position_dodge(width = 0.76),
                 linewidth = 1.5) +
  geom_linerange(aes(xmin = MD_lower, 
                     xmax = MD_upper,
                     colour = factor(analysis, levels = c("Original", "Re-analysis"))),
                 position = position_dodge(width = 0.76),
                 linewidth = 1.5) +
  geom_point(aes(colour = factor(analysis, levels = c("Original", "Re-analysis"))),
             position = position_dodge(width = 0.76),
             size = 4) +
  geom_text(aes(x = MD,
                y = factor(order),
                group = factor(analysis, levels = c("Re-analysis", "Original")),
                label = sprintf("%2.f", MD)),
            size = 4.5,
            position = position_dodge(width = 0.76),
            vjust = -0.8) +
  geom_text(aes(x = MD_lower,
                y = factor(order),
                group = factor(analysis, levels = c("Re-analysis", "Original")),
                label = ifelse(abs(MD_lower - MD) < 20, "", sprintf("%2.f", MD_lower))),
            size = 4.5,
            position = position_dodge(width = 0.76),
            vjust = -0.8) +
  geom_text(aes(x = MD_upper,
                y = factor(order),
                group = factor(analysis, levels = c("Re-analysis", "Original")),
                label = ifelse(MD_upper - MD < 20, "", sprintf("%2.f", MD_upper))),
            size = 4.5,
            position = position_dodge(width = 0.76),
            vjust = -0.8) +
  geom_text(aes(x = pred_lower,
                y = factor(order),
                group = factor(analysis, levels = c("Re-analysis", "Original")),
                label = sprintf("%2.f", pred_lower)),
            size = 4.5,
            position = position_dodge(width = 0.76),
            vjust = -0.8) +
  geom_text(aes(x = pred_upper,
                y = factor(order),
                group = factor(analysis, levels = c("Re-analysis", "Original")),
                label = sprintf("%2.f", pred_upper)),
            size = 4.5,
            position = position_dodge(width = 0.76),
            vjust = -0.8) +
  scale_color_manual(values = c("#009E73","#0072B2")) +
  scale_x_continuous(breaks = sort(c(seq(-700, 350, 200), 0))) +
  scale_y_discrete(labels = unique(merged_data_new$review),
                   expand = c(0.05, 0.4)) +
  labs(x = "Mean difference (in minutes)",
       y = "",
       colour = "Analysis") +
  theme_classic() +
  theme(axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom")
dev.off()


## Interval plot on *tau-squared* ----
# Create interval plot and save as .tiff
tiff("./Figures/Supplementary Figure 4.tiff", 
     height = 25, 
     width = 45, 
     units = 'cm', 
     compression = "lzw", 
     res = 300)
ggplot(merged_data_new,
       aes(x = tau,
           y = factor(order),
           group = factor(analysis, levels = c("Re-analysis", "Original")))) +
  geom_linerange(aes(xmin = tau_lower, 
                     xmax = tau_upper,
                     colour = factor(analysis, levels = c("Original", "Re-analysis"))),
                 position = position_dodge(width = 0.6),
                 linewidth = 1.5) +
  geom_point(aes(colour = factor(analysis, levels = c("Original", "Re-analysis"))),
             position = position_dodge(width = 0.6),
             size = 4) +
  geom_text(aes(x = tau,
                y = factor(order),
                group = factor(analysis, levels = c("Re-analysis", "Original")),
                label = sprintf("%2.f", tau)),
            size = 5,
            position = position_dodge(width = 0.6),
            vjust = -0.8) +
  geom_text(aes(x = tau_lower,
                y = factor(order),
                group = factor(analysis, levels = c("Re-analysis", "Original")),
                label = sprintf("%2.f", tau_lower)),
            size = 5,
            position = position_dodge(width = 0.6),
            vjust = -0.8) +
  geom_text(aes(x = tau_upper,
                y = factor(order),
                group = factor(analysis, levels = c("Re-analysis", "Original")),
                label = sprintf("%2.f", tau_upper)),
            size = 5,
            position = position_dodge(width = 0.6),
            vjust = -0.8) +
  scale_color_manual(values = c("#009E73","#0072B2"),
                     label = c("Original" = "DerSimonian and Laird (Original)",
                               "Re-analysis" = "REML (Re-analysis)")) +
  scale_x_continuous(breaks = seq(0, 350, 50)) +
  labs(x = "Tau (Between-trial standard deviation)",
       y = "",
       colour = "Analysis") +
  scale_y_discrete(labels = unique(merged_data_new$review),
                   expand = c(0, 0.5)) +
  theme_classic() +
  theme(axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom")
dev.off()


## Interval plot on *I-squared* ----
# Add author as a separate column
res_meta_orig$First_author <- unique(data_meta_outcome$First_author_SR)

# Add year as a separate column
res_meta_orig$Year <- unlist(lapply(data_set_split, function(x) unique(x$Year_review)))

# Re-order the whole dataset by year and alphabetically
res_meta_orig_new <- res_meta_orig[order(res_meta_orig$Year, res_meta_orig$First_author), ]

# Create interval plot and save as .tiff
tiff("./Figures/Supplementary Figure 5.tiff", 
     height = 25, 
     width = 45, 
     units = 'cm', 
     compression = "lzw", 
     res = 300) 
ggplot(res_meta_orig_new,
       aes(x = I2 * 100,
           y = factor(review, levels = review))) +
  geom_rect(aes(xmin = 0, xmax = 30, ymin = 0, ymax = Inf,
            fill = "Unimportant"),
            alpha = 0.02) +
  geom_rect(aes(xmin = 30, xmax = 50, ymin = 0, ymax = Inf,
            fill = "Moderate"),
            alpha = 0.02) +
  geom_rect(aes(xmin = 50, xmax = 75, ymin = 0, ymax = Inf,
            fill = "Substantial"),
            alpha = 0.02) +
  geom_rect(aes(xmin = 75, xmax = 100, ymin = 0, ymax = Inf,
            fill = "Considerable"),
            alpha = 0.02) +
  geom_linerange(aes(xmin = I2_lower * 100, 
                     xmax = I2_upper * 100),
                 linewidth = 1.5) +
  geom_point(size = 4) +
  geom_text(aes(x = I2 * 100,
                y = factor(review, levels = review),
                label = sprintf("%2.f", I2 * 100)),
            size = 4.5,
            vjust = -0.8) +
  geom_text(aes(x = I2_lower * 100,
                y = factor(review, levels = review),
                label = sprintf("%2.f", I2_lower * 100)),
            size = 4.5,
            vjust = -0.8) +
  geom_text(aes(x = I2_upper * 100,
                y = factor(review, levels = review),
                label = sprintf("%2.f", I2_upper * 100)),
            size = 4.5,
            vjust = -0.8) +
  labs(x = "I-squared (%)",
       y = "") +
  scale_fill_manual(name = "Relative heterogeneity",
                    values = c("Unimportant" = "#009E73",
                               "Moderate" = "#F0E442",
                               "Substantial" = "#E69F00",
                               "Considerable" = "#D55E00"),
                    breaks = c("Unimportant", "Moderate", "Substantial", "Considerable")) +
  scale_y_discrete(labels = unique(merged_data_new$review)) +
  scale_x_continuous(limits = c(0, 100)) +
  theme_classic() +
  guides(fill = guide_legend(override.aes = list(alpha = 0.4))) +
  theme(axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),
        legend.position = "bottom",
        legend.text =  element_text(color = "black", size = 16),
        legend.title =  element_text(color = "black", face = "bold",size = 16))
dev.off()


## Produce panel of forest plot per review
forest <- lapply(meta_reanal, function(x) forest(x,
                                                 common = FALSE,
                                                 prediction = TRUE,
                                                 print.I2.ci = TRUE,
                                                 print.tau = TRUE,
                                                 print.tau.ci = TRUE,
                                                 print.pval.Q = FALSE,
                                                 digits.sd = 2))
