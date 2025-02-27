#*******************************************************************************
#*
#*                          Supplementary Figures 1-3                                                                                                                                
#*                 (Systematic review, and meta-analysis levels)                                                                
#*                          
#* Author: Loukia M. Spineli
#* Date: February 2025
#*******************************************************************************


## Load libraries ----
list.of.packages <- c("ggplot2", "ggpubr", "plyr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load data ----
# Systematic review level
load("./data/data_syst_level.RData")

# Meta-analysis level
load("./data/data_meta_level.RData")


## Supplementary Figure 1 (SYSTEMATIC REVIEW LEVEL) ----
# Paste First_author with Year
data_syst_level$author_year <- factor(paste(data_syst_level$First_author, data_syst_level$Year))

# Re-order the whole dataset by year and alphabetically
data_syst_new <- data_syst_level[order(data_syst_level$Year, data_syst_level$First_author), ]

# Left-sided barplot (Number of authors, first author, and country) 
authors <- ggplot(data_syst_new,
                  aes(x = Total_authors,
                      y = factor(author_year, levels = author_year),
                      fill = Authors_continent)) +
  geom_col() +
  scale_x_reverse(breaks = 7:0) +
  scale_y_discrete(position = "right") +
  scale_fill_brewer(palette = "Paired") + #"Pastel1"
  geom_text(aes(x = Total_authors,
                y = factor(author_year, levels = author_year),
                label = sprintf("%.0f", Total_authors)),
            size = 5,
            hjust = -0.3,
            col = "white",
            fontface = "bold") + 
  geom_text(aes(x = 0,
                y = factor(author_year, levels = author_year),
                label = Funding),
            size = 5,
            hjust = "right",
            col = "white",
            fontface = "bold") + 
  labs(x = "Number of authors",
       fill = "Continent") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_blank(),
        plot.margin = margin(5.5, 0, 5.5, 5.5),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom")

# Right-sided barplot (Impact factor, first author, and country) 
impact <- ggplot(data_syst_new,
                  aes(x = Impact_factor,
                      y = factor(author_year, levels = author_year),
                      fill = Authors_continent)) +
  geom_col() +
  geom_text(aes(x = Impact_factor,
                y = factor(author_year, levels = author_year),
                label = sprintf("%.1f", Impact_factor)),
            size = 5,
            hjust = 1.2,
            col = "white",
            fontface = "bold") + 
  scale_x_continuous(breaks = seq(0, 9, 1.5)) +
  scale_fill_brewer(palette = "Paired") +  #"Pastel1"
  labs(x = "Impact factor (2023)",
       fill = "Continent") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 16, margin = margin(0, 5.5, 0, 5.5)),
        plot.margin = margin(5.5, 5.5, 5.5, 0),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom")

# Bring together and dave
tiff("./Figures/Supplementary Figure 1.tiff", 
     height = 20, 
     width = 35, 
     units = 'cm', 
     compression = "lzw", 
     res = 300)
ggarrange(authors, impact,
          common.legend = TRUE,
          legend = "bottom")
dev.off()
        

## Supplementary Figure 2 (SYSTEMATIC REVIEW LEVEL) ----
# Turn protocol into binary ('available' versus 'not available')
data_syst_new$Protocol_available_new <- 
  revalue(data_syst_new$Protocol_available, c("Registered to PROSPERO" = "Yes", "Not registered but published" = "Yes",
                                              "No protocol is mentioned" = "No"))

# Re-construct isolated dataset to create a heatmap-like graph
quality_syst <- data.frame(value = unlist(data_syst_new[, c("Protocol_available_new", "PRISMA_mentioned", "RoB_assessed", "GRADE_reported")]),
                           tool = rep(c("Protocol available", "PRISMA mentioned", "RoB assessed", "GRADE reported"), each = dim(data_syst_new)[1]),
                           review = rep(data_syst_new[, "author_year"], 4),
                           year = rep(data_syst_new[, "Year"], 4),
                           author = rep(data_syst_new[, "First_author"], 4))

# Re-order the whole dataset by year and alphabetically
quality_syst_new <- quality_syst[order(quality_syst$year, quality_syst$author), ]

# Obtain the heatmap-like graph
tiff("./Figures/Supplementary Figure 2.tiff", 
     height = 20, 
     width = 35, 
     units = 'cm', 
     compression = "lzw", 
     res = 300)
ggplot(quality_syst_new, 
       aes(x = factor(tool, levels = c("Protocol available", "PRISMA mentioned", "RoB assessed", "GRADE reported")), 
           y = factor(review, levels = unique(review)),
           fill = value)) +
  geom_point(size = 10,
             shape = 21,
             col = "white") +
  geom_text(data = data_syst_new,
            aes(x = 3,
                y = factor(author_year, levels = author_year),
                label = RoB_tool),
            size = 5,
            hjust = "left",
            inherit.aes = FALSE) +
  scale_fill_manual(breaks = c("Yes", "No", "Not applicable"),
                    values = c("#009E73", "#D55E00", "#999999")) +
  labs(x = "",
       y = "",
       fill = "") + 
  theme_bw() +
  ggtitle("Systematic review level") +
  guides(fill = guide_legend(override.aes = list(size = 8))) + 
  theme(title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 16))
dev.off()


## Supplementary Figure 3 (SELECTED META-ANALYSIS LEVEL) ----
# Paste First_author with Year
data_meta_level$author_year <- factor(paste(data_meta_level$First_author, data_meta_level$Year))

# Re-order the whole dataset by year and alphabetically
data_meta_new <- data_meta_level[order(data_meta_level$Year, data_meta_level$First_author), ]

# Prepare dataset on sample size
data_sample <- data.frame(review = rep(data_meta_new$author_year, 2),
                          total_sample = as.numeric(c(data_meta_new$Total_sample_HBB, data_meta_new$Total_sample_ctrl)),
                          arm = rep(c("HBB", "Comparator"), each = dim(data_meta_new)[1]),
                          year = rep(data_meta_new[, "Year"], 2),
                          author = rep(data_meta_new[, "First_author"], 2))

# Re-order the whole dataset by year and alphabetically
data_sample_new <- data_sample[order(data_sample$year, data_sample$author), ]

# Bar plot on sample size per review
tiff("./Figures/Supplementary Figure 3.tiff", 
     height = 20, 
     width = 35, 
     units = 'cm', 
     compression = "lzw", 
     res = 300)
ggplot(data_sample_new,
       aes(x = factor(review, levels = rev(unique(review))),
           y = total_sample,
           fill = factor(arm, levels = c("HBB", "Comparator")))) +
  geom_bar(stat = "identity", 
           width = .5, 
           position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  geom_text(aes(x = factor(review, levels = rev(unique(review))),
                y = total_sample,
                label = total_sample),
            position = position_dodge(width = 0.5),
            vjust = 1.0,
            col = "white",
            fontface = "bold") + 
  geom_text(data = data_meta_new,
            aes(x = factor(author_year, levels = rev(author_year)),
                y = 0,
                label = paste("(n =", Total_studies, ")")),
            vjust = 1.35,
            colour = "grey45",
            size = 4,
            fontface = "bold",
            inherit.aes = FALSE) + 
  scale_y_continuous(breaks = c(0, 250, 500, 750, 1000, 1250),
                     expand = c(0.03, 0)) +
  labs(x = " ",
       y = "Total sample size",
       fill = "Treatment arm") +
  theme_bw() +
  ggtitle("Selected meta-analysis level") +
  theme(title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        legend.position = "bottom")
dev.off()

# Summary of number of studies
summary(as.numeric(data_meta_new$Total_studies))

