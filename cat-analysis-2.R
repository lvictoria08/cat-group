############# analysis data cat group ############


## clear R's head 
rm(list = ls())
graphics.off()

##install packages
install.packages("openxlsx")
install.packages("readxl")
install.packages("dunn.test")

## libraries
library("ggplot2")
library("dplyr")
library("tidyr")
library("ggfortify")
library("tidyverse")
library("GGally")
library("Distance")
library("openxlsx")
library("readxl")
library("dunn.test")

##read data
##first data on wc monitoring genetic data and patches including null values
wc_data <- read_excel("~/Desktop/Master_UZH/FS25/wildlife-management-EEE323/student-project-cat-group/new_dist_CH_inkl_nuller.xlsx")

##then data on site description for the lures including forest proportions and distances from forest edge
site_desc <- read_excel("Desktop/Master_UZH/FS25/wildlife-management-EEE323/student-project-cat-group/SupplMat1_SiteDescription_CaptureHistory_v2_ergänzt2025.xlsx")
#wc_data$Distance

##group RESULTAT column into new column where cat groups are ordered accordginly
wc_data <- wc_data %>%
  mutate(cat_groups = case_when(
    RESULTAT %in% c("Wc", "Wmt") ~ "Wild cat",
    RESULTAT %in% c("Dc", "Dmt") ~ "Domestic Cat",
    is.na(RESULTAT) ~ "Null",
    TRUE ~ "Other"
  ))


## change lure stick names in site description file 
site_desc <- site_desc %>%
  mutate(ID_LureStick = gsub("_.*", "", ID_LureStick)) %>%   # remove everything after the underscore
  mutate(ID_LureStick = gsub("([0-9]+)([A-Z])", "\\1-\\2", ID_LureStick))  # insert dash between numbers and letter

##remove years 2009 - 2010 as we don't want to include them and they are not present in other data sheet
site_desc <- site_desc %>%
  filter(SamplingYear > 2010)

##remove duplicates of stick ID's 
site_desc_new <- site_desc %>%
  distinct(`ID_LureStick`, .keep_all = TRUE)

##change name of ID_LureStick in site description file so it matches the other file
colnames(site_desc_new)[colnames(site_desc) == "ID_LureStick"] <- "Latte_ID"
##change names of coordinates because otherwise they also duplicate
colnames(site_desc)[colnames(site_desc) == "x"] <- "x_site_descr"
colnames(site_desc)[colnames(site_desc) == "y"] <- "y_site_descr"

##join datasets by Latte_ID
new_wc_data <- merge(wc_data, site_desc_new, by = "Latte_ID", all.x = TRUE)


##check for duplicates
#intersect(names(wc_data), names(site_desc))
#wc_dups <- wc_data %>% count(`Latte-ID`) %>% filter(n > 1)
#site_dups <- site_desc %>% count(`Latte-ID`) %>% filter(n > 1)


##new dataset with total counts per cat group per stick
final_data <- new_wc_data %>%
  group_by(Latte_ID, cat_groups, Year) %>%
  summarise(Total_counts = n(),
            Distance = first(Distance),
            Prop_forest = first(ForestProportion),
            Prop_settlement = first(SettlementProportion),
            Type = first(TypoCH),
            Type_num = first(TypoCH_NUM),
            Altitude = first(AltitudeMedian))


##bin the distances into 20m each
max_dist <- max(final_data$Distance, na.rm = TRUE)
max_break <- ceiling(max_dist / 20) * 20

# Now define bins safely in new column named Distance_Bin
cats_final <- final_data %>%
  mutate(Distance_Bin = cut(Distance,
                            breaks = seq(0, max_break, by = 20),
                            labels = FALSE,
                            right = FALSE,
                            include.lowest = TRUE))



####now start analysis for forest proportions

###plotting forest proportion against different cat groups to get an overview 
ggplot(data=cats_final, aes(x=cat_groups, y = Prop_forest)) +
  geom_boxplot() +
  theme_minimal()

##plotting cat groups against distance
ggplot(data=cats_final, aes(x = Distance_Bin , colour = cat_groups)) +
  geom_histogram(fill = "white") +
  theme_minimal()


### Now try to make porportions of cat groups per forest proportion bin (0-10)
## try to bin forest proportion and make new columns for Forest Proportion bins
max_fprop <- max(cats_final$Prop_forest, na.rm = TRUE)
max_break_fprop <- ceiling(max_fprop / 0.1) * 0.1
cats_forest_bins <- cats_final %>%
  mutate(fprop_bin = cut(Prop_forest,
                            breaks = seq(0, max_break_fprop, by = 0.1),
                            labels = FALSE,
                            right = FALSE,
                            include.lowest = TRUE))



##make proportions for cat groups per forest proportion bin
# Step 1: Summarise total cats per bin *and* group
cat_summary <- cats_forest_bins %>%
  group_by(fprop_bin, cat_groups) %>%
  summarise(group_total = sum(Total_counts), .groups = "drop")

# Step 2: Calculate total sticks per bin (across all groups)
bin_totals <- cats_forest_bins %>%
  group_by(fprop_bin) %>%
  summarise(total_detec_per_bin = sum(Total_counts), .groups = "drop")

# Step 3: Join and compute proportions per group
cat_proportions <- cat_summary %>%
  left_join(bin_totals, by = "fprop_bin") %>%
  mutate(cat_proportion = group_total/total_detec_per_bin)


##make new column where forest bins are ranging from 0-1
cat_proportions <- cat_proportions %>%
  mutate(forest_proportions = (fprop_bin / 10))


##plot forest bins and cat proportions in a histogram where each cat group is colored
ggplot(data = cat_proportions, aes(x=forest_proportions, y = cat_proportion, colour = cat_groups)) +
  geom_col(fill = "white") +
  labs(x="Proportion of forest", y = "Proportion of detected cats per group") +
  theme_minimal() + 
  scale_color_brewer(palette = "Set1")


##remove last row of dataset cat_proportions because it is NA
cat_proportions <- cat_proportions %>% drop_na()

###general linear model
##we want to test occurrences and forest proportion and cat groups
#group total = total count per cat group per bin
#forest proportions = our bins 
#cat_group = our different groups
model1 <- glm(group_total ~ forest_proportions * cat_groups, family = poisson, data = cat_proportions)
summary(model1)
plot(model1)

##predict counts
cat_proportions$predicted <- predict(model1, type = "response")

ggplot(cat_proportions, aes(x = forest_proportions, y = predicted, color = cat_groups)) +
  geom_line() +
  labs(y = "Predicted detection count") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")






######### repeat procedure with Distance values ##########

##first we need to calculate total count per cat group per distance bin per group
# Step 1: Summarise total cats per Distance bin and group
cat_distance_summary <- cats_final %>%
  group_by(Distance_Bin, cat_groups) %>%
  summarise(group_total = sum(Total_counts), groups = "drop")

# Step 2: Calculate total cats per bin (across all groups)
bin_distance_totals <- cat_distance_summary %>%
  group_by(Distance_Bin) %>%
  summarise(detect_total = sum(group_total) , .groups = "drop")

# Step 3: Join and compute proportions per group
cat_distance_proportions <- cat_distance_summary %>%
  left_join(bin_distance_totals, by = "Distance_Bin") %>%
  mutate(cat_proportion = group_total / detect_total)


##remove all data points bigger than 36 as data becomes very scarce here
cat_distance_proportions <- cat_distance_proportions %>%
  filter(Distance_Bin < 37)

##make logarithmic scale for the Distance_Bin values
#cat_distance_proportions$log_distance <- log1p(cat_distance_proportions$Distance_Bin)

##mnake plot with proportions again
ggplot(data = cat_distance_proportions, aes(x=Distance_Bin, y = cat_proportion, colour = cat_groups)) +
  geom_col(fill = "white") +
  labs(x="Distance from forest edge", y = "Proportion of detected cat groups") +
  theme_minimal() + 
  scale_color_brewer(palette = "Set1")


##make glm 2 to test for the Distance counts
model2 <- glm(group_total ~ Distance_Bin * cat_groups, family = poisson, data = cat_distance_proportions)
summary(model2)


##predict counts for cats per group per distance
cat_distance_proportions$predicted <- predict(model2, type = "response")

ggplot(cat_distance_proportions, aes(x = Distance_Bin, y = predicted, color = cat_groups)) +
  geom_line() +
  labs(y = "Predicted detection count") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")






######### repeat procedure with TypoCH values ##########
###data set final data: Has cat groups, type by name and total counts

##first we need to calculate total count per forest type per cat group
# Step 1: Summarise total cats per forest type and group
cat_ftype_summary <- cats_final %>%
  group_by(Type, cat_groups) %>%
  summarise(group_total = sum(Total_counts), groups = "drop")

# Step 2: Calculate total cats per bin (across all groups)
ftype_totals <- cat_ftype_summary %>%
  group_by(Type) %>%
  summarise(detect_total = sum(group_total), .groups = "drop")

# Step 3: Join and compute proportions per group
ftype_proportions <- cat_ftype_summary %>%
  left_join(ftype_totals, by = "Type") %>%
  mutate(cat_proportion = group_total / detect_total)



##mnake plot with proportions again, type against cat groups
# ggplot(data = ftype_proportions, aes(x=Type, y = cat_proportion, colour = cat_groups)) +
#   geom_col(fill = "white") +
#   labs(x="Forest types", y = "Proportion of detected cat groups") +
#   theme_minimal() + 
#   scale_color_brewer(palette = "Set1")

##make three seperate graphs per cat group to show preferences

# Create a new column for conditional labels
ftype_proportions <- ftype_proportions %>%
  mutate(Type = as.character(Type)) %>%
  group_by(cat_groups, Type) %>%
  mutate(Type_label = ifelse(group_total > 5, Type, "")) %>%
  ungroup()

# plot
ggplot(data = ftype_proportions, aes(x = Type, y = group_total, fill = cat_groups)) +
  geom_col() +
  facet_wrap(~cat_groups, scales = "free_x") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() + 
  labs(x = "Forest Type", y = "Cat detection counts") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(data = ftype_proportions, aes(x = Type, y = group_total)) +
  geom_col(fill = "steelblue") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() + 
  labs(x = "Forest Type", y = "Detection events") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))




