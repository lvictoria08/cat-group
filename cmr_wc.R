#EEE323 
#R code Catgroup - HS25

######
#Working Directory
setwd("C:/Users/jeann/OneDrive - Universität Zürich UZH/Bureau/Wc_Dc/resultate/capture-recapture")

#####
#Libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)

#####
#Data preparation
df<-read.csv("cmr_wc_dc.csv", header=TRUE, stringsAsFactors=TRUE, sep=";")

# Step 1: extract year and month as character
df$month <- format(as.Date(df$Dat_Kontrolle, format = "%d.%m.%Y"), "%m")
df$year <- format(as.Date(df$Dat_Kontrolle, format = "%d.%m.%Y"), "%Y")
df$year_month <- paste0(df$year, "-", df$month)

# Step 2: summarize to 1 detection per individual per month
df_summary <- summarize(
  group_by(df, IndividuenID, year_month),
  detected = 1
)

# Step 3: create full grid of individuals × months
all_months <- expand.grid(
  IndividuenID = unique(df$IndividuenID),
  year_month = unique(df$year_month)
)

# Step 4: merge and fill NAs with 0
df_full <- merge(all_months, df_summary, by = c("IndividuenID", "year_month"), all.x = TRUE)
df_full$detected[is.na(df_full$detected)] <- 0

# Step 5: order by IndividuenID and year_month
df_full$ym_date <- as.Date(paste0(df_full$year_month, "-01"))
df_full <- df_full[order(df_full$IndividuenID, df_full$ym_date), ]
df_full$year_month <- factor(df_full$year_month, levels = unique(df_full$year_month[order(df_full$ym_date)]))

# Step 6: reshape to wide format
df_wide <- reshape(
  df_full[, c("IndividuenID", "year_month", "detected")],
  timevar = "year_month",
  idvar = "IndividuenID",
  direction = "wide"
)

#Step 7: capture history
ch_string <- paste0(df_wide$`detected.2019-01`, 
                    df_wide$`detected.2019-02`, df_wide$`detected.2019-03`,
                    df_wide$`detected.2019-04`, df_wide$`detected.2019-11`,
                    df_wide$`detected.2019-12`, df_wide$`detected.2020-01`,
                    df_wide$`detected.2020-02`, df_wide$`detected.2020-03`,
                    df_wide$`detected.2020-04`)

df_wide$ch <- NA
df_wide$ch <- ch_string

#####
#Closed-population assumption - Lincoln Peterson model (unbiased)

#Step 1: filter for wildcat only
ch_wc <- df_wide[grepl("^Wc\\d+",df_wide$IndividuenID),] #filter with "^Dc\\d+" for domestic cats

#Step 2: Function to combine capture occasion per 2 monts (Jan-Feb and Mar-Apr)
combine_occasions_df <- function(ch, occ1, occ2) {
  ch <- sprintf("%02s", as.character(ch))  # pad to 2 characters
  occ1_seen <- apply(sapply(occ1, function(i) substr(ch, i, i)) == "1", 1, any)
  occ2_seen <- apply(sapply(occ2, function(i) substr(ch, i, i)) == "1", 1, any)
  
  data.frame(
    occ_A = as.integer(occ1_seen),
    occ_B = as.integer(occ2_seen)
  )
}

#for year 2019 and 2020 (summarize month Jan-Feb and Mar-Apr to 1 value each)
ch_wc_2019 <- with(combine_occasions_df(ch_wc$ch, occ1 = c(1, 2), occ2 = c(3, 4)), paste0(occ_A, occ_B))
ch_wc_2020 <- with(combine_occasions_df(ch_wc$ch, occ1 = c(5,6), occ2 = c(7,8)), paste0(occ_A, occ_B))
#Step 3: Function to calculate Lincoln-Peterson model (unbiased)
lp_pair <- function(ch, occ1, occ2) {
  ch <- as.character(ch)
  
  seen_1 <- substr(ch, occ1, occ1) == "1"
  seen_2 <- substr(ch, occ2, occ2) == "1"
  
  n1 <- sum(seen_1)
  n2 <- sum(seen_2)
  m2 <- sum(seen_1 & seen_2)
  
  if (m2 == 0) {
    warning(sprintf("No recaptures between occasions %d and %d", occ1, occ2))
    return(list(N = NA, CI = c(NA, NA), n1 = n1, n2 = n2, m2 = m2))
  }
  
  N_hat <- ((n1 + 1) * (n2 + 1)) / (m2 + 1) - 1
  SE <- sqrt((n1 + 1) * (n2 + 1) * (n1 - m2) * (n2 - m2) / ((m2 + 1)^2 * (m2 + 2)))
  CI <- c(N_hat - 1.96 * SE, N_hat + 1.96 * SE)
  
  list(N = N_hat, CI = CI, n1 = n1, n2 = n2, m2 = m2)
}

#Apply function to data
wc_lp_2019 <- lp_pair(ch_wc_2019, 1, 2)
wc_lp_2020 <- lp_pair(ch_wc_2020, 1, 2)

#Step 4: Print results
print(wc_lp_2019)
print(wc_lp_2020)

#####
#Closed population assumption not respected, time probably too long between 
#capture occasions, but for the sake of the argument Lincoln-Peterson calculated

#combine months 2019 and 2020 into 2 values and do the same as above
ch_wc_19_20 <- with(combine_occasions_df(ch_wc$ch, occ1 = c(1:4), occ2 = c(5:10)), 
                    paste0(occ_A, occ_B))
wc_lp_19_20 <- lp_pair(ch_wc_19_20, 1, 2)
print(wc_lp_19_20)


#####
#Abundance over the total area for the different N estimates
abundance <- function(lp_results) {
  area_monitored = 121 #km^2
  area_estimated = 246 #km^2
  abundance_monitored_area <- as.numeric(lp_results)
  estimation <- as.integer((abundance_monitored_area*(area_estimated/area_monitored)))
  cat("The estimated abundance of wildcats in Switzerland is", estimation, "\n")
}

abundance(wc_lp_2019[1])
abundance(wc_lp_2020[1])
abundance(wc_lp_19_20[1])


