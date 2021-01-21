setwd("C:\\Users\\s1x\\Downloads\\CWD\\Project\\analysis")

library(ggplot2)
library(stringr)
library(dplyr)
library(scales)

ufcstats_org <- read.csv("C:\\Users\\s1x\\Downloads\\CWD\\Project\\data_scraping\\ufcstats.csv", header = TRUE, sep = ",")
ufcstats <- ufcstats_org


# Converts control time from character string in format "mm:ss" to
# integer that represents total number of seconds of control time.
convert_ctrl_time <- function(vec) {
  vec[vec == "--"] <- "0:00"
  list_split <- strsplit(vec, ":")
  lapply(list_split, function(x) {
    return(as.numeric(x[1]) * 60 + as.numeric(x[2]))
  })
}

ufcstats$red_ctrl_time_tot <- unlist(convert_ctrl_time(ufcstats$red_ctrl_time_tot))
ufcstats$red_ctrl_time_rnd_1 <- unlist(convert_ctrl_time(ufcstats$red_ctrl_time_rnd_1))
ufcstats$red_ctrl_time_rnd_2 <- unlist(convert_ctrl_time(ufcstats$red_ctrl_time_rnd_2))
ufcstats$red_ctrl_time_rnd_3 <- unlist(convert_ctrl_time(ufcstats$red_ctrl_time_rnd_3))
ufcstats$red_ctrl_time_rnd_4 <- unlist(convert_ctrl_time(ufcstats$red_ctrl_time_rnd_4))
ufcstats$red_ctrl_time_rnd_5 <- unlist(convert_ctrl_time(ufcstats$red_ctrl_time_rnd_5))

ufcstats$blue_ctrl_time_tot <- unlist(convert_ctrl_time(ufcstats$blue_ctrl_time_tot))
ufcstats$blue_ctrl_time_rnd_1 <- unlist(convert_ctrl_time(ufcstats$blue_ctrl_time_rnd_1))
ufcstats$blue_ctrl_time_rnd_2 <- unlist(convert_ctrl_time(ufcstats$blue_ctrl_time_rnd_2))
ufcstats$blue_ctrl_time_rnd_3 <- unlist(convert_ctrl_time(ufcstats$blue_ctrl_time_rnd_3))
ufcstats$blue_ctrl_time_rnd_4 <- unlist(convert_ctrl_time(ufcstats$blue_ctrl_time_rnd_4))
ufcstats$blue_ctrl_time_rnd_5 <- unlist(convert_ctrl_time(ufcstats$blue_ctrl_time_rnd_5))


# Checking out situation with divisions
table(ufcstats$division)

# Clearing up data into appropriate categories
ufcstats$division[grepl("Women's Bantamweight", ufcstats$division, fixed = TRUE)] <- "Women's Bantamweight Bout"
ufcstats$division[grepl("Women's Featherweight", ufcstats$division, fixed = TRUE)] <- "Women's Featherweight Bout"
ufcstats$division[grepl("Women's Flyweight", ufcstats$division, fixed = TRUE)] <- "Women's Flyweight Bout"
ufcstats$division[grepl("Women's Strawweight", ufcstats$division, fixed = TRUE)] <- "Women's Strawweight Bout"

ufcstats$division[!grepl("Women's", ufcstats$division, fixed = TRUE) & grepl("Bantamweight", ufcstats$division, fixed = TRUE)] <- "Bantamweight Bout"
ufcstats$division[!grepl("Women's", ufcstats$division, fixed = TRUE) & grepl("Flyweight", ufcstats$division, fixed = TRUE)] <- "Flyweight Bout"
ufcstats$division[!grepl("Women's", ufcstats$division, fixed = TRUE) & grepl("Featherweight", ufcstats$division, fixed = TRUE)] <- "Featherweight Bout"
ufcstats$division[!grepl("Women's", ufcstats$division, fixed = TRUE) & grepl("Lightweight", ufcstats$division, fixed = TRUE)] <- "Lightweight Bout"
ufcstats$division[!grepl("Women's", ufcstats$division, fixed = TRUE) & grepl("Welterweight", ufcstats$division, fixed = TRUE)] <- "Welterweight Bout"
ufcstats$division[!grepl("Women's", ufcstats$division, fixed = TRUE) & grepl("Middleweight", ufcstats$division, fixed = TRUE)] <- "Middleweight Bout"
ufcstats$division[!grepl("Women's", ufcstats$division, fixed = TRUE) & grepl("Light Heavyweight", ufcstats$division, fixed = TRUE)] <- "Light Heavyweight Bout"
ufcstats$division[!grepl("Light", ufcstats$division, fixed = TRUE) & grepl("Heavyweight", ufcstats$division, fixed = TRUE)] <- "Heavyweight Bout"
ufcstats$division[grepl("Tournament", ufcstats$division, fixed = TRUE) | grepl("Championship", ufcstats$division, fixed = TRUE)] <- "Catch Weight Bout"


levels(ufcstats$division)

ufcstats$division_type <- as.factor(ifelse(grepl("Women's", ufcstats$division, fixed = TRUE), "Female Division", "Male Division"))
levels(ufcstats$division_type)
table(ufcstats$division_type)

ufcstats$division <- str_remove(ufcstats$division, fixed("Women's "))
ufcstats$division <- factor(ufcstats$division, levels = c("Strawweight", "Flyweight", "Bantamweight", 
                                                          "Featherweight", "Lightweight", "Welterweight", 
                                                          "Middleweight", "Light Heavyweight", "Heavyweight", 
                                                          "Catch Weight", "Open Weight"))
ufcstats$division <- str_remove(ufcstats$division, fixed(" Bout"))


#womens_divisions <- ufcstats$division
#womens_divisions <- droplevels(womens_divisions, levels(womens_divisions)[!grepl("Women's", levels(womens_divisions), fixed = TRUE)])
#womens_divisions <- womens_divisions[!is.na(womens_divisions)]

################################################################################
# Plotting number of fights per division
ggplot(data = ufcstats, aes(x = division)) +
  geom_bar(aes(fill = division_type)) +
  labs(title = "Number of fights per devision", x = "Division", y = "Number of fights") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))



division_type_df <- data.frame(
  Division = names(table(ufcstats$division_type)),
  value = as.vector(table(ufcstats$division_type))
) %>%
  mutate(
    cumulative = cumsum(rev(value)),
    midpoint = cumulative - rev(value) / 2,
    label = paste0(rev(round(value / sum(value) * 100, 1)), "%")
  )

################################################################################
# Plotting proportion of fights based on division type
ggplot(data = division_type_df, aes(x = "", y = value, fill = Division)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(x = 1.2, y = midpoint, label = label)) +
  ggtitle("Proportion of fights based on division type") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))






table(ufcstats$win_method)

################################################################################
# Plotting win method frequencies depending on divisions
ggplot(data = ufcstats, aes(x = division)) +
  geom_bar(aes(fill = win_method)) +
  labs(title = "Win method frequency over all divisions", x = "Division", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))

ggplot(data = ufcstats, aes(x = division)) +
  geom_bar(aes(fill = win_method), position = "fill") +
  labs(title = "Win method proportions over all divisions", x = "Division", y = "Proportion of win methods in the division") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))

ggplot(data = ufcstats, aes(x = win_method)) +
  geom_bar(aes(y = (..count..)/tapply(..count.., ..PANEL.., sum)[..PANEL..], fill = win_method)) +
  scale_y_continuous(labels = percent_format()) +
  facet_wrap(~ division, nrow = 3) +
  labs(title = "Win method proportions over all divisions", y = "Percentage of win method frequency") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 20)
        )




################################################################################
# Total significant strikes output in a match over all divisions
ufcstats$total_sig_str_attempt <- ufcstats$red_sig_str_attempt_tot + ufcstats$blue_sig_str_attempt_tot

ggplot(data = ufcstats, aes(x = division, y = total_sig_str_attempt)) +
  geom_boxplot(outlier.shape=NA, fill = "#ff7d7d", color = "#bf2222") +
  coord_cartesian(ylim = c(0, 700)) +
  labs(title = "Total significant strikes output in a match over all divisions", x = "Division", y = "Total significant strikes in a match") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))


################################################################################
# Total knockdowns in a match over all divisions
ufcstats$total_kd = ufcstats$red_kd_tot + ufcstats$blue_kd_tot

ggplot(data = ufcstats, aes(x = division, y = total_kd)) + 
  geom_boxplot(fill = "#ff7d7d", color = "#bf2222") +
  labs(title = "Total knockdowns in a match over all divisions", x = "Division", y = "Total knockdowns in a match") +
  theme(plot.title = element_text(hjust = 0.5))


ufcstats$total_kd[ufcstats$division == "Strawweight Bout"]
ufcstats$total_kd[ufcstats$division == "Catch Weight Bout"]
ufcstats$total_kd[ufcstats$division == "Open Weight Bout"]



################################################################################
# Total control time in a match over all divisions

ufcstats$total_ctrl_time <- ufcstats$red_ctrl_time_tot + ufcstats$blue_ctrl_time_tot

ggplot(data = ufcstats, aes(x = division, y = total_ctrl_time)) + 
  geom_boxplot(fill = "#ff7d7d", color = "#bf2222") +
  labs(title = "Total control time in a match over all divisions", x = "Division", y = "Total control time in a match in seconds") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))

ggplot(data = ufcstats, aes(x = total_ctrl_time)) + 
  geom_histogram(binwidth = 200, aes(y = (..count..)/tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
  scale_y_continuous(labels = percent_format()) +
  facet_wrap(~ division, nrow = 3) +
  labs(title = "Total control time in a match over all divisions", x = "Division", y = "Total control time in a match") +
  theme(plot.title = element_text(hjust = 0.5))


################################################################################
# What has an effect on win method

ggplot(ufcstats) +
  geom_density(aes(x = red_sig_str_attempt_tot))
  


ufcstats$red_sig_str_prec_tot
ufcstats$red_sig_str_attempt_tot
ufcstats$red_ctrl_time_tot

winner_df <- data.frame(
  winner_method = ufcstats$win_method,
  winner_sig_str_attempt = ifelse(ufcstats$winner == "red", ufcstats$red_sig_str_attempt_tot, ufcstats$blue_sig_str_attempt_tot),
  winner_sig_str_prec = ifelse(ufcstats$winner == "red", ufcstats$red_sig_str_prec_tot, ufcstats$blue_sig_str_prec_tot),
  winner_ctrl_time = ifelse(ufcstats$winner == "red", ufcstats$red_ctrl_time_tot, ufcstats$blue_ctrl_time_tot),
  winner_distance = ifelse(ufcstats$winner == "red", ufcstats$red_sig_str_dist_attempt_tot, ufcstats$blue_sig_str_dist_attempt_tot),
  winner_clinch = ifelse(ufcstats$winner == "red", ufcstats$red_sig_str_clinch_attempt_tot, ufcstats$blue_sig_str_clinch_attempt_tot),
  winner_ground = ifelse(ufcstats$winner == "red", ufcstats$red_sig_str_gnd_attempt_tot, ufcstats$blue_sig_str_gnd_attempt_tot)
)

winner_df <- winner_df[winner_df$winner_method %in% c("Decision - Split", "Decision - Unanimous", "KO/TKO", "Submission"), ]

ggplot(winner_df, aes(x = winner_sig_str_attempt, fill = winner_method)) +
  geom_density(alpha = 0.4) +
  labs(title = "Significant strikes in a fight density function by win method", x = "# of significant strikes", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))


ggplot(winner_df, aes(x = winner_sig_str_prec, fill = winner_method)) +
  geom_density(alpha = 0.4) +
  labs(title = "Significant strikes accuracy in a fight density function by win method", x = "% of accurate significant strikes", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))

ggplot(winner_df, aes(x = winner_ctrl_time / 60, fill = winner_method)) +
  geom_density(alpha = 0.4) +
  labs(title = "Control time in a fight density function by win method", x = "Control time in minutes (m)", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))






################################################################################
# # of rounds analysis


ufcstats$total_fight_time <- ifelse(ufcstats$win_round > 1, 
                                    (ufcstats$win_round - 1) * 300 + unlist(convert_ctrl_time(ufcstats$win_time)),
                                    unlist(convert_ctrl_time(ufcstats$win_time)))

cropped_df <- ufcstats[ufcstats$time_format == "3 Rnd (5-5-5)" | ufcstats$time_format == "5 Rnd (5-5-5-5-5)", ]
cropped_df <- cropped_df[cropped_df$win_method %in% c("Decision - Split", "Decision - Unanimous", "KO/TKO", "Submission"), ]

cropped_df$strikes_per_second <- cropped_df$total_sig_str_attempt / cropped_df$total_fight_time

ggplot(data = cropped_df, aes(x = time_format, y = ..count..)) +
  geom_bar(aes(fill = win_method), position = "dodge") +
  labs(title = "Win method frequency by fight time format (Count)", x = "Time format", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = cropped_df, aes(x = win_method)) +
  geom_bar(aes(y = (..count..)/tapply(..count.., ..PANEL.., sum)[..PANEL..], fill = win_method)) +
  facet_wrap(~time_format) +
  labs(title = "Win method proportion by fight time format", y = "Win method proportion") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 20)
  )



ggplot(cropped_df, aes(x = strikes_per_second, fill = win_method)) +
  geom_density(alpha = 0.4) +
  labs(title = "Strikes per second density by win method", x = "Strikes per second", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))

ggplot(cropped_df, aes(x = strikes_per_second, fill = time_format)) +
  geom_density(alpha = 0.4) +
  labs(title = "Strikes per second density by time format", x = "Strikes per second", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5))















