## Load packages----

library(ggplot2)
library(zoo)
library(strucchange)
library(aTSA)

# Working Directory
set.seed(1896)
# setwd("YOUR PATH") # specify as needed >> please be aware that the csv file has to be in the same folder and graphs are also saved in the same folder of the script

data <- read.csv("google_trends_topic_modeling.csv")
data$date <- as.Date(paste0(data$year, "-01"), format="%Y-%m-%d")

# Plotting the timeline
ggplot(data, aes(x = date, y = value)) + 
  geom_line(col = "darkred", linetype = "solid", size = 1.4) +
  theme_bw() +
  labs(x = "Date", y = "Relative Interest", title = "Google Trends: Evolution of Searches for Topic Modeling") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "google_trends1.png", plot = last_plot(), scale = 1, dpi = 600) 
  


# Subset the data for dates after 01-01-2018
filtered_data <- subset(data, date >= as.Date("2018-01-01"))

# Plot the filtered data
ggplot(filtered_data, aes(x = date, y = value)) + 
  geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2021-12-01"), ymin = -Inf, ymax = Inf),
            fill = "lightgray", alpha = 0.2) +
  geom_line(col = "darkred", linetype = "solid", size = 1.4) +
  theme_bw() +
  labs(x = "Date", y = "Relative Interest", title = "Google Trends: Evolution of Searches for Topic Modeling - since 2018") +
  theme(plot.title = element_text(hjust = 0.5)) +  ylim(0, 100) 

ggsave(filename = "google_trends2.png", plot = last_plot(), scale = 1, dpi = 600) 

  
### Stationarity tests ----

# Define the functions
l_4 <- function(T) floor(4 * (T/100)^(1/4))
l_12 <- function(T) floor(12 * (T/100)^(1/4))
l <- function(T) floor(4 * ((T/100)^(2/9)))
l_dq <- function(T) floor(3 * (sqrt(T)/13))

# Generate data points
T_values <- seq(0, 500, by = 1)
data2 <- data.frame(T = T_values, l_4 = l_4(T_values), l_12 = l_12(T_values), l = l(T_values), l_dq = l_dq(T_values))

# Create ggplot object and plot
ggplot(data2, aes(x = T)) +
  geom_line(aes(y = l_4+.15, color = "l_4"), size = 1, linetype = "solid") +
  geom_line(aes(y = l_12, color = "l_12"), size = 1) +
  geom_line(aes(y = l, color = "l"), size = 1, linetype = "solid") +
  geom_line(aes(y = l_dq, color = "l_dq"), size = 1, linetype = "solid") +
  geom_vline(xintercept = 241, linetype = "dashed", color = "red", size = .7) + 
  scale_color_manual(values = c("l_4" = "black", "l_12" = "darkred", "l" = "darkgrey", "l_dq" = "darkgreen"), 
                     labels = c(expression("l"[S4]) , expression("l"[S12]), expression("l"[NW]), expression("l"[aTSA]))) +
  labs(x = "Number of time periods", y = "lag length", title = "Optimal lag length according to the presented selection heuristics") +
  theme_bw() +  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
ggsave(filename = "lag_length.png", plot = last_plot(), scale = 1, dpi = 600) 


# 
aTSA::adf.test(data$value)
aTSA::adf.test(data$value, nlag = 5)
aTSA::kpss.test(data$value, lag.short = TRUE)
aTSA::kpss.test(data$value, lag.short = FALSE)

l_4(241)
l_12(241)
l(241)
l_dq(241)

### Structural Changes ----
reg <- data$value ~ data$date
strucchange::sctest(data$value ~ data$date, type = "Chow", point = 204)

results_list <- list()
# Loop through values 10 to 30 for x
for (x in 193:216) {
  # Execute the sctest command for each value of x
  result <- strucchange::sctest(data$value ~ data$date, type = "Chow", point = x)
  
  # Store the result in the list
  results_list[[as.character(x)]] <- result
}

# Display the results
for (x in 193:216) {
  cat("For point =", x, "\n")
  print(results_list[[as.character(x)]])
  cat("\n")
}


fs <- Fstats(data$value ~ data$date, from = 193, to = 216, data = data)
breakpoints(fs)
boundary(fs)
boundary(fs, aveF = TRUE)
boundary(fs, alpha = 0.01)

# Customize x-axis labels
fsz <- merge(
  "Fstats" = as.zoo(fs$Fstats),
  "boundary_supF" = as.zoo(boundary(fs, alpha = 0.05)),
  "boundary_aveF" = as.zoo(boundary(fs, alpha = 0.05, aveF = TRUE))
)

fsd1 <- fortify(fsz)
head(fsd1)

# Plot the F statistics using ggplot
y_inter1 <- fsd1$boundary_supF
y_inter2 <- fsd1$boundary_aveF

ggplot(fsd1, aes(x = Index, y = Fstats)) +
  geom_line(col = "darkred", linetype = "solid", size = 1.4) +
  geom_hline(yintercept = y_inter1, linetype = "dashed", color = "red") +   
  geom_hline(yintercept = y_inter2, linetype = "dashed", color = "blue") +   
  theme_bw() +
  labs(x = "Date", y = "F statistic", title = "F Statistics Over Time") +
  theme(plot.title = element_text(hjust = 0.5)) 



# Subset the data for dates after 01-01-2018
reduced_data <- subset(data, date >= as.Date("2020-01-01"))
reduced_data <- subset(reduced_data, date <= as.Date("2021-12-01"))

app_data <- cbind(reduced_data, fsd1)

# Plot the filtered data

ggplot(app_data, aes(x = date, y = Fstats)) + 
  geom_line(col = "darkred", linetype = "solid", size = 1.4) +
  #geom_hline(yintercept = y_inter, linetype = "dashed", color = "red") +  
  theme_bw() +
  labs(x = "Date", y = "F-Statistics Chow Test", title = "F Statistics for a structural break over time") +
  theme(plot.title = element_text(hjust = 0.5)) +  ylim(0, 20)+
  geom_vline(xintercept = as.Date("2021-10-15"), linetype = "dashed", color = "red", size = 1) +   
  geom_vline(xintercept = as.Date("2021-08-15"), linetype = "dashed", color = "blue", size = 1) +  
  geom_hline(yintercept = y_inter1, linetype = "dashed", color = "red") +   
  geom_hline(yintercept = y_inter2, linetype = "dashed", color = "blue")  

ggsave(filename = "fstats1.png", plot = last_plot(), scale = 1, dpi = 600) 


# Plot the filtered data
ggplot(app_data, aes(x = date, y = value)) + 
  geom_line(col = "darkred", linetype = "solid", size = 1.4) +
  theme_bw() +
  labs(x = "Date", y = "Relative Interest") +
  theme(plot.title = element_text(hjust = 0.5)) +  ylim(0, 100) #+

ggsave(filename = "google_trends3.png", plot = last_plot(), scale = 1, dpi = 600) 



# Plotting the timeline
ggplot(data, aes(x = date, y = value)) + 
  geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2021-12-01"), ymin = -Inf, ymax = Inf),
            fill = "lightgray", alpha = 0.2) +
  geom_line(col = "darkred", linetype = "solid", size = 1.4) +
  theme_bw() +
  labs(x = "Date", y = "Relative Interest", title = "Google Trends: Evolution of Searches for Topic Modeling") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = as.Date("2021-10-15"), linetype = "dashed", color = "red", size = 1) +  
  geom_vline(xintercept = as.Date("2021-08-15"), linetype = "dashed", color = "blue", size = 1)   

ggsave(filename = "google_trends4.png", plot = last_plot(), scale = 1, dpi = 600) 
