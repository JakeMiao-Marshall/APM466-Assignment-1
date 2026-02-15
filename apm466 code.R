library(tidyverse)
library(lubridate)
library(knitr)

setwd("/home/rstudio/apm466")



#this part is unnecessary, i could have just done this by hand
# Pick 10 bonds 

bond_list <- read_csv("bonds_master.csv", show_col_types = FALSE) %>%
  transmute(
    bond_id = as.character(isin),
    coup = as.numeric(coupon),
    born = ymd(issue_date),
    dies = ymd(maturity_date)
  )

snap_day <- ymd("2026-01-05")

# picking bonds
potential_bonds <- bond_list %>%
  mutate(years_left = as.numeric(dies - snap_day) / 365) %>%
  filter(years_left > 0, years_left <= 5) %>%
  arrange(years_left)

goal_years <- seq(0.5, 5.0, length.out = 10) # we are just looking for 10 equally spaced out bonds that make it easy to boostrap

grab_closest_bond <- function(goal) {
  potential_bonds %>%
    mutate(how_off = abs(years_left - goal)) %>%
    arrange(how_off, dies) %>%
    slice(1) %>%
    mutate(goal = goal)
}

my_10_bonds <- map_dfr(goal_years, grab_closest_bond) %>%
  distinct(bond_id, .keep_all = TRUE) %>%
  arrange(goal)

print(
  my_10_bonds %>% select(goal, bond_id, coup, born, dies, years_left),
  n = Inf
)

write_csv(my_10_bonds, "chosen10_bonds.csv")
#not needed for assignment submission but helpful

# 4a

price_book <- read_csv("bond_prices.csv")
bond_info  <- read_csv("bonds_master.csv")

big_table <- merge(price_book, bond_info, by = "isin")


# YTM calculation first


ytm_for_one_line <- function(price_today, settle_day, end_day, coup_rate) {
  
  # find how many half-years left
  half_year_chunks <- round(as.numeric(end_day - settle_day) / 182.5)
  
  face_value <- 100
  coupon_cash <- face_value * coup_rate / 2
  
  price_error <- function(y) {
    k <- 1:half_year_chunks
    fair_price <- sum(coupon_cash / (1 + y/2)^k) + face_value / (1 + y/2)^half_year_chunks
    fair_price - price_today
  }
  
  uniroot(price_error, lower = 0.0001, upper = 0.30)$root
}

# we need to solve for roots here, thus we need uniroot function

#just comparing the pricing differences 
big_table$yield_guess <- mapply(
  ytm_for_one_line,
  big_table$close_price,
  as.Date(big_table$date),
  as.Date(big_table$maturity_date),
  big_table$coupon
)

# Superimposing

count_half_years <- function(settle_day, end_day) {
  as.integer(round(as.numeric(difftime(end_day, settle_day, units = "days")) / 182.5))
}

# years to maturity calc
big_table$years_left <- as.numeric(as.Date(big_table$maturity_date) - as.Date(big_table$date)) / 365

# we will only keep 0–5 years for the chart as requested
tiny <- big_table[big_table$years_left > 0 & big_table$years_left <= 5, ]

grid_years <- 1:5

curves <- do.call(rbind, lapply(split(tiny, tiny$date), function(d) {
  d <- d[order(d$years_left), ]
  y_grid <- approx(d$years_left, d$yield_guess, xout = grid_years, rule = 2)$y
  data.frame(date = unique(d$date), term = grid_years, ytm = y_grid)
}))

ggplot(curves, aes(x = term, y = 100 * ytm, group = date)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 1, alpha = 0.7) +
  labs(
    title = "5 Year Yield Curve Superimposed",
    x = "Term to maturity (years)",
    y = "Yield (%)"
  ) +
  theme_minimal()

#i think this is what the figure they want?

ggsave("yield_curves_superimposed.pdf", width = 6, height = 3)

# 4b spot rates

prices <- read_csv("bond_prices.csv")
info   <- read_csv("bonds_master.csv")
df <- merge(prices, info, by = "isin")

# i really dont need to make new variables for this, but i guess its fine
df$date <- as.Date(df$date)
df$maturity_date <- as.Date(df$maturity_date)

df <- df %>%
  mutate(ttm_half = as.integer(round(as.numeric(maturity_date - date) / 182.5))) %>%
  filter(ttm_half > 0, ttm_half <= 10)

# i would solve spot rates for every year only but then we would only be using data
# from 5 bonds. instead we will do a semi annual computation like the hints suggests.
# use semi annual boostrapping formula: 
bootstrap_day <- function(day_df) {
  face <- 100
  spot_half <- rep(NA_real_, 10)
  
  day_df <- day_df[order(day_df$ttm_half), ]
  
  for (i in 1:nrow(day_df)) {
    n <- day_df$ttm_half[i]
    
    P <- day_df$close_price[i]
    C <- face * day_df$coupon[i] / 2
    
    pv_early <- 0
    if (n > 1) {
      for (k in 1:(n - 1)) {
        pv_early <- pv_early + C / (1 + spot_half[k] / 2)^k
      }
    }
    
    spot_half[n] <- 2 * ((((C + face) / (P - pv_early))^(1 / n)) - 1)
  }
  
  keep <- 2:10
  data.frame(date = unique(day_df$date), term = keep / 2, spot = spot_half[keep])
}
# just rearranging formula in hints. make sure to explain this in pseudo code.
spot_curves <- bind_rows(lapply(split(df, df$date), bootstrap_day))

p <- ggplot(spot_curves, aes(x = term, y = 100 * spot, group = date)) +
  geom_line(alpha = 0.6) +
  geom_point(size = 1, alpha = 0.6) +
  labs(
    title = "5 Year Spot Curve (semi-annual)",
    x = "Term to Maturity (years)",
    y = "Spot rate (%)"
  ) +
  theme_minimal()

p
ggsave("spot_curves.pdf", plot = p, width = 6, height = 3)

 
# 4c

days <- sort(unique(spot_curves$date))

forward_curves <- data.frame()

for (d in days) {
  
  day_spots <- spot_curves[spot_curves$date == d, ]
  day_spots <- day_spots[order(day_spots$term), ]
  # we are already computed spot curves, so forward curves is really straightforward
  # lets just apply forward rate hints from assignment page.
  # remember its semi-annual
  
  S1 <- day_spots$spot[1]
  S2 <- day_spots$spot[2]
  S3 <- day_spots$spot[3]
  S4 <- day_spots$spot[4]
  S5 <- day_spots$spot[5]
  
  f2 <- (( (1 + S2/2)^(2*2) ) / ( (1 + S1/2)^(2*1) ))^(1/(2*(2-1))) - 1
  f3 <- (( (1 + S3/2)^(2*3) ) / ( (1 + S1/2)^(2*1) ))^(1/(2*(3-1))) - 1
  f4 <- (( (1 + S4/2)^(2*4) ) / ( (1 + S1/2)^(2*1) ))^(1/(2*(4-1))) - 1
  f5 <- (( (1 + S5/2)^(2*5) ) / ( (1 + S1/2)^(2*1) ))^(1/(2*(5-1))) - 1
  
  forward_curves <- rbind(forward_curves,
                          data.frame(date = d, term = 2, forward = f2),
                          data.frame(date = d, term = 3, forward = f3),
                          data.frame(date = d, term = 4, forward = f4),
                          data.frame(date = d, term = 5, forward = f5))
}
# we are just applying formula from hints again. 
# each day has its own curve as usual. kinda mind boggling.
ggplot(forward_curves, aes(x = term, y = forward, group = date)) +
  geom_line(alpha = 0.6) +
  geom_point(size = 1, alpha = 0.6) +
  labs(
    title = "1-Year Forward Curve",
    x = "Years",
    y = "Forward rate"
  ) +
  theme_minimal()
p <- ggplot(forward_curves, aes(x = term, y = forward, group = as.factor(date))) +
  geom_line(alpha = 0.6) +
  geom_point(size = 1, alpha = 0.6) +
  labs(
    title = "1-Year Forward Curve",
    x = "Years",
    y = "Forward rate"
  ) +
  theme_minimal()

ggsave("forward_curves_superimposed.pdf", plot = p, width = 6, height = 3)

# q5

# just turning yield curve data into a table so we can compute covariance 
yield_table <- curves %>%
  arrange(date, term) %>%
  mutate(yield_name = paste0("Y", term)) %>%
  select(date, yield_name, ytm) %>%
  pivot_wider(names_from = yield_name, values_from = ytm)

# turning forward rates into tables as well so we can compute covariance 
forward_table <- forward_curves %>%
  arrange(date, term) %>%
  mutate(forward_name = paste0("F1-", term - 1)) %>%
  select(date, forward_name, forward) %>%
  pivot_wider(names_from = forward_name, values_from = forward)

yield_names <- paste0("Y", 1:5)
yield_log_returns <- yield_table %>% arrange(date)

# applying log 
for (name in yield_names) {
  yield_log_returns[[name]] <- log(yield_table[[name]] / dplyr::lag(yield_table[[name]]))
}
yield_log_returns <- yield_log_returns %>% drop_na()

forward_names <- c("F1-1","F1-2","F1-3","F1-4")
forward_log_returns <- forward_table %>% arrange(date)

# again
for (name in forward_names) {
  forward_log_returns[[name]] <- log(forward_table[[name]] / dplyr::lag(forward_table[[name]]))
}
forward_log_returns <- forward_log_returns %>% drop_na()

yield_covariance <- cov(yield_log_returns[, yield_names])
forward_covariance <- cov(forward_log_returns[, forward_names])

options(scipen = 999)

kable(round(yield_covariance, 8), format = "latex", booktabs = TRUE, caption = "Yield log covariance")
kable(round(forward_covariance, 8), format = "latex", booktabs = TRUE, caption = "Forward log covariance")

# Q6
# easy way to find eigenvalues and vectors
eigen_yield <- eigen(yield_covariance)
eigen_forward <- eigen(forward_covariance)

# take advantage that R stores largest index first
yield_largest_eigenvalue <- eigen_yield$values[1]
yield_largest_eigenvector <- eigen_yield$vectors[, 1]

forward_largest_eigenvalue <- eigen_forward$values[1]
forward_largest_eigenvector <- eigen_forward$vectors[, 1]

yield_largest_eigenvalue
yield_largest_eigenvector

forward_largest_eigenvalue
forward_largest_eigenvector