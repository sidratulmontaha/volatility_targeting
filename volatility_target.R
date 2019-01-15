library(quantmod)
library(PerformanceAnalytics)

Sys.setenv(TZ = "Europe/London")
options(scipen = 999)

high_risk_asset <- "MSFT" #S&P 500 index
low_risk_asset <- "SHY" #colnames(high_risk_asset_prices) <- c("price", "delta")

high_risk_asset_prices <- getSymbols(high_risk_asset, src = 'yahoo', from = "2017-01-01", auto.assign = FALSE)
high_risk_asset_prices <- high_risk_asset_prices[, "MSFT.Adjusted"]
high_risk_asset_prices <- cbind(high_risk_asset_prices, Delt(high_risk_asset_prices[, "MSFT.Adjusted"]))
colnames(high_risk_asset_prices) <- c("price", "delta")
high_risk_asset_prices <- cbind(high_risk_asset_prices, vol = rollapplyr(high_risk_asset_prices[, "delta"], 30, sd, fill = NA, by.column = FALSE))
colnames(high_risk_asset_prices) <- c("price", "delta", "vol")

high_risk_asset_implied_vol <- "^VIX"
high_risk_asset_implied_vol_prices <- getSymbols(high_risk_asset_implied_vol, src = 'yahoo', from = "2017-01-01", auto.assign = FALSE)
colnames(high_risk_asset_implied_vol_prices) <- c("Open", "High", "Low", "Close", "Volume", "Adj")
high_risk_asset_implied_vol_prices <- high_risk_asset_implied_vol_prices[, "Adj"]

low_risk_asset_prices <- getSymbols(low_risk_asset, src = 'yahoo', from = "2017-01-01", auto.assign = FALSE)
low_risk_asset_prices <- low_risk_asset_prices[, "SHY.Adjusted"]
low_risk_asset_prices <- cbind(low_risk_asset_prices, Delt(low_risk_asset_prices[, "SHY.Adjusted"]))
colnames(low_risk_asset_prices) <- c("price", "delta")
low_risk_asset_prices <- cbind(low_risk_asset_prices, vol = rollapplyr(low_risk_asset_prices[, "delta"], 30, sd, fill = NA, by.column = FALSE))
colnames(low_risk_asset_prices) <- c("price", "delta", "vol")

vols <- cbind(high_risk_asset_prices[, "price"], low_risk_asset_prices[, "price"], high_risk_asset_prices[, "delta"], low_risk_asset_prices[, "delta"], high_risk_asset_prices[, "vol"], low_risk_asset_prices[, "vol"])
colnames(vols) <- c("price_h", "price_l", "delta_h", "delta_l", "vol_h", "vol_l")
vols <- cbind(vols, rollapplyr(vols, 30, function(x) cov(x[, "delta_h"], x[, "delta_l"], use = "complete.obs"), by.column = FALSE, fill = NA))
colnames(vols) <- c("price_h", "price_l", "delta_h", "delta_l", "vol_h", "vol_l", "cov_hl")

target_vol_annualised <- 0.1
target_vol <- target_vol_annualised / (sqrt(252))
rebal_buffer <- 0.1

vols <- cbind(vols, rollapplyr(vols, 1, function(x) {
    A <- x$cov_hl ^ 2 - x$vol_h ^ 2 * x$vol_l ^ 2 + x$vol_h ^ 2 * target_vol ^ 2 + x$vol_l ^ 2 * target_vol ^ 2 - 2 * target_vol ^ 2 * x$cov_hl
    x$weight_h <- (x$vol_l ^ 2 - x$cov_hl + sqrt(A)) / (x$vol_h ^ 2 + x$vol_l ^ 2 - 2 * x$cov_hl)
}, by.column = FALSE, fill = NA))

colnames(vols) <- c("price_h", "price_l", "delta_h", "delta_l", "vol_h", "vol_l", "cov_hl", "weight_h")

vols <- cbind(vols, c(rep(0, nrow(vols))), c(rep(0, nrow(vols))), c(rep(0, nrow(vols))))
colnames(vols) <- c("price_h", "price_l", "delta_h", "delta_l", "vol_h", "vol_l", "cov_hl", "weight_h", "rebal_h", "rebal_l", "final_weight_h")

for (row in 1:nrow(vols)) {
    if (!is.na(vols[row,]$weight_h)) {

        if (coredata(vols[row, ]$weight_h) < coredata(vols[row - 1, ]$rebal_l) || coredata(vols[row, ]$weight_h) > coredata(vols[row - 1, ]$rebal_h)) {
            vols[row,]$final_weight_h <- vols[row,]$weight_h
            vols[row,]$rebal_l <- vols[row,]$weight_h - rebal_buffer * vols[row,]$weight_h
            vols[row,]$rebal_h <- vols[row,]$weight_h + rebal_buffer * vols[row,]$weight_h
        } else {
            vols[row,]$rebal_l <- vols[row - 1,]$rebal_l
            vols[row,]$rebal_h <- vols[row - 1,]$rebal_h
            vols[row,]$final_weight_h <- vols[row - 1,]$final_weight_h
        }
    }
}

vols$final_weight_l <- 1 - vols$final_weight_h

#----------------------Strategy Performance - ------------------------------------------

vols$portfolio_return <- vols$final_weight_h * vols$delta_h + vols$final_weight_l * vols$delta_l

print(paste0("Achieved overall portfolio volatility: ", sd(vols$portfolio_return, na.rm = TRUE) * sqrt(252)))
Return.annualized(vols$portfolio_return)