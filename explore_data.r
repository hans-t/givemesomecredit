setwd('~/Desktop/honestbee/')
library(data.table)

dt <- fread("data/cs-training.csv")
dt$V1 <- NULL
w.income <- dt[!is.na(MonthlyIncome)]
na.income <- dt[is.na(MonthlyIncome)]


# Take care of edge cases
income.zero <- w.income[MonthlyIncome == 0]
income.non.zero <- w.income[MonthlyIncome > 0]
income.one <- w.income[MonthlyIncome == 1]
income.1000 <- w.income[MonthlyIncome == 1000]

summary(income.zero)
summary(income.non.zero)
summary(income.one)

## How do you compute debtratio if monthly income is zero?
### DebtRatio of incomes zero vs nonzero
hist(log2(income.zero$DebtRatio), probability = TRUE, xlim = c(-20, 20), col=rgb(1,0,0,0.5), xlab = 'Log2(DebtRatio)')
hist(log2(income.non.zero$DebtRatio), probability = TRUE, add = TRUE, col=rgb(0,0,1,0.5))

### DebtRatio of incomes 0 vs 1 vs 1000
hist(log2(income.zero$DebtRatio), probability = TRUE, xlim = c(-20, 20), col=rgb(1,0,0,0.3), xlab = 'Log2(DebtRatio)')
hist(log2(income.one$DebtRatio), probability = TRUE, add = TRUE, col=rgb(0,0,1,0.3))
hist(log2(income.1000$DebtRatio), probability = TRUE, add = TRUE, col=rgb(0,1,0,0.3))

### NA income
hist(log2(income.zero$DebtRatio), probability = TRUE, xlim = c(-20, 20), col=rgb(1,0,0,0.3), xlab = 'Log2(DebtRatio)')
hist(log2(na.income$DebtRatio), probability = TRUE, add = TRUE, col=rgb(0,0,1,0.3))

### Based on the distribution, income.1000 is in average has is shited to the left by 10 units. However,
### for income 0 and 1, it has similar distributions. Apparently, for calculation of DebtRatio, income = 0 or NA is treated as income 1.
### we can actually get monthly debt payment.


monthly.income <- dt$MonthlyIncome
monthly.income[is.na(monthly.income)] <- 1
monthly.income[monthly.income == 0] <- 1

dt$MonthlyDebtPayment <- dt$DebtRatio * monthly.income


# now for income outlier (too low and too high)
# US income dist (25th, 50th, 75th, 85th, 95th percentile): 27500, 52500, 100000, 135000, 250000
# Divide by 12: 2300, 4400, 8300, 11250, 20000
summary(w.income$MonthlyIncome)
summary(w.income[MonthlyIncome < 3000]$MonthlyIncome)
hist(w.income[MonthlyIncome <= 3000]$MonthlyIncome, breaks=seq(0, 3000, 10), probability = TRUE)
# hist(w.income[MonthlyIncome <= 100]$MonthlyIncome, breaks=seq(0, 100, 1), probability = TRUE)

# Known credit lines and income below 100, THIS DOES NOT WORK
very.low.income <- w.income[`NumberOfTime30-59DaysPastDueNotWorse` < 96 & MonthlyIncome < 100 & MonthlyIncome > 0]
summary(very.low.income)
table(very.low.income$NumberRealEstateLoansOrLines)

# These are probably people who put their MonthlyIncome in thousands.
table(very.low.income$MonthlyIncome)
summary(very.low.income[MonthlyIncome > 8])

summary(very.low.income[MonthlyIncome < 8])


## Inspect those people with income > 100000, does it make sense? If not, then assume it's annual income

high.income <- w.income[MonthlyIncome > 16000]
table(high.income$MonthlyIncome)
summary(high.income)


dlq <- dt[SeriousDlqin2yrs == 1]
good <- dt[SeriousDlqin2yrs == 0]
summary(dlq)
summary(good)

names(dt) <- make.names(names(dt))
