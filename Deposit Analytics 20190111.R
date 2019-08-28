#############################################################################################################################################
#
#                                                             DEPOSIT ANALYTICS
#
#############################################################################################################################################

# Gerard Mazi
# Treasury
# gerard.mazi@homestreet.com
# 206.753.3685 

#############################################################################################################################################
# INITIAL DATA PREPARATION

library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(reshape2)
options(scipen = 999)

'--------------------------------------------------------------'
# Define Weekly vs. Monthly Analysis
freq = 'Monthly'
lag = ifelse(freq == 'Weekly', 5, 1)
#date.format = ifelse(freq == 'Weekly', "%m/%d/%Y", "%Y/%m/%d")
'--------------------------------------------------------------'

# Set working directory
wd = "//fs/Groups/Treasury/CORPORATE TREASURY/Weekly Retail Pricing Package/Deposit Analytics"
setwd(wd)
batch = paste(wd,'/',freq,' Deposit Data', sep = "")

'--------------------------------------------------------------'
# Load any saved data
#load(paste0('deposits_', freq, ".R"))
'--------------------------------------------------------------'

# Load deposit data
filenames = list.files(batch, pattern="*.csv", full.names=TRUE)
deposits = do.call(rbind, lapply(filenames, read.csv, stringsAsFactors = F))

# Set certain fiels to Date format
dates = 
  c("Full.Date",
    "Account.Open.Date",
    "Account.TD.Open.Date",
    "Account.TD.Next.Maturity.Date",
    "Account.Closed.Date")

deposits[,dates] = 
  lapply(
    deposits[,dates], 
    as.Date, 
    format = "%m/%d/%Y"
    )

# Load GL Mapping and merge
GL_mapping = read.csv("Input/GL Mapping.csv", header = TRUE)
deposits = 
  merge(
    deposits, 
    GL_mapping[,c("Account.Type.Code","GL_name","class")],
    by = "Account.Type.Code", 
    all.x = T, 
    all.y = F
    )

# Add a status flag for whether the account is new, existing, or closed as of any Full Date
deposits$account.age = floor((deposits$Full.Date - deposits$Account.Open.Date)/30/1)*1
deposits$status = 
  ifelse(
    deposits$account.age == 0, 
    "new",
    ifelse(
      deposits$Account.Closed.Reason %in% c("B", "C", "E", "K", "P"), 
      "closed",
      "existing"
      )
    )

deposits$existing = 
  ifelse(
    deposits$status == "existing", 
    deposits$Account.Ledger.Balance, 
    0
    )

deposits$new = 
  ifelse(
    deposits$status == "new", 
    deposits$Account.Ledger.Balance, 
    0
    )

# Add depositor age group and flag those with missing age
deposits$age.group = 
  as.character(
    cut(
      deposits$Prmy.Cust.Age, 
      breaks = seq(0, 120, by = 10), 
      right = TRUE
      )
    )
deposits[is.na(deposits$age.group), "age.group"] = "Missing"

# Add balance tier
deposits$tier = 
  cut(
    deposits$Account.Ledger.Balance, 
    breaks = 
      c(0, 500, 1000, 5000, 10000, 25000, 50000, 100000, 250000, 250000000, 1000000), 
    right = TRUE, 
    dig.lab = 6, 
    include.lowest = TRUE
    )

# Add branch state
region = read.csv("Input/branch.csv", stringsAsFactors = F)
deposits = 
  merge(
    deposits, 
    region[,c("Branch.Number","State")], 
    by = "Branch.Number", 
    all.x = T, 
    all.y = F
    )

# Load Effective Fed Funds rate from https://fred.stlouisfed.org/series/FEDFUNDS#0
fed.funds = 
  read.table(
    url("https://fred.stlouisfed.org/data/FEDFUNDS.txt"), 
    header = TRUE, 
    skip = 60, 
    col.names = c("DATE","FEDFUNDS")
    )
fed.funds$DATE = 
  as.Date(
    fed.funds$DATE, 
    format = "%Y-%m-%d"
    )
fed.funds$year.month = as.numeric(strftime(fed.funds$DATE, "%Y%m"))       # add unique id for merging 
deposits$year.month = as.numeric(strftime(deposits$Full.Date, "%Y%m"))    # add unique id for merging
deposits = 
  merge(
    deposits, 
    fed.funds[,c("year.month","FEDFUNDS")], 
    by = "year.month", 
    all.x = T, 
    all.y = F
    )

# Assign the latest available fed funds rate to new weekly records for which there is no fed funds rate available
fed.funds = fed.funds[order(fed.funds$DATE),]
deposits[is.na(deposits$FEDFUNDS),"FEDFUNDS"] = fed.funds[nrow(fed.funds), "FEDFUNDS"]

# Assing deposit categories consistent with Rate Cap categories for future surge analytics
CD_types = read.csv("Input/CD types.csv")
deposits = 
  merge(
    deposits,
    CD_types, 
    by = "Account.Instrument.Int.Plan.Number", 
    all.x = T, 
    all.y = F
    )
deposits$deposit_product = as.character(deposits$deposit_product)
deposits[is.na(deposits$deposit_product), "deposit_product"] = 
  paste(
    "Other", 
    deposits[is.na(deposits$deposit_product), "class"], 
    sep = " "
    )
rm(CD_types)

# Complete the categories of Deposit_Product
deposits[deposits$Account.Deposit.Category.Code == "S", "deposit_product"] = "Savings"
deposits[deposits$Account.Deposit.Category.Code == "N", "deposit_product"] = "Interest Checking"
deposits[deposits$Account.Deposit.Category.Code == "M", "deposit_product"] = "Money Market"

# Assign a jumbo flag
deposits$jumbo = ifelse(deposits$Account.Ledger.Balance >= 100000, "jumbo","non-jumbo")

# Rate caps from https://fred.stlouisfed.org/release/tables?rid=317&eid=20543
rate_cap = 
  merge(
    read.table(url("https://fred.stlouisfed.org/data/SAVRCNJ.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","SAVRCNJ")),
    read.table(url("https://fred.stlouisfed.org/data/ICRCNJ.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","ICRCNJ")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/MMRCNJ.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","MMRCNJ")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD1RCNJ.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD1RCNJ")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD3RCNJ.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD3RCNJ")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD6RCNJ.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD6RCNJ")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD12RCNJ.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD12RCNJ")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD24RCNJ.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD24RCNJ")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD36RCNJ.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD36RCNJ")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD48RCNJ.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD48RCNJ")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD60RCNJ.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD60RCNJ")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/MMRCJD.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","MMRCJD")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD1RCJD.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD1RCJD")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD3RCJD.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD3RCJD")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD6RCJD.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD6RCJD")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD12RCJD.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD12RCJD")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap,
    read.table(url("https://fred.stlouisfed.org/data/CD24RCJD.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD24RCJD")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD36RCJD.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD36RCJD")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD48RCJD.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD48RCJD")),
    by = "DATE", all.x = TRUE, all.y = FALSE)
rate_cap = 
  merge(
    rate_cap, 
    read.table(url("https://fred.stlouisfed.org/data/CD60RCJD.txt"), 
               header = TRUE, skip = 29, col.names = c("DATE","CD60RCJD")),
    by = "DATE", all.x = TRUE, all.y = FALSE)

# Apply date formatting and year.week field
rate_cap$DATE = as.Date(rate_cap$DATE, format = "%Y-%m-%d")
rate_cap$year.month = as.numeric(strftime(rate_cap$DATE, "%Y%m"))

rate_cap = 
  merge(
    aggregate(DATE ~ year.month, rate_cap, max), 
    rate_cap[,!names(rate_cap) %in% "year.month"], 
    by = "DATE", all.x = TRUE, all.y = FALSE)

deposits = 
  merge(
    deposits, 
    rate_cap[,!names(rate_cap) %in% "DATE"], 
    by = "year.month", all.x = T, all.y = F)

# Assign the reg Rate Cap to each deposit product type based on the regulatory designation for jumbo and non-jumbo
deposits$rate_cap = 
  ifelse(
    deposits$deposit_product == "Savings", 
    deposits$SAVRCNJ,
    ifelse(
      deposits$deposit_product == "Interest Checking", 
      deposits$ICRCNJ,
      ifelse(
        deposits$deposit_product == "Money Market" & deposits$jumbo == "non-jumbo", 
        deposits$MMRCNJ,
        ifelse(
          deposits$deposit_product == "Money Market" & deposits$jumbo == "jumbo", 
          deposits$MMRCJD,
          ifelse(
            deposits$deposit_product == "1 month CD" & deposits$jumbo == "non-jumbo", 
            deposits$CD1RCNJ,
            ifelse(
              deposits$deposit_product == "1 month CD" & deposits$jumbo == "jumbo", 
              deposits$CD1RCJD,
              ifelse(
                deposits$deposit_product == "3 month CD" & deposits$jumbo == "non-jumbo", 
                deposits$CD3RCNJ,
                ifelse(
                  deposits$deposit_product == "3 month CD" & deposits$jumbo == "jumbo", 
                  deposits$CD3RCJD,
                  ifelse(
                    deposits$deposit_product == "6 month CD" & deposits$jumbo == "non-jumbo", 
                    deposits$CD6RCNJ,
                    ifelse(
                      deposits$deposit_product == "6 month CD" & deposits$jumbo == "jumbo", 
                      deposits$CD6RCJD,
                      ifelse(
                        deposits$deposit_product == "12 month CD" & deposits$jumbo == "non-jumbo", 
                        deposits$CD12RCNJ,
                        ifelse(
                          deposits$deposit_product == "12 month CD" & deposits$jumbo == "jumbo", 
                          deposits$CD12RCJD,
                          ifelse(
                            deposits$deposit_product == "24 month CD" & deposits$jumbo == "non-jumbo", 
                            deposits$CD24RCNJ,
                            ifelse(
                              deposits$deposit_product == "24 month CD" & deposits$jumbo == "jumbo", 
                              deposits$CD24RCJD,
                              ifelse(
                                deposits$deposit_product == "36 month CD" & deposits$jumbo == "non-jumbo", 
                                deposits$CD36RCNJ,
                                ifelse(
                                  deposits$deposit_product == "36 month CD" & deposits$jumbo == "jumbo", 
                                  deposits$CD36RCJD,
                                  ifelse(
                                    deposits$deposit_product == "48 month CD" & deposits$jumbo == "non-jumbo",
                                    deposits$CD48RCNJ,
                                    ifelse(
                                      deposits$deposit_product == "48 month CD" & deposits$jumbo == "jumbo", 
                                      deposits$CD48RCJD,
                                      ifelse(
                                        deposits$deposit_product == "60 month CD" & deposits$jumbo == "non-jumbo", 
                                        deposits$CD60RCNJ,
                                        ifelse(
                                          deposits$deposit_product == "60 month CD" & deposits$jumbo == "jumbo", 
                                          deposits$CD60RCJD,
                                          0
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

# Cleanup cap rate fiels after import and processing
deposits = 
  deposits[,
    !(names(deposits) %in% 
        names(
          rate_cap[-c(1,2)]
          )
      )
    ]

# Assign burnout flag if account age is > 10 years. Burned out accounts are not rate sensitive and are not surge
deposits$burnout = 
  ifelse(
    deposits$account.age >= 120, 
    "burnedout", 
    "sensitive"
    )

# Assign relationship flag showing number of unique accounts for each depositor by month. Multiple accounts are not surge.
deposits$id.unique = 
  paste(
    deposits$Full.Date, 
    deposits$Prmy.Cust.Number, 
    sep = ""
    )
unique = data.frame(table(deposits$id.unique))
colnames(unique) = c("id.unique","relationship")
deposits = 
  merge(
    deposits, 
    unique, 
    by = "id.unique", 
    all.x = T, 
    all.y = F
    )
rm(unique)

# Assign surge flag based on 1) non-burned out depositors, 2) single-account depositors, and 3) accounts > rate cap
deposits$surge = 
  ifelse(
    deposits$Account.Instrument.Current.Int.Rate > (deposits$rate_cap * 1), 
    1, 
    0
    )
deposits[deposits$burnout == "burnedout", "surge"] = 0
deposits[deposits$relationship > 2, "surge"] = 0
deposits[is.na(deposits$surge), "surge"] = 0

# Add regulatory definition of surge "Rate > 0.75% of Cap Rate at origination", but based on current rates not origination
deposits$reg.surge = 
  ifelse(
    deposits$Account.Instrument.Current.Int.Rate > deposits$rate_cap, 
    1, 
    0
    )

# Assing a Plan Number and Description to deposits that have missing values on those two fields. Link to Account Type Code/Description
deposits$Account.Instrument.Int.Plan.Number = 
  ifelse(
    deposits$Account.Instrument.Int.Plan.Number == 0,
    deposits$Account.Type.Code,
    deposits$Account.Instrument.Int.Plan.Number
    )
deposits$Account.Instrument.Int.Plan.Desc = 
  ifelse(
    deposits$Account.Instrument.Int.Plan.Desc == "",
    deposits$Account.Type.Description,
    deposits$Account.Instrument.Int.Plan.Desc
    )

# For Marginal COF calculation and other analysis
# Add customer-level flag for new money vs. repriced based on aggregate balance change of > 15%
customer = 
  aggregate(
    Account.Ledger.Balance ~ Full.Date + Prmy.Cust.Number, 
    deposits, 
    sum
    )
customer$customer.delta = 
  round(
    customer$Account.Ledger.Balance / lag(customer$Account.Ledger.Balance, lag) - 1, 
    digits = 2
    )

# Merge the above customer-level flag back into the account-level data 
customer$id.unique = 
  paste(
    customer$Full.Date, 
    customer$Prmy.Cust.Number, 
    sep = ""
    )
deposits = 
  merge(
    deposits, 
    customer[,c("id.unique","customer.delta")], 
    by = "id.unique", 
    all.x = T, 
    all.y = F
    )
rm(customer)

# Add account-level flag for new money vs. repriced based on balance change of > 15%
deposits = 
  deposits[
    order(
      deposits$Prmy.Cust.Number, 
      deposits$Account.Number...Operational, 
      deposits$Full.Date
      ),
    ]
deposits$account.delta = 
  round(
    deposits$Account.Ledger.Balance / lag(deposits$Account.Ledger.Balance, lag) - 1, 
    digits = 2
    )

# ADD FLAG: "new money" if customer.delta and account.delta > 15% and if account is new, else "repriced"
deposits$money = 
  ifelse(
    deposits$customer.delta >= 0.15 & deposits$account.delta >= 0.15, 
    deposits$money <- "new money", 
    NA
    )
deposits[deposits$status == "new", "money"] = "new money"
deposits$money = 
  ifelse(
    is.na(deposits$money), 
    "existing", 
    deposits$money
    )

# Add new fields with the balance for "new money" and "existing money"
deposits$existing.money = 
  ifelse(
    deposits$money == "existing", 
    deposits$Account.Ledger.Balance, 
    0
    )
deposits$new.money = 
  ifelse(
    deposits$money == "new money", 
    deposits$Account.Ledger.Balance, 
    0
    )

# CD > 250k balance
deposits$high.bal = 
  ifelse(
    deposits$Account.Ledger.Balance > 250000 , 
    "uninsured", 
    "insured"
    )

#====================================================================================================================
# Define fields to be exported
export = 
  c("Full.Date", "Prmy.Cust.Name", "Account.Number...Operational", "Branch.Name", "Account.Deposit.Category.Code",
    "Account.TD.Next.Maturity.Date", "Account.Instrument.Int.Plan.Desc", "Account.Open.Date",
    "Account.Ledger.Balance", "Account.WAR..Acct.Ledger.Balance.", "Account.Instrument.Current.Int.Rate",
    "class", "GL_name", "CD.Term", "existing", "new", "State", "FEDFUNDS", "money", "existing.money",  "new.money", 
    "tier", "high.bal", "surge", "reg.surge")
#====================================================================================================================

save(deposits, file = paste0("deposits_",freq,".R"))

# Max date
max.month = max(deposits$year.month)



#############################################################################################################################################
# SURGE ANALYSIS (MONTHLY)

# Calculate a surge balance for future aggregation and analysis
deposits$surge.balance = deposits$surge * deposits$Account.Ledger.Balance
deposits$non.surge.balance = deposits$Account.Ledger.Balance - deposits$surge.balance
deposits$reg.surge.balance = deposits$reg.surge * deposits$Account.Ledger.Balance
deposits$non.reg.surge.balance = deposits$Account.Ledger.Balance - deposits$reg.surge.balance

surge = 
  deposits[
    deposits$Full.Date == "2019-07-31", 
    c("Full.Date","Account.Number...Operational","GL_name","Account.Ledger.Balance",
      "class", "high.bal", "surge", "reg.surge","Account.Instrument.Current.Int.Rate")
    ]
fwrite(data.table(surge), "Output/surge.csv")
rm(surge)




#############################################################################################################################################
# BETA ANALYSIS (MONTHLY) - Append the output to the existing data -

# Aggregate by month, output and append the new data to the existing
beta.trend = 
  aggregate(
    cbind(
      Account.Ledger.Balance, 
      Account.WAR..Acct.Ledger.Balance.) ~ 
      year.month + Full.Date + GL_name + deposit_product + class, 
    data = deposits[deposits$year.month == max.month,], 
    sum
    )

beta.trend = 
  merge(
    beta.trend, 
    fed.funds[c("year.month","FEDFUNDS")], 
    by = "year.month", all.x = T, all.y = F
    )

fwrite(data.table(beta.trend), "Output/beta.csv")
rm(beta.trend)




#############################################################################################################################################
# DECAY ANALYSIS (WEEKLY/MONTHLY)

# Aggregate by month and output weighted rate, class, deposit product
decay = 
  aggregate(
    cbind(Account.Ledger.Balance, Account.WAR..Acct.Ledger.Balance.) ~
      year.month + 
      Full.Date + 
      GL_name + 
      class + 
      State + 
      Branch.Name + 
      Account.Deposit.Category.Code + 
      deposit_product + 
      Account.Instrument.Int.Plan.Desc,
    data = deposits, 
    sum
    )

decay = 
  merge(
    decay, 
    fed.funds[c("year.month","FEDFUNDS")], 
    by = "year.month", 
    all.x = T, 
    all.y = F
    )
decay[is.na(decay$FEDFUNDS),"FEDFUNDS"] = fed.funds[nrow(fed.funds), "FEDFUNDS"]      # Add rate for missing months
fwrite(data.table(decay), "Output/decay.csv")

ggplot(decay[decay$class %in% c("Consumer","Business"),], 
       aes(Full.Date, Account.Ledger.Balance, fill = Account.Deposit.Category.Code)) + 
  geom_bar(stat = "identity", alpha = 0.6) + 
  facet_grid(class ~ Account.Deposit.Category.Code) + 
  theme_solarized_2(light = FALSE) + 
  theme(legend.position="none")

rm(decay)



#############################################################################################################################################
# ATTRITIION ANALYSIS (AS NEEDED)

# Relationship attriton
attrition.cust = 
  aggregate(
    Prmy.Cust.Number ~ Full.Date + Branch.Name,
    unique(
      deposits[
        deposits$class %in% c("Consumer","Business") & 
          deposits$status != "closed",
        c("Prmy.Cust.Number","Full.Date","Branch.Name")
        ]
    )
    , length
  )
  
fwrite(attrition.cust, 'Output/relationship_attrition.csv')
rm(attrition.cust)


# Account attrition
attrition.account = 
  aggregate(
    Account.Number...Operational ~ 
      Full.Date + 
      GL_name + 
      class + 
      State + 
      Branch.Name +
      Account.Deposit.Category.Code + 
      Account.Instrument.Int.Plan.Desc, 
    deposits[deposits$status != "closed",],
    length
    )

fwrite(attrition.account, 'Output/account_attrition.csv')
rm(attrition.account)


#############################################################################################################################################
# CROSS-SELL RATIO (monthly or as needed)

# Create lookup table that counts instances of deposit category types
cross_sell = 
  aggregate(
    Account.Ledger.Balance ~ Full.Date + Prmy.Cust.Number + Account.Deposit.Category.Code,
    deposits[
      deposits$class %in% c("Consumer","Business") & deposits$status != "closed",
      ],
    length
  )

# Convert from long to wide format
cross_sell = 
  dcast(
    cross_sell, 
    Full.Date + Prmy.Cust.Number ~ Account.Deposit.Category.Code, 
    length
  )

# Count number of unique deposit category types
cross_sell$num_of_accounts = 
  apply(
    cross_sell[,c("C","D","M","N","S")], 
    MARGIN = 1,
    FUN = sum
    )

# Aggregate corss-sell ratio
cross_sell_total = 
  merge(
    aggregate(
      Prmy.Cust.Number ~ Full.Date, 
      cross_sell, 
      length
      ),
    aggregate(
      num_of_accounts ~ Full.Date, 
      cross_sell, 
      sum
      ),
    by = "Full.Date", 
    all.x = TRUE, 
    all.y = FALSE
    )

cross_sell_total$cross_sell_ratio = 
  cross_sell_total$num_of_accounts / cross_sell_total$Prmy.Cust.Number

fwrite(data.table(cross_sell_total), "Output/cross_sell_total.csv")

# Generate cross-sell ratio by branch
cross_sell$id.unique = 
  paste0(
    cross_sell$Full.Date, 
    cross_sell$Prmy.Cust.Number
    )

deposits = 
  merge(
    deposits, 
    cross_sell[,c("id.unique","num_of_accounts")], 
    by = "id.unique", 
    all.x = TRUE, 
    all.y = FALSE
    )

cross_sell_ratio = 
  cbind(
    aggregate(
      Prmy.Cust.Number ~ Full.Date + Branch.Name,
      unique(
        deposits[deposits$class %in% c("Consumer","Business") & deposits$status != "closed",
                 c("Prmy.Cust.Number","Full.Date","Branch.Name")]
      )
      , length
    ),
    aggregate(
      num_of_accounts ~ Full.Date + Branch.Name,
      deposits[deposits$class %in% c("Consumer","Business") & deposits$status != "closed",
               c("num_of_accounts","Full.Date","Branch.Name")]
      , length
    )
  )
  
cross_sell_ratio$cross_sell_ratio = 
  cross_sell_ratio$num_of_accounts / cross_sell_ratio$Prmy.Cust.Number

fwrite(data.table(cross_sell_ratio), "Output/cross_sell_ratio.csv")

#############################################################################################################################################
# CD MATURITIES AND RETENTION - RUN MONTLY, REPORT WEEKLY AND MONTHLY

# Maturities
fwrite(
  data.table(deposits)[
    Account.Deposit.Category.Code == 'C' & Full.Date == max(deposits$Full.Date), 
    c("GL_name", "deposit_product","class", "Account.Instrument.Int.Plan.Desc",  "CD.Term",
      "Account.WAR..Acct.Ledger.Balance.","Account.TD.Next.Maturity.Date", "Account.Ledger.Balance")
    ],
  'Output/maturities.csv'
  )

# CD Retention output
cd_retention = 
  deposits[
    deposits$class %in% c("Consumer","Business") & deposits$Account.Deposit.Category.Code == "C", 
    c("Full.Date","Account.Instrument.Int.Plan.Desc","Account.Number...Operational",
      "Account.TD.Next.Maturity.Date","Account.Ledger.Balance","Branch.Name","deposit_product")
    ]

fwrite(data.table(cd_retention), "Output/cd_retention.csv")
rm(cd_retention)




#############################################################################################################################################
# MONEY MARKET FLOW OF FUNDS ANALYSIS (as needed)

# Consumer MM flows
mm_flows = 
  deposits[
    deposits$class == "Consumer" &
      (deposits$Full.Date == "2019-03-29" |       # Latest
         deposits$Full.Date == "2019-02-28") &    # Prior 
      deposits$Account.Instrument.Int.Plan.Desc %in% c("MAXIMUM MONEY MARKET",
                                                       "MONEY MARKET        ",
                                                       "MONEY MARKET SPECIAL",
                                                       "PROMOTIONAL MMA     ",
                                                       "PROMOTIONAL PLUS MMA",
                                                       "PERSONAL SAVINGS    ",
                                                       "13 MONTH CUSTOM     ",
                                                       "24 MONTH BUMP       ",
                                                       "7 MONTH FEATURED    ",
                                                       "11 MONTH  CD SPECIAL",
                                                       "15 MONTH CUSTOM     ",
                                                       "18 MO FEATURED      ",
                                                       "22 MONTH FEATURED CD",
                                                       "3 YEAR FEATURED     ",
                                                       "PREMIUM SELECT      ",
                                                       "PREMIUM SELECT CHECK",
                                                       "MAXIMUM CHECKING    "),
    c("Full.Date","Prmy.Cust.Name","Account.Instrument.Int.Plan.Desc","Account.Ledger.Balance")]

fwrite(data.table(mm_flows),  'Output/mm_flows.csv')
rm(mm_flows)



#############################################################################################################################################
# CONSUMER AGE ANALYTICS (ONE TIME)

# Create a frequency distribution by count and by balance. Bind the two into one dataset for the latest period.  


cust.bal = 
  aggregate(
    Account.Ledger.Balance ~ Prmy.Cust.Number, 
    deposits[
      deposits$class == "Consumer" & deposits$year.month == max.month & deposits$status == 'existing',
      ], 
    sum
    )

cust.age = 
  aggregate(
    Prmy.Cust.Age ~ Prmy.Cust.Number,
    deposits[
      deposits$class == "Consumer" & deposits$year.month == max.month & deposits$status == 'existing',
      ],
    mean
    )

age = 
  merge(
    cust.bal, 
    cust.age, 
    by = 'Prmy.Cust.Number', 
    all.x = TRUE, 
    all.y = FALSE
    )

age$age.group = 
  cut(
    age$Prmy.Cust.Age, 
    seq(0, 120, by = 10), 
    right = FALSE
    )

age.count = aggregate(Prmy.Cust.Age ~ age.group, age, length) 
age.bal = aggregate(Account.Ledger.Balance ~ age.group, age, sum)

age.w = cbind(age.count, age.bal$Account.Ledger.Balance)
age.w$count.pct = age.w$Prmy.Cust.Age / sum(age.w$Prmy.Cust.Age)
age.w$bal.pct = age.w$`age.bal$Account.Ledger.Balance` / sum(age.w$`age.bal$Account.Ledger.Balance`, na.rm = TRUE)

age.plot = melt(age.w[,c(1,4:5)], "age.group")

library(ggthemes)
ggplot(age.plot, aes(age.group, value, fill = variable)) + 
  geom_col(alpha = 0.7, position = position_dodge(width = 0.5), width = 1.3) + 
  xlab("Age Bins") + ylab("Distribution") +
  ggtitle("Savings") +
  scale_y_continuous(breaks = pretty(age.plot$value, n = 10)) +
  theme_dark()

fwrite(data.table(age.w), 'Output/S.csv')
rm(cust.bal, cust.age, age, age.count, age.bal, age.w, age.plot)



#############################################################################################################################################
# MARGINAL COF (MONTHLY)

# Current Promo Deposits
#   - "MONEY MARKET SPECIAL"  109
#   - "PROMOTIONAL PLUS MMA"  178
#   - "7 MONTH FEATURED    "  407
#   - "11 MONTH  CD SPECIAL"  411
#   - "1 YEAR CUSTOM"         412
#   - "13 MONTH CD SPECIAL "  483
#   - "15 MONTH CUSTOM     "  415
#   - "18 MO FEATURED      "  468
#   - "22 MONTH FEATURED CD"  422
#   - "3 YEAR FEATURED"       437

promo = c(109,178,412,483,415,411, 422,437,468,407)

fwrite(
  deposits[(deposits$Full.Date == "2019-04-30" | deposits$Full.Date == "2019-03-29") & 
             deposits$Account.Instrument.Int.Plan.Number %in% promo, 
           export], 
  "Output/promo.csv"
  )

#############################################################################################################################################
# BRANCH ANALYSIS (MONTHLY)

branch = deposits
branch$IRA = 
  ifelse(
    grepl(
      branch$Account.Instrument.Int.Plan.Desc, 
      pattern = "IRA"
      ), 
    branch$IRA <- "IRA", 
    NA
    )

branch = 
  branch[branch$Full.Date == "2019-04-30" | branch$Full.Date == "2019-03-29", 
         c(export,"CD.Term","IRA")]

fwrite(data.table(branch), 'Output/branch.csv')
rm(branch)


#############################################################################################################################################
# UNINSURED DEPOSITS (> $250,000) (MONTHLY)

high_bal = 
  aggregate(
    Account.Ledger.Balance ~ Full.Date + class + GL_name + high.bal, 
    deposits, 
    sum
    )

fwrite(data.table(high_bal), "Output/highbal.csv")
rm(high_bal)

# CDs greater than 250k (non-core)
retail_cd = c(60, 61, 70, 71, 72, 73, 74, 90, 91, 92, 310, 352)

high_cd = 
  deposits[
    deposits$Account.Type.Code %in% retail_cd & deposits$Account.TD.Next.Maturity.Date > deposits$Full.Date, 
    c("Full.Date","Account.Number...Operational","Account.Ledger.Balance",
      "Account.TD.Next.Maturity.Date","Account.Instrument.Int.Plan.Desc") 
  ]

high_cd$high_retail_cd =
  ifelse(
    high_cd$Account.Ledger.Balance > 250000,
    "Retail CD > 250k",
    "Retail CD <= 250k"
  )

high_cd_sum = 
  aggregate(
    Account.Ledger.Balance ~ Full.Date + Account.Instrument.Int.Plan.Desc,
    high_cd[high_cd$high_retail_cd == "Retail CD > 250k", ],
    sum
  )

ggplot(high_cd_sum, aes(Full.Date, Account.Ledger.Balance)) + 
  geom_bar(stat = "identity") + 
  theme_solarized_2(light = FALSE)

fwrite(data.table(high_cd_sum), "Output/high_cd_sum.csv")
rm(high_cd_sum, high_cd, retail_cd)

################################################################################################################
# COMMERCIAL LOANS

# Load loan data
loan_batch = list.files(paste0(wd, "/Monthly Loan Archives"), pattern="*.csv", full.names=TRUE)
loans = do.call(rbind, lapply(loan_batch, read.csv, stringsAsFactors = F))

# Set Date formats
dates = c("Full.Date","Note.Issue.Date","Note.Maturity.Date")
loans[,dates] = lapply(loans[,dates], as.Date, format = "%m/%d/%Y")

# Create deposit and loan dataset
dep_dat = 
  deposits[
    deposits$status != "closed", 
    c("Full.Date","Prmy.Cust.Number","Branch.Name","Account.Deposit.Category.Code","Account.Ledger.Balance","GL_name")
    ]
colnames(dep_dat) = c("Full.Date","Prmy.Cust.Number","Branch.Name","Category","Balance","GL_name")

loan_dat = loans[,c("Full.Date","Prmy.Cust.Number","Branch.Name","Note.Category.Description","Note.Ledger.Balance")]
loan_dat$GL_name = "Commercial Lending"
colnames(loan_dat) = c("Full.Date","Prmy.Cust.Number","Branch.Name","Category","Balance","GL_name")

complete = rbind(dep_dat, loan_dat)

complete = complete[complete$Prmy.Cust.Number %in% unique(loan_dat$Prmy.Cust.Number),]

complete_out = 
  aggregate(
    Balance ~ Full.Date + Branch.Name + Category + GL_name,
    complete,
    length
  )

write.csv(complete_out, "Output/complete_out.csv", row.names = FALSE)




options(scipen = 999)
cd = read.csv('input/9mocd.csv')
cd$Dollars_per_Cust = cd$New.Balance / cd$New.Count
cd$Total_balance = cd$Existing.Balance + cd$New.Balance
cd$Total_Cust = cd$Existing.Count + cd$New.Count
cd$Total_Bal_per_Cust = cd$Total_balance / cd$Total_Cust

ggplot(cd, aes(x = Existing.Count, y = New.Count, color = State)) + 
  geom_point(aes(size = cd$New.Balance)) + 
  geom_text(label = cd$Customer, hjust=-0.15, vjust=0, size=2) + 
  geom_abline(intercept = 0, slope = 1) + 
  scale_color_brewer(palette="Dark2") + 
  theme_minimal()
  

            