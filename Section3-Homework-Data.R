#Data
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

#Solution

#Profit
profit = revenue - expenses; profit
#------------------------------------------------

#Profit with the tax
tax <- round(profit*0.3, 2); tax
profit_tax = profit - tax; profit_tax
#------------------------------------------------

#Profit margin
profit_margin = round(profit_tax/revenue, 2)*100; profit_margin
#------------------------------------------------

#Calculate mean
mean = 0
for(i in 1:12){
  mean = mean + profit_tax[i]
}
mean = mean/12; mean

mean(profit_tax)
#------------------------------------------------

#Diferent ways to calculate good and bad months
for(i in 1:12){
  if(profit_tax[i] > mean){
    print(paste('month', i))
  }
}
good_months <- profit_tax > mean; good_months

for(i in 1:12){
  if(profit_tax[i] < mean){
    print(paste('month', i))
  }
}
bad_months = !good_months; bad_months
bad_months <- profit_tax < mean; bad_months
#------------------------------------------------

#Best and worst month
best_month <- profit_tax == max(profit_tax); best_month
worst_month <- profit_tax == min(profit_tax); worst_month
#------------------------------------------------

#Rounding values for thousand
revenue_1000 <- round(revenue/1000); revenue_1000
expenses_1000 <- round(expenses/1000); 
profit_1000 <- round(profit/1000); profit_1000
profit_tax_1000 <- round(profit_tax/1000); profit_tax_1000
#------------------------------------------------

#Matrice
M <- rbind(
  revenue_1000,
  expenses_1000,
  profit_1000,
  profit_tax_1000,
  profit_margin,
  good_months,
  bad_months,
  best_month,
  worst_month
);M
#------------------------------------------------

                     