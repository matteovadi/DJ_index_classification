## DOW JONES INDEX ##

rm(list = ls())
library(RColorBrewer)
library(DescTools)
library(e1071)
library(corrplot)
library(tree)
library(pander)

####### Preprocessing & Data Preparation #######
dj_index = read.csv(file = "dow_jones_index_data.csv", header = TRUE, sep = ",")
# Data elimination, features selection & identification of the output variable
dj_index = na.omit(dj_index) 
dj_index = data.frame(dj_index$quarter, dj_index$stock, dj_index$percent_change_price, dj_index$percent_change_volume_over_last_wk, dj_index$days_to_next_dividend, dj_index$percent_return_next_dividend, dj_index$percent_change_next_weeks_price) 
colnames(dj_index) = c("quarter", "stock", "percent_change_price", "percent_change_volume_over_last_wk", "days_to_next_dividend", "percent_return_next_dividend", "percent_change_next_weeks_price")
buy_sell = matrix(0, nrow = length(dj_index$percent_change_next_weeks_price), ncol = 1) # output variable
for (i in 1:length(dj_index$percent_change_next_weeks_price)){
  if (dj_index$percent_change_next_weeks_price[i] >= 0){
    buy_sell[i] = "Buy / not sell"
  } else {
    buy_sell[i] = "Sell / not buy"
  }
} 
colnames(buy_sell) = "decision"
dj_index = cbind(dj_index, buy_sell)
pandoc.table(head(dj_index))

####### Data Visualization #######
colors = brewer.pal(8, "Set1") 
# 1. Visualization of numerical attributes 
par(mfrow=c(2,3), mar = c(5,5.3,3,1.5))
for (k in 3:7){
  hist(dj_index[,k],freq=FALSE, col=colors, main=paste("Histogram of", names(dj_index[k])),xlab=names(dj_index[k]))
}
# 2. Visualization of categorical attributes
freq_quarter = table(dj_index[,1])
freq_pursell = table(dj_index[,8])
par(mfrow = c(1,2), mar=c(3,0,6,1.5))
pie(freq_quarter, labels =c("1st quarter", "2nd quarter"), main = "Pie Chart for 1st & 2nd quarter data", col = colors, radius = 0.95)
barplot(freq_pursell, main = "Histogram of the output variable", col = c("green", "orange"))

####### Data Exploration #######
## Univariate Analysis ##
# 1. Summary statistics for numerical attributes (central tendency & dipersion measures)
source("getmode.r")
ss_matrix = matrix(0, nrow = 5, ncol = 7)
for (r in 1:5){
  ss_matrix[r,1] = mean(dj_index[,r+2])
  ss_matrix[r,2] = median(dj_index[,r+2])
  ss_matrix[r,3] = getmode(dj_index[,r+2])
  ss_matrix[r,4] = MeanAD(dj_index[,r+2])
  ss_matrix[r,5] = var(dj_index[,r+2])
  ss_matrix[r,6] = sd(dj_index[,r+2])
  ss_matrix[r,7] = 100*sd(dj_index[,r+2])/mean(dj_index[,r+2])
}
colnames(ss_matrix) = c("Mean","Median","Mode","MAD","Variance","Std.deviation","Coeff.ofVariation")
rownames(ss_matrix) = names(dj_index[,3:7])

# 2. Measures of relative location for numerical attributes
quartiles = matrix(0, nrow = 5, ncol = 5)
for (c in 1:5){
  quartiles[c,] = quantile(dj_index[,c+2])
}
colnames(quartiles) = c("0%","25%","50%","75%","100%")
rownames(quartiles) = names(dj_index[,3:7])
source("MidMean.r")
source("TrimmedMean.r")
source("WinsMean.r")
quant_mean = matrix (0, nrow = 5, ncol= 3)
for (a in 1:5){
  quant_mean[a,1] = mid.mean(dj_index[,a+2])
  quant_mean[a,2] = trimmed.mean(dj_index[,a+2])
  quant_mean[a,3] = wins.mean(dj_index[,a+2])
}
colnames(quant_mean) = c("Mid mean","Trimmed mean","Winsorized mean")
rownames(quant_mean) = names(dj_index[,3:7])

# 3. Box-plots & empirical density analysis (skewness & kurtosi indices, normal quantile plots)
par(mfrow = c(2,5), mar=c(4,2.7,2.5,2))
for (m in 3:7){
  hist(dj_index[,m],freq=FALSE, col=brewer.pal(8, "Set2"), main=names(dj_index[m]), xlab=names(dj_index[m]))
}
for (n in 3:7){
  boxplot(dj_index[,n], xlab=names(dj_index[n]), col="red", horizontal = TRUE)  
}
ed_analysis = matrix (0, nrow = 5, ncol= 2) # empirical density analysis matrix
for (b in 1:5){
  ed_analysis[b,1] = skewness(dj_index[,b+2])
  ed_analysis[b,2] = kurtosis(dj_index[,b+2])
}
colnames(ed_analysis) = c("Skewness index","Kurtosi index")
rownames(ed_analysis) = names(dj_index[,3:7])
par(mfrow = c(3,2)) # qq plots
for (q in 3:7){
  qqnorm(dj_index[,q], col=brewer.pal(8, "Set2")[1], main=paste("Normal quantile plot of", names(dj_index[q])))
  qqline(dj_index[,q], col="red")
}

## Bivariate Analysis ##
# Time series of the Dow Jones indices which belong to the hi-tech, financial, oil & chemical-pharmaceutical fields
dj_index_ts = read.csv(file = "dow_jones_index_data.csv", header = TRUE, sep = ",")
ibm = ts(dj_index_ts$percent_change_next_weeks_price[which(dj_index_ts$stock== "IBM" & dj_index_ts$date!="4/1/2011")], start = 1, end = c(6,4), frequency = 4)
intc = ts(dj_index_ts$percent_change_next_weeks_price[which(dj_index_ts$stock== "INTC" & dj_index_ts$date!="4/1/2011")], start = 1, end = c(6,4), frequency = 4)
msft = ts(dj_index_ts$percent_change_next_weeks_price[which(dj_index_ts$stock== "MSFT" & dj_index_ts$date!="4/1/2011")], start = 1, end = c(6,4), frequency = 4)
hpq = ts(dj_index_ts$percent_change_next_weeks_price[which(dj_index_ts$stock== "HPQ" & dj_index_ts$date!="4/1/2011")], start = 1, end = c(6,4), frequency = 4)
axp = ts(dj_index_ts$percent_change_next_weeks_price[which(dj_index_ts$stock== "AXP" & dj_index_ts$date!="4/1/2011")], start = 1, end = c(6,4), frequency = 4)
jpm = ts(dj_index_ts$percent_change_next_weeks_price[which(dj_index_ts$stock== "JPM" & dj_index_ts$date!="4/1/2011")], start = 1, end = c(6,4), frequency = 4)
cvx = ts(dj_index_ts$percent_change_next_weeks_price[which(dj_index_ts$stock== "CVX" & dj_index_ts$date!="4/1/2011")], start = 1, end = c(6,4), frequency = 4)
xom = ts(dj_index_ts$percent_change_next_weeks_price[which(dj_index_ts$stock== "XOM" & dj_index_ts$date!="4/1/2011")], start = 1, end = c(6,4), frequency = 4)
dd = ts(dj_index_ts$percent_change_next_weeks_price[which(dj_index_ts$stock== "DD" & dj_index_ts$date!="4/1/2011")], start = 1, end = c(6,4), frequency = 4)
mrk = ts(dj_index_ts$percent_change_next_weeks_price[which(dj_index_ts$stock== "MRK" & dj_index_ts$date!="4/1/2011")], start = 1, end = c(6,4), frequency = 4)

# 1. Time series plots for the hi-tech field
par(mfrow = c(4,1), mar = c(5,5,2,1)) 
plot.ts(ibm, main = "Time series of the DJ indices which belong to the hi-tech field", sub = "IBM index", xlab = "Months from January to June (1st & 2nd quarters)", ylab = "% change of price in the following week", lwd = 2, col = "blue") 
grid()
plot.ts(intc, sub = "Intel index", xlab = "Months from January to June (1st & 2nd quarters)", ylab = "% change of price in the following week", lwd = 2, col = "blue")
grid()
plot.ts(msft, sub = "Microsoft index", xlab = "Months from January to June (1st & 2nd quarters)", ylab = "% change of price in the following week", lwd = 2, col = "blue")
grid()
plot.ts(hpq, sub = "Hewlett-Packard index", xlab = "Months from January to June (1st & 2nd quarters)", ylab = "% change of price in the following week", lwd = 2, col = "blue")
grid()

# 2. Time series plots for the financial field
par(mfrow = c(2,1), mar = c(5.5,5,2,1)) 
plot.ts(axp, main = "Time series of the DJ indices which belong to the financial field", sub = "American Express index", xlab = "Months from January to June (1st & 2nd quarters)", ylab = "% change of price in the following week", lwd = 2, col = "red")
grid()
plot.ts(jpm, sub = "JPMorgan Chase index", xlab = "Months from January to June (1st & 2nd quarters)", ylab = "% change of price in the following week", lwd = 2, col = "red")
grid()

# 3. Time series plots for the oil field
par(mfrow = c(2,1), mar = c(5.5,5,2,1)) 
plot.ts(cvx, main = "Time series of the DJ indices which belong to the oil field", sub = "Chevron index", xlab = "Months from January to June (1st & 2nd quarters)", ylab = "% change of price in the following week", lwd = 2, col = "green")
grid()
plot.ts(xom, sub = "ExxonMobil index", xlab = "Months from January to June (1st & 2nd quarters)", ylab = "% change of price in the following week", lwd = 2, col = "green")
grid()

# 4. Time series plots for the chemical-pharmaceutical field
par(mfrow = c(2,1), mar = c(5.5,5,2,1)) 
plot.ts(dd, main = "Time series of the DJ indices which belong to the chemical-pharmaceutical field", sub = "DuPont index", xlab = "Months from January to June (1st & 2nd quarters)", ylab = "% change of price in the following week", lwd = 2, col = "orange")
grid()
plot.ts(mrk, sub = "Merck index", xlab = "Months from January to June (1st & 2nd quarters)", ylab = "% change of price in the following week", lwd = 2, col = "orange")
grid()

## Multivariate Analysis ##
# Covariance & correlation matrices for numerical attributes 
cov_matrix = cov(dj_index[,4:7])
corr_matrix = cor(dj_index[,4:7])
par(mfrow = c(1,1), mar = c(1,0,0,4))
corrplot(corr_matrix, tl.cex = 0.5, col = c("#7F0000", "red", "#FF7F00", "orange","green", "#007FFF", "blue","#00007F"))

####### Data Mining #######
# 1. Data standardization (factorization of the categorical attributes & min-max normalization between [0,1] of the numerical attributes)
source("normalize.r")
dj_index = data.frame(factor(dj_index[,1], labels = c("I", "II")), factor(dj_index[,2]), normalize(dj_index[,3:7]), factor(dj_index[,8]))
colnames(dj_index) = c("quarter", "stock", "percent_change_price", "percent_change_volume_over_last_wk", "days_to_next_dividend", "percent_return_next_dividend", "percent_change_next_weeks_price", "decision")

# 2. Classification trees
train_set = which(dj_index$quarter =="I")
# train_set = sample(1:nrow(dj_index), round(nrow(dj_index)*2/3)) # 2/3 for the train set, 1/3 for the test set
 tree_djindex = tree(decision~.-(percent_change_next_weeks_price+stock+quarter), data = dj_index , subset = train_set, split = "deviance")
# tree_djindex = tree(decision~.-(percent_change_next_weeks_price+stock+quarter), data = dj_index , subset = train_set, split = "gini")
tree_pred = predict(tree_djindex, dj_index[-train_set,], type = "class")
check = with(dj_index[-train_set,], table(tree_pred, decision))
# post-pruning
cv_djindex = cv.tree(tree_djindex, FUN = prune.misclass)
plot(cv_djindex)
prune_djindex = prune.misclass(tree_djindex, best = 10)
tree_pred_prune = predict(prune_djindex, dj_index[-train_set,], type="class")
check_prune = with(dj_index[-train_set,], table(tree_pred_prune, decision))
# Comparing the trees & the confusion matrices pre & post pruning
par(mfrow = c(2,1), mar = c(2,2,3,2))
plot(tree_djindex)
text(tree_djindex, pretty=0, cex = 0.49)
plot(prune_djindex)
text(prune_djindex, pretty=0, cex = 0.8)
conf_matrix_tree = rbind(c("----","----"), check, c("----","----"), check_prune)
rownames(conf_matrix_tree) = c("Pre pruning", "Buy / not sell", "Sell / not buy", "Post pruning", "Buy / not sell", "Sell / not buy")
conf_matrix_tree
# Accuracy of the classifcation tree model
# a. Accuracy index
exact_class_tree = check_prune[1,1]+check_prune[2,2]
accuracy_class_tree = exact_class_tree/sum(check_prune) 
# b. True negative (tnr), true positive (tpr), false negative (fn) and false positive (fp) rates
tnr = check_prune[1,1]/sum(check_prune[1,1]+check_prune[1,2])
tpr = check_prune[2,2]/sum(check_prune[2,1]+check_prune[2,2])
fn = check_prune[2,1]/sum(check_prune[2,1]+check_prune[2,2])
fp = check_prune[1,2]/sum(check_prune[1,1]+check_prune[1,2])

# 3. Naive Bayes
classifier = naiveBayes(decision~.-(percent_change_next_weeks_price+stock+quarter), data = dj_index, subset = train_set)
predicted_classes = predict(classifier, dj_index[-train_set,])
# Accuracy of the Naive Bayes classification
conf_matrix_naive = table(predicted_classes, dj_index[-train_set,8], dnn=list('predicted','actual'))
conf_matrix_naive
exact_naive = conf_matrix_naive[1,1]+conf_matrix_naive[2,2]
accuracy_naive = exact_naive/sum(conf_matrix_naive)

# 4. Comparison between the two classification approaches
comparison = rbind(accuracy_class_tree, accuracy_naive)
if (accuracy_class_tree > accuracy_naive){
  print("The Classification Trees approach is more accurate than the Naive Bayes one")
  pandoc.table(comparison)
} else if (accuracy_class_tree < accuracy_naive){
  print("The Naive Bayes approach is more accurate than the Classification Trees one")
  pandoc.table(comparison)
} else {
  print("The two classification approaches have the same accuracy level")
  pandoc.table(comparison)
}

