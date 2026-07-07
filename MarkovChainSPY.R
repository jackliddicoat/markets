#markov chain
library(quantmod)
library(dplyr)
getSymbols("SPY")
Market<-data.frame(dailyReturn(SPY))
Market$Date<-rownames(Market)
Market$Date<-as.Date(Market$Date)
rownames(Market) <- 1:nrow(Market)
Market<-Market %>% rename(Return=daily.returns)
#Make dummy variables
Market$LessN1<-ifelse(Market$Return < -.01, 1, 0)
Market$BN1ND5<-ifelse(Market$Return >= -.01 & Market$Return < -.005, 1, 0)
Market$BND5Z<-ifelse(Market$Return >= -.005 & Market$Return < 0, 1, 0)
Market$BZPD5<-ifelse(Market$Return >= 0 & Market$Return < 0.005, 1, 0)
Market$BPD5P1<-ifelse(Market$Return >= .005 & Market$Return < 0.01, 1, 0)
Market$GreP1<-ifelse(Market$Return > .01, 1, 0)

ReturnsAfterLessN1<-numeric()
ReturnsAfterBN1ND5<-numeric()
ReturnsAfterBND5Z<-numeric()
ReturnsAfterBZPD5<-numeric()
ReturnsAfterBPD5P1<-numeric()
ReturnsAfterGreP1<-numeric()
for (i in 1:(nrow(Market)-1)) {
  if (Market$LessN1[i]==1) {
    ReturnsAfterLessN1 <- append(ReturnsAfterLessN1, Market$Return[i+1])
  }
  else if (Market$BN1ND5[i]==1) {
    ReturnsAfterBN1ND5 <- append(ReturnsAfterBN1ND5, Market$Return[i+1])
  }
  else if (Market$BND5Z[i]==1) {
    ReturnsAfterBND5Z <- append(ReturnsAfterBND5Z, Market$Return[i+1])
  }
  else if (Market$BZPD5[i]==1) {
    ReturnsAfterBZPD5 <- append(ReturnsAfterBZPD5, Market$Return[i+1])
  }
  else if (Market$BPD5P1[i]==1) {
    ReturnsAfterBPD5P1 <- append(ReturnsAfterBPD5P1, Market$Return[i+1])
  }
  else if (Market$GreP1[i]==1) {
    ReturnsAfterGreP1 <- append(ReturnsAfterGreP1, Market$Return[i+1])
  }
}
categories <- function(x){
  cat <- case_when (
    x< -.01 ~ "LessN1",
    (x>=-.01 & x< -.005) ~ "BN1ND5",
    (x>=-.005 & x<0) ~ "BND5Z",
    (x>=0 & x<.005) ~ "BZPD5",
    (x>=.005 & x<.01) ~ "BPD5P1",
    (x>.01) ~ "GreP1",
  )
  cat <- factor(cat, levels = c("LessN1", "BN1ND5", "BND5Z", "BZPD5", 
                                "BPD5P1", "GreP1"))
  return(cat)
}
vec_props <- function(x){
  ftable <- table(x)
  ptable <- prop.table(ftable)
  props <- as.numeric(ptable)
  return(props)
}
returns_list <- list(ReturnsAfterLessN1, ReturnsAfterBN1ND5, ReturnsAfterBND5Z,
     ReturnsAfterBZPD5, ReturnsAfterBPD5P1,ReturnsAfterGreP1)
mat <- matrix(nrow=length(returns_list),ncol=length(returns_list))
for (j in 1:length(returns_list)) {
  x <- sapply(returns_list[[j]], categories)
  row <- vec_props(x)
  mat[j,] <- row
}
namesmat<-c("< -1%", "-1to-0.5%", "-0.5%to0%", "0%to0.5%", ".5%to1%", 
         "> 1%")
colnames(mat)<-namesmat
rownames(mat)<-namesmat
mat <- round(mat,2)
View(mat)
#Checks for the sample size of each row to compute CIs
row_obs <- matrix(nrow=length(returns_list),ncol=length(returns_list))
for (j in 1:length(returns_list)) {
  print(length(returns_list[[j]]))
}
#Backtest
#Create Train vs Test Split and Train the Data
returns <- Market$Return
states <- categories(Market$Return)
n <- length(states)
split <- floor(0.7 * n)
train_states <- states[1:split]
test_states  <- states[(split+1):n]
p_counts <- table(train_states[-length(train_states)],
                  train_states[-1])
P <- prop.table(p_counts, 1)
#Compute Log-likelihoods
log_likelihood <- 0
for (i in 1:(length(test_states)-1)) {
  prev_s <- test_states[i]
  next_s <- test_states[i+1]
  prob <- P[prev_s, next_s]
  log_likelihood <- log_likelihood + log(prob + 1e-10)
}
log_likelihood
#Unconditional probabilities
uncond_probs <- prop.table(table(train_states))
log_likelihood_uncond <- 0
for (i in 1:(length(test_states)-1)) {
  prev_s <- test_states[i]
  prob <- uncond_probs[prev_s]
  log_likelihood_uncond <- log_likelihood_uncond + log(prob + 1e-10)
}
log_likelihood_uncond

