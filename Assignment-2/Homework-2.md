Homework 2
================
Donald Kane
February 3, 2022

``` r
library(qrnn)
## load prostate data
prostate <- 
  read.table(url(
    'https://web.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data'))
```

``` r
## subset to training examples
prostate_train <- subset(prostate, train==TRUE)

## plot lcavol vs lpsa
plot_psa_data <- function(dat=prostate_train) {
  plot(dat$lpsa, dat$lcavol,
       xlab="log Prostate Screening Antigen (psa)",
       ylab="log Cancer Volume (lcavol)",
       pch = 20)
}
plot_psa_data()
```

![](Homework-2_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
############################
## regular linear regression
############################

## L2 loss function
L2_loss <- function(y, yhat)
  (y-yhat)^2

## fit simple linear model using numerical optimization
fit_lin <- function(y, x, loss=L2_loss, beta_init = c(-0.51, 0.75)) {
  err <- function(beta)
    mean(loss(y,  beta[1] + beta[2]*x))
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}

## make predictions from linear model
predict_lin <- function(x, beta){
  beta[1] + beta[2]*x
}

## fit linear model
lin_beta <- fit_lin(y=prostate_train$lcavol,
                    x=prostate_train$lpsa,
                    loss=L2_loss)

## compute predictions for a grid of inputs
x_grid <- seq(min(prostate_train$lpsa),
              max(prostate_train$lpsa),
              length.out=100)
lin_pred <- predict_lin(x=x_grid, beta=lin_beta$par)

## plot data
plot_psa_data()

## plot predictions
lines(x=x_grid, y=lin_pred, col='darkgreen', lwd=2)

## do the same thing with 'lm'
lin_fit_lm <- lm(lcavol ~ lpsa, data=prostate_train)

## make predictins using 'lm' object
lin_pred_lm <- predict(lin_fit_lm, data.frame(lpsa=x_grid))

## plot predictions from 'lm'
lines(x=x_grid, y=lin_pred_lm, col='pink', lty=2, lwd=2)
```

![](Homework-2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
##################################
## try modifying the loss function
##################################

## custom loss function
custom_loss <- function(y, yhat)
  (y-yhat)^2 + abs(y-yhat)

## plot custom loss function
err_grd <- seq(-1,1,length.out=200)
plot(err_grd, custom_loss(err_grd,0), type='l',
     xlab='y-yhat', ylab='custom loss')
```

![](Homework-2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
## fit linear model with custom loss
lin_beta_custom <- fit_lin(y=prostate_train$lcavol,
                    x=prostate_train$lpsa,
                    loss=custom_loss)

lin_pred_custom <- predict_lin(x=x_grid, beta=lin_beta_custom$par)

## plot data
plot_psa_data()

## plot predictions from L2 loss
lines(x=x_grid, y=lin_pred, col='darkgreen', lwd=2)

## plot predictions from custom loss
lines(x=x_grid, y=lin_pred_custom, col='pink', lwd=2, lty=2)
```

![](Homework-2_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

# Question 1

``` r
#L1 Loss
L1_loss <- function(y, yhat){
  abs(y-yhat)
}

tilted_loss <- function(y, yhat){
  tilted.abs(y - yhat, tau = 0.5)
}
```

# Question 2

``` r
## L2 loss function
L2_loss <- function(y, yhat)
  (y-yhat)^2

## fit simple linear model using numerical optimization
fit_lin <- function(y, x, loss=L2_loss, beta_init = c(-0.51, 0.75)) {
  err <- function(beta)
    mean(loss(y,  beta[1] + beta[2]*x))
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}

## make predictions from linear model
predict_lin <- function(x, beta)
  beta[1] + beta[2]*x

## fit linear model
lin_beta <- fit_lin(y=prostate_train$lcavol,
                    x=prostate_train$lpsa,
                    loss=L2_loss)

## compute predictions for a grid of inputs
x_grid <- seq(min(prostate_train$lpsa),
              max(prostate_train$lpsa),
              length.out=100)
lin_pred <- predict_lin(x=x_grid, beta=lin_beta$par)

## plot data
plot_psa_data()

## plot predictions
lines(x=x_grid, y=lin_pred, col='black', lwd=2)

########

#L1 Loss Function
L1_loss <- function(y, yhat){
  abs(y-yhat)
}

## fit simple linear model using numerical optimization
fit_lin <- function(y, x, loss=L1_loss, beta_init = c(-0.51, 0.75)) {
  err <- function(beta)
    mean(loss(y,  beta[1] + beta[2]*x))
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}

## make predictions from linear model
predict_lin <- function(x, beta)
  beta[1] + beta[2]*x

## fit linear model
lin_beta <- fit_lin(y=prostate_train$lcavol,
                    x=prostate_train$lpsa,
                    loss=L1_loss)

## compute predictions for a grid of inputs
x_grid <- seq(min(prostate_train$lpsa),
              max(prostate_train$lpsa),
              length.out=100)
lin_pred <- predict_lin(x=x_grid, beta=lin_beta$par)

## plot predictions
lines(x=x_grid, y=lin_pred, col='red', lwd=2)

#########

tilted_loss <- function(y, y_hat){
  tilted.abs(y - y_hat, tau = 0.25)
}

## fit simple linear model using numerical optimization
fit_lin <- function(y, x, loss=tilted_loss, beta_init = c(-0.51, 0.75)) {
  err <- function(beta)
    mean(loss(y,  beta[1] + beta[2]*x))
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}

## make predictions from linear model
predict_lin <- function(x, beta)
  beta[1] + beta[2]*x

## fit linear model
lin_beta <- fit_lin(y=prostate_train$lcavol,
                    x=prostate_train$lpsa,
                    loss=tilted_loss)

## compute predictions for a grid of inputs
x_grid <- seq(min(prostate_train$lpsa),
              max(prostate_train$lpsa),
              length.out=100)
lin_pred <- predict_lin(x=x_grid, beta=lin_beta$par)

## plot predictions
lines(x=x_grid, y=lin_pred, col='blue', lwd=2)

#########

tilted_loss <- function(y, y_hat){
  tilted.abs(y - y_hat, tau = 0.75)
}

## fit simple linear model using numerical optimization
fit_lin <- function(y, x, loss=tilted_loss, beta_init = c(-0.51, 0.75)) {
  err <- function(beta)
    mean(loss(y,  beta[1] + beta[2]*x))
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}

## make predictions from linear model
predict_lin <- function(x, beta)
  beta[1] + beta[2]*x

## fit linear model
lin_beta <- fit_lin(y=prostate_train$lcavol,
                    x=prostate_train$lpsa,
                    loss=tilted_loss)

## compute predictions for a grid of inputs
x_grid <- seq(min(prostate_train$lpsa),
              max(prostate_train$lpsa),
              length.out=100)
lin_pred <- predict_lin(x=x_grid, beta=lin_beta$par)

## plot predictions
lines(x=x_grid, y=lin_pred, col='yellow', lwd=2)

legend(0, 4, 
       legend=c("L2 loss", "L1 loss", "tau = 0.25", "tau = 0.75"),
       col=c("black", "red", "blue", "yellow"), 
       lty=1, 
       cex=0.8, 
       text.font=4)
```

![](Homework-2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Question 3

``` r
## fit simple nonlinear model using numerical optimization
fit_lin <- function(y, x, loss=L2_loss, beta_init = c(-0.51, 0.75)) {
  err <- function(beta){
    beta[1] + beta[2]*exp(-beta[3]*x)
  }
  beta <- optim(par = c(-1.0, 0.0, -0.3) , fn = err)
  return(beta)
}

## make predictions from nonlinear model
predict_lin <- function(x, beta){
  beta[1] + beta[2]*exp(-beta[3]*x)
}
```

# Question 4

``` r
## L2 loss function
L2_loss <- function(y, yhat){
  (y-yhat)^2
}
## fit simple nonlinear model using numerical optimization
fit_lin <- function(y, x, loss=L2_loss, beta_init = c(-1.0, 0.0, -0.3) ) {
  err <- function(beta){
    mean(loss(y, beta[1] + beta[2]*exp(-beta[3]*x)))
  }
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}

## make predictions from nonlinear model
predict_lin <- function(x, beta){
  beta[1] + beta[2]*exp(-beta[3]*x)
}

## fit linear model
lin_beta <- fit_lin(y=prostate_train$lcavol,
                    x=prostate_train$lpsa,
                    loss=L2_loss)

## compute predictions for a grid of inputs
x_grid <- seq(min(prostate_train$lpsa),
              max(prostate_train$lpsa),
              length.out=100)
lin_pred <- predict_lin(x=x_grid, beta=lin_beta$par)

## plot data
plot_psa_data()

## plot predictions
lines(x=x_grid, y=lin_pred, col='black', lwd=2)

########

#L1 Loss Function
L1_loss <- function(y, yhat){
  abs(y-yhat)
}

## fit simple nonlinear model using numerical optimization
fit_lin <- function(y, x, loss=L1_loss, beta_init = c(-1.0, 0.0, -0.3) ) {
  err <- function(beta){
    mean(loss(y, beta[1] + beta[2]*exp(-beta[3]*x)))
  }
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}

## make predictions from nonlinear model
predict_lin <- function(x, beta){
  beta[1] + beta[2]*exp(-beta[3]*x)
}

## fit linear model
lin_beta <- fit_lin(y=prostate_train$lcavol,
                    x=prostate_train$lpsa,
                    loss=L1_loss)

## compute predictions for a grid of inputs
x_grid <- seq(min(prostate_train$lpsa),
              max(prostate_train$lpsa),
              length.out=100)
lin_pred <- predict_lin(x=x_grid, beta=lin_beta$par)

## plot predictions
lines(x=x_grid, y=lin_pred, col='red', lwd=2)

#########

tilted_loss <- function(y, y_hat){
  tilted.abs(y - y_hat, tau = 0.25)
}

## fit simple nonlinear model using numerical optimization
fit_lin <- function(y, x, loss=tilted_loss, beta_init = c(-1.0, 0.0, -0.3) ) {
  err <- function(beta){
    mean(loss(y, beta[1] + beta[2]*exp(-beta[3]*x)))
  }
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}

## make predictions from nonlinear model
predict_lin <- function(x, beta)
  beta[1] + beta[2]*exp(-beta[3]*x)

## fit nonlinear model
lin_beta <- fit_lin(y=prostate_train$lcavol,
                    x=prostate_train$lpsa,
                    loss=tilted_loss)

## compute predictions for a grid of inputs
x_grid <- seq(min(prostate_train$lpsa),
              max(prostate_train$lpsa),
              length.out=100)
lin_pred <- predict_lin(x=x_grid, beta=lin_beta$par)

## plot predictions
lines(x=x_grid, y=lin_pred, col='blue', lwd=2)

#########

tilted_loss <- function(y, y_hat){
  tilted.abs(y - y_hat, tau = 0.75)
}

## fit simple linear model using numerical optimization
fit_lin <- function(y, x, loss=tilted_loss, beta_init = c(-1.0, 0.0, -0.3)) {
  err <- function(beta)
    mean(loss(y, beta[1] + beta[2]*exp(-beta[3]*x)))
  beta <- optim(par = beta_init, fn = err)
  return(beta)
}

## make predictions from linear model
predict_lin <- function(x, beta){
  beta[1] + beta[2]*exp(-beta[3]*x)
}

## fit linear model
lin_beta <- fit_lin(y=prostate_train$lcavol,
                    x=prostate_train$lpsa,
                    loss=tilted_loss)

## compute predictions for a grid of inputs
x_grid <- seq(min(prostate_train$lpsa),
              max(prostate_train$lpsa),
              length.out=100)
lin_pred <- predict_lin(x=x_grid, beta=lin_beta$par)

## plot predictions
lines(x=x_grid, y=lin_pred, col='yellow', lwd=2)

legend(0, 4, 
       legend=c("L2 loss", "L1 loss", "tau = 0.25", "tau = 0.75"),
       col=c("black", "red", "blue", "yellow"), 
       lty=1, 
       cex=0.8, 
       text.font=4)
```

![](Homework-2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
