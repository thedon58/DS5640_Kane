Homework 4
================
Donald Kane
February 22, 2022

# Exercise 4

### “When the number of features p is large, there tends to be a deterioration in the performance of KNN and other local approaches that perform prediction using only observations that are near the test observation for which a prediction must be made. This phenomenon is known as the curse of dimensionality, and it ties into the fact that non-parametric approaches often perform poorly when p is large. We will now investigate this curse.”

### 4.a

#### Suppose that we have a set of observations, each with measure- ments on p = 1 feature, X. We assume that X is uniformly (evenly) distributed on \[0, 1\]. Associated with each observation is a response value. Suppose that we wish to predict a test obser- vation’s response using only observations that are within 10 % of the range of X closest to that test observation. For instance, in order to predict the response for a test observation with X = 0.6, we will use observations in the range \[0.55,0.65\]. On average, what fraction of the available observations will we use to make the prediction?

``` r
(.05 + .06 + .07 + .08 + .09 + .1*90 + .09 + .08 + 0.07 + .06 + .05)
```

    ## [1] 9.7

Because the values near the ends (0 and 1) cannot fully use the 10%
rule, their values will be less. This applies to the values from 0 to
0.04 and 0.96 to 1. So for value 0.01, it can only contain 6%. Above, I
added up these values along with the 10% for the 90 values between 0.05
and 0.95 and got an average of 9.7%.

### 4.b

#### Now suppose that we have a set of observations, each with measurements on p = 2 features, X1 and X2. We assume that (X1, X2) are uniformly distributed on \[0, 1\] × \[0, 1\]. We wish to predict a test observation’s response using only observations that are within 10 % of the range of X1 and within 10 % of the range of X2 closest to that test observation. For instance, in order to predict the response for a test observation with X1 = 0.6 and X2 = 0.35, we will use observations in the range \[0.55, 0.65\] for X1 and in the range \[0.3,0.4\] for X2. On average, what fraction of the available observations will we use to make the prediction?

Using the average from question 4.a, when we use the formula (0.097)^p,
we find out answer of 0.94%

``` r
0.097^2
```

    ## [1] 0.009409

### 4.c

#### Now suppose that we have a set of observations on p = 100 fea- tures. Again the observations are uniformly distributed on each feature, and again each feature ranges in value from 0 to 1. We wish to predict a test observation’s response using observations within the 10 % of each feature’s range that is closest to that test observation. What fraction of the available observations will we use to make the prediction?

``` r
(0.097^100)
```

    ## [1] 4.755251e-102

Using the average from question 4.a, when we use the formula (0.097)^p,
we find out answer of 4.755e-100%

### 4.d

#### Using your answers to parts (a)–(c), argue that a drawback of KNN when p is large is that there are very few training obser- vations “near” any given test observation.

Using the formula of (average percentage)^observations, we can see that
as observations increase, the fraction of available observations we use
to make the prediction decreases exponentially. With one observation we
had 9.7%, with 2 observations we had 0.94%, and with 100 observations we
only used 4.755e-100%.

### 4.e

#### Now suppose that we wish to make a prediction for a test observation by creating a p-dimensional hypercube centered around the test observation that contains, on average, 10 % of the train- ing observations. For p = 1,2, and 100, what is the length of each side of the hypercube? Comment on your answer.

To make a prediction for a test observation by creating a p-dimensional
hypercube, the only change we need to make is in the exponent. Our new
formula (0.097)^(1/p) will show an exponential growth for the fraction
of available observations we use to make the prediction. Below we can
see the calculations. p = 1: 9.7% p = 2: 31.1% p = 100: 97.7% This tells
us that the higher the observations, the longer the sides are and the
further the nearest neighbor is.

``` r
(0.097)^(1/1)
```

    ## [1] 0.097

``` r
(0.097)^(1/2)
```

    ## [1] 0.3114482

``` r
(0.097)^(1/100)
```

    ## [1] 0.9769396

# Exercise 13

### This question should be answered using the Weekly data set, which is part of the ISLR2 package. This data is similar in nature to the Smarket data from this chapter’s lab, except that it contains 1, 089 weekly returns for 21 years, from the beginning of 1990 to the end of 2010.

``` r
library(ISLR)
library(tidyverse)
library(stats)
library(MASS)
library(e1071)
library(class)
data("Weekly")
```

### 13.a

#### Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any patterns?

``` r
head(Weekly)
```

    ##   Year   Lag1   Lag2   Lag3   Lag4   Lag5    Volume  Today Direction
    ## 1 1990  0.816  1.572 -3.936 -0.229 -3.484 0.1549760 -0.270      Down
    ## 2 1990 -0.270  0.816  1.572 -3.936 -0.229 0.1485740 -2.576      Down
    ## 3 1990 -2.576 -0.270  0.816  1.572 -3.936 0.1598375  3.514        Up
    ## 4 1990  3.514 -2.576 -0.270  0.816  1.572 0.1616300  0.712        Up
    ## 5 1990  0.712  3.514 -2.576 -0.270  0.816 0.1537280  1.178        Up
    ## 6 1990  1.178  0.712  3.514 -2.576 -0.270 0.1544440 -1.372      Down

``` r
summary(Weekly)
```

    ##       Year           Lag1               Lag2               Lag3         
    ##  Min.   :1990   Min.   :-18.1950   Min.   :-18.1950   Min.   :-18.1950  
    ##  1st Qu.:1995   1st Qu.: -1.1540   1st Qu.: -1.1540   1st Qu.: -1.1580  
    ##  Median :2000   Median :  0.2410   Median :  0.2410   Median :  0.2410  
    ##  Mean   :2000   Mean   :  0.1506   Mean   :  0.1511   Mean   :  0.1472  
    ##  3rd Qu.:2005   3rd Qu.:  1.4050   3rd Qu.:  1.4090   3rd Qu.:  1.4090  
    ##  Max.   :2010   Max.   : 12.0260   Max.   : 12.0260   Max.   : 12.0260  
    ##       Lag4               Lag5              Volume            Today         
    ##  Min.   :-18.1950   Min.   :-18.1950   Min.   :0.08747   Min.   :-18.1950  
    ##  1st Qu.: -1.1580   1st Qu.: -1.1660   1st Qu.:0.33202   1st Qu.: -1.1540  
    ##  Median :  0.2380   Median :  0.2340   Median :1.00268   Median :  0.2410  
    ##  Mean   :  0.1458   Mean   :  0.1399   Mean   :1.57462   Mean   :  0.1499  
    ##  3rd Qu.:  1.4090   3rd Qu.:  1.4050   3rd Qu.:2.05373   3rd Qu.:  1.4050  
    ##  Max.   : 12.0260   Max.   : 12.0260   Max.   :9.32821   Max.   : 12.0260  
    ##  Direction 
    ##  Down:484  
    ##  Up  :605  
    ##            
    ##            
    ##            
    ## 

``` r
cor(Weekly[,-9])
```

    ##               Year         Lag1        Lag2        Lag3         Lag4
    ## Year    1.00000000 -0.032289274 -0.03339001 -0.03000649 -0.031127923
    ## Lag1   -0.03228927  1.000000000 -0.07485305  0.05863568 -0.071273876
    ## Lag2   -0.03339001 -0.074853051  1.00000000 -0.07572091  0.058381535
    ## Lag3   -0.03000649  0.058635682 -0.07572091  1.00000000 -0.075395865
    ## Lag4   -0.03112792 -0.071273876  0.05838153 -0.07539587  1.000000000
    ## Lag5   -0.03051910 -0.008183096 -0.07249948  0.06065717 -0.075675027
    ## Volume  0.84194162 -0.064951313 -0.08551314 -0.06928771 -0.061074617
    ## Today  -0.03245989 -0.075031842  0.05916672 -0.07124364 -0.007825873
    ##                Lag5      Volume        Today
    ## Year   -0.030519101  0.84194162 -0.032459894
    ## Lag1   -0.008183096 -0.06495131 -0.075031842
    ## Lag2   -0.072499482 -0.08551314  0.059166717
    ## Lag3    0.060657175 -0.06928771 -0.071243639
    ## Lag4   -0.075675027 -0.06107462 -0.007825873
    ## Lag5    1.000000000 -0.05851741  0.011012698
    ## Volume -0.058517414  1.00000000 -0.033077783
    ## Today   0.011012698 -0.03307778  1.000000000

### 13.b

#### Use the full data set to perform a logistic regression with Direction as the response and the five lag variables plus Volume as predictors. Use the summary function to print the results. Do any of the predictors appear to be statistically significant? If so, which ones?

``` r
glm_fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm_fits)
```

    ## 
    ## Call:
    ## glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
    ##     Volume, family = binomial, data = Weekly)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6949  -1.2565   0.9913   1.0849   1.4579  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)  0.26686    0.08593   3.106   0.0019 **
    ## Lag1        -0.04127    0.02641  -1.563   0.1181   
    ## Lag2         0.05844    0.02686   2.175   0.0296 * 
    ## Lag3        -0.01606    0.02666  -0.602   0.5469   
    ## Lag4        -0.02779    0.02646  -1.050   0.2937   
    ## Lag5        -0.01447    0.02638  -0.549   0.5833   
    ## Volume      -0.02274    0.03690  -0.616   0.5377   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1496.2  on 1088  degrees of freedom
    ## Residual deviance: 1486.4  on 1082  degrees of freedom
    ## AIC: 1500.4
    ## 
    ## Number of Fisher Scoring iterations: 4

Only Lag2 is statistically significant at the 5% alpha level.

### 13.c

#### Compute the confusion matrix and overall fraction of correct predictions. Explain what the confusion matrix is telling you about the types of mistakes made by logistic regression.

``` r
Direction <- Weekly$Direction
glm_probs <- predict(glm_fits, type = "response")
glm_probs[1:10]
```

    ##         1         2         3         4         5         6         7         8 
    ## 0.6086249 0.6010314 0.5875699 0.4816416 0.6169013 0.5684190 0.5786097 0.5151972 
    ##         9        10 
    ## 0.5715200 0.5554287

``` r
contrasts(Direction)
```

    ##      Up
    ## Down  0
    ## Up    1

``` r
glm_pred <- rep("Down", length(Direction))
glm_pred[glm_probs > 0.5] = "Up"
table(glm_pred, Direction)
```

    ##         Direction
    ## glm_pred Down  Up
    ##     Down   54  48
    ##     Up    430 557

``` r
mean(glm_pred == Direction)
```

    ## [1] 0.5610652

Our model correctly predicts the direction 56.1% of the time. Based on
the confusion matrix, the model is less accurate in its predictions when
the direction is down.

### 13.d

#### Now fit the logistic regression model using a training data period from 1990 to 2008, with Lag2 as the only predictor. Compute the confusion matrix and the overall fraction of correct predictions for the held out data (that is, the data from 2009 and 2010).

``` r
train <- (Weekly$Year <= 2008)
Weekly.2008 <- Weekly[!train,]
dim(Weekly.2008)
```

    ## [1] 104   9

``` r
Direction.2008 <- Direction[!train]

glm_fits <- glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
glm_probs <- predict(glm_fits, Weekly.2008, type = "response")
glm_pred <- rep("Down", length(Direction.2008))
glm_pred[glm_probs > 0.5] <- "Up"
table(glm_pred, Direction.2008)
```

    ##         Direction.2008
    ## glm_pred Down Up
    ##     Down    9  5
    ##     Up     34 56

``` r
mean(glm_pred == Direction.2008)
```

    ## [1] 0.625

``` r
mean(glm_pred != Direction.2008)
```

    ## [1] 0.375

### 13.e

#### Repeat (d) using LDA.

``` r
lda_fits <- lda(Direction ~ Lag2, data = Weekly, subset = train)
lda_fits
```

    ## Call:
    ## lda(Direction ~ Lag2, data = Weekly, subset = train)
    ## 
    ## Prior probabilities of groups:
    ##      Down        Up 
    ## 0.4477157 0.5522843 
    ## 
    ## Group means:
    ##             Lag2
    ## Down -0.03568254
    ## Up    0.26036581
    ## 
    ## Coefficients of linear discriminants:
    ##            LD1
    ## Lag2 0.4414162

``` r
lda_pred <- predict(lda_fits, Weekly.2008)
names(lda_pred)
```

    ## [1] "class"     "posterior" "x"

``` r
lda_class <- lda_pred$class
table(lda_class, Direction.2008)
```

    ##          Direction.2008
    ## lda_class Down Up
    ##      Down    9  5
    ##      Up     34 56

``` r
mean(lda_class == Direction.2008)
```

    ## [1] 0.625

### 13.f

#### Repeat (d) using QDA.

``` r
qda_fit <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda_fit
```

    ## Call:
    ## qda(Direction ~ Lag2, data = Weekly, subset = train)
    ## 
    ## Prior probabilities of groups:
    ##      Down        Up 
    ## 0.4477157 0.5522843 
    ## 
    ## Group means:
    ##             Lag2
    ## Down -0.03568254
    ## Up    0.26036581

``` r
qda_class <- predict(qda_fit, Weekly.2008)$class
table(qda_class, Direction.2008)
```

    ##          Direction.2008
    ## qda_class Down Up
    ##      Down    0  0
    ##      Up     43 61

``` r
mean(qda_class == Direction.2008)
```

    ## [1] 0.5865385

### 13.g

#### Repeat (d) using KNN with K = 1.

``` r
#train.X <- cbind(Weekly$Lag1, Weekly$Lag2)[train, ]
#test.X <- cbind(Weekly$Lag1, Weekly$Lag2)[!train]
#train.Direction <- Weekly$Direction[train]

#set.seed(1)
#knn_pred <- knn(train.X, test.X, train.Direction, k = 1)
#table(knn_pred, Direction.2008)
```

### 13.h

#### Repeat (d) using naive Bayes.

``` r
nb_fit <- naiveBayes(Direction ~ Lag2, data = Weekly, subset = train)
nb_fit
```

    ## 
    ## Naive Bayes Classifier for Discrete Predictors
    ## 
    ## Call:
    ## naiveBayes.default(x = X, y = Y, laplace = laplace)
    ## 
    ## A-priori probabilities:
    ## Y
    ##      Down        Up 
    ## 0.4477157 0.5522843 
    ## 
    ## Conditional probabilities:
    ##       Lag2
    ## Y             [,1]     [,2]
    ##   Down -0.03568254 2.199504
    ##   Up    0.26036581 2.317485

``` r
nb_class <- predict(nb_fit, Weekly.2008)
table(nb_class, Direction.2008)
```

    ##         Direction.2008
    ## nb_class Down Up
    ##     Down    0  0
    ##     Up     43 61

``` r
mean(nb_class == Direction.2008)
```

    ## [1] 0.5865385

### 13.i

#### Which of these methods appears to provide the best results on this data?

Based on the models ran, our best results come from either the QDA model
or the LDA. (KNN model not running)

### 13.j

#### Experiment with different combinations of predictors, includ- ing possible transformations and interactions, for each of the methods. Report the variables, method, and associated confu- sion matrix that appears to provide the best results on the held out data. Note that you should also experiment with values for K in the KNN classifier.
