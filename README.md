# ECO-HW-3
Homework 3:

Orginal Lap Work:
```
> borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))
> norm_varb <- function(X_in) {
+     (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
+ }
> is.na(OWNCOST) <- which(OWNCOST == 9999999)
> housing_cost <- OWNCOST + RENT
> norm_inc_tot <- norm_varb(INCTOT)
> norm_housing_cost <- norm_varb(housing_cost)
> data_use_prelim <- data.frame(norm_inc_tot,norm_housing_cost)
> good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
> dat_use <- subset(data_use_prelim,good_obs_data_use)
> y_use <- subset(borough_f,good_obs_data_use)
> set.seed(12345)
> NN_obs <- sum(good_obs_data_use == 1)
> select1 <- (runif(NN_obs) < 0.8)
> train_data <- subset(dat_use,select1)
> test_data <- subset(dat_use,(!select1))
> cl_data <- y_use[select1]
> true_data <- y_use[!select1]

`````
> summary(cl_data)
        Bronx     Manhattan Staten Island      Brooklyn        Queens 
         4880          5250          1891         12416         10923 
````
> prop.table(summary(cl_data))
        Bronx     Manhattan Staten Island      Brooklyn        Queens 
   0.13800905    0.14847285    0.05347851    0.35113122    0.30890837 

````
> summary(train_data)
  norm_inc_tot     norm_housing_cost
 Min.   :0.00000   Min.   :0.00000  
 1st Qu.:0.01191   1st Qu.:0.02493  
 Median :0.02693   Median :0.96917  
 Mean   :0.04265   Mean   :0.58972  
 3rd Qu.:0.05219   3rd Qu.:0.97784  
 Max.   :1.00000   Max.   :1.00000

````
 KNN Results where our interval for K is between {1,9} where K is every odd number 
 
 ````> for (indx in seq(1, 9, by= 2)) {
+     pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
+     num_correct_labels <- sum(pred_borough == true_data)
+     correct_rate <- num_correct_labels/length(true_data)
+     print(c(indx,correct_rate))
+ 
[1] 1.0000000 0.3540087
[1] 3.0000000 0.3437859
[1] 5.0000000 0.3550425
[1] 7.0000000 0.3708936
[1] 9.0000000 0.3721571
 
````
 We replicated the experiment again with a new seed and new set of values our results were the following
 
 ````> for (indx in seq(1, 9, by= 2)) {
+     pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
+     num_correct_labels <- sum(pred_borough == true_data)
+     correct_rate <- num_correct_labels/length(true_data)
+     print(c(indx,correct_rate))
+ 
[1] 1.0000000 0.3494142
[1] 3.0000000 0.3452791
[1] 5.0000000 0.3560763
[1] 7.0000000 0.3696301
[1] 9.0000000 0.3780152

``````

We then expanded our K value to increase to 15

`````

> for (indx in seq(1, 15, by= 2)) {
+     pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
+     num_correct_labels <- sum(pred_borough == true_data)
+     correct_rate <- num_correct_labels/length(true_data)
+     print(c(indx,correct_rate))
+ }
[1] 1.0000000 0.3533195
[1] 3.0000000 0.3451643
[1] 5.0000000 0.3543533
[1] 7.0000000 0.3680221
[1] 9.000000 0.374799
[1] 11.000000  0.383184
[1] 13.0000000  0.3872042
[1] 15.0000000  0.3808867

`````
Lastly for the expierement we increased our value to 99, based on the previous results it seems that there is a positive correlation between 
an increase in K or nearest neighbors and a positive success rate of our algorthim guessing the correct borough relative to housing cost, total income and rent.

````
 for (indx in seq(1, 99, by= 2)) {
+     pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
+     num_correct_labels <- sum(pred_borough == true_data)
+     correct_rate <- num_correct_labels/length(true_data)
+     print(c(indx,correct_rate))
+ }
[1] 1.0000000 0.3536641
[1] 3.0000000 0.3480358
[1] 5.0000000 0.3577992
[1] 7.0000000 0.3704342
[1] 9.0000000 0.3746841
[1] 11.0000000  0.3799678
[1] 13.0000000  0.3822651
[1] 15.0000000  0.3824948
[1] 17.0000000  0.3843326
[1] 19.0000000  0.3889272
[1] 21.0000000  0.3895015
[1] 23.0000000  0.3850218
[1] 25.0000000  0.3873191
[1] 27.0000000  0.3921433
[1] 29.0000000  0.3889272
[1] 31.0000000  0.3895015
[1] 33.0000000  0.3888123
[1] 35.0000000  0.3898461
[1] 37.0000000  0.3914542
[1] 39.0000000  0.3922582
[1] 41.0000000  0.3909947
[1] 43.0000000  0.3901907
[1] 45.0000000  0.3889272
[1] 47.0000000  0.3889272
[1] 49.0000000  0.3898461
[1] 51.0000000  0.3943258
[1] 53.0000000  0.3908799
[1] 55.0000000  0.3903055
[1] 57.0000000  0.3921433
[1] 59.0000000  0.3939812
[1] 61.0000000  0.3930623
[1] 63.0000000  0.3922582
[1] 65.000000  0.391569
[1] 67.0000000  0.3975419
[1] 69.0000000  0.3960487
[1] 71.0000000  0.3962784
[1] 73.0000000  0.3977717
[1] 75.0000000  0.3989203
[1] 77.0000000  0.4000689
[1] 79.0000000  0.3986906
[1] 81.0000000  0.3999541
[1] 83.0000000  0.4011027
[1] 85.0000000  0.4022513
[1] 87.0000000  0.3999541
[1] 89.0000000  0.4002986
[1] 91.000000  0.400873
[1] 93.000000  0.398346
[1] 95.0000000  0.3973122
[1] 97.0000000  0.3953595
[1] 99.0000000  0.3971973

```Our Correct rate was the following:

> mean(correct_rate)
[1] 0.395153026

> summary(correct_rate)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.34  0.3972  0.3972  0.3951  0.3961  0.48735
 ````
 Group Experiment
 
For our own expieriment, in order to try and create a KNN alogorithm that could help determine one's particular borough we decided to use age and room size as a factor, the idea being that one's age and living space could be a helpful indicator in location.  A younger individual with a smaller room size, might be in
 the start of their career as a result they would have a higher propensity to be living in the outer boroughs (areas with lower median rental costs)  in order to find more   more affordable housing options  relative to an older person who has more expierence in their career and would have more disposable cash available for the extra space in a higher cost of living borough
 
 ````
 Code:
> dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
> attach(dat_NYC)
> borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))
> 
> norm_varb <- function(X_in) {
+     (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
+ }
 
 ````
 * Our two main variables were Age and ROOM space which we designated as Respondent_Age = Age 
 and Living_Space = ROOMS
 
 ````
> Respondent_Age <- AGE
> Living_Space <- ROOMS
````
We normalized normalized respondents Age and Room Size

> norm_varb(Respondent_Age)

> norm_varb(Living_Space)

````
> data_use_prelim <- data.frame (Living_Space, Respondent_Age)
> good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
> dat_use <- subset(data_use_prelim,good_obs_data_use)
> y_use <- subset(borough_f,good_obs_data_use)
> set.seed(12345)
> NN_obs <- sum(good_obs_data_use == 1)
> select1 <- (runif(NN_obs) < 0.8)
> train_data <- subset(dat_use,select1)
> test_data <- subset(dat_use,(!select1))
> cl_data <- y_use[select1
+ ]
> true_data <- y_use[!select1]

> summary(cl_data)
        Bronx     Manhattan Staten Island      Brooklyn 
         4880          5250          1891         12416 
       Queens 
        10923 
> prop.table(summary(cl_data))
        Bronx     Manhattan Staten Island      Brooklyn 
   0.13800905    0.14847285    0.05347851    0.35113122 
       Queens 
   0.30890837 
> summary(train_data)
  Living_Space    Respondent_Age 
 Min.   : 0.000   Min.   :21.00  
 1st Qu.: 3.000   1st Qu.:30.00  
 Median : 4.000   Median :41.00  
 Mean   : 4.762   Mean   :41.96  
 3rd Qu.: 6.000   3rd Qu.:53.00  
 Max.   :16.000   Max.   :65.00  
```
KNN Code Algorithim
> for (indx in seq(1, 9, by= 2)) {
+     pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
+     num_correct_labels <- sum(pred_borough == true_data)
+     correct_rate <- num_correct_labels/length(true_data)
+     print(c(indx,correct_rate))
+ }
[1] 1.0000000 0.3742247
[1] 3.0000000 0.3737652
[1] 5.0000000 0.3777854
[1] 7.0000000 0.3764071
[1] 9.0000000 0.3757179

`````
KNN Code Algorithim for 21 Nearest Neighbors

````

> for (indx in seq(1, 21, by= 2)) {
+     pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
+     num_correct_labels <- sum(pred_borough == true_data)
+     correct_rate <- num_correct_labels/length(true_data)
+     print(c(indx,correct_rate))
+ }
[1] 1.000000 0.374799
[1] 3.0000000 0.3749139
[1] 5.0000000 0.3766368
[1] 7.000000 0.374799
[1] 9.0000000 0.3769814
[1] 11.0000000  0.3754882
[1] 13.0000000  0.3750287
[1] 15.0000000  0.3757179
[1] 17.0000000  0.3760625
[1] 19.0000000  0.3738801
[1] 21.0000000  0.3754882

````

KNN Code for 100 Nearest Neighbors 
```

for (indx in seq(1, 99, by= 2)) {
+     pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
+     num_correct_labels <- sum(pred_borough == true_data)
+     correct_rate <- num_correct_labels/length(true_data)
+     print(c(indx,correct_rate))
+ }
[1] 1.0000000 0.3742247
[1] 3.0000000 0.3735355
[1] 5.000000 0.374799
[1] 7.000000 0.375603
[1] 9.0000000 0.3759476
[1] 11.0000000  0.3754882
[1] 13.000000  0.375603
[1] 15.0000000  0.3745693
[1] 17.0000000  0.3760625
[1] 19.0000000  0.3749139
[1] 21.0000000  0.3745693
[1] 23.0000000  0.3744544
[1] 25.0000000  0.3729612
[1] 27.0000000  0.3741098
[1] 29.0000000  0.3731909
[1] 31.0000000  0.3734206
[1] 33.0000000  0.3743395
[1] 35.0000000  0.3741098
[1] 37.0000000  0.3753733
[1] 39.0000000  0.3736504
[1] 41.000000  0.375603
[1] 43.0000000  0.3737652
[1] 45.0000000  0.3731909
[1] 47.0000000  0.3744544
[1] 49.0000000  0.3737652
[1] 51.0000000  0.3738801
[1] 53.0000000  0.3749139
[1] 55.000000  0.375603
[1] 57.0000000  0.3754882
[1] 59.0000000  0.3751436
[1] 61.0000000  0.3749139
[1] 63.0000000  0.3751436
[1] 65.0000000  0.3750287
[1] 67.0000000  0.3749139
[1] 69.0000000  0.3752584
[1] 71.0000000  0.3750287
[1] 73.0000000  0.3754882
[1] 75.0000000  0.3753733
[1] 77.0000000  0.3761773
[1] 79.0000000  0.3752584
[1] 81.0000000  0.3762922
[1] 83.0000000  0.3760625
[1] 85.0000000  0.3764071
[1] 87.0000000  0.3764071
[1] 89.0000000  0.3759476
[1] 91.0000000  0.3764071
[1] 93.0000000  0.3757179
[1] 95.0000000  0.3767517
[1] 97.0000000  0.3776706
[1] 99.0000000  0.3769814

`````

