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


