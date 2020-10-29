
``` r
knitr::opts_chunk$set(
  warning = FALSE, # show warnings
  message = TRUE, # show messages
  error = TRUE, # do not interrupt generation in case of errors,
  echo = TRUE  # show R code
)
```

# Geological visualization of elements

The aim of this folder is to…. create a grouped box and whisker plot
with the ggplot2 package for visualization of a variety of trace element
concentrations in different forms of Carrollite in the central African
Copperbelt, then a principle component analysis on the different styles
of mineralization

load packages in R

``` r
library(ggplot2) 
pacman::p_load(tidyverse)
library(wesanderson)
library(FactoMineR)
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

The use of ggplots2 to create the box and whisker The grouped plots that
will be used to test the geological data

loading the excel data via clipboard this will not work without it being
copied\!\!

``` r
# This is just an easy way of copying in the data and then once local storage is required the write.csv can 
# be used so that the data can be knitted
#CarJack <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
# moving it to csv files for the knit function
#write.csv(CarJack,"/Users/charlesrandell/Other/Random projects/Box and Whisker/data/CarJack.csv", row.names = FALSE)

setwd("/Users/charlesrandell/Other/Random projects/Box and Whisker/data")

CarStrat <- read.csv("CarStrat.csv", header = TRUE)

CarDissem <- read.csv("CarDissem.csv", header = TRUE)

CarCross <- read.csv("CarCross.csv", header = TRUE)

CarJack <- read.csv("CarJack.csv", header = TRUE)

combination <- read.csv("Combination.csv", header = TRUE)
```

## Tidyr

Lets see what happens… Changing the data imported in wide format into a
long tidy format

``` r
TidyCarCross <- CarCross %>% tidyr::gather(Header, val)

TidyCarDissem <- CarDissem %>% tidyr::gather(Header, val)

TidyCarStrat <- CarStrat  %>% tidyr::gather(Header, val)

TidyCarJack <- CarJack  %>% tidyr::gather(Header, val)
```

## dplyr

This creates the new variable that has the extra column with the added
indicator to be merged to the final dataset. The use of the tidyverse to
convert the data into a long format rather that a wide so that ggplot2
can create the boxplot with it

``` r
Tcarcross <- TidyCarCross %>% mutate(Classes = "4CarCross")

Tcardissem <- TidyCarDissem %>% mutate(Classes = "2CarDissem")

Tcarstrat <- TidyCarStrat %>% mutate(Classes = "1CarStrat")

Tcarjack <- TidyCarJack %>% mutate(Classes = "3CarJack")
```

## binding and filtering the data

Bind the datasets together Secondly the readings that are below the
detectable level are filtered out

``` r
Final <- rbind.data.frame(Tcarstrat, Tcardissem, Tcarcross, Tcarjack)
Final <- Final %>% filter(val != "Below LOD")
Final <- Final %>% filter(Header != "Te_ppm_m125")
```

## gsub to format elements

mutate function that can remove certain phrases within values in a
column

``` r
Final <- Final %>% mutate(Header = gsub("_ppm", "", Header))
Final <- Final %>% mutate(Header = gsub("_m126", "", Header))
```

Convert the variable Classes and Header from a character to a factor
variable Convert the value variable from a character to a numerical
variable - this is very important and the code will not run without it

``` r
Final[, 'Classes'] <- as.factor(Final[, 'Classes'])
Final[, 'val'] <- as.numeric(Final[, 'val'])
Final[, 'Header'] <- as.factor(Final[, 'Header'])
```

Firstly the outliers are removed, any value that exceeds 150 is taked
out

``` r
Final <- Final %>% filter(val < 150)
```

## ggplot to create graphs

The plot is constructed using ggplot, with two iterations of the same
graph, the first shows the raw output on a linear scale And second has a
log scale y axis, along with updated labels and different colour palette

``` r
# The Main ggplot

sfplot <- ggplot(Final, aes(x=Header, y=val, fill=Classes)) + 
    geom_boxplot(outlier.size = 0.01) + stat_summary(fun = mean, shape = 4, aes(group=Classes), position=position_dodge(0.75), show.legend = FALSE, color="black", size=0.25)

sfplot
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# For some reason this is what worked for the color codes to range from 2-5

wes1 <- wes_palette("FantasticFox1")[2:5]
roy2 <- wes_palette("Royal2")[1:3]


# The Final Wes Anderson themed plot

sfplot  +
coord_trans(y = "log") + scale_y_continuous(breaks = c(0.01, 0.1, 1, 10, 100), labels = c(0.01, 0.1, 1, 10, 100)) + labs(x="Elements", y = "Log (Concentrations) / ppm") + theme(legend.position="bottom") + scale_fill_manual(guide = guide_legend(title = ""), breaks=c("1CarStrat", "2CarDissem", "3CarJack", "4CarCross"), values=c(wes1), labels=c("Stratiform Carrollite", "Disseminated Carrollite", "Jack Vein Carrollite", "Cross-Cutting Vein Carrollite")) + theme(axis.text.x = element_text(size=12, face = "bold"), axis.text.y = element_text(size=12, face = "bold"), legend.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))
```

![](README_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

## t tests

``` r
CarCross <- mutate_all(CarCross, function(x) as.numeric(as.character(x)))
CarDissem <- mutate_all(CarDissem, function(x) as.numeric(as.character(x)))
CarJack <- mutate_all(CarJack, function(x) as.numeric(as.character(x)))
CarStrat <- mutate_all(CarStrat, function(x) as.numeric(as.character(x)))

combination <- mutate_all(combination, function(x) as.numeric(as.character(x)))

CarCross[is.na(CarCross)] <- 0
combination[is.na(combination)] <- 0

t.test(CarCross$Zn_ppm, combination$Zn_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Zn_ppm and combination$Zn_ppm
    ## t = 2.3796, df = 34.957, p-value = 0.02292
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##   1.700024 21.454587
    ## sample estimates:
    ## mean of x mean of y 
    ##  16.61388   5.03657

``` r
t.test(CarCross$Te_ppm, combination$Te_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Te_ppm and combination$Te_ppm
    ## t = 0.24777, df = 42.736, p-value = 0.8055
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.08807555  0.11274328
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.1866250 0.1742911

``` r
t.test(CarCross$Sn_ppm, combination$Sn_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Sn_ppm and combination$Sn_ppm
    ## t = -3.9063, df = 79.128, p-value = 0.0001963
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.9180788 -0.2982883
    ## sample estimates:
    ## mean of x mean of y 
    ##  0.675500  1.283684

``` r
t.test(CarCross$Se_ppm, combination$Se_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Se_ppm and combination$Se_ppm
    ## t = 2.2612, df = 41.412, p-value = 0.02906
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##   1.31094 23.15844
    ## sample estimates:
    ## mean of x mean of y 
    ##  32.70469  20.47000

``` r
t.test(CarCross$Sb_ppm, combination$Sb_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Sb_ppm and combination$Sb_ppm
    ## t = 0.58141, df = 47.478, p-value = 0.5637
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -2.614152  4.740165
    ## sample estimates:
    ## mean of x mean of y 
    ##  16.39997  15.33696

``` r
t.test(CarCross$Pb_ppm, combination$Pb_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Pb_ppm and combination$Pb_ppm
    ## t = 0.55339, df = 76.619, p-value = 0.5816
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -12.3587  21.8707
    ## sample estimates:
    ## mean of x mean of y 
    ##  28.05003  23.29403

``` r
t.test(CarCross$Mo_ppm, combination$Mo_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Mo_ppm and combination$Mo_ppm
    ## t = 2.6768, df = 41.345, p-value = 0.01061
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  1.386401 9.897097
    ## sample estimates:
    ## mean of x mean of y 
    ## 12.593813  6.952063

``` r
t.test(CarCross$In_ppm, combination$In_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$In_ppm and combination$In_ppm
    ## t = 2.4922, df = 35.887, p-value = 0.01745
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.01123388 0.10947324
    ## sample estimates:
    ##  mean of x  mean of y 
    ## 0.10411875 0.04376519

``` r
t.test(CarCross$Ge_ppm, combination$Ge_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Ge_ppm and combination$Ge_ppm
    ## t = 2.6741, df = 32.87, p-value = 0.01158
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.02003699 0.14760146
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.3537812 0.2699620

``` r
t.test(CarCross$Ga_ppm, combination$Ga_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Ga_ppm and combination$Ga_ppm
    ## t = 0.95812, df = 58.932, p-value = 0.3419
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1360807  0.3861135
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.5521063 0.4270899

``` r
t.test(CarCross$Cd_ppm, combination$Cd_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Cd_ppm and combination$Cd_ppm
    ## t = 0.70096, df = 59.531, p-value = 0.4861
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1251565  0.2601614
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.2174062 0.1499038

``` r
t.test(CarCross$Bi_ppm, combination$Bi_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Bi_ppm and combination$Bi_ppm
    ## t = 3.2645, df = 32.68, p-value = 0.002574
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##   8.017254 34.566371
    ## sample estimates:
    ## mean of x mean of y 
    ## 28.080541  6.788728

``` r
t.test(CarCross$Au_ppm, combination$Au_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Au_ppm and combination$Au_ppm
    ## t = -0.86177, df = 70.535, p-value = 0.3917
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.01736332  0.00688468
    ## sample estimates:
    ##  mean of x  mean of y 
    ## 0.03340625 0.03864557

``` r
t.test(CarCross$Ag_ppm, combination$Ag_ppm)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  CarCross$Ag_ppm and combination$Ag_ppm
    ## t = -0.048197, df = 95.661, p-value = 0.9617
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.03159172  0.03009402
    ## sample estimates:
    ##  mean of x  mean of y 
    ## 0.07960937 0.08035823

``` r
confidence_interval <- function(vector, interval) {
  # Standard deviation of sample
  vec_sd <- sd(vector)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(result)
}
```

# pca

This section will perform a principle component analysis on the all of
the grouped data with the “Below LOD” reduced to zero, and an
identification style attached to each dataset.

``` r
CarStrat$Classes <- 'CarStrat'
CarCross$Classes <- "CarCross"
CarDissem$Classes <- "CarDissem"
CarJack$Classes <- "CarJack"

pcat <- rbind(CarStrat, CarCross, CarDissem, CarJack)
pcat[, 'Classes'] <- as.factor(pcat[, 'Classes'])

car.pca <- PCA(pcat %>% select(-Classes), graph = FALSE)

pcat[is.na(pcat)] <- 0

fviz_contrib(car.pca, choice = "var", axes = 1)
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
fviz_pca_biplot(car.pca, habillage = pcat$Classes, addEllipses = TRUE, 
    col.var = "red", alpha.var = "cos2", label = "var") + scale_color_brewer(palette = "Dark2") + 
    theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->
