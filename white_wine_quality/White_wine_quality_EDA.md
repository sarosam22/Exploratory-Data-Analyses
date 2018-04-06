Exploratory Data Analysis - White Wine Quality
================

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.3

``` r
library(GGally)
```

    ## Warning: package 'GGally' was built under R version 3.4.4

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:GGally':
    ## 
    ##     nasa

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggcorrplot)
```

    ## Warning: package 'ggcorrplot' was built under R version 3.4.4

``` r
wine_qual <- read.csv('wineQualityWhites.csv')
```

After loading in the dataset, we should start with understanding the composition , shape of the dataset as well as the datatypes of the different variables

Let us get an idea about the dimensions of the dataset.

``` r
dim(wine_qual)
```

    ## [1] 4898   13

The "quality" variable is obviously a categorical variable and it would be beneficial to introduce a new column in the dataset that is a a factor variable of the quality with the avaialble levels in the dataset

``` r
wine_qual$quality.factor <- factor(wine_qual$quality)
```

Let us now get an idea of the datatypes of the variables in the dataset after adding the new factor variable.

``` r
str(wine_qual)
```

    ## 'data.frame':    4898 obs. of  14 variables:
    ##  $ X                   : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ fixed.acidity       : num  7 6.3 8.1 7.2 7.2 8.1 6.2 7 6.3 8.1 ...
    ##  $ volatile.acidity    : num  0.27 0.3 0.28 0.23 0.23 0.28 0.32 0.27 0.3 0.22 ...
    ##  $ citric.acid         : num  0.36 0.34 0.4 0.32 0.32 0.4 0.16 0.36 0.34 0.43 ...
    ##  $ residual.sugar      : num  20.7 1.6 6.9 8.5 8.5 6.9 7 20.7 1.6 1.5 ...
    ##  $ chlorides           : num  0.045 0.049 0.05 0.058 0.058 0.05 0.045 0.045 0.049 0.044 ...
    ##  $ free.sulfur.dioxide : num  45 14 30 47 47 30 30 45 14 28 ...
    ##  $ total.sulfur.dioxide: num  170 132 97 186 186 97 136 170 132 129 ...
    ##  $ density             : num  1.001 0.994 0.995 0.996 0.996 ...
    ##  $ pH                  : num  3 3.3 3.26 3.19 3.19 3.26 3.18 3 3.3 3.22 ...
    ##  $ sulphates           : num  0.45 0.49 0.44 0.4 0.4 0.44 0.47 0.45 0.49 0.45 ...
    ##  $ alcohol             : num  8.8 9.5 10.1 9.9 9.9 10.1 9.6 8.8 9.5 11 ...
    ##  $ quality             : int  6 6 6 6 6 6 6 6 6 6 ...
    ##  $ quality.factor      : Factor w/ 7 levels "3","4","5","6",..: 4 4 4 4 4 4 4 4 4 4 ...

The chemical properties of the wine are represented as continous variables and the quality rating of the wine has been designated by an integer

Quality is the dependent variable being dependent on multiple chemical properties of the wine

Let's print out summary of dataset and start concentrating on each variable in turn

``` r
summary(wine_qual)
```

    ##        X        fixed.acidity    volatile.acidity  citric.acid    
    ##  Min.   :   1   Min.   : 3.800   Min.   :0.0800   Min.   :0.0000  
    ##  1st Qu.:1225   1st Qu.: 6.300   1st Qu.:0.2100   1st Qu.:0.2700  
    ##  Median :2450   Median : 6.800   Median :0.2600   Median :0.3200  
    ##  Mean   :2450   Mean   : 6.855   Mean   :0.2782   Mean   :0.3342  
    ##  3rd Qu.:3674   3rd Qu.: 7.300   3rd Qu.:0.3200   3rd Qu.:0.3900  
    ##  Max.   :4898   Max.   :14.200   Max.   :1.1000   Max.   :1.6600  
    ##                                                                   
    ##  residual.sugar     chlorides       free.sulfur.dioxide
    ##  Min.   : 0.600   Min.   :0.00900   Min.   :  2.00     
    ##  1st Qu.: 1.700   1st Qu.:0.03600   1st Qu.: 23.00     
    ##  Median : 5.200   Median :0.04300   Median : 34.00     
    ##  Mean   : 6.391   Mean   :0.04577   Mean   : 35.31     
    ##  3rd Qu.: 9.900   3rd Qu.:0.05000   3rd Qu.: 46.00     
    ##  Max.   :65.800   Max.   :0.34600   Max.   :289.00     
    ##                                                        
    ##  total.sulfur.dioxide    density             pH          sulphates     
    ##  Min.   :  9.0        Min.   :0.9871   Min.   :2.720   Min.   :0.2200  
    ##  1st Qu.:108.0        1st Qu.:0.9917   1st Qu.:3.090   1st Qu.:0.4100  
    ##  Median :134.0        Median :0.9937   Median :3.180   Median :0.4700  
    ##  Mean   :138.4        Mean   :0.9940   Mean   :3.188   Mean   :0.4898  
    ##  3rd Qu.:167.0        3rd Qu.:0.9961   3rd Qu.:3.280   3rd Qu.:0.5500  
    ##  Max.   :440.0        Max.   :1.0390   Max.   :3.820   Max.   :1.0800  
    ##                                                                        
    ##     alcohol         quality      quality.factor
    ##  Min.   : 8.00   Min.   :3.000   3:  20        
    ##  1st Qu.: 9.50   1st Qu.:5.000   4: 163        
    ##  Median :10.40   Median :6.000   5:1457        
    ##  Mean   :10.51   Mean   :5.878   6:2198        
    ##  3rd Qu.:11.40   3rd Qu.:6.000   7: 880        
    ##  Max.   :14.20   Max.   :9.000   8: 175        
    ##                                  9:   5

<h2>
Univariate Analysis
</h2>
Section Objectives : The objective of this section is to analyse the frequency distributions of different variables in the dataset, create coherent plots plots of these distributions and then make adequate observations about the distributions.

Let' start by creating a frequency distribution of the quality.

``` r
hist_quality_factor <- ggplot(aes(x=quality.factor), data=wine_qual) +
  geom_histogram(stat='count')
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

``` r
hist_quality_factor
```

![](White_wine_quality_EDA_files/figure-markdown_github/Histogram%20of%20quality%20factor%20variable-1.png)

We can see that the distribution has a fairly normal shape with a slight right skew.

We can get a better idea of the mean (and this the skew) and median of the quality variable (since it is a integral value). For this we need to consider the integral "quality variable" in the dataset

``` r
hist_quality <- ggplot(aes(x=quality), data=wine_qual) +
  geom_histogram() +
  scale_x_continuous(breaks=seq(0,max(wine_qual$quality),1)) +
    geom_vline(aes(xintercept=mean(quality),color='mean')) +
  geom_vline(aes(xintercept=median(quality),color='median')) +
  scale_color_manual(name = 'Quality Statistics', values = c(median = 'blue', 
                                                             mean = 'red'))

hist_quality
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](White_wine_quality_EDA_files/figure-markdown_github/Histogram%20of%20quality%20variable-1.png)

<u>Observation:</u>The distribution shows a somewhat normal trend with most wines having been classified as having a quality of 6. We also notice that more wines have rating value less than the median rating value.

Next, let's create a freqfrequency distribution of the alcohol content of wines

``` r
hist_alcohol <- ggplot(aes(x=alcohol), data=wine_qual) +
  geom_histogram(color=I('orange'), fill=NA) +
  scale_x_continuous(breaks=seq(0,max(wine_qual$alcohol),0.5)) +
  geom_vline(aes(xintercept=mean(alcohol),color='mean')) +
  geom_vline(aes(xintercept=median(alcohol),color='median')) +
  scale_color_manual(name = 'Alcohol Statistics', values = c(median = 'blue', 
                                                             mean = 'red'))

hist_alcohol
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](White_wine_quality_EDA_files/figure-markdown_github/Histogram%20of%20alcohol%20content%20variable-1.png)

Presented below is the summary of the alcohol content

``` r
summary(wine_qual$alcohol)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    8.00    9.50   10.40   10.51   11.40   14.20

<u>Observation:</u>The alcohol content distribution is quite interesting. The distribution is not quite normal. .We can see two peaks in the distribution, one in the 9.0-9.5 alcohol content range and again in the 10.75 to 11.0 interval .

Let' create a freqfrequency districution of wine pH

``` r
hist_pH <- ggplot(aes(x=pH), data=wine_qual) +
  geom_histogram(color=I('orange'), fill=NA) +
  scale_x_continuous(breaks=seq(0,max(wine_qual$pH),0.1)) +
  geom_vline(aes(xintercept=mean(pH),color='mean')) +
  geom_vline(aes(xintercept=median(pH),color='median')) +
  scale_color_manual(name = 'pH Statistics', values = c(median = 'blue', 
                                                        mean = 'red'))

hist_pH
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](White_wine_quality_EDA_files/figure-markdown_github/Histogram%20of%20pH%20variable-1.png)

Presented below is the summary of the pH values

``` r
summary(wine_qual$pH)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.720   3.090   3.180   3.188   3.280   3.820

<u>Observation:</u> The wine pH variable has a substantial normal distribution shape. There are also no discernable tails in the distribution. The pH level varies from 2.72 to 3.82 and this tallies with some additional research that I did which suggests that most wines have a pH value between 2.8-3.9

Let us now analyse the frequency distribution of the residual sugar amounts in the wines.

``` r
hist_residual_sugar <- ggplot(aes(x=residual.sugar), data=wine_qual) +
  geom_histogram() +
  scale_x_continuous(breaks=seq(0,max(wine_qual$residual.sugar),1),
                     limits=c(0,quantile(wine_qual$residual.sugar,0.99))) +
  geom_vline(aes(xintercept=mean(residual.sugar),color='mean')) +
  geom_vline(aes(xintercept=median(residual.sugar),color='median')) +
  scale_color_manual(name = 'Residual Sugar Statistics', values = c(
    median = 'blue', mean = 'red'))

hist_residual_sugar
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 47 rows containing non-finite values (stat_bin).

    ## Warning: Removed 1 rows containing missing values (geom_bar).

![](White_wine_quality_EDA_files/figure-markdown_github/Histogram%20of%20residual%20sugar%20variable-1.png)

Presented below is the summary of the residual sugar content

``` r
summary(wine_qual$residual.sugar)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.600   1.700   5.200   6.391   9.900  65.800

<u>Observation:</u>The distribution for the residual sugar amount is a prime example of a long-tailed distribution. A majority of the wine samples have sugar contents in the 1-3 range.

Let us log transform (10 base) the distribution to get a better, garnular idea of potential peaks

``` r
hist_residual_sugar_log <- ggplot(aes(x=residual.sugar), data=wine_qual) +
  geom_histogram() +
  scale_x_log10(breaks=seq(0,max(wine_qual$residual.sugar),1),
                limits=c(1,quantile(wine_qual$residual.sugar,0.95))) +
  geom_vline(aes(xintercept=mean(residual.sugar),color='mean')) +
  geom_vline(aes(xintercept=median(residual.sugar),color='median')) +
  scale_color_manual(name = 'Residual Sugar Statistics', values = c(
    median = 'blue', mean = 'red')) +
  labs(x='residual sugar (log_10)')

hist_residual_sugar_log
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 317 rows containing non-finite values (stat_bin).

![](White_wine_quality_EDA_files/figure-markdown_github/Histogram%20of%20residual%20sugar%20variable%20log%20transformed-1.png)

<u>Observation:</u>

Let' create a freqfrequency districution of free and total suphur dioxide

``` r
library(gridExtra)
```

    ## Warning: package 'gridExtra' was built under R version 3.4.3

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
plot_total_so2 <- ggplot(aes(x=total.sulfur.dioxide), data=wine_qual) +
  geom_histogram() +
  scale_x_continuous(breaks=seq(0,max(wine_qual$total.sulfur.dioxide),50),
                     limits = c(0,quantile(wine_qual$total.sulfur.dioxide,0.99)
                                )) +
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide),color='mean')) +
  geom_vline(aes(xintercept=median(total.sulfur.dioxide),color='median')) +
  scale_color_manual(name = 'Total SO2 Statistics', values = c(
    median = 'blue', mean = 'red'))


plot_free_so2 <- ggplot(aes(x=free.sulfur.dioxide), data=wine_qual) +
  geom_histogram() +
  scale_x_continuous(breaks=seq(0,max(wine_qual$free.sulfur.dioxide),25),
                limits = c(0,quantile(wine_qual$free.sulfur.dioxide,0.99))) +
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide),color='mean')) +
  geom_vline(aes(xintercept=median(free.sulfur.dioxide),color='median')) +
  scale_color_manual(name = 'Free SO2 Statistics', 
                     values = c(median = 'blue', mean = 'red'))

label_plot <-   scale_color_manual(name = 'Free SO2 Statistics', values = c(
  median = 'blue', mean = 'red'))

grid.arrange(plot_total_so2,plot_free_so2, ncol=2)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 49 rows containing non-finite values (stat_bin).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 43 rows containing non-finite values (stat_bin).

![](White_wine_quality_EDA_files/figure-markdown_github/Histogram%20of%20sulphur%20dioxide%20contents-1.png)

Presented below is the summary of the total sulfur dioxide content

``` r
summary(wine_qual$total.sulfur.dioxide)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     9.0   108.0   134.0   138.4   167.0   440.0

and the free sulfur dioxide content

``` r
summary(wine_qual$free.sulfur.dioxide)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    2.00   23.00   34.00   35.31   46.00  289.00

<u>Observation:</u> While the total sulfur dioxide distribution is quite straightforward with no noticeable skew, the free sulfur dioxide distribution seems to almost bimodal with two distinct peaks at the 20 and 35 free sulfur dioxide levels. There is also a sharp and noticeable dip at the 32-34 level of the distribution.

Let us now create the distribution for chlorides in the wine samples in the dataset

``` r
hist_chlorides <- ggplot(aes(x=chlorides), data=wine_qual) +
  geom_histogram() +
  scale_x_continuous(
    limits=c(0.0,quantile(wine_qual$chlorides,0.99))) +
  geom_vline(aes(xintercept=mean(chlorides),color='mean')) +
  geom_vline(aes(xintercept=median(chlorides),color='median')) +
  scale_color_manual(name = 'Chlorides Statistics', values = c(
    median = 'blue', mean = 'red'))

hist_chlorides
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 48 rows containing non-finite values (stat_bin).

![](White_wine_quality_EDA_files/figure-markdown_github/Histogram%20of%20chlorides%20variable-1.png)

Presented below is the summary of the chlorides

``` r
summary(wine_qual$chlorides)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.00900 0.03600 0.04300 0.04577 0.05000 0.34600

<u>Observation:</u>We can see that majority of wines have a chloride level less than 0.10. The distribution is also a long tailed one with a distinctive right skew.

Another final variable of interest is the density. Let us also look into the frequency distribution for the density.

``` r
hist_density <- ggplot(aes(x=density), data=wine_qual) +
  geom_histogram() +
  scale_x_continuous(
    limits=c(0.99,quantile(wine_qual$density,0.99))) +
  geom_vline(aes(xintercept=mean(density),color='mean')) +
  geom_vline(aes(xintercept=median(density),color='median'), linetype=2) +
  scale_color_manual(name = 'Density Statistics', values = c(median = 'blue', 
                                                             mean = 'red'))

hist_density
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 394 rows containing non-finite values (stat_bin).

    ## Warning: Removed 1 rows containing missing values (geom_bar).

![](White_wine_quality_EDA_files/figure-markdown_github/Histogram%20of%20density%20variable-1.png)

Presented below is the summary of the density of the wine

``` r
summary(wine_qual$density)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.9871  0.9917  0.9937  0.9940  0.9961  1.0390

<u>Observation:</u>The density distribution shows a distinct decrease in count with increasing density although the decline is gradual rather than steep (such as one that creates a long tailed distribution)

This concludes our Univariate Analysis section. Presented below are some of the relevant questions asked and the answers thereof

<b>What are the variables of most interest in the dataset?</b> </br> Out of the variables considered in the section above, I suspect that the alcohol level of the wines, pH level of the wines, residual sugar content and density of the wines will be the most important determinants of the wine's quality. Further analysis in the following sections will either confirm of deny my suspicions. Other variables that may have a lesser impact are the chloride level and the level of free and total sulfur dioxides in the wine.

<b>Were any new/calculated variables created?</b> </br> For the purposes of the analysis I have created a new factor variable of the quality out of the continous variable of the same present in the dataset. This factor variable will be of interest in the Bivariate analysis section as well.

<b>Were there any unusual distributions? Were any transformations made to accommodate for such data?</b> </br> While creating histograms for the different continuous variables, I noticed that the residual sugar distribution was distinctly long tailed. Therefore I log transformed the distribution to get a better idea of the distribution. As a result, I was able to discern three distinctive peaks in the frequency distribution. One at the 1.5 residual sugar level, one at 7.5 and the last one at the 13.5 residual sugar level.

<h2>
Bivariate Analysis
</h2>
Section Objectives : The objective of this section is to analyse the co-relations between pairs of variables in the dataset. The pairs of variables may or may not include the dependent variable vector viz. the wine quality.

The section will also include analysis of the trends between variables and analysis based on conditional means and medians. This will give us a more hollistic idea of the trends between variables and allow us to tackle issues such as the bias/variance tradeoff.

A quick and convenient way to ascertain the co-relations between variables is to create a corelation matrix by which we can focus in on the individual relationships between the continuous variables. This also makes sense as within the dataset are continous variables and there are no categorical/factor variables present so we can expect a numerical co-relation value between pairs of variables.

``` r
data_cor <- wine_qual %>% select(-quality.factor)
corr <- round(cor(data_cor),1)
ggcorrplot(corr, hc.order=TRUE, type='lower', lab=TRUE)
```

![](White_wine_quality_EDA_files/figure-markdown_github/Co-relation%20matrix%20of%20different%20variables-1.png)

As we can see from the matrix produced there does not seem to be a very high level of co-relation between the other variables and the quality of the white wine sample. The strongest co-relation(which by itself is quite a moderate one) is between the alcohol content of the wine and its quality. This can providde us with a starting point for our investigations.

<h3>
Analysis of different variable pairs
</h3>
Lets begin the analysis by creating boxplots of all of the wine qualities against each of the independent variables. We will use our factor variable of qualities that we created in the dataset earlier.

``` r
var_names <- data.frame(names(wine_qual))

var_names_fil <- var_names %>%
  filter(!names.wine_qual. %in% c('X','quality','quality.factor'))
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.4

``` r
var_names_fil <- var_names_fil$names.wine_qual.

p=list()

for (i in var_names_fil){
  p[[i]] <- ggplot(aes_string(x='quality.factor', y=i, color='quality.factor'), 
                   data=wine_qual) +
    geom_boxplot(show.legend = FALSE)

}

do.call(grid.arrange,c(p, padding=5))
```

![](White_wine_quality_EDA_files/figure-markdown_github/Boxplots%20of%20different%20variables%20across%20quality%20values-1.png)

From the above plots we can see that for most of the variables the variations in central tendency(in this case the median) is not pronounced. However we can find a distinct trend of increasing quality with increasing alcohol content and decreasing quality with increasing density. Let's plot out the co-relation between these variables and more in greater detail.

Let's explore these two in detail.

We start with the boxplot for alcohol content vs quality

``` r
ggplot(aes(x=quality.factor, y=alcohol, color=quality.factor), data=wine_qual) +
  geom_boxplot() +
  stat_summary(fun.y=median, geom='line', aes(group=1)) +
  stat_summary(fun.y=median, geom="point")
```

![](White_wine_quality_EDA_files/figure-markdown_github/Boxplot%20of%20alcohol%20variable%20with%20quality%20factor%20variable-1.png)

We can see that after the quality factor of 5, for all higher qualities, the medians of the alcohol contents increase consistently with quality. We can also make an additional observation i.e quality factor 5 has significantly higher outliers than the other quality factor groups.

Here is a representation of the boxplot data in tabular form.

``` r
wine_qual %>%
  group_by(quality.factor) %>%
  summarise(min=min(alcohol),
            first_quantile=quantile(alcohol,0.25),
            median=median(alcohol),
            third_quartile=quantile(alcohol,0.75),
            max=max(alcohol))
```

    ## # A tibble: 7 x 6
    ##   quality.factor   min first_quantile median third_quartile   max
    ##   <fct>          <dbl>          <dbl>  <dbl>          <dbl> <dbl>
    ## 1 3               8.00           9.55  10.4            11.0  12.6
    ## 2 4               8.40           9.40  10.1            10.8  13.5
    ## 3 5               8.00           9.20   9.50           10.3  13.6
    ## 4 6               8.50           9.60  10.5            11.4  14.0
    ## 5 7               8.60          10.6   11.4            12.3  14.2
    ## 6 8               8.50          11.0   12.0            12.6  14.0
    ## 7 9              10.4           12.4   12.5            12.7  12.9

Let's consider the density boxplot now

``` r
ggplot(aes(x=quality.factor, y=density, color=quality.factor), data=wine_qual) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0.99,1.01)) +
  stat_summary(fun.y=median, geom='line', aes(group=1)) +
  stat_summary(fun.y=median, geom="point")
```

    ## Warning: Removed 348 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 348 rows containing non-finite values (stat_summary).

    ## Warning: Removed 348 rows containing non-finite values (stat_summary).

![](White_wine_quality_EDA_files/figure-markdown_github/Boxplot%20of%20density%20variable%20with%20quality%20factor%20variable-1.png)

In this plot, we see a pattern of consistently decreasing median of the density with increasing wine quality.

A tabular representation of the density boxplot data has been presented below.

``` r
wine_qual %>%
  group_by(quality.factor) %>%
  summarise(min=min(density),
            first_quantile=quantile(density,0.25),
            median=median(density),
            third_quartile=quantile(density,0.75),
            max=max(density))
```

    ## # A tibble: 7 x 6
    ##   quality.factor   min first_quantile median third_quartile   max
    ##   <fct>          <dbl>          <dbl>  <dbl>          <dbl> <dbl>
    ## 1 3              0.991          0.993  0.994          0.997 1.00 
    ## 2 4              0.989          0.993  0.994          0.996 1.00 
    ## 3 5              0.987          0.993  0.995          0.997 1.00 
    ## 4 6              0.988          0.992  0.994          0.996 1.04 
    ## 5 7              0.987          0.991  0.992          0.994 1.00 
    ## 6 8              0.987          0.990  0.992          0.993 1.00 
    ## 7 9              0.990          0.990  0.990          0.991 0.997

Now let us start comparing individual datapoints in the dataset and discern possible co-relations in between pairs of variables.

Let us begin with the quality vs alcohol content variable pair. Provided below is a plot (jitter being applied to deal with overplotting) between alcohol content and quality of the wine.

``` r
ggplot(aes(x=alcohol, y=quality), data=wine_qual) + 
  geom_jitter(alpha=0.05) +
  scale_x_continuous(breaks=seq(0,max(wine_qual$alcohol),1)) +
  scale_y_continuous(breaks=seq(0,max(wine_qual$quality),1)) +
  stat_smooth(method='lm')
```

![](White_wine_quality_EDA_files/figure-markdown_github/Alcohol%20content%20vs%20quality-1.png)

By itself, the above plot is not too insightful. The linear trend line shows us a feeble positive co-relation which doesn't say anything we don't already know.

Maybe we could aggreagate over the alcohol content to obtain conditional mean and median qualities. A similar trend would only reaffirm our suspicions.

Before we do so we need to perform an analysis of the unique alcohol values so as to not obtain a graph with high degrees of variance.

Lets find out the number unique values of alcohol content and their distribution

``` r
length(unique(wine_qual$alcohol))  
```

    ## [1] 103

``` r
table(wine_qual$alcohol)
```

    ## 
    ##                8              8.4              8.5              8.6 
    ##                2                3                9               23 
    ##              8.7              8.8              8.9                9 
    ##               78              107               95              185 
    ##              9.1              9.2              9.3              9.4 
    ##              144              199              134              229 
    ##              9.5 9.53333333333333             9.55              9.6 
    ##              228                3                2              128 
    ## 9.63333333333333              9.7 9.73333333333333             9.75 
    ##                1              105                2                1 
    ##              9.8              9.9               10 10.0333333333333 
    ##              136              109              162                1 
    ##             10.1 10.1333333333333            10.15             10.2 
    ##              114                2                3              130 
    ##             10.3             10.4 10.4666666666667             10.5 
    ##               85              153                2              160 
    ## 10.5333333333333            10.55 10.5666666666667             10.6 
    ##                1                2                1              114 
    ##            10.65             10.7             10.8             10.9 
    ##                1               96              135               88 
    ## 10.9333333333333 10.9666666666667            10.98               11 
    ##                2                3                1              158 
    ##            11.05 11.0666666666667             11.1             11.2 
    ##                2                1               83              112 
    ## 11.2666666666667             11.3 11.3333333333333            11.35 
    ##                1              101                3                1 
    ## 11.3666666666667             11.4 11.4333333333333            11.45 
    ##                1              121                1                4 
    ## 11.4666666666667             11.5            11.55             11.6 
    ##                1               88                1               46 
    ## 11.6333333333333            11.65             11.7 11.7333333333333 
    ##                2                1               58                1 
    ##            11.75             11.8            11.85             11.9 
    ##                2               60                1               53 
    ##            11.94            11.95               12            12.05 
    ##                2                1              102                1 
    ## 12.0666666666667             12.1            12.15             12.2 
    ##                1               51                2               86 
    ##            12.25             12.3 12.3333333333333             12.4 
    ##                1               62                1               68 
    ##             12.5             12.6             12.7            12.75 
    ##               83               63               56                3 
    ##             12.8 12.8933333333333             12.9               13 
    ##               54                2               39               36 
    ##            13.05             13.1 13.1333333333333             13.2 
    ##                1               18                1               14 
    ##             13.3             13.4             13.5            13.55 
    ##                7               20               12                1 
    ##             13.6             13.7             13.8             13.9 
    ##                9                7                2                3 
    ##               14            14.05             14.2 
    ##                5                1                1

With a 103 unique values the plot should not lend itself to high degrees of overplotting. Let's use the dplyr package to aggregate over the alcohol content and find mean and median quality values for each inviduidual alcohol content value.

``` r
library(dplyr)
wine_qual.grp_by_alcohol <- wine_qual %>%
  group_by(alcohol) %>%
  summarise(mean_quality=mean(quality),
            median_quality=median(quality),
            variance_quality=var(quality),
            n=n()) %>%
  arrange(alcohol)
```

Now let us create the plot of the conditional means of the wine quality.

``` r
ggplot(aes(x=alcohol, y=mean_quality), data=wine_qual.grp_by_alcohol) +
  geom_line() +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'loess'

![](White_wine_quality_EDA_files/figure-markdown_github/Plot%20of%20conditional%20means%20of%20Quality%20with%20alcohol%20content-1.png)

Let us also perform a co-relation test between the alcohol content groups and the graph for the conditional means of quality

``` r
cor.test(x=wine_qual.grp_by_alcohol$alcohol, 
         y=wine_qual.grp_by_alcohol$mean_quality)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  wine_qual.grp_by_alcohol$alcohol and wine_qual.grp_by_alcohol$mean_quality
    ## t = 9.567, df = 101, p-value = 8.073e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.5723357 0.7790630
    ## sample estimates:
    ##       cor 
    ## 0.6894915

The co-relation co-efficient shows us a greater degree of co-relation than for the individual alcohol vs quality plot.

We can also plot for the conditional variance. But we have to be careful here. For alcohol contents with only one observation the variance is calculated as NA. We should disregard these values before devising our plot. Even after the exclusion of these values we can have a distinct trend of the conditional variance of quality with respect to alcoohol content.

``` r
ggplot(aes(x=alcohol, y=variance_quality), 
       data=subset(wine_qual.grp_by_alcohol, !is.na(variance_quality))) +
  geom_line()+
  geom_smooth()
```

    ## `geom_smooth()` using method = 'loess'

![](White_wine_quality_EDA_files/figure-markdown_github/Plot%20of%20conditional%20variance%20of%20Quality%20with%20alcohol%20content-1.png)

From the plot above we can deduce that wines with high alcohol contents have relatively lower variations among their quality values. The disparity is especially visible in wines with alcohol content less that 10 and thereafter the dispersion is not as pronounced.

A similar co-relation that may be explored is between wine density and quality. Let us begin the same way we did with alcohol vs quality and create a basic plot. Let us add jitter and opacity to deal with overplotting.

``` r
ggplot(aes(x=density, y=quality), data=wine_qual) + 
  geom_point(position='jitter',alpha=0.05) +
  scale_x_continuous(breaks=seq(0,max(wine_qual$density),0.005),
                     limits=c(0.985,1.005)) +
  scale_y_continuous(breaks=seq(0,max(wine_qual$quality),1)) +
  stat_smooth(method='lm')
```

    ## Warning: Removed 3 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 3 rows containing missing values (geom_point).

![](White_wine_quality_EDA_files/figure-markdown_github/Density%20vs%20quality-1.png)

Another pair of variables that would be interesting for analysis is the alcohol vs density. These two variables have a strong negetive co-relation between them and density has the second best co-relation with quality among the other variables. It would be interesting to see the and analyse the co-relation between these two variables. Later, as part of our multivariate analysis we can consider including alcohol, denisty and quality and observe their inter-relationships.

``` r
ggplot(aes(x=alcohol, y=density), data=wine_qual) +
  geom_point()
```

![](White_wine_quality_EDA_files/figure-markdown_github/Plot%20of%20Alcohol%20content%20vs%20density-1.png)

The plot definitely suffers from overplotting. Let us rectify that by adding jitter and opacity to the datapoints. Also, since we have very few values of density greater than 1.01, a few adjustments to the y-axis will create a more robust plot.

``` r
ggplot(aes(x=alcohol, y=density), data=wine_qual) +
  geom_point(position='jitter', alpha=0.05) +
  scale_y_continuous(breaks=seq(0.99,1.01,0.005),
                     limits=c(0.99,1.01)) +
  geom_line(stat='summary', fun.y=mean, color='red') +
  geom_smooth(size=2)
```

    ## Warning: Removed 348 rows containing non-finite values (stat_summary).

    ## `geom_smooth()` using method = 'gam'

    ## Warning: Removed 348 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 364 rows containing missing values (geom_point).

![](White_wine_quality_EDA_files/figure-markdown_github/Refined%20plot%20of%20Alcohol%20content%20vs%20density-1.png)

From the plot we can distinctively notice the negative co-relation between the density and alcohol content of the wines. The plot further shows us that majority of the datapoints are confined with alcohol contents less than roughly 11.5.

Anoher feature that has a high co-relation co-efficient(in magnitude) with the density is the residual sugar content. This co-relation can also be the basis of any further co-relation between residual sugar and alcohol content. Let us first look at density and residual sugar.

``` r
ggplot(aes(x=density, y=residual.sugar), data=wine_qual) +
  geom_point()
```

![](White_wine_quality_EDA_files/figure-markdown_github/Plot%20of%20Density%20vs%20residual%20sugar%20content-1.png)

Just as in the plot for density and alcohol, changes for accomodating overplotting and axis modifications need to be made for creating a clearer plot.

``` r
ggplot(aes(x=density, y=residual.sugar), data=wine_qual) +
  geom_point(position='jitter', alpha=0.1, color='orange') +
  scale_x_continuous(limits=c(0.99,1.005)) +
  scale_y_continuous(limits=c(0,25)) +
  geom_smooth(span=10) +
  geom_rect(xmin=0.990, xmax=0.995, ymin=0, ymax=2.5, fill=NA, color='black')
```

    ## `geom_smooth()` using method = 'gam'

    ## Warning: Removed 350 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 359 rows containing missing values (geom_point).

![](White_wine_quality_EDA_files/figure-markdown_github/Refined%20plot%20of%20Density%20vs%20residual%20sugar%20content-1.png)

The plot and the smoothened curve gives us the high positive co-relation we were expecting. There is however one interesting observation. There is a significant clutter of datapoints with density less than 0.995 (highlighted in plot) that do not have a strong positive co-relation. These points seem to maintain a constant residual sugar value with increasing density. It would be interesting to analyse the cause of the behaviour of these points and whether any other variables are responsible for this effect. We can take up this analysis in the multivariate analysis section.

A final analysis is between the variables free sulfur dioxide and total sulfur dioxide. The variables obviously have a positive co-relation since total sulfur dioxide will obviously increase with increased free sulfur dioxide.

Lets create one such plot and discuss its implications.

``` r
ggplot(aes(x=free.sulfur.dioxide, y=total.sulfur.dioxide), data=wine_qual) +
  geom_line(color='yellow',position=position_jitter(w=0.01, h=0)) +
  stat_smooth(method='lm', span=20)
```

![](White_wine_quality_EDA_files/figure-markdown_github/Total%20sulfur%20dioxide%20vs%20Free%20sulfur%20dioxide-1.png)

The co-relation is unmistakable and it should be so. In fact after a free\_sulfur\_dioxide content of ~140, we see an almost linear trend. However, this co-relation may be of consequence when trying to create predictive models based on multiple linear regression methods. The level of multicollinearity between these two variables may create some level of unpredictability in the models and therefore it would be best to avoid one of these variables when creating such models.

Another such inferable relationship should be between the different acidities and the pH value. A higher acidity does suggest a lower pH value and we actually see such a trend for all of the acidities and pH level. The strongest co-relations seems to be between the fixed acidity level and the pH. Let us consider this relationship through a plot

``` r
ggplot(aes(x=pH, y=fixed.acidity), data=wine_qual) +
  geom_point(position='jitter',alpha=0.1, color='orange')+
  geom_smooth(color='blue', linetype=2)
```

    ## `geom_smooth()` using method = 'gam'

![](White_wine_quality_EDA_files/figure-markdown_github/Fixed%20acidity%20vs%20pH-1.png)

As expected we do notice a negative co-relation. We must be careful to disregard the region created by the smoothened trned line outside the data point space. Even so we do notice our speculated co-relation.

This concludes our Bivariate Analysis section. Presented below are some of the relevant questions asked and the answers thereof

<b>What are the co-relations/relationships that are observed between different pairs of variables in the dataset?</b> </br> There are quite a few analyses that have been performed in the bivariate analysis section. They are listed below
<ol>
<li>
With respect to the dependent variable viz. the wine quality, the variable that has the most discernable co-relation is the alcohol content of the wine. The boxplot reveals that with increasing wine quality from rating 5, the median of the alcohol content also increases. Below quality level 5 however, the co-relation is a negative one. But for most wines, the quality of the wine is directly co-related with the alcohol content.
</li>
<li>
We also see that with increasing wine quality, the density of the wine decreases. This co-relation affirms that better wines are more fluid.
</li>
<li>
The co-relation between alcohol content and density is also quite strong and negative. More alcoholic wines are less dense throughout the dataset
</li>
<li>
The distribution between density and residual sugar is also quite marked. The c0-relations shows that sweeter wines have a higher density. There is a segment of wines with lower densities have more or less constant residual sugar amounts with increasing density. We will take up this co-relation in the multivariate analysis section
</li>
<li>
We also explored the co-relation between the free and total sulfur dioxide contents and found out that, as expected, total sulfur dioxide increases with free sulfur dioxide content, indicating a strong positive co-relation.
</li>
</ol>
<b>What variable has the strongest impact on the dependent variable? What are some other strong relationships that exclude the dependent variable</b> </br> From the analysis above, we can discern that the alcohol level has the strongest co-relation with the wine quality level.

Other strong co-relations included the ones between alcohol content and density, between residual sugar content and density, between total sulfur dioxide and free sulfur dioxide and between fixed acidity and pH level of wines.

<h2>
Multivariate Analysis
</h2>
Section Objectives : In this section we intend to advance our investigation into the variables and consider the relationships thath exist between more than two variables at a time. This will give us a better understanding of the ways in which the variables relate to each other and influence each other.

To begin with we can take up the triplet of quality, alcohol content and density. Let us create a scatterplot and observe the co-relation between the three. Also we will include points whose density values are less than the 99.9th percentile for density values. That way, we would certainly not be excluding any significant number of points and would also be able to create a discernable contrast of colors to highlight the density variation.

``` r
library(viridis)
```

    ## Warning: package 'viridis' was built under R version 3.4.4

    ## Loading required package: viridisLite

    ## Warning: package 'viridisLite' was built under R version 3.4.3

``` r
ggplot(aes(x=alcohol, y=quality), 
       data=subset(wine_qual,density<=quantile(density,0.999))) +
  geom_point(aes(colour=density), position='jitter') +
  geom_smooth(color='red') +
  scale_color_viridis(discrete=FALSE, option='magma')
```

    ## `geom_smooth()` using method = 'gam'

![](White_wine_quality_EDA_files/figure-markdown_github/Alcohol%20contents%20vs%20quality%20vs%20density-1.png)

From the trend line in this plot we can see that wines with greater alcohol content generally have higher quality rating(as seen from the trend line) whereas they have lower density.

Further let us perform the same analysis for the quality, alcohol and residual sugar amounts. We can expect to see a similar trend. We will also consider values of residual sugar with values less than the 99.9th percentile for reasons stated in the density plot above.

``` r
ggplot(aes(x=alcohol, y=quality, color=residual.sugar), data=subset(wine_qual, residual.sugar<=quantile(residual.sugar,0.999))) +
  geom_point(position='jitter') +
  geom_smooth(color='blue') +
  scale_color_viridis(discrete=FALSE, option='plasma')
```

    ## `geom_smooth()` using method = 'gam'

![](White_wine_quality_EDA_files/figure-markdown_github/Alcohol%20contents%20vs%20quality%20vs%20residual%20sugar-1.png)

Inaddition to the already known positive co-relation between alcohol content and quality we also see that that wines with greater alcohol content have lesser levels of residual sugar and can thus be assume to be less sweet.

Let us now consider the three variables of alcohol, density and residual sugar.

``` r
ggplot(aes(x=alcohol, y=density, color=residual.sugar), 
       data=subset(wine_qual, residual.sugar<=quantile(residual.sugar,0.999))) +
  geom_point(position='jitter') +
  scale_y_continuous(limits=c(0.99,1.01)) +
  geom_smooth(color='blue', alpha=0.05) +
  scale_color_gradientn(colours = heat.colors(10))
```

    ## `geom_smooth()` using method = 'gam'

    ## Warning: Removed 345 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 355 rows containing missing values (geom_point).

![](White_wine_quality_EDA_files/figure-markdown_github/Alcohol%20contents%20vs%20residual%20sugar%20vs%20density-1.png)

The plot cleary shows what we already know. Sweeter wines have higher densities but lower alcohol contents.

We should also check to see if this co-relation between density and residual sugar holds for each individual quality level.

``` r
ggplot(aes(x=quality.factor, y=density, color=residual.sugar), data=wine_qual) +
  geom_point(position='jitter') +
  scale_y_continuous(limits=c(0.99,1.01)) +
  geom_smooth(color='blue', alpha=0.05) +
  scale_color_gradientn(colours = rainbow(10))
```

    ## `geom_smooth()` using method = 'gam'

    ## Warning: Removed 348 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 365 rows containing missing values (geom_point).

![](White_wine_quality_EDA_files/figure-markdown_github/Density%20vs%20quality%20vs%20density-1.png)

Except for the quality levels of 3 and 9 (where there are too few datapoints to make concrete conclusions), we can see that indeed with increasing density residual sugar levels in wines do tend to increase.

It would be good to have a greater level of granularity for this plot for each quality level.

``` r
ggplot(aes(x=alcohol, y=density, color=residual.sugar), data=subset(wine_qual, residual.sugar<=quantile(residual.sugar,0.999))) +
  geom_point(position='jitter') +
  geom_smooth(color='red') +
  facet_wrap(~quality.factor) +
  scale_color_viridis(discrete=FALSE, option='magma')
```

    ## `geom_smooth()` using method = 'gam'

    ## Warning: Computation failed in `stat_smooth()`:
    ## x has insufficient unique values to support 10 knots: reduce k.

![](White_wine_quality_EDA_files/figure-markdown_github/Alcohol%20content%20vs%20density%20vs%20residual%20sugar%20across%20different%20quality-1.png)

The quality levels of 3 and 9 hardly have enough data points for consideration. For the other quality levels we observe see that the number of wines with higher residual sugar contents decreases with increasing quality. We also observer that for a particular quality level, residual sugar level decreases with increasing alcohol content. These observations supplement the infomation we have obtained from the previous plot.

We finish this section with an analysis of residual sugar vs density that we took up in the bivariate analysis section. We saw that for lower densities residual sugar did not have a significant positive co-relation with density. To figure out the cause we should look into two variables in particular viz the total\_sulfur\_dioxide and the alcohol content in wines. We plot the graphs in question.

``` r
res_sugar_density_so2 <- ggplot(aes(x=density, y=residual.sugar, 
                                color=total.sulfur.dioxide), data=wine_qual) +
  geom_point(position='jitter', alpha=0.1) +
  scale_x_continuous(limits=c(0.99,1.005)) +
  scale_y_continuous(limits=c(0,25)) +
  geom_smooth(span=10) +
  scale_color_gradientn(colors=terrain.colors(100)) +
  geom_rect(xmin=0.990, xmax=0.995, ymin=0, ymax=2.5, fill=NA, color='black')

res_sugar_density_alcohol <- ggplot(aes(x=density, y=residual.sugar, 
                                        color=alcohol), data=wine_qual) +
  geom_point(position='jitter', alpha=0.1) +
  scale_x_continuous(limits=c(0.99,1.005)) +
  scale_y_continuous(limits=c(0,25)) +
  geom_smooth(span=10) +
  scale_color_gradientn(colors=heat.colors(100)) +
  geom_rect(xmin=0.990, xmax=0.995, ymin=0, ymax=2.5, fill=NA, color='black')

grid.arrange(res_sugar_density_so2,res_sugar_density_alcohol, nrow=2)
```

    ## `geom_smooth()` using method = 'gam'

    ## Warning: Removed 350 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 368 rows containing missing values (geom_point).

    ## `geom_smooth()` using method = 'gam'

    ## Warning: Removed 350 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 367 rows containing missing values (geom_point).

![](White_wine_quality_EDA_files/figure-markdown_github/Density%20vs%20residual%20sugar%20with%20alcohol%20and%20total%20sulfur%20dioxide-1.png)

Now both alcohol and total sulfur dioxide have negative co-relations with residual sugar quantity for wines with lower density. It is thus possible that a combination of both higher alcohol volume and lower total sulfur dioxide levels for wines with lower densities gives rise to the lessned residual sugar amounts. Although it would be incorrect to assume that any one variable is more significant than the other.

This concludes our Multivariate Analysis section. Presented below is the summary of this section.

<b>What are the co-relations/relationships that are observed between different variables in this multivariate analysis section?</b> </br> In this section we have dealt with the relationships among multiple variables. Observed co-relations are as below
<ol>
<li>
We see that quality of wines, intially drops a tad with increasing alcohol content but for the majority of wines in the dataset, quality increases with increasing alcohol content. At the same time we see a distinct drop in density for wines that have more alcohol.
</li>
<li>
We also see a similar pattern for the residual sugar content of wines. With increasing alcohol content the sweetness of wines falls.
</li>
<li>
From the third and fourth plots, we are able to obtain certain additional information in addition to what has been detailed above. It is that for a particular alcohol content the residual sugar content tends to increase with density of the wine. The same is also true for a particular quality level of wine.
</li>
<li>
We can also discern a co-relation between total sulfur dioxide content and density. we see that sulfur dioxide contents of wines tend to increase with density.
</li>
<li>
An important onservation is that for a certain region of ower density wines , residual sugar content tends to remain constant with increasing density. This <b>may be</b> due to the effect of lower sulfur dioxide content and higher alcohol contents in these wines but we really cannot conclude any concrete causation.
</li>
</ol>
<h2>
Final Plots and Summary
<h2>
<h3>
Plot One
</h3>
``` r
ggplot(aes(x=residual.sugar), data=wine_qual) +
  geom_histogram() +
  scale_x_log10(breaks=seq(0,max(wine_qual$residual.sugar),1),
                limits=c(1,quantile(wine_qual$residual.sugar,0.95))) +
  labs(x='Residual Sugar Amount', y='Number of Wines', 
       title='Log_10 Residual Sugar')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 317 rows containing non-finite values (stat_bin).

![](White_wine_quality_EDA_files/figure-markdown_github/Histogram%20of%20residual%20sugar%20variable%20log%20transformed%20final-1.png)

<h3>
Description for Plot One
</h3>
The right-skewed residual sugar histogram has been log transformed(base 10) to get a better idea of the frequency. Thus we caan spot three distonctive peaks in the distribution at residual sugar levels of 1.5 7.5 and 13.5.

<h3>
Plot Two
</h3>
``` r
bp1 <- ggplot(aes(x=quality.factor, y=alcohol, color=quality.factor), 
              data=wine_qual) +
  geom_boxplot() +
  stat_summary(fun.y=median, geom='line', aes(group=1)) +
  stat_summary(fun.y=median, geom="point") +
  labs(x='Quality', 
       y='Alcohol Content', 
       title='Alcohol Content distribution by quality level')


bp2 <- ggplot(aes(x=alcohol, y=mean_quality), data=wine_qual.grp_by_alcohol) +
  geom_line() +
  geom_smooth() +
  labs(x='Alcohol Content', y='Mean of Quality level', 
       title='Conditional Mean of wine quality by alcohol level')

grid.arrange(bp1, bp2, ncol=1)
```

    ## `geom_smooth()` using method = 'loess'

![](White_wine_quality_EDA_files/figure-markdown_github/Alcohol%20content%20vs%20quality%20final-1.png)

<h3>
Description for Plot Two
</h3>
The plots above work in conjunnction to display the fact that increasing alcohol content is linked to higher wine quality. The first boxplot shows the increasing trend of median alcohol content with increasing wine quality. The second plot also bears testimony to this relationship. With increasing alcohol levels the conditional means of quality levels also increases.

<h3>
Plot Three
</h3>
``` r
ggplot(aes(x=alcohol, y=density, color=residual.sugar), 
       data=subset(wine_qual, residual.sugar<=quantile(residual.sugar,0.999))) +
  geom_point(position='jitter') +
  scale_y_continuous(limits=c(0.99,1.01)) +
  geom_smooth(color='blue', alpha=0.05) +
  scale_color_gradientn(colours = heat.colors(10), 
                        name='Residual Sugar Content') +
  labs(x='Alcohol Content', y='Density', 
       title='Density vs Alcohol Content and Residual Sugar')
```

    ## `geom_smooth()` using method = 'gam'

    ## Warning: Removed 345 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 359 rows containing missing values (geom_point).

![](White_wine_quality_EDA_files/figure-markdown_github/Alcohol%20contents%20vs%20residual%20sugar%20vs%20density%20final-1.png)

<h3>
Description for Plot Three
</h3>
The plot reveals that wines with a higher alcohol content typically have lower densities. The plot also shows that with increasing density the sweetness of wines (represented by residual sugar amount) also increases. We can summarily deduce that for a greater alcohol content a greater proportion of wine samples will be less sweet and will be less dense.

<h2>
Reflection
</h2>
The white wine dataset contains 4898 observations on different chemical properties of white wine that act as a factor to its overall quality. A noticeable feature of the dataset is the absence of any categorical variables.I started the exploratory data analysis by considering individual variables and their frequency distributions and then took the analysis further by considering the relationships between pairs of and even more than two of these variables and touched on interesting questions pertaining to the data.

On detailed analysis of the dataset, I came to realise that alcohol content plays the most crucial role in determing wine quality. The alcohol content also strongly co-relates with the density of the wine and a higher alcohol content usually means a less dense wine. The specific gravity of ethanol(0.791) is less than that of water and it makes sense that a larger alcohol (ethanol) content would reduce the overall density of the wine. Likewise an increase in the sugar content of the wine also adds to its density and thus the two are positively co-related. This co-relation may also be influenced by the level of total sulfur dioxide and alcohol in the wine and such an effect has been addressed in the appropriate section of the multivariate analysis. Also I noticed certain common sense relationshis such as the one between total sulfur dioxide and free sulfur dioxide as well as between fixed acidity levels and pH levels.

The analysis is not without its limitations. All relationships among variables are only co-relations and no causative effects can be inferred for any particular trend in any of the variables and their relationships.Additionally I would have liked to analyse the effect of non checmical properties such as vineyard and period of fermentation on the wine quality as they may have significant effects on the quality. I would really like, in a subsequent analysis, to acquire and consider this data for a more rounded analysis.
