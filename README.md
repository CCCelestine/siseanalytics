
## R Markdown

This is an R Markdown document that generates a github readme.md file.

``` r
summary(iris)
```

    ##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
    ##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
    ##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
    ##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
    ##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
    ##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
    ##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
    ##        Species  
    ##  setosa    :50  
    ##  versicolor:50  
    ##  virginica :50  
    ##                 
    ##                 
    ## 

# siseanalytics

siseanalytics is a package to characterize the classes obtained after a
clustering. It makes it possible to characterize the classes in a
univariate way, the variables are taken individually. But also
multivariate. In addition, the pacakge allows to have the evaluation
measures.

In this tutorial we will see the concrete functionality of the package

  - Installation
  - Evaluation metrics
      - Object creation and display
      - Performance indicators
      - Comparaison of clustering results
  - Univariate characterization
  - Multivariate characterization

## Installation

In your script, install the package using these command lines :

``` r
#install.packages("devtools")
library(devtools)
```

    ## Loading required package: usethis

``` r
#install_github("CCCelestine/siseanalytics")
library(siseanalytics)
```

    ## 
    ## Attaching package: 'siseanalytics'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     boxplot

``` r
data(df_test)
```

## Univariate characterization

### Characterization of the partition

In this part, we assess how each variable contributes to the
constitution of the partition.  
First of all, we will see the case of **qualitative variables**.

``` r
barplotYX(df_test, "sexe", "val_pred")
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
barplotXY(df_test, "sexe", "val_pred")
```

![](README_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
khi2(df_test$sexe,df_test$val_pred)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  tableau
    ## X-squared = 427.85, df = 4, p-value < 2.2e-16

``` r
vcramer(df_test$sexe,df_test$val_pred)
```

    ## [1] 0.774098

Next, we will approach the case of **quantitative variables**.

``` r
boxplot(df_test, "val_pred", "taille")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#tab.quanti(df_test,val_pred)
```

### Characterization of the clusters

We will now compare the clusters with each other.

``` r
#resCluster(df_test,val_pred,"grand")
```

## Evaluation metrics

### Creating the metrics object

We create 3 objects using the EvalMetrics function. This function takes
as input dataframe containing the real values and the values
predicted by a clustering.

``` r
#Obj2c <- EvalMetrics(val_reel_22,val_pred_22)
#Obj3c <- EvalMetrics(val_reel_32,val_pred_32)
#Obj2c_bis <- EvalMetrics(val_reel_22,val_pred_bis)
```

ggMatconf function display the confusion matrix under a ggplot graph

``` r
#ggMatConf(Obj2c)
#ggMatConf(Obj3c)
```

Thanks to the overload of the print method we can display the different
attributes of our object.

``` r
#print(Obj2c)
#print(Obj3c)

#[1] "Matrice de confusion et indincateurs"
#       valreel
# valpred   -   +
#      - 175   4
#      +   3 174
#                      Valeur
# Erreur            0.01966292
# Accuracy          0.98033708
# Précision         0.97765363
# Sensibilite       0.98314607
# Specificity       0.97752809
# Balanced Accuracy 0.98033708
# F1                0.98039216
```

### Performance indicators

The package allows to calculate different indicators to evaluate the
results of clustering.

| Indicators        | Details                                                       |
| ----------------- | ------------------------------------------------------------- |
| Erreur            | Overall model performance                                     |
| Accuracy          | Overall model performance (1- erreur)                         |
| Précision         | How accurate positive predictions are                         |
| Sensibilite       | Truly positive observation coverage                           |
| Specificity       | Coverage of truly negative observations                       |
| Balanced Accuracy | Overall performance of the model, when classes are unbalanced |
| F1 score          | Hybrid indicator used for unbalanced classes                  |

### Comparaison of clustering results

Finally in this clustering evaluation part, the pacakge allows you to
compare two clustering results stored in two different objects

``` r
#compareRes(Obj2c,Obj2c_bis)
```

The output will be a ggplot chart confronting the indicators
