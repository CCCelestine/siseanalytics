
# siseanalytics

siseanalytics is a package to characterize the classes obtained after a
clustering. It makes it possible to characterize the classes in a
univariate way, the variables are taken individually. But also
multivariate. In addition, the pacakge allows to have the evaluation
measures.

In this tutorial we will see the concrete functionality of the package

  - Installation
  - Univariate characterization
      - Characterization of the partition
      - Characterization of clusters
  - Multivariate characterization
      - ANOVA
      - ACP
      - LDA
  - Evaluation metrics
      - Object creation and display
      - Performance indicators
      - Comparaison of clustering results

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

    ## Warning: replacing previous import 'MASS::select' by 'dplyr::select' when
    ## loading 'siseanalytics'

    ## 
    ## Attaching package: 'siseanalytics'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     boxplot

Access to datasets :

``` r
data(df_test) # data frame test 357 rows and 8 variables
data(pred_reel_3c) # predicted and real values 3 class 
data(pred_reel_2c) # predicted and real values 2 class 
data(pred_reel_2c_bis) # predicted and real values 2 class 
data(fromage) #dataframe test with quantitatives variables and clusters variable
```

## Univariate characterization

### Characterization of the partition

In this part, we assess how each variable contributes to the
constitution of the partition.  
First of all, we will see the case of **qualitative variables**.  
Barplot is a graph that shows categorical variables with rectangular
bars with heights or lengths proportional to the values they represent.
We can see in this first graph how the numbers of our categorical
variable (here the sex) are distributed among the clusters (here small /
medium / large).

``` r
barplotYX(df_test, "sexe", "val_pred")
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> In this
second barplot, we can see how the numbers of clusters are distributed
among the categories of the categorical variable.

``` r
barplotXY(df_test, "sexe", "val_pred")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Since we are in the presence of two categorical variables (the cluster
variable is one), we can create a contingency table between both. This
table provides a first approach to the composition of clusters.
tab.quanti.line() and tab.quanti.col() functions are used to display the
contingency tables in percentage. The calculation of the proportions is
made either in rows or in columns.

``` r
tab.quali.ligne(df_test$val_pred,df_test$sexe)
```

    ##            grand     moyen      petit Total Effectif
    ## femme 0.02247191 0.3370787 0.64044944     1      178
    ## homme 0.64804469 0.3296089 0.02234637     1      179
    ## Sum   0.33613445 0.3333333 0.33053221     1      357

``` r
tab.quali.col(df_test$val_pred,df_test$sexe)
```

    ##                 grand       moyen        petit         Sum
    ## femme      0.03333333   0.5042017   0.96610169   0.4985994
    ## homme      0.96666667   0.4957983   0.03389831   0.5014006
    ## Total      1.00000000   1.0000000   1.00000000   1.0000000
    ## Effectif 120.00000000 119.0000000 118.00000000 357.0000000

We can also perform a Chi-Square Independence Test as well as calculate
Cramer’s V, which is a measure derived from Chi-Square. These results
allow us to know if the two variables are related. Regarding Chi-square,
the p-value must be less than alpha (often 5%) to be able to affirm that
the variables are dependent. As for Cramer’s V, 0 means no liaison and 1
means perfect liaison.

``` r
khi2(df_test$sexe,df_test$val_pred)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  tableau
    ## X-squared = 207.08, df = 2, p-value < 2.2e-16

``` r
vcramer(df_test$sexe,df_test$val_pred)
```

    ## [1] 0.7616195

Next, we will approach the case of **quantitative variables**.

The boxplot summarizes indicators of the position of the studied
variable (median, quartiles, minimum, maximum or deciles). This plot is
mainly used to compare the same variable in several populations. Here we
will compare the size of people in our three clusters.

``` r
boxplot(df_test, "val_pred", "taille")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

In this table, we find the means by variable and by cluster, called the
conditional means. The eta indicator indicates the proportion of
variance explained by the clusters.

``` r
df_test=as.data.frame(df_test)
data_quanti=df_test[,3:5]
tab.quanti(data_quanti,df_test$val_pred)
```

    ##              grand     moyen     petit      eta
    ## taille   192.55833 170.22689 150.86441 88.14153
    ## poids     84.70833  66.85714  46.31356 65.87357
    ## pointure  46.88333  39.47899  36.47458 72.29326

### Characterization of the clusters

We will now compare the clusters with each other.

The resCluster () function displays a summary of the different
indicators for a cluster. First, there is the proportion of individuals
who are part of this cluster among all individuals. Then, there are two
tables : one for the quantitative variables and one for the qualitative
variables. The first compares the mean of a variable for a cluster and
the overall mean, while the other compares the proportion of a variable
in a cluster and the proportion in the overall population. Both tables
have another indicator : the test value. It makes it possible to
distinguish the important variables in the interpretation of clusters.

``` r
data=as.data.frame(df_test[,-c(1,2)])
resCluster(data,df_test$val_pred,"grand")
```

    ## [1] "Caracterisation du cluster k = grand"
    ## [1] "33.61 % de la population"
    ## [1] "Variables quantitatives"
    ##          test_value     group   overall
    ## poids     -8.135136  84.70833  66.06723
    ## pointure -65.319893  46.88333  40.97479
    ## taille   -89.908924 192.55833 171.33333
    ## [1] "Variables qualitatives"
    ##                  test_value   group % overall %
    ## val_parent$grand  13.986667 37.815126 88.333333
    ## sexe$homme        12.493225 50.140056 96.666667
    ## sport$volley       7.735522  7.843137 23.333333
    ## sport$basket       6.795126  6.162465 18.333333
    ## sport$foot         6.739522 33.053221 56.666667
    ## val_parent$moyen  -4.817785 27.731092 11.666667
    ## sport$tennis      -5.175690 15.686275  1.666667
    ## val_parent$petit  -9.733882 34.453782  0.000000
    ## sport$gym        -10.345304 37.254902  0.000000
    ## sexe$femme       -12.493225 49.859944  3.333333

Beyond the calculated value of the VT, we will focus on the disparities
and concomitances between clusters. This can be visualized on a radar
diagram for quantitative variables.

``` r
radar(data_quanti,df_test$val_pred)
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Multivariate characterization

This part is devoted to the multivariate characterization of
post-clustering classes. Three analyzes are applicable: ANOVA, PCA and
ADL. For all of these three functions, 2 variables are taken as inputs,
the first, X, comprises a data frame of the various explanatory
variables. And the second, y, the column containing the value of the
previously generated classes.

### ANOVA

First of all, we need to set up the entrance values

``` r
X=data.frame(fromage["calories"],fromage["magnesium"],fromage["lipides"],fromage["retinol"])
y=fromage$groupes.cah
```

The AOV2 () function is available to allow the user to perform a
multifactorial ANOVA in order to observe or not a significant difference
between the means. Here is an example of use:

``` r
anova2<-AOV2(X,y)
```

    ## VariableExpl1 =  calories 
    ## VariableExpl2 =  magnesium 
    ## VariableExpl3 =  lipides 
    ## VariableExpl4 =  retinol 
    ## VariableCible =  y

``` r
anova2
```

    ## Call:
    ##    aov(formula = VariableCible ~ VariableExpl1 * VariableExpl2 * 
    ##     VariableExpl3 * VariableExpl4, data = df)
    ## 
    ## Terms:
    ##                 VariableExpl1 VariableExpl2 VariableExpl3 VariableExpl4
    ## Sum of Squares      20.625235      2.866080      0.110079      2.998576
    ## Deg. of Freedom             1             1             1             1
    ##                 VariableExpl1:VariableExpl2 VariableExpl1:VariableExpl3
    ## Sum of Squares                     4.464596                    0.069160
    ## Deg. of Freedom                           1                           1
    ##                 VariableExpl2:VariableExpl3 VariableExpl1:VariableExpl4
    ## Sum of Squares                     0.724097                    0.359177
    ## Deg. of Freedom                           1                           1
    ##                 VariableExpl2:VariableExpl4 VariableExpl3:VariableExpl4
    ## Sum of Squares                     0.633437                    0.000498
    ## Deg. of Freedom                           1                           1
    ##                 VariableExpl1:VariableExpl2:VariableExpl3
    ## Sum of Squares                                   0.160459
    ## Deg. of Freedom                                         1
    ##                 VariableExpl1:VariableExpl2:VariableExpl4
    ## Sum of Squares                                   0.634765
    ## Deg. of Freedom                                         1
    ##                 VariableExpl1:VariableExpl3:VariableExpl4
    ## Sum of Squares                                   0.000898
    ## Deg. of Freedom                                         1
    ##                 VariableExpl2:VariableExpl3:VariableExpl4
    ## Sum of Squares                                   1.258666
    ## Deg. of Freedom                                         1
    ##                 VariableExpl1:VariableExpl2:VariableExpl3:VariableExpl4
    ## Sum of Squares                                                 0.013771
    ## Deg. of Freedom                                                       1
    ##                 Residuals
    ## Sum of Squares   1.218437
    ## Deg. of Freedom        13
    ## 
    ## Residual standard error: 0.3061469
    ## Estimated effects may be unbalanced

When the function is executed, the explanatory and class variables are
defined and an ANOVA crossing the variables is generated.

### ACP

In the package, there is a function to perform a PCA, once again,
post-clustering, with input the X and y defined previously as well as an
integer numerical value which will define the number of axes that we
want keep.

``` r
d=3
ACP2(X,y,d)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

    ## -------------------------------------------------- 
    ## Graphiques des individus 
    ##  
    ## -------------------------------------------------- 
    ## Graphiques des variables 
    ##  
    ## -------------------------------------------------- 
    ## Description des dimensions creees 
    ##  
    ## -------------------------------------------------- 
    ## Visualisation des ellipses de confiance 
    ##  
    ## Les classes sont representees par les couleurs
    ## Call:
    ## PCA(X = df, scale.unit = TRUE, quali.sup = 1, graph = T) 
    ## 
    ## 
    ## Eigenvalues
    ##                        Dim.1   Dim.2   Dim.3   Dim.4
    ## Variance               2.625   1.001   0.361   0.013
    ## % of var.             65.619  25.034   9.024   0.323
    ## Cumulative % of var.  65.619  90.653  99.677 100.000
    ## 
    ## Individuals (the 10 first)
    ##                 Dist    Dim.1    ctr   cos2    Dim.2    ctr   cos2    Dim.3
    ## CarredelEst |  0.970 | -0.044  0.003  0.002 | -0.604  1.256  0.388 | -0.757
    ## Babybel     |  0.254 |  0.174  0.040  0.469 | -0.143  0.071  0.320 | -0.112
    ## Beaufort    |  2.106 |  2.052  5.532  0.949 | -0.439  0.663  0.043 |  0.170
    ## Bleu        |  1.488 |  0.704  0.651  0.224 | -1.199  4.954  0.650 | -0.524
    ## Camembert   |  1.766 | -1.001  1.317  0.321 |  1.445  7.192  0.670 | -0.005
    ## Cantal      |  1.259 |  0.980  1.262  0.605 | -0.710  1.735  0.318 | -0.328
    ## Chabichou   |  1.420 |  0.950  1.186  0.448 |  0.966  3.216  0.463 |  0.424
    ## Chaource    |  2.072 | -0.169  0.038  0.007 |  2.061 14.628  0.989 |  0.013
    ## Cheddar     |  1.617 |  1.355  2.412  0.702 |  0.517  0.922  0.102 | -0.698
    ## Comte       |  2.679 |  2.448  7.871  0.835 | -0.455  0.713  0.029 |  0.986
    ##                ctr   cos2  
    ## CarredelEst  5.473  0.609 |
    ## Babybel      0.120  0.195 |
    ## Beaufort     0.276  0.007 |
    ## Bleu         2.620  0.124 |
    ## Camembert    0.000  0.000 |
    ## Cantal       1.025  0.068 |
    ## Chabichou    1.721  0.089 |
    ## Chaource     0.002  0.000 |
    ## Cheddar      4.652  0.186 |
    ## Comte        9.294  0.136 |
    ## 
    ## Variables
    ##                Dim.1    ctr   cos2    Dim.2    ctr   cos2    Dim.3    ctr
    ## calories    |  0.978 36.428  0.956 |  0.056  0.310  0.003 | -0.184  9.357
    ## magnesium   |  0.861 28.254  0.742 | -0.053  0.279  0.003 |  0.505 70.787
    ## lipides     |  0.959 35.012  0.919 |  0.083  0.696  0.007 | -0.261 18.888
    ## retinol     | -0.090  0.305  0.008 |  0.994 98.714  0.988 |  0.059  0.968
    ##               cos2  
    ## calories     0.034 |
    ## magnesium    0.256 |
    ## lipides      0.068 |
    ## retinol      0.003 |
    ## 
    ## Supplementary categories
    ##                 Dist    Dim.1   cos2 v.test    Dim.2   cos2 v.test    Dim.3
    ## y_1         |  0.598 |  0.443  0.548  1.722 | -0.262  0.191 -1.647 | -0.305
    ## y_2         |  2.126 |  1.986  0.873  2.594 | -0.104  0.002 -0.219 |  0.750
    ## y_3         |  2.037 | -0.496  0.059 -0.648 |  1.970  0.935  4.166 |  0.143
    ## y_4         |  3.479 | -3.372  0.940 -4.405 | -0.754  0.047 -1.594 |  0.404
    ##               cos2 v.test  
    ## y_1          0.261 -3.202 |
    ## y_2          0.125  2.644 |
    ## y_3          0.005  0.505 |
    ## y_4          0.013  1.424 |

A lot of information results from the execution of the function. We
notice the presence of the eigenvalues associated with the dimensions as
well as other descriptive indicators.

We can see graphs, the first one is about observation and the other one
is about variable.

With acpGraph(), a graph is displayed, it representing the clusters
according to the first two axes of the PCA.

``` r
acpGraph(X,y)
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### LDA

It is possible to observe indicators resulting from a linear
discriminant analysis such as for example the relative and absolute
distribution of the classes or even the conditional means.

``` r
LDA2(X,y)
```

    ## -------------------------------------------------- 
    ## Distribution relative des classes 
    ##  
    ##         1         2         3         4 
    ## 0.5862069 0.1379310 0.1379310 0.1379310 
    ## -------------------------------------------------- 
    ## Distribution absolue des classes 
    ##  
    ##  1  2  3  4 
    ## 17  4  4  4 
    ## -------------------------------------------------- 
    ## Moyenne conditionlle des classes 
    ##  
    ## NULL
    ## -------------------------------------------------- 
    ## Matrice des coeeficients des fonctions discriminantes 
    ##  
    ##                   LD1         LD2           LD3
    ## calories  -0.01759698 -0.01809810 -0.0136736785
    ## magnesium -0.02977032  0.04835176 -0.1349818210
    ## lipides   -0.11988764  0.12042520  0.3002589555
    ## retinol   -0.02481023  0.06505577  0.0009106115
    ## -------------------------------------------------- 
    ## Liste des classes 
    ##  
    ## [1] "1" "2" "3" "4"
    ## ================================================== 
    ## Fonctions canoniques 
    ##  
    ## -------------------------------------------------- 
    ## Moyenne des varibles 
    ##  
    ##  calories magnesium   lipides   retinol 
    ## 300.03448  26.96552  24.15862  67.56207 
    ## -------------------------------------------------- 
    ## Obvservation des constantes 
    ##  
    ##        LD1        LD2        LD3 
    ## 10.6550224 -3.1783839  0.4270647 
    ## -------------------------------------------------- 
    ## Moyennes conditionnelles 
    ##  
    ##          LD1        LD2        LD3
    ## 1 -0.7181943 -0.6918092  0.4784756
    ## 2 -2.8346252 -0.1475351 -1.8162033
    ## 3 -0.5220837  3.2201390  0.3311144
    ## 4  6.4090349 -0.1324149 -0.5484324
    ## -------------------------------------------------- 
    ## Coefficient des fonctions canoniques 
    ##  
    ##                     1           2           3          4
    ## calories   0.01861596  0.07738512 -0.05361887 -0.1028841
    ## magnesium -0.07665483  0.32240855  0.12654757 -0.1231731
    ## lipides    0.14645796 -0.22326171  0.54979732 -0.9489819
    ## retinol   -0.02675191  0.05907583  0.22274315 -0.1681234
    ## -------------------------------------------------- 
    ## Constante de fonction de classement 
    ##  
    ## [1]  -6.394955 -38.168440 -23.012997  45.797038

    ## Call:
    ## lda(y ~ ., data = df)
    ## 
    ## Prior probabilities of groups:
    ##         1         2         3         4 
    ## 0.5862069 0.1379310 0.1379310 0.1379310 
    ## 
    ## Group means:
    ##   calories magnesium  lipides   retinol
    ## 1 331.1176  26.88235 27.15294  60.09412
    ## 2 389.7500  45.75000 30.65000  64.27500
    ## 3 276.5000  24.25000 22.82500 115.00000
    ## 4 101.7500  11.25000  6.27500  55.15000
    ## 
    ## Coefficients of linear discriminants:
    ##                   LD1         LD2           LD3
    ## calories  -0.01759698 -0.01809810 -0.0136736785
    ## magnesium -0.02977032  0.04835176 -0.1349818210
    ## lipides   -0.11988764  0.12042520  0.3002589555
    ## retinol   -0.02481023  0.06505577  0.0009106115
    ## 
    ## Proportion of trace:
    ##    LD1    LD2    LD3 
    ## 0.7507 0.1811 0.0682

## Evaluation metrics

### Creating the metrics object

We create 3 objects using the EvalMetrics function. This function takes
as input dataframe containing the real values and the values predicted
by a clustering.

``` r
Obj2c <- EvalMetrics(pred_reel_2c$val_reel,pred_reel_2c$val_pred)
Obj3c <- EvalMetrics(pred_reel_3c$val_reel,pred_reel_3c$val_pred)
Obj2c_bis <- EvalMetrics(pred_reel_2c_bis$val_reel,pred_reel_2c_bis$val_pred)
```

ggMatconf function display the confusion matrix under a ggplot graph

``` r
ggMatConf(Obj2c)
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
ggMatConf(Obj3c)
```

![](README_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

Thanks to the overload of the print method we can display the different
attributes of our object.

``` r
print(Obj2c)
```

    ## [1] "Matrice de confusion et indicateurs"
    ##        valreel
    ## valpred   -   +
    ##       - 175   4
    ##       +   3 174
    ##                       Valeur
    ## Erreur            0.01966292
    ## Accuracy          0.98033708
    ## Precision         0.97765363
    ## Sensibilite       0.98314607
    ## Specificity       0.97752809
    ## Balanced Accuracy 0.98033708
    ## F1                0.98039216

``` r
print(Obj3c)
```

    ## [1] "Matrice de confusion et indicateurs"
    ##        valreel
    ## valpred grand moyen petit
    ##   grand   118     2     0
    ##   moyen     3   114     2
    ##   petit     0     3   115
    ##                       grand     moyen     petit
    ## Erreur            0.0280112 0.0280112 0.0280112
    ## Accuracy          0.9719888 0.9719888 0.9719888
    ## Precision         0.9752066 0.9579832 0.9829060
    ## Sensibilite       0.9833333 0.9579832 0.9745763
    ## Specificity       0.9873418 0.9789916 0.9916318
    ## Balanced Accuracy 0.9853376 0.9684874 0.9831040
    ## F1                0.9792531 0.9579832 0.9787234

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
compareRes(Obj2c,Obj2c_bis)
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

The output will be a ggplot chart confronting the indicators
