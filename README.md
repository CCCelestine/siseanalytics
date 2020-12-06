# siseanalytics


siseanalytics is a package to characterize the classes obtained after a clustering. It makes it possible to characterize the classes in a univariate way, the variables are taken individually. But also multivariate. In addition, the pacakge allows to have the evaluation measures.

In this tutorial we will see the concrete functionality of the package

* Installation
* Evaluation metrics
    - Object creation and display
    - Performance indicators
    - Comparaison of clustering results

* Univariate characterization
* Multivariate characterization



## Installation

In your script, install the package using these command lines :

```{r}
install.packages("devtools")
library(devtools)
install_github("CCCelestine/siseanalytics")
library(siseanalytics)
```
## Univariate characterization of clusters

### Characterization of the partition

In this part, we assess how each variable contributes to the constitution of the partition.
First of all, we will see the case of qualitative variables.


## Evaluation metrics

### Creating the metrics object

We create 3 objects using the EvalMetrics function. This function takes as input dataframe containing the real values ​​and the values ​​predicted by a clustering.

```{r}
Obj2c <- EvalMetrics(val_reel_22,val_pred_22)
Obj3c <- EvalMetrics(val_reel_32,val_pred_32)
Obj2c_bis <- EvalMetrics(val_reel_22,val_pred_bis)
```

ggMatconf function display the confusion matrix under a ggplot graph

```{r}
ggMatConf(Obj2c)
ggMatConf(Obj3c)
```

Thanks to the overload of the print method we can display the different attributes of our object.
```{r}
print(Obj2c)
print(Obj3c)

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

The package allows to calculate different indicators to evaluate the results of clustering.


Indicators | Details
--- | --- 
Erreur  | Overall model performance
Accuracy | Overall model performance (1- erreur)
Précision | How accurate positive predictions are
Sensibilite | Truly positive observation coverage
Specificity | Coverage of truly negative observations
Balanced Accuracy| Overall performance of the model, when classes are unbalanced
F1 score| Hybrid indicator used for unbalanced classes

### Comparaison of clustering results


Finally in this clustering evaluation part, the pacakge allows you to compare two clustering results stored in two different objects

```{r}
compareRes(Obj2c,Obj2c_bis)
```

The output will be a ggplot chart confronting the indicators
