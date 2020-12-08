#' Graphique resultat kmeans couple acp
#'
#' @param vars DataFrame with quantitatives variable
#' @param reel DataFrame with quantitatives variable
#' @param resKM clustering results
#'
#' @return
#' @import factoextra ggplot2 ggpubr
#' @export
#'
#' @examples
acpGraph <- function(vars,reel,resKM){

  res.pca <- prcomp(vars,  scale = TRUE)
  ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
  ind.coord$cluster <- factor(resKM$cluster)
  ind.coord$valreel <- as.matrix(reel)
  eigenvalue <- round(get_eigenvalue(res.pca), 1)
  variance.percent <- eigenvalue$variance.percent

  ggscatter(
    ind.coord, x = "Dim.1", y = "Dim.2",
    color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
    shape = "valreel", size = 1.5,  legend = "right", ggtheme = theme_bw(),
    xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
    ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
  ) +
    stat_mean(aes(color = cluster), size = 4)
}


#' ANOVA
#'
#' @param X data.frame, with quantitative variable
#' @param y integer,  with class variable
#'
#' @return aov, multivariate variance analysis
#' @export
#'
#' @examples
#' X=data.frame(fromage["calories"],fromage["magnesium"],fromage["lipides"],fromage["retinol"])
#' y=fromage$groupes.cah
#' AOV2(X,y)
AOV2<-function(X,y){

  df<- as.data.frame(cbind(X,y))
  ncol(df)
  lst_nom=names(df)

  # Cas pour 2 variables explicatives
  if (ncol(df)==3){
    colnames(df) = c("VariableExpl1","VariableExpl2","VariableCible")



    fit2 <- aov(VariableCible ~ VariableExpl1*VariableExpl2,
                data = df)

    cat("VariableExpl1 = ", lst_nom[1], "\n")
    cat("VariableExpl2 = ", lst_nom[2], "\n")
    cat("VariableCible = ", lst_nom[3], "\n")

    # Cas pour 3 variables explicatives
  }else if (ncol(df)==4){
    colnames(df) = c("VariableExpl1","VariableExpl2","VariableExpl3","VariableCible")



    fit2 <- aov(VariableCible ~ VariableExpl1*VariableExpl2*VariableExpl3,
                data = df)

    cat("VariableExpl1 = ", lst_nom[1], "\n")
    cat("VariableExpl2 = ", lst_nom[2], "\n")
    cat("VariableExpl3 = ", lst_nom[3], "\n")
    cat("VariableCible = ", lst_nom[4], "\n")

    # Cas pour 4 variables explicatives
  }else if (ncol(df)==5){
    colnames(df) = c("VariableExpl1","VariableExpl2","VariableExpl3","VariableExpl4","VariableCible")



    fit2 <- aov(VariableCible ~ VariableExpl1*VariableExpl2*VariableExpl3*VariableExpl4,
                data = df)

    cat("VariableExpl1 = ", lst_nom[1], "\n")
    cat("VariableExpl2 = ", lst_nom[2], "\n")
    cat("VariableExpl3 = ", lst_nom[3], "\n")
    cat("VariableExpl4 = ", lst_nom[4], "\n")
    cat("VariableCible = ", lst_nom[5], "\n")

    # Cas pour 5 variables explicatives
  }else if (ncol(df)==6){
    colnames(df) = c("VariableExpl1","VariableExpl2","VariableExpl3","VariableExpl4","VariableExpl5","VariableCible")



    fit2 <- aov(VariableCible ~ VariableExpl1*VariableExpl2*VariableExpl3*VariableExpl4*VariableExpl5,
                data = df)

    cat("VariableExpl1 = ", lst_nom[1], "\n")
    cat("VariableExpl2 = ", lst_nom[2], "\n")
    cat("VariableExpl3 = ", lst_nom[3], "\n")
    cat("VariableExpl4 = ", lst_nom[4], "\n")
    cat("VariableExpl5 = ", lst_nom[5], "\n")
    cat("VariableCible = ", lst_nom[6], "\n")

    # Cas pour 6 variables explicatives
  }else if (ncol(df)==7){
    colnames(df) = c("VariableExpl1","VariableExpl2","VariableExpl3","VariableExpl4","VariableExpl5","VariableExpl6","VariableCible")



    fit2 <- aov(VariableCible ~ VariableExpl1*VariableExpl2*VariableExpl3*VariableExpl4*VariableExpl5*VariableExpl6,
                data = df)

    cat("VariableExpl1 = ", lst_nom[1], "\n")
    cat("VariableExpl2 = ", lst_nom[2], "\n")
    cat("VariableExpl3 = ", lst_nom[3], "\n")
    cat("VariableExpl4 = ", lst_nom[4], "\n")
    cat("VariableExpl5 = ", lst_nom[5], "\n")
    cat("VariableExpl6 = ", lst_nom[6], "\n")
    cat("VariableCible = ", lst_nom[7], "\n")

    # Cas pour 7 variables explicatives
  } else if (ncol(df)==8){
    colnames(df) = c("VariableExpl1","VariableExpl2","VariableExpl3","VariableExpl4","VariableExpl5","VariableExpl6","VariableExpl7","VariableCible")



    fit2 <- aov(VariableCible ~ VariableExpl1*VariableExpl2*VariableExpl3*VariableExpl4*VariableExpl5*VariableExpl6*VariableExpl7,
                data = df)

    cat("VariableExpl1 = ", lst_nom[1], "\n")
    cat("VariableExpl2 = ", lst_nom[2], "\n")
    cat("VariableExpl3 = ", lst_nom[3], "\n")
    cat("VariableExpl4 = ", lst_nom[4], "\n")
    cat("VariableExpl5 = ", lst_nom[5], "\n")
    cat("VariableExpl6 = ", lst_nom[6], "\n")
    cat("VariableExpl7 = ", lst_nom[7], "\n")
    cat("VariableCible = ", lst_nom[8], "\n")

    # Cas pour 8 variables explicatives
  }else if (ncol(df)==9){
    colnames(df) = c("VariableExpl1","VariableExpl2","VariableExpl3","VariableExpl4","VariableExpl5","VariableExpl6","VariableExpl7","VariableExpl8","VariableCible")



    fit2 <- aov(VariableCible ~ VariableExpl1*VariableExpl2*VariableExpl3*VariableExpl4*VariableExpl5*VariableExpl6*VariableExpl7*VariableExpl8,
                data = df)

    cat("VariableExpl1 = ", lst_nom[1], "\n")
    cat("VariableExpl2 = ", lst_nom[2], "\n")
    cat("VariableExpl3 = ", lst_nom[3], "\n")
    cat("VariableExpl4 = ", lst_nom[4], "\n")
    cat("VariableExpl5 = ", lst_nom[5], "\n")
    cat("VariableExpl6 = ", lst_nom[6], "\n")
    cat("VariableExpl7 = ", lst_nom[7], "\n")
    cat("VariableExpl7 = ", lst_nom[8], "\n")
    cat("VariableCible = ", lst_nom[9], "\n")

  }

  return(fit2)
}




#' ACP
#'
#' @param X data.frame, with quantitative variable
#' @param y integer,  with class variable
#' @param d integer, number of axes wanted
#'
#' @return pca,  principal component analysis
#' @import FactoMineR
#' @export
#'
#' @examples
#' X=data.frame(fromage["calories"],fromage["magnesium"],fromage["lipides"],fromage["retinol"])
#' y=fromage$groupes.cah
#' d=3
#' ACP2(X,y,d)
ACP2<-function(X,y,d){
  #Modification du type de la variable cible
  y=as.character(y)
  df<- as.data.frame(cbind(y,X))
  #Realisation ACP
  res.pca<- PCA(df, scale.unit=TRUE , graph=T, quali.sup=1)
  class(res.pca)
  # CrÃ©ation du graphe des individus
  cat("--------------------------------------------------","\n")
  cat("Graphiques des individus","\n","\n")
  plot.PCA(res.pca, axes=c(1, 2), habillage =1, title="Graphe des individus")

  # CrÃ©ation du graphe des variables
  cat("--------------------------------------------------","\n")
  cat("Graphiques des variables","\n","\n")
  plot.PCA(res.pca, axes=c(1, 2),choix="var", title="Graphe des variables")

  # Description des diffÃ©rentes dimensions (axes) crÃ©Ã©es
  cat("--------------------------------------------------","\n")
  cat("Description des dimensions crÃ©Ã©es","\n","\n")
  dimdesc(res.pca, axes=c(1,2))

  # CrÃ©ation des ellipses de confiance
  cat("--------------------------------------------------","\n")
  cat("Visualisation des ellipses de confiance","\n","\n")
  plotellipses(res.pca)
  cat("Les classes sont reprÃ©sentÃ©es par les couleurs")

  return(summary(res.pca, ncp=d))
}


#' Linear Discriminant Analysis
#'
#' @param X data.frame, with quantitative variable
#' @param y integer,  with class variable
#'
#' @return lda, linear discriminant analysis
#' @import MASS
#' @export
#'
#' @examples
#' X=data.frame(fromage["calories"],fromage["magnesium"],fromage["lipides"],fromage["retinol"])
#' y=fromage$groupes.cah
#' LDA2(X,y)
LDA2<-function(X,y){
  df<- as.data.frame(cbind(y,X))
  mLda <- lda(y~. ,data=df)
  # Calcul du projetÃ© des individus sur D1 et D2
  D1 <- mLda$scaling[,1] # vecteur 1
  D2 <- mLda$scaling[,2] # vecteur 2
  Xfromage <- df[-1]
  # Observation de la distribution relative des classes
  cat("--------------------------------------------------","\n")
  cat("Distribution relative des classes","\n","\n")
  dis_rel <-mLda$prior
  print(dis_rel)

  # Observation de la distribution absolue des classes
  cat("--------------------------------------------------","\n")
  cat("Distribution absolue des classes","\n","\n")
  dis_abs<-mLda$counts
  print(dis_abs)

  # Observation de la moyenne conditionnelle des classes
  cat("--------------------------------------------------","\n")
  cat("Moyenne conditionlle des classes","\n","\n")
  moy_cond <- mLda$meannes
  print(moy_cond)

  # Matrice des coefficients des fonctions discriminantes
  cat("--------------------------------------------------","\n")
  cat("Matrice des coeeficients des fonctions discriminantes","\n","\n")
  mat_coef <- mLda$scaling
  print(mat_coef)

  # Liste des classes
  cat("--------------------------------------------------","\n")
  cat("Liste des classes","\n","\n")
  l_class <- mLda$lev
  print(l_class)

  ## Constante des fonctions canoniques
  cat("==================================================","\n")
  cat("Fonctions canoniques","\n","\n")
  # Moyenne des variables
  cat("--------------------------------------------------","\n")
  cat("Moyenne des varibles","\n","\n")
  xb <- colMeans(df[2:ncol(df)])
  print(xb)

  # Calcul des constante
  cat("--------------------------------------------------","\n")
  cat("Obvservation des constantes","\n","\n")
  const_ <- apply(mLda$scaling,2,function(v){-sum(v*xb)})
  print(const_)

  # Calcul des moyennes conditionnelles
  cat("--------------------------------------------------","\n")
  cat("Moyennes conditionnelles","\n","\n")
  cond_Xb <- sapply(1:ncol(mLda$scaling),function(j)
  {sapply(mLda$lev,
          function(niveau){sum(mLda$scaling[,j]*mLda$means[niveau,]) + const_[j]})
  })
  colnames(cond_Xb) <- paste("LD",1:ncol(mLda$scaling),sep="")
  rownames(cond_Xb) <- mLda$lev

  print(cond_Xb)


  # Coefficients de fonctions canoniques
  cat("--------------------------------------------------","\n")
  cat("Coefficient des fonctions canoniques","\n","\n")
  coef_ <- sapply(mLda$lev,function(niveau)
  {rowSums(sapply(1:ncol(mLda$scaling),
                  function(j){mLda$scaling[,j]*cond_Xb[niveau,j]}))
  })

  print(coef_)

  # Constante de la fonction de classment
  cat("--------------------------------------------------","\n")
  cat("Constante de fonction de classement","\n","\n")
  intercept_ <- sapply(mLda$lev,function(niveau)
  {sum(mapply(prod,const_,cond_Xb[niveau,]))-0.5*sum(cond_Xb[niveau,]^2)+log(mLda$prior[niveau])})
  names(intercept_) <- levels(Xfromage)

  print(intercept_)

  return(mLda)
}
