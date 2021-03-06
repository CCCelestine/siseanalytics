#' Barplot Y versus X
#'
#' @param df dataframe which includes the categorical variable and the clusters variable
#' @param x name of the categorical variable
#' @param y name of the clusters variable
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' barplotYX(df_test, "sexe", "val_pred")
barplotYX <- function (df, x, y){
  ggplot(df, aes_string(x = x, fill = y)) +
    geom_bar() +
    ggtitle("Effectifs de Y en fonction de X") +
    xlab("X") +
    ylab("Effectifs")
}

#' Barplot X versus Y
#'
#' @param df dataframe which includes the categorical variable and the clusters variable
#' @param x name of the categorical variable
#' @param y name of the clusters variable
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' barplotXY(df_test, "sexe", "val_pred")
barplotXY <- function (df, x, y){
  ggplot(df, aes_string(x = y, fill = x)) +
    geom_bar(position=position_dodge()) +
    ggtitle("Effectifs de X en fonction de Y") +
    xlab("Clusters") +
    ylab("Effectifs")
}

#' Chi-Square Test
#'
#' @param x categorical variable
#' @param y clusters variable
#'
#' @return A list with class "htest"
#' @importFrom stats chisq.test
#' @export
#'
#' @examples
#' khi2(df_test$sexe,df_test$val_pred)
khi2<-function(x,y){
  #tableau de contingence
  tableau=table(x,y)
  #khi deux d'independance
  #si p-value < 0.05 les vars sont dependantes
  res=chisq.test(tableau)
  return(res)
}

#' Cramer's V
#'
#' @param x categorical variable
#' @param y clusters variable
#'
#' @return a float
#' @import questionr
#' @export
#'
#' @examples
#' vcramer(df_test$sexe,df_test$val_pred)
vcramer<-function(x,y){
  #tableau de contingence
  tableau=table(x,y)
  #V de Cramer
  #0 (absence de liaison) et 1 (liaison parfaite)
  res=cramer.v(tableau)
  return(res)
}


#' Table of row profiles for a categorical variable
#'
#' @param y clusters variable
#' @param x categorical variable
#'
#' @return table
#' @export
#'
#' @examples
#' tab.quali.ligne(df_test$val_pred,df_test$sexe)
tab.quali.ligne<-function(x,y){
  #thanks to http://olivier.godechot.free.fr/hoparticle.php?id_art=465
  #tableau de contingence
  tableau=table(y,x)
  #calcul des proportions par ligne, ajout des colonnes total et effectif
  tabligne=cbind(addmargins(prop.table(addmargins(tableau,1),1),2), c(margin.table(tableau,1),sum(tableau)))
  #nommage avec colonnes
  colnames(tabligne)<-c(colnames(tableau),"Total","Effectif")
  return(tabligne)
}

#' Table of column profiles for a categorical variable
#'
#' @param y clusters variable
#' @param x categorical variable
#'
#' @return table
#' @export
#'
#' @examples
#' tab.quali.col(df_test$val_pred,df_test$sexe)
tab.quali.col<-function(x,y){
  #thanks to http://olivier.godechot.free.fr/hoparticle.php?id_art=465
  #tableau de contingence
  tableau=table(y,x)
  #calcul des proportions par colonne, ajout des colonnes total et effectif
  tabcol=rbind(addmargins(prop.table(addmargins(tableau,2),2),1), c(margin.table(tableau,2),sum(tableau)))
  #nommage avec colonnes
  rownames(tabcol)<-c(rownames(tableau),"Total","Effectif")
  return(tabcol)
}


#' Table of conditional means and % explained variance
#'
#' @param X one or more quantitative variables
#' @param y clusters variable
#'
#' @return table
#' @importFrom stats aov
#' @export
#'
#' @examples
#' df_test=as.data.frame(df_test)
#' data_quanti=df_test[,3:5]
#' tab.quanti(data_quanti,df_test$val_pred)
tab.quanti<-function(X,y){
  #nb de variables quantitatives
  nc=ncol(X)
  #initialisation
  m=c()
  #calcul des moyennes conditionnelles pour chaque variable
  for(j in 1:nc){
    m=rbind(m,tapply(X[,j],y,mean))
  }
  #initialisation
  eta=c()
  #calcul la proportion de variance de X expliquee par les groupes
  for(i in 1:nc){
    #recuperation des calculs de l'anova
    modele=aov(X[,i]~y)
    val=anova(modele)
    sct=sum(val$`Sum Sq`)
    sce=val$`Sum Sq`[1]
    #calcul de eta
    eta[i]=(sce/sct)*100
  }
  #reunion des moyennes conditionnelles et de eta
  res=cbind(m,eta)
  #nommage des lignes
  rownames(res)=colnames(X)
  return(res)
}

#' Boxplot
#'
#' @param df dataframe which includes the quantitative variable and the clusters variable
#' @param x name of the quantitative variable
#' @param y name of the clusters variable
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' boxplot(df_test, "val_pred", "taille")
boxplot <- function (df, x, y){
  ggplot(df, aes_string(x=x, y=y, fill=x))+
    geom_boxplot(outlier.colour="red")
}

#' Characterization of a clusters with a qualitative variable
#'
#' @param x qualitative variable
#' @param k name of the cluster selected
#' @param y clusters variable
#'
#' @return test value, plg, pl for each modality
#' @export
.vt.quali<-function(x,k,y){
  #transtypage chaine de caracteres
  x=as.character(x)
  #creation d'un dataframe
  df=as.data.frame(cbind(x,y))
  #nb de lignes du dataframe
  n=nrow(df)
  #individus appartenant au cluster choisi
  g=df[df$y==k,]
  #nb de lignes de g
  ng=nrow(g)
  #modalites de x
  val=unique(x)
  #nombre de modalites de x
  nb=length(val)
  #initialisation
  vt=c()
  plg=c()
  pl=c()
  plg_affichage=c()
  pl_affichage=c()
  #calcul de plg, pl et vt pour chaque modalite
  for(i in 1:nb){
    #frequence de la modalite dans le groupe
    plg[i]=nrow(g[g$x==val[i],])/nrow(g)
    #frequence de la modalite dans la population
    pl[i]=nrow(df[df$x==val[i],])/nrow(df)
    #valeur test
    vt[i]=sqrt(ng)*(plg[i]-pl[i])/sqrt(((n-ng)/(n-1))*pl[i]*(1-pl[i]))
    #affichage en pourcentage
    plg_affichage[i]=plg[i]*100
    pl_affichage[i]=pl[i]*100
  }
  #reunion des trois valeurs
  res=cbind(vt,pl_affichage,plg_affichage)
  #nommage des lignes
  rownames(res)=val
  return(res)
}

#' Characterization of a cluster with a quantitative variable
#'
#' @param x quantitative variable
#' @param k name of the cluster selected
#' @param y clusters variable
#'
#' @return test value
#' @export
.vt.quanti<-function(x,k,y){
  #creation d'un dataframe
  df=as.data.frame(cbind(x,y))
  #moyenne de la population
  xb=mean(x)
  #nb de lignes de la population
  n=nrow(df)
  #variance de la population
  v=var(x)
  #individus appartenant au cluster choisi
  g=df[df$y==k,]
  #moyenne du cluster
  xg=mean(as.numeric(g$x))
  #nb de lignes du cluster
  ng=nrow(g)
  #calcul de la valeur test
  res=(xg-xb)/sqrt(((n-ng)/(n-1))*(v/ng))
  return(res)
}

#' Characterization of a selected cluster
#'
#' @param X all variables
#' @param y name of the cluster selected
#' @param k clusters variable
#'
#' @return object with class S3 "cluster"
#' @export
#'
#' @examples
#' data=as.data.frame(df_test[,-c(1,2)])
#' resCluster(data,df_test$val_pred,"grand")
resCluster<-function(X,y,k){
  #nombre de variables
  n=ncol(X)
  #nombre d'individus
  l=nrow(X)
  #separation des variables quantitatives et qualitatives
  var_quanti=c()
  var_quali=c()
  for(i in 1:n){
    if(is.numeric(X[,i])){
      var_quanti=cbind(var_quanti,X[,i])
      tmp=ncol(var_quanti)
      colnames(var_quanti)[tmp]=colnames(X)[i]
    } else {
      var_quali=cbind(var_quali,X[,i])
      tmp=ncol(var_quali)
      colnames(var_quali)[tmp]=colnames(X)[i]
    }
  }
  #transformation en dataframe
  var_quanti=as.data.frame(var_quanti)
  var_quali=as.data.frame(var_quali)
  #traitement des variables quantitatives
  #moyenne globale
  overall=sapply(var_quanti,mean)
  #ajout de la variable classes au df
  df_quanti=as.data.frame(cbind(var_quanti,y))
  #nb de variables quanti
  n_quanti=ncol(df_quanti)
  #individus appartenant au cluster choisi
  g_quanti=df_quanti[df_quanti$y==k,-n_quanti]
  #nb d'individus du cluster
  lg=nrow(g_quanti)
  #proportion d'individus dans le cluster parmi tous
  prop=round((lg/l)*100,2)
  #moyenne du cluster
  grp_quanti=sapply(g_quanti,mean)
  #calcul de la valeur test
  vt_quanti=sapply(df_quanti[,-n_quanti],.vt.quanti,k=k,y=y)
  #reunion de moyenne globale, moyenne groupe et valeur test
  m_quanti=rbind(vt_quanti,grp_quanti,overall)
  #transposition de la matrice
  m_quanti=t(m_quanti)
  #transformation en dataframe
  m_quanti=as.data.frame(m_quanti)
  #tri du tableau selon la valeur test
  m_quanti=m_quanti[order(-m_quanti$vt_quanti),]
  #nommage des colonnes
  colnames(m_quanti)=c("test_value","group","overall")

  #traitement des variables qualiitatives
  #ajout de la variable classes au df
  df_quali=as.data.frame(cbind(var_quali,y))
  #nb de variables quali
  n_quali=ncol(df_quali)
  #individus appartenant au cluster choisi
  g_quali=df_quali[df_quali$y==k,-n_quali]
  #calcul de plg, pl et valeur test
  all_quali=sapply(var_quali,.vt.quali,y=y,k=k)
  #nb de variables quali
  nn=length(all_quali)
  #initialisation
  res=c()
  #pour chaque variable on ajoute son nom au rownames a cote de la modalite
  #exemple : homme devient sexe$homme
  for(i in 1:nn){
    nom=names(all_quali)[i]
    tmp=as.data.frame(all_quali[i])
    rownames(tmp)=sapply(rownames(tmp),FUN=function(x,y){
      return(paste(y,x,sep="$"))
    } ,y=nom)
    tmp=as.matrix(tmp)
    res=rbind(res,tmp)
  }
  #transformation en dataframe
  m_quali=as.data.frame(res)
  #nommage des colonnes
  colnames(m_quali)=c("test_value","group %","overall %")
  #tri du tableau selon la valeur test
  m_quali=m_quali[order(-m_quali$test_value),]

  #On instancie la class cluster
  instance <- list()
  instance$var_quanti_clus <- g_quanti
  instance$nomVar_quanti_clus <- rownames(g_quanti)
  instance$var_quali_clus <- g_quali
  instance$nomvar_quali_clus <- rownames(g_quali)
  instance$nomCluster <- k
  instance$resQuanti <- m_quanti
  instance$resQuali <- m_quali
  instance$prop <- prop

  #on donne un nom a la classe
  class(instance) <- "cluster"
  return(instance)
}

#' Overload print function for displaying the "cluster" object
#'
#' @param obj cluster class instance
#'
#' @export
print.cluster <- function(obj){
  print(paste("Caracterisation du cluster k =",obj$nomCluster))
  print(paste(obj$prop,"% de la population"))
  print("Variables quantitatives")
  print(obj$resQuanti)
  print("Variables qualitatives")
  print(obj$resQuali)
}


#' Radar diagram
#'
#' @param X one or more quantitative variables
#' @param y clusters variable
#'
#' @import ggradar scales tibble dplyr
#' @export
#'
#' @examples
#' df_test=as.data.frame(df_test)
#' data_quanti=df_test[,3:5]
#' radar(data_quanti,df_test$val_pred)
radar<-function(X,y){
  #thanks to https://github.com/ricardo-bion/ggradar
  #nb de variables
  nc=ncol(X)
  #initialisation
  m=c()
  #calcul des moyennes conditionnelles pour chaque variable
  for(j in 1:nc){
    m=rbind(m,tapply(X[,j],y,mean))
  }
  #transposition de la matrice
  data_radar=t(m)
  #nommage des colonnes
  colnames(data_radar)=colnames(X)
  #preparation des données radar
  radar <- data_radar %>%
    as_tibble(rownames = "group") %>%
    mutate_at(vars(-group), rescale)
  #affciahge du graphique
  ggradar(radar)
}
