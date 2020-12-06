#CARACTERISATION UNIVARIE
#caracterisation de la partition
#graphiques entre une variable explicative numérique et la variable classe d'appartenance
#df = dataframe contenant la variable numérique et la variable classe d'appartenance
#x = nom de la variable numérique
#y = nom de la variable classe d'appartenance
#' Title
#'
#' @param df
#' @param x
#' @param y
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
barplot.YX <- function (df, x, y){
  ggplot(df, aes_string(x = x, fill = y)) +
    geom_bar() +
    ggtitle("Effectifs de Y en fonction de X") +
    xlab("X") +
    ylab("Effectifs")
}

#' Title
#'
#' @param df
#' @param x
#' @param y
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
barplot.XY <- function (df, x, y){
  ggplot(df, aes_string(x = y, fill = x)) +
    geom_bar(position=position_dodge()) +
    ggtitle("Effectifs de X en fonction de Y") +
    xlab("Clusters") +
    ylab("Effectifs")
}

#Test du khi deux
#x = variable qualitative
#y = variable classe d'appartenance
#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
khi2<-function(x,y){
  #tableau de contingence
  tableau=table(x,y)
  #khi deux d'independance
  #si p-value < 0.05 les vars sont dépendantes
  res=chisq.test(tableau)
  return(res)
}

#V de Cramer
#x = variable qualitative
#y = variable classe d'appartenance
#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @import questionr
#' @export
#'
#' @examples
vcramer<-function(x,y){
  #tableau de contingence
  tableau=table(x,y)
  #V de Cramer
  #0 (absence de liaison) et 1 (liaison parfaite)
  res=cramer.v(tableau)
  return(res)
}


#var quanti
#tableau moyennes conditionnelles et eta
#eta indique la proportion de variance de X expliquée par les
#groupes (0 < eta < 1). On peut l'interpréter (avec beaucoup de
#prudence) comme le pouvoir discriminant de la variable.
#X=une ou plusieurs vars quanti ; y=classes
#' Title
#'
#' @param X
#' @param y
#'
#' @return
#' @export
#'
#' @examples
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
  #calcul la proportion de variance de X expliquée par les groupes
  for(i in 1:nc){
    #recuperation des calculs de l'anova
    modele=aov(X[,i]~y)
    val=anova(modele)
    sct=sum(val$`Sum Sq`)
    sce=val$`Sum Sq`[1]
    #calcul de eta
    eta[i]=(sce/sct)*100
  }
  #réunion des moyennes conditionnelles et de eta
  res=cbind(m,eta)
  #nommage des lignes
  rownames(res)=colnames(X)
  return(res)
}

#graphique
#boxplot
#df=variables explicatives + classes ; x=nom var quanti ; y=nom var classe
#' Title
#'
#' @param df
#' @param x
#' @param y
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
boxplot <- function (df, x, y){
  ggplot(df, aes_string(x=x, y=y, fill=x))+
    geom_boxplot(outlier.colour="red")
}


#caracterisation des groupes
#variable qualitative
#calcul de la valeur test
#pour une variable et un groupe donné
#va renvoyer une valeur test pour chaque modalité de X
#x = variable qualitative, k = cluster choisi , y = variable classe d'appartenance
#' Title
#'
#' @param x
#' @param k
#' @param y
#'
#' @return
#' @export
#'
#' @examples
vt.quali<-function(x,k,y){
  #transtypage chaine de caracteres
  x=as.character(x)
  #création d'un dataframe
  df=as.data.frame(cbind(x,y))
  #nb de lignes du dataframe
  n=nrow(df)
  #individus appartenant au cluster choisi
  g=df[df$y==k,]
  #nb de lignes de g
  ng=nrow(g)
  #modalités de x
  val=unique(x)
  #nombre de modalités de x
  nb=length(val)
  #initialisation
  vt=c()
  plg=c()
  pl=c()
  plg_affichage=c()
  pl_affichage=c()
  #calcul de plg, pl et vt pour chaque modalité
  for(i in 1:nb){
    #fréquence de la modalité dans le groupe
    plg[i]=nrow(g[g$x==val[i],])/nrow(g)
    #fréquence de la modalité dans la population
    pl[i]=nrow(df[df$x==val[i],])/nrow(df)
    #valeur test
    vt[i]=sqrt(ng)*(plg[i]-pl[i])/sqrt(((n-ng)/(n-1))*pl[i]*(1-pl[i]))
    #affichage en pourcentage
    plg_affichage[i]=plg[i]*100
    pl_affichage[i]=pl[i]*100
  }
  #réunion des trois valeurs
  res=cbind(vt,pl_affichage,plg_affichage)
  #nommage des lignes
  rownames(res)=val
  return(res)
}

#variable quantitative
#calcul de la valeur test
#pour une seule variable et un groupe donné
#x = variable quantitative, k = cluster choisi , y = variable classe d'appartenance
#' Title
#'
#' @param x
#' @param k
#' @param y
#'
#' @return
#' @export
#'
#' @examples
vt.quanti<-function(x,k,y){
  #création d'un dataframe
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

#afficher pour chaque cluster, la vt, la m par groupe et la m au total par variable
#X = toutes les vars, k = cluster choisi , y = variables classe d'appartenance
#' Title
#'
#' @param X
#' @param y
#' @param k
#'
#' @return
#' @export
#'
#' @examples
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
  vt_quanti=sapply(df_quanti[,-n_quanti],vt.quanti,k=k,y=y)
  #réunion de moyenne globale, moyenne groupe et valeur test
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
  all_quali=sapply(var_quali,vt.quali,y=y,k=k)
  #nb de variables quali
  nn=length(all_quali)
  #initialisation
  res=c()
  #pour chaque variable on ajoute son nom au rownames a coté de la modalité
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

  #on donne un nom à la classe
  class(instance) <- "cluster"
  return(instance)
}

#Surcharge de print pour l'affichage de l'objet "cluster"
#' Title
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
print.cluster <- function(obj){
  print(paste("Caractérisation du cluster k =",obj$nomCluster))
  print(paste(obj$prop,"% de la population"))
  print("Variables quantitatives")
  print(obj$resQuanti)
  print("Variables qualitatives")
  print(obj$resQuali)
}
