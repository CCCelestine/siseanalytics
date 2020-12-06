#' Création de l'objet "metrics" a partir de deux df contenant les valeurs prédites et réels
#'
#' @param valreel Un dataframe contenant les classes réels
#' @param valpred Un dataframe contenant les classes prédites par un clustering
#'
#' @return Un objet de la classe metrics
#' @export
#'
#' @examples
EvalMetrics <- function(valreel,valpred){
  #Création des variables pour la construction de notre class
  valpred <- as.factor(valpred)
  valreel <- as.factor(valreel)
  #On récupère les taille des vecteurs et le nombre de valeur que peut prendre la variable à trouver
  a <- length(valpred)
  b <- length(valreel)
  c <- length(levels(valpred))
  d <- length(levels(valreel))
  # On vérifie certaines conditions avant de donner les attributs à notre objet
  if (a !=b){stop("Les deux vecteurs rentrée n'ont pas la même taille")}
  if (c !=d){stop("Les deux vecteurs ne prennet pas le même nombre de valeur")}
  if (c < 2 ){stop("Il doit avoir au moins deux classes pour la variable prédites ")}

  #On instancie la class metrics
  instance <- list()
  instance$mc <- table(valpred,valreel)
  instance$nomclass <- rownames(instance$mc)
  instance$nbclasse <- d

  #Selon le nombre de valeur que prendre la variable cible
  if(c == 2){
    #Calcul des éléments de la matrice de confusion
    data <- instance$mc
    positive <- rownames(data)[1]
    negative <- instance$nomclass[!(instance$nomclass %in% positive)]
    tp <- data[positive,positive]
    tn <- data[negative,negative]
    fp <- data[positive,negative]
    fn <- data[negative,positive]

    #On crée un tableau avec les indicateurs calculé sur la matrice de confusion
    ind <- matrix(NA,nrow=7,ncol=1,dimnames = list(c("Erreur","Accuracy","Précision","Sensibilite", "Specificity", "Balanced Accuracy", "F1"),
                                                   c("Valeur")))

    ind["Erreur",] <- 1.0-sum(diag(instance$mc))/sum(instance$mc) #erreur:Performance globale du modèle
    ind["Accuracy",] <- sum(diag(instance$mc))/sum(instance$mc) #Accuracy:Performance globale du modèle

    ind["Précision",] <- (sum(tp))/(sum(tp+fp)) #Précision : À quel point les prédictions positives sont précises
    ind["Sensibilite",] <- (sum(tp))/(sum(tp+fn))#Ou rappel : Couverture des observations vraiment positives
    ind["Specificity",] <- (sum(tn))/(sum(tn+fp))#Spécificité : Couverture des observations vraiment négatives
    ind["Balanced Accuracy",] <- (ind["Sensibilite",]+ind["Specificity",])/2 #précision équilibrée : Performance globale du modèle,lorsque les classes sont déséquilibrées
    ind["F1",]<- (2*tp)/(2*tp+fp+fn)#Indicateur hybride utilisé pour les classes non-balancées
    instance$indicateurs <- ind
  } else {
    # Pour le cas ou le nombre de class > 2 alors on calcul les indicateurs pour chaque classe
    ind <- matrix(NA,nrow=7,ncol=instance$nbclasse,dimnames = list(c("Erreur","Accuracy","Précision","Sensibilite", "Specificity", "Balanced Accuracy", "F1"),instance$nomclass))
    j=1
    ind["Erreur",] <- 1.0-sum(diag(instance$mc))/sum(instance$mc)
    ind["Accuracy",] <- sum(diag(instance$mc))/sum(instance$mc)
    for(i in seq(along = instance$nomclass)) {
      data <- instance$mc
      rs <- rowSums(data)
      cs <- colSums(data)
      tp <- data[i,i]
      fp <- cs[i] - tp
      fn <- rs[i] - tp
      tn <- sum(data) - (tp+fp+fn)

      ind["Précision",j] <- (sum(tp))/(sum(tp+fp))
      ind["Sensibilite",j] <- (sum(tp))/(sum(tp+fn))
      ind["Specificity",j] <- (sum(tn))/(sum(tn+fp))
      ind["Balanced Accuracy",j] <- (ind["Sensibilite",j]+ind["Specificity",j])/2
      ind["F1",j]<- (2*tp)/(2*tp+fp+fn)
      j=j+1
    }
    #On récupère la matrice pour le mettre dans l'attribut indicateurs de l'objet
    instance$indicateurs <- ind
  }
  class(instance) <- "metrics"
  #La fonction renvoie notre instance de manirère à créer un objet de type metrics
  return(instance)
}


#' Surcharge de print pour l'affichage de l'objet "metrics"
#'
#' @param obj La fonction print affichera la matrice de confusion et les indicateurs lorsque elle recoit un paramètre de type metrics
#'
#' @return
#' @export
#'
#' @examples
print.metrics <- function(obj){
  print("Matrice de confusion et indicateurs")
  print(obj$mc)
  print(obj$indicateurs)
}


#' Fonction qui permet de comparer le resultat de deux clustering
#'
#' @param object1 Le premier objet suite à un premier clustering
#' @param object2 Le second objet suite à un premier clustering
#'
#' @return
#' @import ggplot2 ggpubr
#' @export
#'
#' @examples
compareRes <- function(object1,object2){
  if (object1$nbclasse != object2$nbclasse){stop("Les deux objets n'ont pas le même nombre de classe")}
  if(object1$nbclasse == 2){
    temp1<-as.data.frame(object1$indicateurs)
    temp2 <-as.data.frame(object2$indicateurs)
    names(temp2) <- "Valeurs_obj2"
    dt <- cbind(temp1,temp2)
    remove <- c("Erreur","Accuracy")
    dt <- dt[!(row.names(dt) %in% remove), ]
    ggballoonplot(dt, fill = "value",shape=23)
  } else {
    temp1<-as.data.frame(object1$indicateurs)
    temp2 <-as.data.frame(object2$indicateurs)

    moy1 <- as.data.frame(rowMeans(temp1))
    moy2 <- as.data.frame(rowMeans(temp2))


    dt <- data.frame(x = moy1, y = moy2)
    colnames(dt) <- c("Moyenne Objet1","Moyenne Objet2")
    remove <- c("Erreur","Accuracy")
    dt <- dt[!(row.names(dt) %in% remove), ]

    ggballoonplot(dt, fill = "value",shape=23)
  }
}

#' Graphique Matrice de confusion
#'
#' @param objet #Variable de type metrics
#'
#' @return
#' @import dplyr ggplot2
#' @export
#'
#' @examples
ggMatConf <- function(objet){

  table <- data.frame(objet$mc)
  plotTable <- table %>%
    mutate(goodbad = ifelse(table$valpred == table$valreel, "good", "bad")) %>%
    group_by(valreel) %>%
    mutate(prop = Freq/sum(Freq))
  ggplot(data = plotTable, mapping = aes(x = valreel, y = valpred, fill = goodbad, alpha = prop)) +
    geom_tile() +
    geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
    scale_fill_manual(values = c(good = "green", bad = "red")) +
    theme_bw() +
    xlim(rev(levels(table$valreel)))
}
