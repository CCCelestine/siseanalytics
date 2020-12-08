#' Creation of the "metrics" object from two df containing the predicted and actual values
#'
#' @param valreel A dataframe containing the real classes
#' @param valpred A dataframe containing the classes predicted by a clustering
#'
#' @return An object of the metrics class
#' @export
#'
#' @examples
#' EvalMetrics(pred_reel_2c$val_reel,pred_reel_2c$val_pred)
#'
EvalMetrics <- function(valreel,valpred){
  #Creation des variables pour la construction de notre class
  valpred <- as.factor(valpred)
  valreel <- as.factor(valreel)
  #On recupere les taille des vecteurs et le nombre de valeur que peut prendre la variable a trouver
  a <- length(valpred)
  b <- length(valreel)
  c <- length(levels(valpred))
  d <- length(levels(valreel))
  # On verifie certaines conditions avant de donner les attributs a notre objet
  if (a !=b){stop("Les deux vecteurs rentree n'ont pas la mame taille")}
  if (c !=d){stop("Les deux vecteurs ne prennet pas le mame nombre de valeur")}
  if (c < 2 ){stop("Il doit avoir au moins deux classes pour la variable predites ")}

  #On instancie la class metrics
  instance <- list()
  instance$mc <- table(valpred,valreel)
  instance$nomclass <- rownames(instance$mc)
  instance$nbclasse <- d

  #Selon le nombre de valeur que prendre la variable d'appartenance aux classes
  if(c == 2){
    #Calcul des elements de la matrice de confusion
    data <- instance$mc
    positive <- rownames(data)[1]
    negative <- instance$nomclass[!(instance$nomclass %in% positive)]
    tp <- data[positive,positive]
    tn <- data[negative,negative]
    fp <- data[positive,negative]
    fn <- data[negative,positive]

    #On cree un tableau avec les indicateurs calcule sur la matrice de confusion
    ind <- matrix(NA,nrow=7,ncol=1,dimnames = list(c("Erreur","Accuracy","Precision","Sensibilite", "Specificity", "Balanced Accuracy", "F1"),
                                                   c("Valeur")))

    ind["Erreur",] <- 1.0-sum(diag(instance$mc))/sum(instance$mc) #erreur:Performance globale du modele
    ind["Accuracy",] <- sum(diag(instance$mc))/sum(instance$mc) #Accuracy:Performance globale du modele

    ind["Precision",] <- (sum(tp))/(sum(tp+fp)) #Precision : a quel point les predictions positives sont precises
    ind["Sensibilite",] <- (sum(tp))/(sum(tp+fn))#Ou rappel : Couverture des observations vraiment positives
    ind["Specificity",] <- (sum(tn))/(sum(tn+fp))#Specificite : Couverture des observations vraiment negatives
    ind["Balanced Accuracy",] <- (ind["Sensibilite",]+ind["Specificity",])/2 #precision equilibree : Performance globale du modele,lorsque les classes sont desequilibrees
    ind["F1",]<- (2*tp)/(2*tp+fp+fn)#Indicateur hybride utilise pour les classes non-balancees
    instance$indicateurs <- ind
  } else {
    # Pour le cas ou le nombre de class > 2 alors on calcul les indicateurs pour chaque classe
    ind <- matrix(NA,nrow=7,ncol=instance$nbclasse,dimnames = list(c("Erreur","Accuracy","Precision","Sensibilite", "Specificity", "Balanced Accuracy", "F1"),instance$nomclass))
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

      ind["Precision",j] <- (sum(tp))/(sum(tp+fp))
      ind["Sensibilite",j] <- (sum(tp))/(sum(tp+fn))
      ind["Specificity",j] <- (sum(tn))/(sum(tn+fp))
      ind["Balanced Accuracy",j] <- (ind["Sensibilite",j]+ind["Specificity",j])/2
      ind["F1",j]<- (2*tp)/(2*tp+fp+fn)
      j=j+1
    }
    #On recupere la matrice pour le mettre dans l'attribut indicateurs de l'objet
    instance$indicateurs <- ind
  }
  class(instance) <- "metrics"
  #La fonction renvoie notre instance de manirere a creer un objet de type metrics
  return(instance)
}


#' Overload of print for displaying the "metrics" object
#'
#' @param obj The print function will display the confusion matrix and the indicators when it receives a parameter of type metrics
#'
#' @export
#'
#' @examples
#' Obj2c <- EvalMetrics(pred_reel_2c$val_reel,pred_reel_2c$val_pred)
#' print(Obj2c)
print.metrics <- function(obj){
  print("Matrice de confusion et indicateurs")
  print(obj$mc)
  print(obj$indicateurs)
}


#' Function which allows to compare the result of two clustering
#'
#' @param object1 The first object following a first clustering
#' @param object2 The second object following a second clustering
#'
#' @import ggplot2 ggpubr
#' @export
#'
#' @examples
#' Obj2c <- EvalMetrics(pred_reel_2c$val_reel,pred_reel_2c$val_pred)
#' Obj2c_bis <- EvalMetrics(pred_reel_2c_bis$val_reel,pred_reel_2c_bis$val_pred)
#' compareRes(Obj2c,Obj2c_bis)
compareRes <- function(object1,object2){
  if (object1$nbclasse != object2$nbclasse){stop("Les deux objets n'ont pas le mame nombre de classe")}
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

#' Confusion Matrix Graph
#'
#' @param objet #Metrics variable
#'
#' @import dplyr ggplot2
#' @export
#'
#' @examples
#' Obj2c <- EvalMetrics(pred_reel_2c$val_reel,pred_reel_2c$val_pred)
#' ggMatConf(Obj2c)
ggMatConf <- function(objet){

  # On stock la matrice de confusion dans une variable
  table <- data.frame(objet$mc)
  # Grace a dplyr on ajoute une colonne pour les bonnes predictions et les mauvaises
  # On ajoute aussi une colonne prop pour calculer la proportion
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
