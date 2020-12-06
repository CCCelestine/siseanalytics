#' Graphique resultat kmeans couplé acp
#'
#' @param vars DataFrame contenant les variables quantitatives
#' @param reel DataFrame contenant les variables quantitatives
#' @param resKM Resultats d'un clustering
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
