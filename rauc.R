#' @title roc curve
#' @description
#' This is a drawing ROC.
#'
#' @param tru Actual results
#' @param pred Model prediction results
#' @export

#' @return a global object ggr

#' @examples rauc(c(1,0,1,0,1,1),c(0.6,0.1,0.2,0.5,0.6,0.7))
rauc <- function(tru,pred){
  library(pROC)
  library(ggplot2)
  rocobj<-roc(tru,pred)
  auc<-round(auc(tru,pred),4)
  ggr <- ggroc(rocobj,color = "red",
        linetype = 1,
        size = 1,
        alpha = 1,
        legacy.axes = T)+
    geom_abline(intercept = 0,
                slope = 1,
                color = "grey",
                size = 1,
                linetype = 1)+
    labs(x = "False Postive Rate(1 - Specificity)",
         y = "True Positive Rate(Sensivity or Recall)")+
    annotate("text",x = 0.70,y = 0.30,
             label = paste("AUC =",auc),
             size = 5,
             family = "serif")+
    coord_cartesian(xlim = c(0,1),
                    ylim = c(0,1))+
    theme_bw()+
    theme(panel.background = element_rect(fill = "transparent"),
          axis.ticks.length = unit(0.4,"lines"),
          axis.ticks = element_line(color = "black"),
          axis.line = element_line(linewidth = 0.5,
                                   colour = "black"),
          axis.title = element_text(colour = "black",
                                    size = 10,
                                    face = "bold",
                                    family = "serif"),
          axis.text = element_text(colour = "black",
                                   size = 10,
                                   face = "bold",
                                   family = "serif"),
          text = element_text(size = 8,
                              color = "black",
                              family = "serif"))
  ggr
}
