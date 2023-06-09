
#' Plot the z-score figure
#'
#' @param mydata the row is the metabolites, and the column is the sample
#' @param group the sample's group information
#'
#' @return test
#' @export
#'
#' @examples
#' diff_result <- DM(2**meta_dat,group)
#' # filter the differential metabolites by default fold change >1.5 or < 1/1.5 ,fdr < 0.05 and VIP>1
#' diff_result_filter <- diff_result %>%
#'   dplyr::filter(fold_change >1.3 | fold_change < 1/1.3) %>%
#'   dplyr::filter(fdr_wilcox<0.1) %>%
#'   dplyr::filter(vip>0.8)
#' meta_dat_diff <- meta_dat[rownames(meta_dat) %in% diff_result_filter$name,]
#' p_zscore <- pZscore(meta_dat_diff,group)
pZscore <- function(mydata,group,tumor_color="#d53e4f",normal_color="#7FC8A9",shape_size=3,ysize=5) {

  label <- value <- ave <- NULL
  clinical <- data.frame(sample=names(mydata),group=group)

  mydata_norm <- myscale(mydata) %>%
    tibble::rownames_to_column(var="label")

  mydata_melt <- reshape2::melt(mydata_norm,id="label")


  mydata_melt_group <- mydata_melt %>%
    dplyr::left_join(clinical,by=c("variable"="sample"))

  mydata_melt_sort <- mydata_melt_group %>%
    dplyr::filter(group=="tumor") %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(ave=mean(value)) %>%
    dplyr::arrange(ave)

  mydata_melt_group$label <- factor(mydata_melt_group$label,levels=mydata_melt_sort$label)

  p <- ggplot2::ggplot(mydata_melt_group,ggplot2::aes(label,value,color=group))+
    ggplot2::geom_point(shape="|",size=shape_size)+
    ggplot2::scale_color_manual(values=c(normal_color,tumor_color))+
    ggplot2::labs(y="z-score")+
    ggplot2::coord_flip()+
    ggplot2::geom_hline(yintercept=0, linetype = 'dashed')+
    ggplot2::theme_bw()+
    labs(x=NULL,y=NULL)+
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),panel.grid.minor = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text( hjust = 1,size = ysize))

  return(p)

}
