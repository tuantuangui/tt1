#' the heatmap of differential metabolites in the data
#'
#' @param mydata_diff the data of differential metabolites' value,the row is
#' metabolites, and the column is the samples' names.
#' @param group character vector,the data's group information, must tumor and normal
#' @param fontsize_row numeric, the value of row's fontsize, default is 5
#' @param fontsize_col numeric, the value of column's fontsize, default is 4
#' @param clustering_method the method that cluster,default is complete
#' @param clustering_distance_cols the distance for cluster,default is correlation
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
#' p_heatmap <- pHeatmap(meta_dat_diff,group,fontsize_row=5,fontsize_col=4,clustering_method="ward.D",clustering_distance_cols="correlation")
pHeatmap <- function(mydata_diff,group,fontsize_row=5,fontsize_col=4,clustering_method="complete",clustering_distance_cols="correlation",tumor_color="#d53e4f",normal_color="#7FC8A9") {

#  mydata_diff <- mydata[which(rownames(mydata) %in% metabolites_diff$name),]

  mydata_dif_norm <- myscale(mydata_diff)

  color = c(grDevices::colorRampPalette(colors = c("blue","white"))(length(seq(round(min(mydata_dif_norm),1),-0.01,by=0.01))),
            grDevices::colorRampPalette(colors = c("white","red"))(length(seq(0,round(max(mydata_dif_norm),1),by=0.01))))

  #clustering_method = "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC)

  #euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski
  annotation_column <- data.frame(group=group)
  rownames(annotation_column) <- names(mydata_diff)

  annotation_colors = list(
    group=c(tumor=tumor_color,normal=normal_color)
  )
  p <- pheatmap::pheatmap(mydata_dif_norm,color=color,fontsize_row = fontsize_row,fontsize_col=fontsize_col,
                          annotation_col = annotation_column,annotation_colors=annotation_colors,clustering_distance_cols=clustering_distance_cols,
                          clustering_method = clustering_method,show_colnames = F)

  return(p)

}
