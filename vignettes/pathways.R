Pathways <- list()
for (i in unique(kegg_pathway$PATHWAY)) {
  kegg_1 <- c()
  kegg_pathway_1 <- kegg_pathway %>%
    dplyr::filter(PATHWAY==i)
  Pathways[[i]]=kegg_pathway_1$ENTRY
}

save(Pathways,file="~/Desktop/R_packages/MNet/data/Pathways.rda")
