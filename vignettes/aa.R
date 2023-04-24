all_dat <- diff_gene

all_dat1 <- all_dat %>%
  dplyr::filter(logFC>0.58) %>%
  dplyr::filter(adj.P.Val < 0.05)

result <- PathwayAnalysis(all_dat$name,out="gene",p_cutoff=0.01)
#result$gp
#View(result$output)
ggsave("result_v0131/PE_gene1.png",result$gp,width=100,height = 20,limitsize = FALSE)
ggsave("result_v0131/PE_gene1.pdf",result$gp,width=100,height = 20,limitsize = FALSE)

kegg_pathway_uniq <- PathwayExtendData %>%
  dplyr::select(kegg_pathwayname,kegg_category) %>%
  dplyr::rename("PATHWAY"="kegg_pathwayname") %>%
  dplyr::rename("pathway_type"="kegg_category") %>%
  unique()

result_1 <- result$output %>%
  dplyr::filter(pvalue<0.01) %>%
  dplyr::arrange(pvalue) %>%
  dplyr::left_join(kegg_pathway_uniq,by=c("name"="PATHWAY"))

result_1$name <- factor(result_1$name,levels = rev(result_1$name))
result_1$pathway_type <- factor(result_1$pathway_type,levels=unique(kegg_pathway_uniq$pathway_type))

p1 <- ggplot(result_1,aes(name,-log10(pvalue)))+
 # geom_bar(stat="identity",fill=RColorBrewer::brewer.pal(12,"Set3")[5],color="black")+
  geom_bar(stat="identity",aes(fill=pathway_type))+
  scale_fill_manual(values=brewer.pal(11, "Set3"),
                    breaks=unique(kegg_pathway_uniq$pathway_type))+
  coord_flip()+
  theme_bw()+
  labs(x=NULL)

ggsave("result_v0131/PE_gene_cluster1.pdf",p1,width=10,height = 10)
ggsave("result_v0131/PE_gene_cluster1.png",p1,width=10,height = 10)

all_dat2 <- all_dat %>%
  dplyr::filter(logFC < -0.58) %>%
  dplyr::filter(adj.P.Val < 0.05)

result <- PathwayAnalysis(all_dat2$name,out="gene",p_cutoff=0.01)
ggsave("result_v0131/PE_gene2.png",result$gp)
ggsave("result_v0131/PE_gene2.pdf",result$gp)

result_2 <- result$output %>%
  dplyr::filter(pvalue<0.01) %>%
  dplyr::arrange(pvalue) %>%
dplyr::left_join(kegg_pathway_uniq,by=c("name"="PATHWAY"))

result_2$name <- factor(result_2$name,levels = rev(result_2$name))
result_2$pathway_type <- factor(result_2$pathway_type,levels=unique(kegg_pathway_uniq$pathway_type))

p2 <- ggplot(result_2,aes(name,-log10(pvalue)))+
 # geom_bar(stat="identity",fill=RColorBrewer::brewer.pal(12,"Set3")[5],color="black")+
  geom_bar(stat="identity",aes(fill=pathway_type))+
  scale_fill_manual(values=brewer.pal(11, "Set3"),
                    breaks=unique(kegg_pathway_uniq$pathway_type))+
  coord_flip()+
  theme_bw()+
  labs(x=NULL)

ggsave("result_v0131/PE_gene_cluster2.pdf",p2,width=10,height = 10)
ggsave("result_v0131/PE_gene_cluster2.png",p2,width=10,height = 10)

p <- cowplot::plot_grid(plotlist = list(p1,p2))
ggsave("result_v0131/PE_gene.png",p,width=20,height = 10)
ggsave("result_v0131/PE_gene.pdf",p,width=20,height = 10)

