sample_dat <- readxl::read_excel("/Users/guituantuan/Desktop/R_packages/MNet/vignettes/data1/can-21-2745_supplementary_table_s1_supps1.xlsx") %>%
  as.data.frame() %>%
  dplyr::filter(!is.na(`Sample Name`)) %>%
  dplyr::filter(!grepl("Normal",`Sample Name`)) %>%
  dplyr::select(`Sample Name`,`Cluster Classification`)

gene_dat <- data.table::fread("/Users/guituantuan/Desktop/R_packages/MNet/vignettes/data1/40n-cleaned_logtrans_70flt.txt") %>%
  as.data.frame()

name_new <- str_split(names(gene_dat),"[|]",simplify =TRUE)
name_new[1,3]=name_new[1,1]
names(gene_dat) <- name_new[,3]

gene_dat1 <- gene_dat %>%
  dplyr::distinct(CLID,.keep_all = TRUE) %>%
  tibble::column_to_rownames("CLID") %>%
  dplyr::select_if(!grepl("^N", names(.))) 

names(gene_dat1) <- gsub("TNBC","1TNBC-",names(gene_dat1))
names(gene_dat1) <- gsub("^T","ER-",names(gene_dat1))
names(gene_dat1) <- gsub("1TNBC","TNBC",names(gene_dat1))
names(gene_dat1) <- gsub("PDX1","WHIM30-1",names(gene_dat1))
names(gene_dat1) <- gsub("PDX2","WHIM30-2",names(gene_dat1))
names(gene_dat1) <- gsub("PDX3","WHIM30-3",names(gene_dat1))
names(gene_dat1) <- gsub("PDX4","WHIM30-4",names(gene_dat1))
names(gene_dat1) <- gsub("PDX5","WHIM2-1",names(gene_dat1))
names(gene_dat1) <- gsub("PDX6","WHIM2-2",names(gene_dat1))
names(gene_dat1) <- gsub("PDX7","WHIM2-3",names(gene_dat1))

gene_dat <- gene_dat1 %>%
  dplyr::select(sample_dat$`Sample Name`) %>%
  exp()
save(gene_dat,file="data/gene_dat.rda")

group <- sample_dat$`Cluster Classification`
group[which(group=="Cluster 1")] <- "tumor"
group[which(group=="Cluster 2")] <- "normal"

annotation <- readxl::read_excel("/Users/guituantuan/Desktop/R_packages/MNet/vignettes/data1/can-21-2745_supplementary_table_s2_supps2.xlsx",sheet=3) %>%
  as.data.frame() %>%
  dplyr::filter(KEGG!= "NA") %>%
  dplyr::select(`Metabolite Name`,KEGG)

meta_dat <- readxl::read_excel("/Users/guituantuan/Desktop/R_packages/MNet/vignettes/data1/can-21-2745_supplementary_table_s2_supps2.xlsx",sheet=1) %>%
  as.data.frame() %>%
  dplyr::select(-`Chemical Formaula`,-MZ,-HMDB,-KEGG,-PubChem) %>%
  tibble::column_to_rownames("Metabolite Name") %>%
  dplyr::select(sample_dat$`Sample Name`) %>%
  log2() %>%
  tibble::rownames_to_column(var="Metabolite Name") %>%
  dplyr::inner_join(annotation,by="Metabolite Name") %>%
  dplyr::select(-`Metabolite Name`) %>%
  tibble::column_to_rownames("KEGG")
save(meta_dat,file="data/meta_dat.rda")

save(group,file="data/group.rda")

