% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pZscore.R
\name{pZscore}
\alias{pZscore}
\title{Plot the z-score figure}
\usage{
pZscore(
  mydata,
  group,
  tumor_color = "#d53e4f",
  normal_color = "#7FC8A9",
  shape_size = 3,
  ysize = 5
)
}
\arguments{
\item{mydata}{the row is the metabolites, and the column is the sample}

\item{group}{the sample's group information}
}
\value{
test
}
\description{
Plot the z-score figure
}
\examples{
diff_result <- DM(2**meta_dat,group)
# filter the differential metabolites by default fold change >1.5 or < 1/1.5 ,fdr < 0.05 and VIP>1
diff_result_filter <- diff_result \%>\%
  dplyr::filter(fold_change >1.3 | fold_change < 1/1.3) \%>\%
  dplyr::filter(fdr_wilcox<0.1) \%>\%
  dplyr::filter(vip>0.8)
meta_dat_diff <- meta_dat[rownames(meta_dat) \%in\% diff_result_filter$name,]
p_zscore <- pZscore(meta_dat_diff,group)
}
