library("dplyr")

# market capitalization weight
mktcap <- openxlsx::read.xlsx("data-raw/assetall.xlsx", sheet = 1)

djia_col_names <- mktcap$symbol

mktcap <- mktcap %>%
  dplyr::select(-symbol) %>%
  as.matrix() %>%
  t() %>%
  `colnames<-`(djia_col_names)

usethis::use_data(mktcap, overwrite = TRUE)

# implied equilibrium weight
impw <- openxlsx::read.xlsx("data-raw/assetall.xlsx", sheet = 2) %>%
  dplyr::select(-symbol) %>%
  as.matrix() %>%
  `rownames<-`(djia_col_names)

usethis::use_data(impw, overwrite = TRUE)

# covariance matrix of historical returns
covmat <- openxlsx::read.xlsx("data-raw/assetall.xlsx", sheet = 3) %>%
  dplyr::select(-X1) %>%
  as.matrix()

usethis::use_data(covmat, overwrite = TRUE)

# views
pmat <- openxlsx::read.xlsx("data-raw/assetall.xlsx", sheet = 4) %>%
  as.matrix() %>%
  `colnames<-`(djia_col_names)

usethis::use_data(pmat, overwrite = TRUE)
