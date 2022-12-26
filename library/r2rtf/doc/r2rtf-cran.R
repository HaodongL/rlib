## ---- eval = FALSE------------------------------------------------------------
#  library(dplyr)
#  library(r2rtf)
#  
#  head(iris) %>%
#    rtf_body() %>% # Step 1 Add attributes
#    rtf_encode() %>% # Step 2 Convert attributes to RTF encode
#    write_rtf(file = "ex-tbl.rtf") # Step 3 Write to a .rtf file

