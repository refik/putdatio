library(dplyr)

# To have default data when you don't
# want to wait for the query
sizes_log <- putdatio::file_sizes_db()
devtools::use_data(sizes_log, internal = TRUE)
