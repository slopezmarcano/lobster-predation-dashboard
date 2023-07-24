library(arrow)
library(tidyverse)
      data1 <- read_and_join_all_parquet_files('/Users/s2985905/Dropbox/GithubRepos/utas-lobster-predation/data/merged') %>%
              corrected_frames() %>%
              select(event_ID, category_id, correct_frame, cx, cy) 

# Export parquet to directory called www
write_parquet(data1, sink = "output_file.parquet")

data1<-read_parquet("output_file.parquet")
