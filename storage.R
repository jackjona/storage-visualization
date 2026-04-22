path <- "/Volumes/Coffer/Media" 

cat("Scanning SMB share...\n")
cat("This may take a while depending on the number of files.\n\n")

all_paths <- fs::dir_ls(path, recurse = TRUE, type = "file")

total <- length(all_paths)
cat("Found", total, "files.\n\n")

pb <- txtProgressBar(min = 0, max = total, style = 3)
start_time <- Sys.time()

info_list <- vector("list", total)

for (i in seq_along(all_paths)) {
  
  info_list[[i]] <- fs::file_info(all_paths[i])
  setTxtProgressBar(pb, i)
  
  if (i %% 500 == 0 || i == total) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    rate <- i / elapsed
    remaining <- (total - i) / rate
    
    cat(sprintf("\nProcessed %d/%d files | Elapsed: %.1fs | ETA: %.1fs\n",
                i, total, elapsed, remaining))
  }
}

close(pb)
cat("\nScan complete.\n\n")

df <- bind_rows(info_list)

df$rel <- fs::path_rel(df$path, start = path)
df$parts <- strsplit(df$rel, "/")
df$top <- sapply(df$parts, function(x) x[1])

folder_sizes <- df %>%
  group_by(top) %>%
  summarise(size_bytes = sum(size, na.rm = TRUE)) %>%
  arrange(desc(size_bytes))

output_file <- "~/smb_folder_sizes.csv"
write.csv(folder_sizes, output_file, row.names = FALSE)

cat("CSV exported to:", output_file, "\n")
