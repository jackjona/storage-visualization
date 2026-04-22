library(processx)
library(vroom)
library(data.table)
library(fs)

path <- "/Volumes/Coffer/Media"

frames  <- c("⠋","⠙","⠹","⠸","⠼","⠴","⠦","⠧","⠇","⠏")
frame_i <- 1L
start_time <- Sys.time()

spinner_tick <- function(label = "") {
  elapsed  <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  time_str <- if (elapsed >= 60) sprintf("%dm %04.1fs", floor(elapsed/60), elapsed%%60) else sprintf("%.1fs", elapsed)
  cat(sprintf("\r  %s  %s  [%s]     ", frames[[frame_i]], label, time_str))
  frame_i <<- (frame_i %% length(frames)) + 1L
}
stamp <- function(label) {
  cat(sprintf("\r  ✔  %s  [%.1fs]\n", label,
              as.numeric(difftime(Sys.time(), start_time, units = "secs"))))
}

# ── Scan ──────────────────────────────────────────────────────────────────────
tmp_tsv <- tempfile(fileext = ".tsv")
tmp_sh  <- tempfile(fileext = ".sh")
writeLines(
  sprintf('find "%s" -type f -exec stat -f "%%z\t%%N" {} + > "%s"', path, tmp_tsv),
  tmp_sh
)
bg <- processx::process$new("bash", args = tmp_sh)
while (bg$is_alive()) { spinner_tick("Scanning"); Sys.sleep(0.08) }
file.remove(tmp_sh)
stamp("Scan complete")

# ── Parse ─────────────────────────────────────────────────────────────────────
cat("  ⠿  Parsing...")
df <- vroom::vroom(tmp_tsv, delim = "\t", col_names = c("size_bytes", "full_path"),
                   col_types = vroom::cols(size_bytes = "d", full_path = "c"),
                   progress = FALSE, altrep = TRUE)
file.remove(tmp_tsv)
stamp(sprintf("Parsed %s files", format(nrow(df), big.mark = ",")))

# ── Build ALL ancestor rows ───────────────────────────────────────────────────
# For every file, emit a row for EACH of its ancestor folders.
# e.g. /Volumes/Coffer/Media/Movies/Action/foo.mkv contributes to:
#   Movies, Movies/Action
cat("  ⠿  Building folder hierarchy...")
dt <- data.table::as.data.table(df)
dt[, rel := substring(full_path, nchar(path) + 2L)]

# Expand each file into all its ancestor paths
expand_ancestors <- function(rel_paths, sizes) {
  out_path <- character(0)
  out_size <- numeric(0)
  for (i in seq_along(rel_paths)) {
    parts <- strsplit(rel_paths[i], "/", fixed = TRUE)[[1]]
    n     <- length(parts) - 1L          # folders only, not the filename
    if (n < 1L) next
    for (depth in seq_len(n)) {
      out_path <- c(out_path, paste(parts[seq_len(depth)], collapse = "/"))
      out_size <- c(out_size, sizes[i])
    }
  }
  data.table(folder = out_path, size_bytes = out_size)
}

folder_dt <- expand_ancestors(dt$rel, dt$size_bytes)
folder_sizes <- folder_dt[, .(size_bytes = sum(size_bytes), file_count = .N), by = folder]

stamp(sprintf("Built %s folder nodes", format(nrow(folder_sizes), big.mark = ",")))

data.table::fwrite(folder_sizes, path.expand("./smb_folder_sizes_deep.csv"))
cat("  ✔  Saved → ./smb_folder_sizes_deep.csv\n")