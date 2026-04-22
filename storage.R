library(dplyr)
library(processx)
library(fs)
library(vroom)
library(data.table)

path <- "/Volumes/Coffer/Media"

# ── Spinner ───────────────────────────────────────────────────────────────────
frames     <- c("⠋","⠙","⠹","⠸","⠼","⠴","⠦","⠧","⠇","⠏")
frame_i    <- 1L
start_time <- Sys.time()

spinner_tick <- function(label = "") {
  elapsed  <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  mins     <- floor(elapsed / 60)
  secs     <- elapsed %% 60
  time_str <- if (mins > 0) sprintf("%dm %04.1fs", mins, secs) else sprintf("%.1fs", secs)
  cat(sprintf("\r  %s  %s  [%s]     ", frames[[frame_i]], label, time_str))
  frame_i <<- (frame_i %% length(frames)) + 1L
}

stamp <- function(label) {
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("\r  ✔  %s  [%.1fs]\n", label, elapsed))
}

# ── STEP 1: scan with macOS `stat` via find ───────────────────────────────────
# BSD find doesn't support -printf, so we pipe through xargs + stat instead.
# stat -f "%z%t%N" emits: size_bytes<TAB>full_path — native macOS, no GNU tools needed.
tmp_tsv <- tempfile(fileext = ".tsv")
tmp_sh  <- tempfile(fileext = ".sh")

# Shell script so processx can run the pipe cleanly
writeLines(
  sprintf('find "%s" -type f -exec stat -f "%%z\t%%N" {} + > "%s"', path, tmp_tsv),
  tmp_sh
)

bg <- processx::process$new("bash", args = tmp_sh, stdout = NULL, stderr = NULL)

while (bg$is_alive()) {
  spinner_tick("Scanning with stat (macOS native)")
  Sys.sleep(0.08)
}
file.remove(tmp_sh)
stamp("Scan complete")

# ── STEP 2: parse ─────────────────────────────────────────────────────────────
cat("  ⠿  Parsing results...")

df <- vroom::vroom(
  tmp_tsv,
  delim     = "\t",
  col_names = c("size_bytes", "full_path"),
  col_types = vroom::cols(size_bytes = "d", full_path = "c"),
  progress  = FALSE,
  altrep    = TRUE
)
file.remove(tmp_tsv)
stamp(sprintf("Parsed %s files", format(nrow(df), big.mark = ",")))

# ── STEP 3: extract top-level folder ─────────────────────────────────────────
cat("  ⠿  Aggregating...")

prefix_len <- nchar(path) + 2L
next_slash  <- regexpr("/", substring(df$full_path, prefix_len), fixed = TRUE)
df$top      <- ifelse(
  next_slash == -1L,
  substring(df$full_path, prefix_len),
  substring(df$full_path, prefix_len, prefix_len + next_slash - 2L)
)

dt <- data.table::as.data.table(df)
folder_sizes <- dt[,
                   .(size_bytes = sum(size_bytes, na.rm = TRUE), file_count = .N),
                   by = top
][order(-size_bytes)]

folder_sizes[, size_human := fs::fs_bytes(size_bytes)]
stamp(sprintf("Aggregated %s top-level folders", format(nrow(folder_sizes), big.mark = ",")))

# ── STEP 4: export ────────────────────────────────────────────────────────────
cat("  ⠿  Writing CSV...")
output_file <- path.expand("~/smb_folder_sizes.csv")
data.table::fwrite(folder_sizes, output_file)
stamp(sprintf("CSV saved → %s", output_file))

cat("\n")
print(folder_sizes)