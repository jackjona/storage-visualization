library(plotly)
library(dplyr)
library(fs)
library(data.table)

df <- data.table::fread("./data/smb_folder_sizes_deep.csv")
df <- df[size_bytes > 0]

ROOT <- "__root__"

# ── Trim each path component to catch "Name (2019) /sub" trailing spaces ──────
trim_path_components <- function(paths) {
  vapply(paths, function(p) {
    parts <- strsplit(p, "/", fixed = TRUE)[[1]]
    paste(trimws(parts), collapse = "/")
  }, character(1), USE.NAMES = FALSE)
}

df[, folder := trim_path_components(folder)]

# Deduplicate after trimming — two paths may now be identical
df <- df[, .(size_bytes = sum(size_bytes), file_count = sum(file_count)), by = folder]

df[, label  := sub(".*/", "", folder)]
df[, parent := sub("/[^/]+$", "", folder)]
df[, parent := ifelse(parent == folder, ROOT, parent)]

orphans <- df[!parent %in% c(df$folder, ROOT)]
cat(if (nrow(orphans) == 0) "✔ No orphans\n" else sprintf("WARNING: %d orphans remain\n", nrow(orphans)))

# ── Root node — sum only leaf nodes to avoid double-counting ──────────────────
leaf_nodes  <- df[!folder %in% df$parent]
total_bytes <- sum(leaf_nodes$size_bytes)
total_files <- sum(leaf_nodes$file_count)

root <- data.table(
  folder     = ROOT,
  size_bytes = total_bytes,
  file_count = total_files,
  label      = "Media",
  parent     = ""
)
df_plot <- rbind(root, df, fill = TRUE)

# ── Labels & hover ────────────────────────────────────────────────────────────
df_plot[, size_human := as.character(fs::fs_bytes(size_bytes))]
df_plot[, pct        := round(size_bytes / total_bytes * 100, 2)]
df_plot[, hover := ifelse(
  parent == "",
  sprintf("<b>%s</b><br>%s total<br>%s files",
          label, size_human, format(file_count, big.mark = ",")),
  sprintf("<b>%s</b><br>Size: %s<br>Files: %s<br>Share: %s%%",
          label, size_human,
          format(file_count, big.mark = ","), pct)
)]

# ── Depth-based colour ────────────────────────────────────────────────────────
df_plot[, depth := ifelse(parent == "", 0L,
                          lengths(strsplit(folder, "/", fixed = TRUE)))]
depth_pal <- c("#0d1b2a","#1b4f72","#1a6b8a","#1abc9c","#48d1a8","#a8e6cf","#d4f5e9")
df_plot[, colour := depth_pal[pmin(depth + 1L, length(depth_pal))]]

# ── Plot ──────────────────────────────────────────────────────────────────────
plot_ly(
  data          = df_plot,
  type          = "treemap",
  ids           = ~folder,
  labels        = ~label,
  parents       = ~parent,
  values        = ~size_bytes,
  customdata    = ~hover,
  hovertemplate = "%{customdata}<extra></extra>",
  text          = ~paste0(size_human, "\n", pct, "%"),
  textinfo      = "label+text",
  textfont      = list(family = "JetBrains Mono, monospace", size = 12, color = "#ffffff"),
  marker        = list(
    colors = ~colour,
    line   = list(width = 2, color = "#050e17"),
    pad    = list(t = 22, l = 4, r = 4, b = 4)
  ),
  pathbar = list(
    visible   = TRUE,
    thickness = 28,
    textfont  = list(family = "JetBrains Mono, monospace", size = 12)
  ),
  maxdepth = 3,
  tiling   = list(packing = "squarify", squarifyratio = 1.618)
) |>
  layout(
    title = list(
      text = sprintf(
        "<b>Media Storage</b>  <span style='font-size:13px;color:#888'>%s across %s files</span>",
        fs::fs_bytes(total_bytes),
        format(total_files, big.mark = ",")),
      font = list(family = "Georgia, serif", size = 20, color = "#ffffff"),
      x = 0.01
    ),
    margin        = list(t = 60, l = 10, r = 10, b = 10),
    paper_bgcolor = "#0d1b2a",
    plot_bgcolor  = "#0d1b2a",
    font          = list(color = "#ffffff")
  ) |>
  config(
    modeBarButtons       = list(list("toImage")),
    toImageButtonOptions = list(format = "png", filename = "media_storage", scale = 2),
    displaylogo          = FALSE
  )