library(plotly)
library(dplyr)
library(fs)

df <- read.csv("~/smb_folder_sizes.csv", stringsAsFactors = FALSE)
df$size_bytes <- as.numeric(df$size_bytes)
df <- df[!is.na(df$size_bytes) & df$size_bytes > 0, ]

# ── Derived labels ────────────────────────────────────────────────────────────
df$size_human <- fs::fs_bytes(df$size_bytes)
df$pct        <- round(df$size_bytes / sum(df$size_bytes) * 100, 1)
df$file_count <- as.integer(df$file_count)

total_bytes <- sum(df$size_bytes)
total_label <- paste0("Total: ", fs::fs_bytes(total_bytes))

# Add a synthetic root node so all folders nest under one parent
root_row <- data.frame(
  top         = total_label,
  size_bytes  = total_bytes,
  file_count  = sum(df$file_count),
  size_human  = fs::fs_bytes(total_bytes),
  pct         = 100,
  stringsAsFactors = FALSE
)
df$parent <- total_label
root_row$parent <- ""

df_plot <- bind_rows(root_row, df)

# ── Rich hover text ───────────────────────────────────────────────────────────
df_plot$hover <- ifelse(
  df_plot$parent == "",
  sprintf("<b>%s</b><br>%s files total",
          df_plot$top,
          format(df_plot$file_count, big.mark = ",")),
  sprintf("<b>%s</b><br>Size:  %s<br>Files: %s<br>Share: %s%%",
          df_plot$top,
          df_plot$size_human,
          format(df_plot$file_count, big.mark = ","),
          df_plot$pct)
)

# ── Colour scale: dark navy → vivid teal ─────────────────────────────────────
n      <- nrow(df)
pal    <- colorRampPalette(c("#0d1b2a", "#1b4f72", "#1abc9c", "#a8e6cf"))(n)
# Sort by size so largest gets the most saturated colour
df_sorted_idx          <- order(df$size_bytes, decreasing = FALSE)
colour_map             <- character(nrow(df_plot))
colour_map[1]          <- "#0d1b2a"          # root node
colour_map[-1][df_sorted_idx] <- pal

plot_ly(
  data        = df_plot,
  type        = "treemap",
  labels      = ~top,
  parents     = ~parent,
  values      = ~size_bytes,
  customdata  = ~hover,
  hovertemplate = "%{customdata}<extra></extra>",
  
  # Show human-readable size + % inside each tile
  text        = ~paste0(size_human, "\n", pct, "%"),
  textinfo    = "label+text",
  textfont    = list(family = "JetBrains Mono, monospace", size = 13, color = "#ffffff"),
  
  marker = list(
    colors     = colour_map,
    line       = list(width = 2, color = "#0a0a0a"),
    pad        = list(t = 22, l = 4, r = 4, b = 4)
  ),
  
  pathbar = list(
    visible   = TRUE,
    thickness = 26,
    textfont  = list(family = "JetBrains Mono, monospace", size = 12)
  ),
  
  tiling = list(packing = "squarify", squarifyratio = 1.618)  # golden ratio
) |>
  layout(
    title = list(
      text = paste0("<b>Media Storage</b>  <span style='font-size:13px;color:#888'>",
                    total_label, " across ",
                    format(sum(df$file_count), big.mark = ","), " files</span>"),
      font = list(family = "Georgia, serif", size = 20, color = "#1a1a1a"),
      x    = 0.01
    ),
    margin     = list(t = 60, l = 10, r = 10, b = 10),
    paper_bgcolor = "#0d1b2a",
    plot_bgcolor  = "#0d1b2a",
    font = list(color = "#ffffff")
  ) |>
  config(
    displayModeBar  = TRUE,
    modeBarButtons  = list(list("toImage")),
    toImageButtonOptions = list(
      format   = "png",
      filename = "media_storage",
      scale    = 2          # 2× resolution export
    ),
    displaylogo = FALSE
  )