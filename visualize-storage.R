library(plotly)

df <- read.csv("~/smb_folder_sizes.csv", stringsAsFactors = FALSE)
df$size_bytes <- as.numeric(gsub("[^0-9]", "", df$size_bytes))
df <- df[!is.na(df$size_bytes), ]

plot_ly(
  df,
  type = "treemap",
  labels = ~top,
  parents = "",
  values = ~size_bytes,
  textinfo = "label+value+percent entry"
)


