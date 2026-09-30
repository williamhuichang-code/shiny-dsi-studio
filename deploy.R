# =================================================================================
# deploy.R  -  publish DSI Studio to shinyapps.io
# =================================================================================
#
# One-time setup:
#   1. install.packages("rsconnect")
#   2. Sign in at https://www.shinyapps.io, open Account > Tokens > Show,
#      copy the rsconnect::setAccountInfo(...) line and run it once in R.
#
# Then, from this folder, run:   source("deploy.R")
# Re-run it whenever you change the app; it updates the same URL.

rsconnect::deployApp(
  appDir      = ".",
  appName     = "dsi-studio",
  appTitle    = "DSI Studio",
  appFiles    = c("global.R", "ui.R", "server.R", "modules", "data", "SavedModels"),
  forceUpdate = TRUE
)
