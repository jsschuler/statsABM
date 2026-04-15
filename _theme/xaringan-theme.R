# ============================================================
# xaringan-theme.R — xaringanthemer scaffold for slide decks
# PLACEHOLDER SCAFFOLD — colors and font weights are placeholders
# for post-content refinement. Run this script once to regenerate
# xaringan-custom.css after any theme changes.
#
# Usage: source("_theme/xaringan-theme.R")  (run once, not at render time)
# Output: _theme/xaringan-custom.css
# ============================================================

library(xaringanthemer)

style_mono_accent(
  # Base colors — placeholders, to be refined
  base_color              = "#2D5F8A",
  white_color             = "#FAFAF7",
  black_color             = "#1C1C1E",
  background_color        = "#FAFAF7",
  header_color            = "#1C1C1E",
  text_color              = "#1C1C1E",
  link_color              = "#2D5F8A",
  text_slide_number_color = "#AAAAAA",

  # Typography
  header_font_google  = google_font("Raleway", "300, 500, 700"),
  text_font_google    = google_font("Source Serif 4", "400, 400i, 600"),
  code_font_google    = google_font("Fira Code"),

  # Sizing
  text_font_size      = "1.05rem",
  header_h1_font_size = "2.2rem",
  header_h2_font_size = "1.7rem",
  header_h3_font_size = "1.3rem",
  code_font_size      = "0.85rem",

  # Output
  outfile = "_theme/xaringan-custom.css"
)
