#!/usr/bin/env bash
# build.sh — Render the course book and prepend the cover via ghostscript.
#
# Usage:
#   cd book && bash build.sh
#
# Requires: R (with rmarkdown), ghostscript (gs)

set -euo pipefail

BOOK_RMD="abm_course_book.Rmd"
BOOK_PDF="abm_course_book.pdf"
COVER_PDF="cover.pdf"
OUT_PDF="abm_for_statisticians.pdf"

# 1. Render the cover (if cover.R has changed or cover.pdf is absent)
if [ ! -f "$COVER_PDF" ] || [ "cover.R" -nt "$COVER_PDF" ]; then
  echo "→ Rendering cover..."
  Rscript cover.R
fi

# 2. Render the book
echo "→ Rendering book..."
Rscript -e "rmarkdown::render('${BOOK_RMD}', output_format = 'pdf_document')"

# 3. Copy to output — cover is already embedded via \maketitle in preamble.tex
echo "→ Copying to output..."
cp "${BOOK_PDF}" "${OUT_PDF}"

echo "✓ Done: ${OUT_PDF}"
