pandoc Report.md --biblatex --from markdown+implicit_figures+citations \
  --lua-filter="columns.lua" --lua-filter="diagram-generator.lua" -s --output "./report.tex"
pdflatex report.tex
biber report
pdflatex report.tex
