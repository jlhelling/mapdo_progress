#' render quarto files into output-folder
#'
#' @param report_file quarto document to be rendered
#'
#' @importFrom quarto quarto_render
#' 
#'
#' @examples
#' render_report("reports/2024-06-01-report.qmd")
render_report <- function(report_file) {
  output_file <- paste0("output/", sub(".qmd$", ".html", basename(report_file)))
  quarto::quarto_render(report_file, output_file = output_file)
}

