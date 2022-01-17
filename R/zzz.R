pdf2png <- function(filename, format = "png", outdir = here::here("graphs/png/"), dpi = 300) {
  if (!fs::dir_exists(outdir)) {
    fs::dir_create(outdir)
  }

  outfile <- fs::path(outdir, fs::path_file(fs::path_ext_set(filename, format)))

  pdftools::pdf_convert(
    pdf = filename,
    filenames = outfile,
    format = format,
    dpi = dpi,
    verbose = FALSE
  )

  invisible(outfile)
}
