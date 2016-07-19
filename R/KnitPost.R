#' simply knits .md to .rmd
#'
#' http://jfisher-usgs.github.io/
#'
#' @param input file name of .Rmd
#' @param base.url some other thing
#' @return .md into directory
#' @author j Fisher
#' @export
#'
KnitPost <- function(input, base.url = "/") {
  require(knitr)
  opts_knit$set(base.url = base.url)
  fig.path <- paste0("figs/", sub(".Rmd$", "", basename(input)), "/")
  opts_chunk$set(fig.path = fig.path)
  opts_chunk$set(fig.cap = "center")
  render_jekyll()
  knit(input, envir = parent.frame())
}
