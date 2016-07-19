#' KnitPost by Andrew Brooks
#'
#' How to use KnitPost
#' 1. Configure the directories at the top of KnitPost to match the file system of your blog.
#' I could have included these as parameters, but I figured I wouldn’t re-architect my blog
#' very often… so I left them hard-coded.
#'
#' 2. Create an R Markdown post and save it as a .Rmd file in rmd.path. Be sure to include the
#' proper YAML front matter for Jekyll. This tripped me up initially. I forgot to change the
#' date from the knitr style date (“Month, day YYYY”) that auto-generates when you create a
#' new .Rmd to a Jekyll style date (‘YYYY-MM-DD’).
#'
#' 3. Run KnitPost to publish your R Markdown file.
#'
#' http://brooksandrew.github.io/simpleblog/articles/blogging-with-r-markdown-and-jekyll-using-knitr/
#'
#' @param site.path
#' @param overwriteAll
#' @param overwriteOne
#'
#' @return
#' @author Andrew Brooks
#' @export
#'
#' @examples
KnitPost_AB <- function(site.path="C:/Users/asus/Documents/GitHub/Pokemon_FieldStudies/", overwriteAll=F, overwriteOne=NULL) {
  if(!'package:knitr' %in% search()) library('knitr')

  ## Blog-specific directories.  This will depend on how you organize your blog.
  site.path <- site.path # directory of jekyll blog (including trailing slash)
  rmd.path <- paste0(site.path, "_source/publish/") # directory where your Rmd-files reside (relative to base)
  fig.dir <- "{{ site.github.url  }}/assets/Rfig/" # directory to save figures DON@T KNOW IF LIQUID TAG WILL WORK
  posts.path <- paste0(site.path, "_posts") # directory for converted markdown files
  cache.path <- paste0(site.path, "_cache") # necessary for plots

  render_jekyll(highlight = "pygments")
  opts_knit$set(base.url = '/', base.dir = site.path)
  opts_chunk$set(fig.path=paste0(site.path,fig.dir), fig.width=8.5, fig.height=5.25, dev='svg', cache=F,
                 warning=F, message=F, cache.path=cache.path, tidy=F)


  setwd(rmd.path) # setwd to base

  # some logic to help us avoid overwriting already existing md files
  files.rmd <- data.frame(rmd = list.files(path = rmd.path,
                                           full.names = T,
                                           pattern = "\\.Rmd$",
                                           ignore.case = T,
                                           recursive = F),
                          stringsAsFactors=F
                          )
  files.rmd$corresponding.md.file <- paste0(posts.path,
                                            "/",
                                            basename(
                                              gsub(
                                                pattern = "\\.Rmd$",
                                                replacement = ".md",
                                                x = files.rmd$rmd
                                                )
                                              )
                                            )
  files.rmd$corresponding.md.exists <- file.exists(files.rmd$corresponding.md.file)

  ## determining which posts to overwrite from parameters overwriteOne & overwriteAll
  files.rmd$md.overwriteAll <- overwriteAll
  if(is.null(overwriteOne)==F) files.rmd$md.overwriteAll[grep(overwriteOne, files.rmd[,'rmd'], ignore.case=T)] <- T
  files.rmd$md.render <- F
  for (i in 1:dim(files.rmd)[1]) {
    if (files.rmd$corresponding.md.exists[i] == F) {
      files.rmd$md.render[i] <- T
    }
    if ((files.rmd$corresponding.md.exists[i] == T) && (files.rmd$md.overwriteAll[i] == T)) {
      files.rmd$md.render[i] <- T
    }
  }

  # For each Rmd file, render markdown (contingent on the flags set above)
  for (i in 1:dim(files.rmd)[1]) {
    if (files.rmd$md.render[i] == T) {
      out.file <- knit(as.character(files.rmd$rmd[i]),
                       output = as.character(files.rmd$corresponding.md.file[i]),
                       envir = parent.frame(),
                       quiet = T)
      message(paste0("KnitPost(): ", basename(files.rmd$rmd[i])))
    }
  }

}
