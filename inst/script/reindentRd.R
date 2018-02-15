# find bad indentation in manual files when running roxygenise
# hacked from BiocCheck:::checkFormatting
tellLines <- function (pkgdir)
{
  files <- c(file.path(pkgdir, "NAMESPACE"), dir(file.path(pkgdir,
                                                           "man"), pattern = "\\.Rd$", ignore.case = TRUE, full.names = TRUE),
             dir(file.path(pkgdir, "vignettes"), full.names = TRUE,
                 pattern = "\\.Rnw$|\\.Rmd$|\\.Rrst$|\\.Rhtml$|\\.Rtex$",
                 ignore.case = TRUE), dir(file.path(pkgdir, "R"),
                                          pattern = "\\.R$", ignore.case = TRUE, full.names = TRUE))
  longlines <- 0L
  totallines <- 0L
  tablines <- 0L
  badindentlines <- 0L
  ok <- TRUE
  for (file in files) {
    if (file.exists(file) && file.info(file)$size == 0) {
      pkgname <- getPkgNameFromPkgDir(pkgdir)
      handleNote(sprintf("Add content to the empty file %s.",
                         mungeName(file, pkgname)))
    }
    if (file.exists(file) && file.info(file)$size > 0) {
      lines <- readLines(file, warn = FALSE)
      totallines <- totallines + length(lines)
      n <- nchar(lines, allowNA = TRUE)
      n <- n[!is.na(n)]
      names(n) <- seq_along(1:length(n))
      long <- n[n > 80]
      if (length(long)) {
        message(
          "Lines in file ",
          file,
          " with >80 characters: ",
          paste(long, collapse = " ")
        )
        
        longlines <- longlines + length(long)
      }
      tabs <- grepl("\t", lines)
      if (any(tabs)) {
        tablines <- tablines + length(which(tabs))
      }
      res <- regexpr("^([ ]+)", lines)
      if (any(res > -1)) {
        match.length <- attr(res, "match.length")
        indents <- match.length[match.length > -1]
        
        lines <- which(indents%%4 != 0)
        if (length(lines))
          message(
            "Lines in file ",
            file,
            " with bad indentation: ",
            paste(lines, collapse = " ")
          )
        
        badindentlinesinthisfile <- length(which(indents%%4 !=
                                                   0))
        badindentlines <- badindentlines + badindentlinesinthisfile
      }
    }
  }
  if (longlines > 0) {
    ok <- FALSE
    handleNote(sprintf("Consider shorter lines; %s lines (%i%%) are > 80 characters long.",
                       longlines, as.integer((longlines/totallines) * 100)))
  }
  if (tablines > 0) {
    ok <- FALSE
    handleNote(sprintf("Consider 4 spaces instead of tabs; %s lines (%i%%) contain tabs.",
                       tablines, as.integer((tablines/totallines) * (100/1))))
  }
  if (badindentlines > 0) {
    ok <- FALSE
    handleNote(sprintf(paste("Consider indenting lines with a multiple of 4 spaces;",
                             "%s lines (%i%%) are not."), badindentlines, as.integer((badindentlines/totallines) *
                                                                                       100)))
  }
  if (!ok) {
    message("  See http://bioconductor.org/developers/how-to/coding-style/")
  }
}

# replace bad indentation in RD files
# if indent is two spaces, place four
reindentRd <- function(rdpath = "man") {
  rd_files <- list.files(rdpath, full.names = TRUE)
  
  plyr::l_ply(
    rd_files,
    function(filename) {
      f <- readLines(filename)
      
      # indents starting with 2 spaces
      f_fix <- grepl(pattern = "^([ ]{2}[^ ])", f)
      f_prefix <- ifelse(f_fix, "  ", "")
      
      writeLines(paste0(f_prefix, f), con = filename)
    }
  )
}
