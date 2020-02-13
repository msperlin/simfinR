.onAttach <- function(libname,pkgname) {

  do_color <- crayon::make_style("#18bc9c")

  if (interactive()) {
    msg <- paste0('\nHi ', Sys.getenv('USER'), '!\n',
                  'Want to learn more about using R in Finance and Economics? ',
                  'The second edition (2020) of my R book is freely available at ',
                  do_color('https://www.msperlin.com/afedR/'))
  } else {
    msg <- ''
  }
  packageStartupMessage(msg)

}
