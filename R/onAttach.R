.onAttach <- function(libname,pkgname) {

  do_color <- crayon::make_style("#FF4141")
  this_pkg <- 'simfinR'

  if (interactive()) {
    msg <- paste0('\nWant to learn more about ',
                  do_color(this_pkg), ' and other R packages for Finance and Economics?',
                  '\nThe second edition (2020) of ',
                  do_color('Analyzing Financial and Economic Data with R'), ' is available at\n',
                  do_color('https://www.msperlin.com/afedR/') )
  } else {
    msg <- ''
  }

  packageStartupMessage(msg)

}
