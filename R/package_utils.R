
# # Global reference to R environment with wrapped python functions
pyenv <- new.env()

.onAttach <- function(libname, pkgname) {
  if (reticulate::py_module_available('pandas') & reticulate::py_module_available('cvxopt')) {
    reticulate::source_python(system.file("python/utils.py", package = 'evflex'), envir = pyenv)
  } else {
    if (!reticulate::py_module_available('pandas')) {
      packageStartupMessage("Python dependencies warning: pandas module not available. Please install it into the defined Python environment.")
    }
    if (!reticulate::py_module_available('cvxopt')) {
      packageStartupMessage("Python dependencies warning: cvxopt module not available. Please install it into the defined Python environment.")
    }
  }
}


pyenv.exists <- function() {
  ifelse(length(objects(envir = pyenv)) > 0, TRUE, FALSE)
}


load.pyenv <- function() {
  reticulate::source_python(system.file("python/utils.py", package = 'evflex'), envir = pyenv)
}



# To ask for a python path ------------------------------------------------

# ask_for_python_path <- function() {
#   python_path <- readline(prompt="Enter python3 path (or nothing to run Sys.which('python') ): ")
#   if (python_path == "") {
#     python_path <- Sys.which('python')
#   }
#   return( python_path )
# }

# python_path <- Sys.getenv('PYTHON3_PATH')
# if (python_path == "") {
#   if (interactive()) {
#     python_path <- ask_for_python_path()
#     if (python_path == "") {
#       message("Python not found. Please install Python3 and the required dependencies.")
#     }
#   } else {
#     message("Python 3 path not defined. Please, define the
#                 PYTHON3_PATH environment variable before loading the package.")
#   }
# }
