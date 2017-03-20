.onAttach <- function(lib, pkg)  {
  packageStartupMessage("This is tRophicPosition ",
                        utils::packageDescription("tRophicPosition",
                                                  fields="Version"),
                        appendLF = TRUE)
}
