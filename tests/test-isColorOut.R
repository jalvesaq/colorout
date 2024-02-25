if (requireNamespace("colorout", quietly = TRUE)) {
    #options("colorout.verbose" = 2)

    # ensure colorout is not attached
    pkg <- "package:colorout"
    if (pkg %in% search()) {
        detach("package:colorout")
    }

    # isColorOut() should return FALSE if package is not attached
    stopifnot(!colorout::isColorOut())

    # isColorOut() should return TRUE if package is attached
    if (interactive()) {
        # colorout only works in interactive sessions
        require("colorout", quietly = TRUE)
        stopifnot(isColorOut())
    }
}
