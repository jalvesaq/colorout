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

    # Regression test: the Windows fallback must keep print() a real S3
    # generic. S7 (used by ggplot2) inspects the generic body for
    # UseMethod() when registering S3 methods.
    if (.Platform$OS.type == "windows") {
        was_active <- colorout::isColorOut()
        colorout::noColorOut()
        orig_print <- base::print
        tbl <- get(".__S3MethodsTable__.", envir = asNamespace("base"))
        had_dispatch <- exists("print.colorout_dispatch", envir = tbl,
                               inherits = FALSE)
        colorout:::enableColoroutFallback()
        stopifnot(!identical(base::print, orig_print))
        stopifnot(any(grepl("UseMethod", deparse(body(base::print)),
                            fixed = TRUE)))
        stopifnot(exists("print.colorout_dispatch", envir = tbl,
                         inherits = FALSE))
        colorout:::disableColoroutFallback()
        stopifnot(identical(base::print, orig_print))
        stopifnot(identical(exists("print.colorout_dispatch", envir = tbl,
                                   inherits = FALSE), had_dispatch))
        if (was_active)
            colorout::ColorOut()
    }
}
