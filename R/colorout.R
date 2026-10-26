# This file is part of colorout R package
#
# It is distributed under the GNU General Public License.
# See the file ../LICENSE for details.
#
# (c) 2011-2018 Jakson Aquino: jalvesaq@gmail.com
# (c) 2014-2018 Dominique-Laurent Couturier: dlc48@medschl.cam.ac.uk
#
###############################################################


.onLoad <- function(libname, pkgname) {
    library.dynam("colorout", pkgname, libname, local = FALSE);

    if(is.null(getOption("colorout.anyterm")))
        options(colorout.anyterm = FALSE)
    if(is.null(getOption("colorout.dumb")))
        options(colorout.dumb = FALSE)
    if(is.null(getOption("colorout.noninteractive")))
        options(colorout.noninteractive = FALSE)
    if(is.null(getOption("colorout.notatty")))
        options(colorout.notatty = FALSE)
    if(is.null(getOption("colorout.verbose")))
        options(colorout.verbose = 0)

    msg <- testTermForColorOut()
    if(msg != "OK" && getOption("colorout.verbose") > 0){
        msg <- paste(gettext("The R output will not be colorized because it seems that your terminal does not support ANSI escape codes.", domain = "R-colorout"),
                     msg)
        warning(msg, call. = FALSE, immediate. = TRUE)
    }
}

.onAttach <- function(libname, pkgname) {
    msg <- testTermForColorOut()
    if (msg == "OK") {
        ColorOut()
    } else if(getOption("colorout.verbose") > 0){
        msg <- paste(gettext("The R output will not be colorized because it seems that your terminal does not support ANSI escape codes.",
                             domain = "R-colorout"),
                     msg)
        warning(msg, call. = FALSE, immediate. = TRUE)
    }
    return(invisible(NULL))
}

.onUnload <- function(libpath) {
    noColorOut()
    library.dynam.unload("colorout", libpath)
}

testTermForColorOut <- function()
{
    if(getOption("colorout.anyterm"))
        return("OK")

    if(interactive() == FALSE && getOption("colorout.noninteractive") == FALSE)
        return(gettext("Not in an interactive session.\n", domain = "R-colorout"))

    if(.Platform$OS.type == "windows")
        return(testTermForColorOutWindows())

    if(isatty(stdout()) == FALSE && getOption("colorout.notatty") == FALSE && Sys.getenv("RSTUDIO") == "")
        return(gettext("isatty(stdout()) returned FALSE.\n", domain = "R-colorout"))

    termenv <- Sys.getenv("TERM")

    if(termenv != "" && termenv != "dumb")
        return("OK")

    if(termenv == "dumb")
        if(getOption("colorout.dumb"))
            return("OK")

    return(gettextf("Sys.getenv('TERM') returned '%s'.", Sys.getenv("TERM"), domain = "R-colorout"))
}

testTermForColorOutWindows <- function()
{
    # Windows RGui cannot display ANSI colors.
    if(.Platform$GUI == "Rgui")
        return(gettext("RGui does not support ANSI escape codes. Use Windows Terminal, RStudio or VS Code instead.\n",
                       domain = "R-colorout"))

    if(isatty(stdout()) == FALSE && getOption("colorout.notatty") == FALSE &&
       Sys.getenv("RSTUDIO") == "" && Sys.getenv("VSCODE_PID") == "" &&
       Sys.getenv("WT_SESSION") == "" && Sys.getenv("TERM_PROGRAM") == "")
        return(gettext("isatty(stdout()) returned FALSE.\n", domain = "R-colorout"))

    # Known ANSI capable front-ends on Windows.
    if(Sys.getenv("RSTUDIO") != "")
        return("OK")
    if(Sys.getenv("WT_SESSION") != "")
        return("OK")
    if(Sys.getenv("VSCODE_PID") != "" || Sys.getenv("TERM_PROGRAM") != "")
        return("OK")
    if(Sys.getenv("ConEmuANSI") == "ON" || Sys.getenv("ANSICON") != "")
        return("OK")

    termenv <- Sys.getenv("TERM")

    if(termenv != "" && termenv != "dumb")
        return("OK")

    if(termenv == "dumb")
        if(getOption("colorout.dumb"))
            return("OK")

    # Windows 10+ consoles understand ANSI codes once virtual terminal
    # processing is enabled (done by ColorOut()), so do not refuse
    # colorization just because TERM is unset.
    return("OK")
}

.colorout_has_chook <- function()
{
    .Platform$OS.type != "windows"
}

ColorOut <- function()
{
    msg <- testTermForColorOut()
    if(msg != "OK")
        stop(paste(gettext("The output colorization was canceled.",
                           domain = "R-colorout"), msg), call. = FALSE)

    .C("colorout_ColorOutput", PACKAGE = "colorout")
    if(!.colorout_has_chook())
        enableColoroutFallback()
    return (invisible(NULL))
}

noColorOut <- function()
{
    if(!.colorout_has_chook())
        disableColoroutFallback()
    .C("colorout_noColorOutput", PACKAGE = "colorout")
    return (invisible(NULL))
}

isColorOut <- function()
{
    .Call("colorout_is_enabled", PACKAGE = "colorout")
}

# R-level fallback for platforms without a C console hook (Windows).
# R on Windows does not expose ptr_R_WriteConsoleEx to packages, so on
# Windows ColorOut() enables virtual terminal processing and wraps
# base::cat() and base::print() to colorize console output with pure R
# code. The print() replacement stays a real S3 generic (its body starts
# with UseMethod()) so packages that inspect S3 generics keep working.
.colorout_env <- new.env(parent = emptyenv())
.colorout_env$fallback_active <- FALSE
.colorout_env$orig_cat <- NULL
.colorout_env$orig_print <- NULL
.colorout_env$orig_print_dispatch <- NULL
.colorout_env$colors <- NULL
.colorout_env$zero_limit <- NA_real_

.colorout_default_colors <- function()
{
    list(normal = "\033[0;38;5;40m",
         number = "\033[0;38;5;214m",
         negnum = "\033[0;38;5;209m",
         date = "\033[0;38;5;179m",
         string = "\033[0;38;5;85m",
         const = "\033[0;38;5;35m",
         stderror = "\033[0;38;5;213m",
         warn = "\033[0;1;38;5;1m",
         error = "\033[0;48;5;1;38;5;15m",
         true = "\033[0;38;5;78m",
         false = "\033[0;38;5;203m",
         infinite = "\033[0;38;5;39m",
         index = "\033[0;38;5;30m",
         zero = "\033[0;38;5;226m")
}

.colorout_ensure_colors <- function()
{
    if(is.null(.colorout_env$colors))
        .colorout_env$colors <- .colorout_default_colors()
    .colorout_env$colors
}

# Convert a colorout user pattern (with '*' and '[a-z]' wildcards) to a
# regular expression. Returns NULL for empty patterns.
.colorout_pattern_to_regex <- function(pat)
{
    if(!nzchar(pat))
        return(NULL)
    # Protect the colorout escape for a literal star.
    pat <- gsub("\\\\\\*", "\001STAR\001", pat)
    # Escape regex metacharacters that are literal in colorout syntax.
    pat <- gsub("([.^{($|+?}])", "\\\\\\1", pat)
    pat <- gsub("\001STAR\001", "\\\\*", pat)
    pat
}

.colorout_is_zero_token <- function(tok, limit)
{
    if(is.na(limit))
        return(FALSE)
    v <- suppressWarnings(as.numeric(gsub(",", ".", tok, fixed = TRUE)))
    !is.na(v) && abs(v) < limit
}

# Colorize a single console string for stdout. Mirrors the C parser
# approximately: user patterns, quoted strings, indexes, hex numbers,
# dates, times, numbers (negative/zero aware), R constants.
.colorout_colorize_stdout <- function(txt, cols)
{
    if(is.na(txt) || !nzchar(txt))
        return(txt)
    if(grepl("\x1b", txt, fixed = TRUE))
        return(txt) # already colorized

    reset <- "\033[0m"
    normal <- cols$normal

    # Keep trailing newlines outside the color wrapper (like the C hook,
    # which emits the reset before the newline).
    core <- sub("[\r\n]+$", "", txt)
    trail <- substr(txt, nchar(core) + 1L, nchar(txt))
    if(!nzchar(core))
        return(txt)
    txt <- core

    # Placeholder alphabet (letters only so later passes ignore them).
    ph_ids <- character(0)
    ph_vals <- character(0)
    ph_n <- 0L
    new_placeholder <- function(val) {
        ph_n <<- ph_n + 1L
        # base-26 letters: a, b, ..., z, aa, ab, ...
        n <- ph_n
        s <- ""
        while(n > 0L) {
            r <- (n - 1L) %% 26L
            s <- paste0(intToUtf8(97L + r), s)
            n <- (n - 1L) %/% 26L
        }
        ph_ids <<- c(ph_ids, paste0("\001", s, "\001"))
        ph_vals <<- c(ph_vals, val)
        ph_ids[ph_n]
    }

    # 1. User patterns first (same priority as in C).
    pats <- tryCatch(listPatterns(), error = function(e) NULL)
    if(!is.null(pats) && length(pats)) {
        pcols <- attr(pats, "color")
        for(i in seq_along(pats)) {
            rx <- .colorout_pattern_to_regex(pats[i])
            if(is.null(rx))
                next
            m <- gregexpr(rx, txt, perl = TRUE)[[1]]
            if(m[1] == -1L)
                next
            ml <- attr(m, "match.length")
            # Replace from last to first to keep positions valid.
            for(k in rev(seq_along(m))) {
                if(ml[k] <= 0L)
                    next
                hit <- substr(txt, m[k], m[k] + ml[k] - 1L)
                repl <- paste0(pcols[i], hit, normal)
                ph <- new_placeholder(repl)
                txt <- paste0(substr(txt, 1L, m[k] - 1L), ph,
                              substr(txt, m[k] + ml[k], nchar(txt)))
            }
        }
    }

    # 2. Double quoted strings (stops at newline like the C version).
    m <- gregexpr("\"(?:[^\"\\\\\n]|\\\\.)*\"", txt, perl = TRUE)[[1]]
    if(m[1] != -1L) {
        ml <- attr(m, "match.length")
        for(k in rev(seq_along(m))) {
            hit <- substr(txt, m[k], m[k] + ml[k] - 1L)
            ph <- new_placeholder(paste0(cols$string, hit, normal))
            txt <- paste0(substr(txt, 1L, m[k] - 1L), ph,
                          substr(txt, m[k] + ml[k], nchar(txt)))
        }
    }

    # 3. Everything else in a single pass.
    altrep <- paste(
        "\\[[,0-9 ]*\\d[,0-9 ]*\\]",             # index
        "0x[0-9a-fA-F]+",                        # hex
        "\\d{4}[-/]\\d{2}[-/]\\d{2} \\d{2}:\\d{2}:\\d{2}", # datetime
        "\\d{4}[-/]\\d{2}[-/]\\d{2}",            # date YMD
        "\\d{2}[-/]\\d{2}[-/]\\d{4}",            # date DMY/MDY
        "\\d{2}:\\d{2}:\\d{2}",                  # time
        "-?\\d+(?:[.,]\\d+)*(?:[eE][+-]?\\d+)?", # number
        "NULL|TRUE|FALSE|NaN|-?Inf|NA",          # constants
        sep = "|")
    # Delimiter guards are applied per alternative below.
    m <- gregexpr(paste0("(?:", altrep, ")"), txt, perl = TRUE)[[1]]
    if(m[1] != -1L) {
        ml <- attr(m, "match.length")
        out <- ""
        pos <- 1L
        lim <- .colorout_env$zero_limit
        for(k in seq_along(m)) {
            s <- m[k]
            e <- s + ml[k] - 1L
            if(s > pos)
                out <- paste0(out, substr(txt, pos, s - 1L))
            hit <- substr(txt, s, e)
            before <- if(s > 1L) substr(txt, s - 1L, s - 1L) else ""
            after <- substr(txt, e + 1L, e + 1L)
            is_word_char <- function(ch) grepl("[A-Za-z0-9_.]", ch, perl = TRUE)
            col <- NULL
            if(grepl("^\\[[,0-9 ]*\\d[,0-9 ]*\\]$", hit, perl = TRUE)) {
                col <- cols$index
            } else if(grepl("^0x[0-9a-fA-F]+$", hit, perl = TRUE)) {
                col <- cols$number
            } else if(grepl("^\\d{4}[-/]\\d{2}[-/]\\d{2}( \\d{2}:\\d{2}:\\d{2})?$", hit, perl = TRUE) ||
                      grepl("^\\d{2}[-/]\\d{2}[-/]\\d{4}$", hit, perl = TRUE) ||
                      grepl("^\\d{2}:\\d{2}:\\d{2}$", hit, perl = TRUE)) {
                col <- cols$date
            } else if(grepl("^-?\\d+(?:[.,]\\d+)*(?:[eE][+-]?\\d+)?$", hit, perl = TRUE)) {
                # Approximate the C delimiter checks.
                if((before == "" || !is_word_char(before)) &&
                   (after == "" || after == "\n" || !grepl("[A-Za-z0-9_]", after, perl = TRUE))) {
                    if(startsWith(hit, "-")) {
                        if(.colorout_is_zero_token(substring(hit, 2L), lim))
                            col <- cols$zero
                        else
                            col <- cols$negnum
                    } else {
                        if(.colorout_is_zero_token(hit, lim))
                            col <- cols$zero
                        else
                            col <- cols$number
                    }
                }
            } else if(hit %in% c("NULL", "NA", "NaN")) {
                if((before == "" || !is_word_char(before)) &&
                   (after == "" || after == "\n" || !grepl("[A-Za-z0-9_]", after, perl = TRUE)))
                    col <- cols$const
            } else if(hit %in% c("TRUE")) {
                if((before == "" || !is_word_char(before)) &&
                   (after == "" || after == "\n" || !grepl("[A-Za-z0-9_]", after, perl = TRUE)))
                    col <- cols$true
            } else if(hit %in% c("FALSE")) {
                if((before == "" || !is_word_char(before)) &&
                   (after == "" || after == "\n" || !grepl("[A-Za-z0-9_]", after, perl = TRUE)))
                    col <- cols$false
            } else if(grepl("^-?Inf$", hit, perl = TRUE)) {
                if((before == "" || before == "-" || !is_word_char(before)) &&
                   (after == "" || after == "\n" || !grepl("[A-Za-z0-9_]", after, perl = TRUE)))
                    col <- cols$infinite
            }
            if(is.null(col))
                out <- paste0(out, hit)
            else
                out <- paste0(out, col, hit, normal)
            pos <- e + 1L
        }
        if(pos <= nchar(txt))
            out <- paste0(out, substr(txt, pos, nchar(txt)))
        txt <- out
    }

    # 4. Restore placeholders (already colorized, skip re-parsing).
    if(length(ph_ids)) {
        for(i in seq_along(ph_ids))
            txt <- gsub(ph_ids[i], ph_vals[i], txt, fixed = TRUE)
    }

    paste0(normal, txt, reset, trail)
}

# Colorize a stderr buffer the same way the C hook does: a single color
# chosen from the message prefix (warning/error/stderr).
.colorout_colorize_stderr <- function(txt, cols)
{
    if(is.na(txt) || !nzchar(txt))
        return(txt)
    if(grepl("\x1b", txt, fixed = TRUE))
        return(txt)
    is_warn <- startsWith(txt, "Warning") || startsWith(txt, "WARNING") ||
        startsWith(txt, "Lost warning messages") ||
        startsWith(txt, gettext("Warning", domain = "R-colorout")) ||
        startsWith(txt, gettext("WARNING", domain = "R-colorout"))
    is_err <- startsWith(txt, "Error") || startsWith(txt, "ERROR") ||
        startsWith(txt, gettext("Error", domain = "R-colorout")) ||
        startsWith(txt, gettext("ERROR", domain = "R-colorout"))
    col <- cols$stderror
    if(is_warn)
        col <- cols$warn
    else if(is_err)
        col <- cols$error
    paste0(col, txt, "\033[0m")
}

.colorout_is_stderr_target <- function(file)
{
    if(missing(file))
        return(FALSE)
    if(inherits(file, "connection")) {
        d <- tryCatch(suppressWarnings(summary(file)$description),
                      error = function(e) "")
        return(identical(d, "stderr"))
    }
    FALSE
}

.colorout_is_console_target <- function(file)
{
    if(missing(file))
        return(TRUE)
    if(is.character(file))
        return(length(file) == 1L && (file == ""))
    if(inherits(file, "connection")) {
        d <- tryCatch(suppressWarnings(summary(file)$description),
                      error = function(e) "")
        return(d %in% c("stdout", "stderr", "console", ""))
    }
    FALSE
}

.colorout_cat_wrapper <- function(..., file = "", sep = " ", fill = FALSE,
                                  labels = NULL, append = FALSE)
{
    orig <- .colorout_env$orig_cat
    dots <- list(...)
    # Never insert escape codes into files, pipes or sunk output
    # (capture.output(), sink(), knitr, testthat, ...).
    diverted <- tryCatch(sink.number() > 0L, error = function(e) FALSE)
    if(!diverted && length(dots) && .colorout_is_console_target(file)) {
        cols <- .colorout_ensure_colors()
        to_stderr <- .colorout_is_stderr_target(file)
        dots <- lapply(dots, function(x) {
            if(is.character(x)) {
                vapply(x, function(s) {
                    if(is.na(s) || !nzchar(s))
                        return(s)
                    if(to_stderr)
                        .colorout_colorize_stderr(s, cols)
                    else
                        .colorout_colorize_stdout(s, cols)
                }, character(1), USE.NAMES = FALSE)
            } else if(is.numeric(x) || is.logical(x)) {
                s <- as.character(x)
                vapply(s, function(elt) {
                    if(is.na(elt) || !nzchar(elt))
                        return(elt)
                    if(to_stderr)
                        .colorout_colorize_stderr(elt, cols)
                    else
                        .colorout_colorize_stdout(elt, cols)
                }, character(1), USE.NAMES = FALSE)
            } else {
                x
            }
        })
    }
    do.call(orig, c(dots, list(file = file, sep = sep, fill = fill,
                               labels = labels, append = append)))
}

# Wrapper for base::print(). Catches explicit print() calls and autoprint
# of classed objects on platforms without a C console hook. Autoprint of
# plain atomic vectors is done in C and cannot be intercepted here.
#
# The wrapper must remain a real S3 generic: packages such as S7 (used by
# ggplot2) inspect the generic's body for a UseMethod() call when
# registering S3 methods. A plain wrapper that calls the original print()
# makes library(ggplot2) fail with "`generic` is a function, but not an
# S3 generic function".
#
# Dispatching on an extra class lets us run .colorout_print_dispatch()
# for every print() call while still passing the original object to the
# original generic. UseMethod() only uses the dummy object's class for
# dispatch; the method receives the original arguments. Using a dummy
# avoids modifying x, which would fail for S4 objects.
.colorout_print_wrapper <- function(x, ...)
{
    UseMethod("print",
              structure(list(), class = c("colorout_dispatch", class(x))))
}

.colorout_print_dispatch <- function(x, ...)
{
    orig_print <- .colorout_env$orig_print
    orig_cat <- .colorout_env$orig_cat
    diverted <- tryCatch(sink.number() > 0L, error = function(e) FALSE)
    if(diverted)
        return(orig_print(x, ...))
    txt <- utils::capture.output(orig_print(x, ...))
    cols <- .colorout_ensure_colors()
    for(ln in txt)
        orig_cat(.colorout_colorize_stdout(ln, cols), "\n", sep = "")
    invisible(x)
}

# Base namespace bindings are locked; assignInNamespace() refuses them on
# recent R, so unlock/assign/relock explicitly (restored on disable).
.colorout_patch_base <- function(name, fun)
{
    ns <- asNamespace("base")
    unlockBinding(name, ns)
    on.exit(lockBinding(name, ns), add = TRUE)
    assign(name, fun, envir = ns)
    invisible(NULL)
}

.colorout_unpatch_base <- function(name, orig)
{
    ns <- asNamespace("base")
    unlockBinding(name, ns)
    on.exit(lockBinding(name, ns), add = TRUE)
    assign(name, orig, envir = ns)
    invisible(NULL)
}

enableColoroutFallback <- function()
{
    if(isTRUE(.colorout_env$fallback_active))
        return(invisible(NULL))
    .colorout_ensure_colors()
    if(is.null(.colorout_env$orig_cat))
        .colorout_env$orig_cat <- base::cat
    if(is.null(.colorout_env$orig_print))
        .colorout_env$orig_print <- base::print

    # Give the wrapper the base namespace as its environment so that
    # packages which infer a generic's package from environment() (S7)
    # still see print() as a base generic.
    print_wrapper <- .colorout_print_wrapper
    environment(print_wrapper) <- asNamespace("base")

    .colorout_patch_base("cat", .colorout_cat_wrapper)
    .colorout_patch_base("print", print_wrapper)

    tbl <- get(".__S3MethodsTable__.", envir = asNamespace("base"))
    if(is.null(.colorout_env$orig_print_dispatch) &&
       exists("print.colorout_dispatch", envir = tbl, inherits = FALSE))
        .colorout_env$orig_print_dispatch <-
            get("print.colorout_dispatch", envir = tbl, inherits = FALSE)
    registerS3method("print", "colorout_dispatch", .colorout_print_dispatch,
                     envir = baseenv())

    .colorout_env$fallback_active <- TRUE
    invisible(NULL)
}

disableColoroutFallback <- function()
{
    if(!isTRUE(.colorout_env$fallback_active))
        return(invisible(NULL))
    if(!is.null(.colorout_env$orig_cat)) {
        .colorout_unpatch_base("cat", .colorout_env$orig_cat)
        .colorout_env$orig_cat <- NULL
    }
    if(!is.null(.colorout_env$orig_print)) {
        .colorout_unpatch_base("print", .colorout_env$orig_print)
        .colorout_env$orig_print <- NULL
    }

    tbl <- get(".__S3MethodsTable__.", envir = asNamespace("base"))
    if(exists("print.colorout_dispatch", envir = tbl, inherits = FALSE)) {
        if(is.null(.colorout_env$orig_print_dispatch))
            rm(list = "print.colorout_dispatch", envir = tbl)
        else
            assign("print.colorout_dispatch",
                   .colorout_env$orig_print_dispatch, envir = tbl)
    }
    .colorout_env$orig_print_dispatch <- NULL
    .colorout_env$fallback_active <- FALSE
    invisible(NULL)
}

GetColorCode <- function(x, name)
{
    fname <- "setOutputColors: "
    if(length(x) == 1 && is.na(x))
        x <- ""
    if(!is.character(x) && !is.numeric(x))
        stop(paste0(fname,
                    gettextf("The value of '%s' must be either a number correspoding to an ANSI escape code or a character string.",
                             name, domain = "R-colorout")))

    if(is.character(x) && length(x) != 1)
        stop(paste0(fname,
                    gettextf("'%s' must be a character vector of length 1",
                             name, domain = "R-colorout")))

    if(is.character(x)){
        if(nchar(x, type = "bytes") > 63)
            stop(paste0(fname,
                        gettextf("'%s' must have no more than 63 characters.",
                                 name, domain = "R-colorout")))
        colstr <- x
    } else {
        if(length(x) > 3)
            stop(paste0(fname,
                        gettextf("'%s' must be a number vector of at most 3 elements.",
                                 name, domain = "R-colorout")))
        x[x > 255] <- NA
        x[x < 0] <- NA
        if(length(x) < 3)
            x <- c(rep(NA, 3 - length(x)), x)

        ## if "fbterm" && maxcolour = 255 (osx has "xterm-256color")
        if(Sys.getenv("TERM") == "fbterm" && max(x) > 7){
            colstr <- ""
            if(!is.na(x[2]))
                colstr <- paste0("\033[2;", x[2], "}")
            if(!is.na(x[3]))
                colstr <- paste0(colstr, "\033[1;", x[3], "}")
        } else {
            colstr <- "\033[0"
            if(!is.na(x[1]) && x[1] > 0 && x[1] < 8)
                colstr <- paste0(colstr, ";", x[1])
            if(max(x, na.rm = TRUE) > 7){
                if(!is.na(x[2]))
                    colstr <- paste0(colstr, ";48;5;", x[2])
                if(!is.na(x[3]))
                    colstr <- paste0(colstr, ";38;5;", x[3])
            } else {
                if(!is.na(x[2]))
                    colstr <- paste0(colstr, ";4", x[2])
                if(!is.na(x[3]))
                    colstr <- paste0(colstr, ";3", x[3])
            }
            colstr <- paste0(colstr, "m")
        }
    }
    colstr
}

setOutputColors <- function(normal = 40, negnum = 209, zero = 226,
                            number = 214, date = 179, string = 85, const = 35,
                            false = 203, true = 78, infinite = 39, index = 30,
                            stderror = 213, warn = c(1, 16, 196),
                            error = c(160, 231), verbose = TRUE, zero.limit = NA)
{
    if(!is.logical(verbose))
        verbose <- FALSE
    if(is.na(zero.limit)){
        unsetZero()
    } else {
        if(is.numeric(zero.limit) && zero.limit > 0)
            setZero(zero.limit)
        else
            unsetZero()
    }

    newline <- as.integer(.Options$width < c(110, 140)[is.na(zero.limit) + 1])

    crnormal   <- GetColorCode(normal,      "normal")
    crnegnum   <- GetColorCode(negnum,      "negnum")
    crzero     <- GetColorCode(zero,          "zero")
    crnumber   <- GetColorCode(number,      "number")
    crdate     <- GetColorCode(date,          "date")
    crstring   <- GetColorCode(string,      "string")
    crconst    <- GetColorCode(const,        "const")
    crfalse    <- GetColorCode(false,        "false")
    crtrue     <- GetColorCode(true,          "true")
    crinfinite <- GetColorCode(infinite,  "infinite")
    crindex    <- GetColorCode(index,        "index")
    crstderr   <- GetColorCode(stderror,  "stderror")
    crwarn     <- GetColorCode(warn,          "warn")
    crerror    <- GetColorCode(error,        "error")

    .C("colorout_SetColors", crnormal, crnumber, crnegnum, crdate, crstring,
       crconst, crstderr, crwarn, crerror, crtrue, crfalse, crinfinite, crindex,
       crzero, as.integer(verbose), as.integer(newline), PACKAGE = "colorout")

    # Keep an R-side copy for the Windows fallback (no C console hook there).
    .colorout_env$colors <- list(normal = crnormal, number = crnumber,
                                 negnum = crnegnum, date = crdate,
                                 string = crstring, const = crconst,
                                 stderror = crstderr, warn = crwarn,
                                 error = crerror, true = crtrue,
                                 false = crfalse, infinite = crinfinite,
                                 index = crindex, zero = crzero)
    if(is.na(zero.limit)){
        .colorout_env$zero_limit <- NA_real_
    } else {
        if(is.numeric(zero.limit) && zero.limit > 0)
            .colorout_env$zero_limit <- as.double(abs(zero.limit))
        else
            .colorout_env$zero_limit <- NA_real_
    }

    return(invisible(NULL))
}

setOutputColors256 <- function(...)
{
    # TODO: Uncomment in the future:
    # warning("The function 'setOutputColors256' is deprecated. Please, call 'setOutputColors' with the same arguments.")
    setOutputColors(...)
    return (invisible(NULL))
}

addPattern <- function(pattern, color)
{
    if(!is.character(pattern))
        stop(gettext("pattern must be a string.", domain = "R-colorout"),
                     call. = FALSE)
    color <- GetColorCode(color, "color")
    .C("colorout_AddPattern", pattern, color, PACKAGE = "colorout")
    return(invisible(NULL))
}

deletePattern <- function(pattern)
{
    for(p in pattern)
        .C("colorout_DeletePattern", p, PACKAGE = "colorout")
    return(invisible(NULL))
}

print.coloroutPattern <- function(x, ...)
{
    cat(paste0(attr(x, "color"), x, "\033[0m\n"), sep = "")
}

listPatterns <- function()
{
    p <- .Call("colorout_ListPatterns", PACKAGE = "colorout")
    v <- names(p)
    attr(v, "color") <- unname(p)
    class(v) <- "coloroutPattern"
    v
}

unsetZero <- function()
{
    .C("colorout_UnsetZero", PACKAGE = "colorout")
    .colorout_env$zero_limit <- NA_real_
    return(invisible(NULL))
}

setZero <- function(z = 1e-12)
{
    if(!is.double(z))
        stop(gettext("z must be a real number.", domain = "R-colorout"),
             call. = FALSE)
    z <- as.double(abs(z))
    .C("colorout_SetZero", z, PACKAGE = "colorout")
    .colorout_env$zero_limit <- z
    return(invisible(NULL))
}

show256Colors <- function(outfile = file.path(tempdir(), "table256.html"))
{
    c256 <- c("#000000", "#c00000", "#008000", "#804000", "#0000c0", "#c000c0",
              "#008080", "#c0c0c0", "#808080", "#ff6060", "#00ff00", "#ffff00",
              "#8080ff", "#ff40ff", "#00ffff", "#ffffff", "#000000", "#00005f",
              "#000087", "#0000af", "#0000d7", "#0000ff", "#005f00", "#005f5f",
              "#005f87", "#005faf", "#005fd7", "#005fff", "#008700", "#00875f",
              "#008787", "#0087af", "#0087d7", "#0087ff", "#00af00", "#00af5f",
              "#00af87", "#00afaf", "#00afd7", "#00afff", "#00d700", "#00d75f",
              "#00d787", "#00d7af", "#00d7d7", "#00d7ff", "#00ff00", "#00ff5f",
              "#00ff87", "#00ffaf", "#00ffd7", "#00ffff", "#5f0000", "#5f005f",
              "#5f0087", "#5f00af", "#5f00d7", "#5f00ff", "#5f5f00", "#5f5f5f",
              "#5f5f87", "#5f5faf", "#5f5fd7", "#5f5fff", "#5f8700", "#5f875f",
              "#5f8787", "#5f87af", "#5f87d7", "#5f87ff", "#5faf00", "#5faf5f",
              "#5faf87", "#5fafaf", "#5fafd7", "#5fafff", "#5fd700", "#5fd75f",
              "#5fd787", "#5fd7af", "#5fd7d7", "#5fd7ff", "#5fff00", "#5fff5f",
              "#5fff87", "#5fffaf", "#5fffd7", "#5fffff", "#870000", "#87005f",
              "#870087", "#8700af", "#8700d7", "#8700ff", "#875f00", "#875f5f",
              "#875f87", "#875faf", "#875fd7", "#875fff", "#878700", "#87875f",
              "#878787", "#8787af", "#8787d7", "#8787ff", "#87af00", "#87af5f",
              "#87af87", "#87afaf", "#87afd7", "#87afff", "#87d700", "#87d75f",
              "#87d787", "#87d7af", "#87d7d7", "#87d7ff", "#87ff00", "#87ff5f",
              "#87ff87", "#87ffaf", "#87ffd7", "#87ffff", "#af0000", "#af005f",
              "#af0087", "#af00af", "#af00d7", "#af00ff", "#af5f00", "#af5f5f",
              "#af5f87", "#af5faf", "#af5fd7", "#af5fff", "#af8700", "#af875f",
              "#af8787", "#af87af", "#af87d7", "#af87ff", "#afaf00", "#afaf5f",
              "#afaf87", "#afafaf", "#afafd7", "#afafff", "#afd700", "#afd75f",
              "#afd787", "#afd7af", "#afd7d7", "#afd7ff", "#afff00", "#afff5f",
              "#afff87", "#afffaf", "#afffd7", "#afffff", "#d70000", "#d7005f",
              "#d70087", "#d700af", "#d700d7", "#d700ff", "#d75f00", "#d75f5f",
              "#d75f87", "#d75faf", "#d75fd7", "#d75fff", "#d78700", "#d7875f",
              "#d78787", "#d787af", "#d787d7", "#d787ff", "#d7af00", "#d7af5f",
              "#d7af87", "#d7afaf", "#d7afd7", "#d7afff", "#d7d700", "#d7d75f",
              "#d7d787", "#d7d7af", "#d7d7d7", "#d7d7ff", "#d7ff00", "#d7ff5f",
              "#d7ff87", "#d7ffaf", "#d7ffd7", "#d7ffff", "#ff0000", "#ff005f",
              "#ff0087", "#ff00af", "#ff00d7", "#ff00ff", "#ff5f00", "#ff5f5f",
              "#ff5f87", "#ff5faf", "#ff5fd7", "#ff5fff", "#ff8700", "#ff875f",
              "#ff8787", "#ff87af", "#ff87d7", "#ff87ff", "#ffaf00", "#ffaf5f",
              "#ffaf87", "#ffafaf", "#ffafd7", "#ffafff", "#ffd700", "#ffd75f",
              "#ffd787", "#ffd7af", "#ffd7d7", "#ffd7ff", "#ffff00", "#ffff5f",
              "#ffff87", "#ffffaf", "#ffffd7", "#ffffff", "#080808", "#121212",
              "#1c1c1c", "#262626", "#303030", "#3a3a3a", "#444444", "#4e4e4e",
              "#585858", "#626262", "#6c6c6c", "#767676", "#808080", "#8a8a8a",
              "#949494", "#9e9e9e", "#a8a8a8", "#b2b2b2", "#bcbcbc", "#c6c6c6",
              "#d0d0d0", "#dadada", "#e4e4e4", "#eeeeee")

    sink(file = outfile)
    cat("<!DOCTYPE HTML SYSTEM>\n<html>\n<head>\n  <title>256 terminal emulator colors</title>\n")
    cat("<style type=\"text/css\">\n  table td { height: 20px; width: 20px; }\n</style>\n")
    cat("</head>\n<body bgcolor=\"#000000\">\n")
    cat("\n<p>&nbsp;</p>\n\n")
    cat("<p><font color=\"#DDDDDD\">Hover the mouse over the table cells to see the color numbers:</font></p>\n")
    cat("\n<p>&nbsp;</p>\n\n")
    cat("<table>\n")
    cat("<tr>\n  ")
    for(i in 0:7){
        cat("<td title=\"", i, " ", c256[i+1], "\" style=\"background: ", c256[i+1], "\"></td>", sep = "")
    }
    cat("\n</tr>\n<tr>\n  ")
    for(i in 8:15){
        cat("<td title=\"", i, " ", c256[i+1], "\" style=\"background: ", c256[i+1], "\"></td>", sep = "")
    }
    cat("\n</tr>\n</table>\n")
    cat("\n<p>&nbsp;</p>\n\n")
    cat("<table>\n<tr>\n  ")
    for(red in 0:5){
        for(green in 0:5){
            for(blue in 0:5){
                i <- 16 + (36 * red) + (6 * green) + blue
                cat("<td title=\"", i, " ", c256[i+1], "\" style=\"background: ", c256[i+1], "\"></td>", sep = "")
            }
            cat("<td ></td>\n")
            if(green < 5) cat("  ")
        }
        cat("</tr>\n")
        if(red < 5) cat("<tr>\n")
    }
    cat("</table>\n")
    cat("\n<p>&nbsp;</p>\n\n")
    cat("<table>\n<tr>\n  ")
    for(i in 232:255){
        cat("<td title=\"", i, " ", c256[i+1], "\" style=\"background: ", c256[i+1], "\"></td>", sep = "")
    }
    cat("\n</tr>\n</table>\n</body>\n</html>")
    sink()

    browseURL(outfile)

}

