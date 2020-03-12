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
    if(msg == "OK")
        ColorOut()
    else if(getOption("colorout.verbose") > 0){
        msg <- paste(gettext("The R output will not be colorized because it seems that your terminal does not support ANSI escape codes.", domain = "R-colorout"),
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

ColorOut <- function()
{
    msg <- testTermForColorOut()
    if(msg != "OK")
        stop(paste(gettext("The output colorization was canceled.",
                           domain = "R-colorout"), msg), call. = FALSE)

    .C("colorout_ColorOutput", PACKAGE = "colorout")
    return (invisible(NULL))
}

noColorOut <- function()
{
    .C("colorout_noColorOutput", PACKAGE = "colorout")
    return (invisible(NULL))
}

isColorOut <- function()
{
    .Call("colorout_is_enabled", PACKAGE = "colorout")
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
    return(invisible(NULL))
}

setZero <- function(z = 1e-12)
{
    if(!is.double(z))
        stop(gettext("z must be a real number.", domain = "R-colorout"),
             call. = FALSE)
    z <- as.double(abs(z))
    .C("colorout_SetZero", z, PACKAGE = "colorout")
    return(invisible(NULL))
}

show256Colors <- function(outfile = "/tmp/table256.html")
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

