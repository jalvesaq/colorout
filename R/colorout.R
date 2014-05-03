# This file is part of colorout R package
# 
# It is distributed under the GNU General Public License.
# See the file ../LICENSE for details.
# 
# (c) 2011 Jakson Aquino: jalvesaq@gmail.com
# 
###############################################################


.onLoad <- function(libname, pkgname) {
    library.dynam("colorout", pkgname, libname, local = FALSE);

    if(is.null(getOption("colorout.anyterm")))
        options(colorout.anyterm = FALSE)
    if(is.null(getOption("colorout.emacs")))
        options(colorout.emacs = FALSE)
    if(is.null(getOption("colorout.dumb")))
        options(colorout.dumb = FALSE)
    if(is.null(getOption("colorout.verbose")))
        options(colorout.verbose = 1)

    if(testTermForColorOut() == FALSE)
        return(invisible(NULL))
    ColorOut()
}

.onUnload <- function(libpath) {
    noColorOut()
    library.dynam.unload("colorout", libpath)
}

testTermForColorOut <- function()
{
    if(getOption("colorout.anyterm"))
        return(TRUE)

    if(interactive() == FALSE)
        return(FALSE)

    termenv <- Sys.getenv("TERM")

    if(Sys.getenv("RSTUDIO") != "")
        return(FALSE)

    if(termenv != "" && termenv != "dumb")
        return(TRUE)

    if(Sys.getenv("INSIDE_EMACS") != "" && getOption("colorout.emacs") == TRUE)
        return(TRUE)

    msg <- sprintf(gettext("The R output will not be colorized because it seems that your terminal does not support ANSI escape codes.\nSys.getenv('TERM') returned '%s'.",
                           domain = "R-colorout"), termenv)
    if(termenv == ""){
        if(options("colorout.verbose") > 0)
            warning(msg, call. = FALSE, immediate. = TRUE)
        return(FALSE)
    }

    if(termenv == "dumb"){
        if(getOption("colorout.dumb"))
            return(TRUE)
        if(Sys.getenv("INSIDE_EMACS") != "")
            msg <- paste(msg,
                         gettext("Please, do ?ColorOut to know how to enable the colorizing of R output on Emacs+ESS.",
                                 domain = "R-colorout"), sep = "\n")
        if(options("colorout.verbose") > 0)
            warning(msg, call. = FALSE, immediate. = TRUE)
        return(FALSE)
    }

    return(TRUE)
}

ColorOut <- function()
{
    if(testTermForColorOut() == FALSE)
        stop(gettext("The output colorization was canceled.",
                     domain = "R-colorout"), call. = FALSE)

    .C("colorout_ColorOutput", PACKAGE="colorout")
    return (invisible(NULL))
}

noColorOut <- function()
{
    .C("colorout_noColorOutput", PACKAGE="colorout")
    return (invisible(NULL))
}

number_to_ansi_color <- function(normal, number, negnum, date, string,
                               const, stderror, warn, error, verbose, maxcolor)
{
    if(!is.numeric(normal))
        stop(gettextf("The value of '%s' must be a number correspoding to an ANSI escape code.", "normal", domain = "R-colorout"))
    if(!is.numeric(number))
        stop(gettextf("The value of '%s' must be a number correspoding to an ANSI escape code.", "number", domain = "R-colorout"))
    if(!is.numeric(negnum))
        stop(gettextf("The value of '%s' must be a number correspoding to an ANSI escape code.", "negnum", domain = "R-colorout"))
    if(!is.numeric(date))
        stop(gettextf("The value of '%s' must be a number correspoding to an ANSI escape code.", "date", domain = "R-colorout"))
    if(!is.numeric(string))
        stop(gettextf("The value of '%s' must be a number correspoding to an ANSI escape code.", "string", domain = "R-colorout"))
    if(!is.numeric(const))
        stop(gettextf("The value of '%s' must be a number correspoding to an ANSI escape code.", "const", domain = "R-colorout"))
    if(!is.numeric(stderror))
        stop(gettextf("The value of '%s' must be a number correspoding to an ANSI escape code.", "stderror", domain = "R-colorout"))
    if(!is.numeric(error))
        stop(gettextf("The value of '%s' must be a number correspoding to an ANSI escape code.", "error", domain = "R-colorout"))
    if(!is.numeric(warn))
        stop(gettextf("The value of '%s' must be a number correspoding to an ANSI escape code.", "warn", domain = "R-colorout"))
    if(!is.logical(verbose))
        stop(gettextf("'verbose' must be of mode 'logical'.", domain = "R-colorout"))

    const[const > maxcolor]   <- 0
    date[date > maxcolor]     <- 0
    error[error > maxcolor]   <- 0
    negnum[negnum > maxcolor] <- 0
    normal[normal > maxcolor] <- 0
    number[number > maxcolor] <- 0
    string[string > maxcolor] <- 0
    warn[warn > maxcolor]     <- 0
    stderror[stderror > maxcolor] <- 0

    const[const < 0]   <- 0
    date[date < 0]     <- 0
    error[error < 0]   <- 0
    negnum[negnum < 0] <- 0
    normal[normal < 0] <- 0
    number[number < 0] <- 0
    string[string < 0] <- 0
    warn[warn < 0]     <- 0
    stderror[stderror < 0] <- 0

    if(length(normal) < 3)
        normal <- c(rep(0, 3 - length(normal)), normal)
    if(length(number) < 3)
        number <- c(rep(0, 3 - length(number)), number)
    if(length(negnum) < 3)
        negnum <- c(rep(0, 3 - length(negnum)), negnum)
    if(length(date) < 3)
        date <- c(rep(0, 3 - length(date)), date)
    if(length(string) < 3)
        string <- c(rep(0, 3 - length(string)), string)
    if(length(const) < 3)
        const <- c(rep(0, 3 - length(const)), const)
    if(length(stderror) < 3)
        stderror <- c(rep(0, 3 - length(stderror)), stderror)
    if(length(warn) < 3)
        warn <- c(rep(0, 3 - length(warn)), warn)
    if(length(error) < 3)
        error <- c(rep(0, 3 - length(error)), error)

    if(Sys.getenv("TERM") == "fbterm" && maxcolor == 255){
        crnormal <- ""
        crnumber <- ""
        crnegnum <- ""
        crdate   <- ""
        crstring <- ""
        crconst  <- ""
        crstderr <- ""
        crwarn   <- ""
        crerror  <- ""

        if(normal[2])
            crnormal <- paste0("\033[2;", normal[2], "}")
        if(number[2])
            crnumber <- paste0("\033[2;", number[2], "}")
        if(negnum[2])
            crnegnum <- paste0("\033[2;", negnum[2], "}")
        if(date[2])
            crdate <-   paste0("\033[2;", date[2], "}")
        if(string[2])
            crstring <- paste0("\033[2;", string[2], "}")
        if(const[2])
            crconst <-  paste0("\033[2;", const[2], "}")
        if(stderror[2])
            crstderr <- paste0("\033[2;", stderr[2], "}")
        if(warn[2])
            crwarn <-   paste0("\033[2;", warn[2], "}")
        if(error[2])
            crerror <-  paste0("\033[2;", error[2], "}")

        if(normal[3])
            crnormal <- paste0(crnormal, "\033[1;", normal[3], "}")
        if(number[3])
            crnumber <- paste0(crnumber, "\033[1;", number[3], "}")
        if(negnum[3])
            crnegnum <- paste0(crnegnum, "\033[1;", negnum[3], "}")
        if(date[3])
            crdate <-   paste0(crdate,   "\033[1;", date[3], "}")
        if(string[3])
            crstring <- paste0(crstring, "\033[1;", string[3], "}")
        if(const[3])
            crconst <-  paste0(crconst,  "\033[1;", const[3], "}")
        if(stderror[3])
            crstderr <- paste0(crstderr, "\033[1;", stderror[3], "}")
        if(warn[3])
            crwarn <-   paste0(crwarn,   "\033[1;", warn[3], "}")
        if(error[3])
            crerror <-  paste0(crerror,  "\033[1;", error[3], "}")
    } else {
        crnormal <- "\033[0"
        crnumber <- "\033[0"
        crnegnum <- "\033[0"
        crdate   <- "\033[0"
        crstring <- "\033[0"
        crconst  <- "\033[0"
        crstderr <- "\033[0"
        crwarn   <- "\033[0"
        crerror  <- "\033[0"

        if(normal[1])
            crnormal <- paste0(crnormal, ";", normal[1])
        if(number[1])
            crnumber <- paste0(crnumber, ";", number[1])
        if(negnum[1])
            crnegnum <- paste0(crnegnum, ";", negnum[1])
        if(date[1])
            crdate <- paste0(crdate, ";", date[1])
        if(string[1])
            crstring <- paste0(crstring, ";", string[1])
        if(const[1])
            crconst <- paste0(crconst, ";", const[1])
        if(stderror[1])
            crstderr <- paste0(crstderr, ";", stderror[1])
        if(warn[1])
            crwarn <- paste0(crwarn, ";", warn[1])
        if(error[1])
            crerror <- paste0(crerror, ";", error[1])

        if(maxcolor == 255)
            txt2 <- ";48;5;"
        else
            txt2 <- ";4"

        if(normal[2])
            crnormal <- paste0(crnormal, txt2, normal[2])
        if(number[2])
            crnumber <- paste0(crnumber, txt2, number[2])
        if(negnum[2])
            crnegnum <- paste0(crnegnum, txt2, negnum[2])
        if(date[2])
            crdate <-   paste0(crdate,   txt2, date[2])
        if(string[2])
            crstring <- paste0(crstring, txt2, string[2])
        if(const[2])
            crconst <-  paste0(crconst,  txt2, const[2])
        if(stderror[2])
            crstderr <- paste0(crstderr, txt2, stderror[2])
        if(warn[2])
            crwarn <-   paste0(crwarn,   txt2, warn[2])
        if(error[2])
            crerror <-  paste0(crerror,  txt2, error[2])

        if(maxcolor == 255)
            txt3 <- ";38;5;"
        else
            txt3 <- ";3"

        if(normal[3])
            crnormal <- paste0(crnormal, txt3, normal[3])
        if(number[3])
            crnumber <- paste0(crnumber, txt3, number[3])
        if(negnum[3])
            crnegnum <- paste0(crnegnum, txt3, negnum[3])
        if(date[3])
            crdate <-   paste0(crdate,   txt3, date[3])
        if(string[3])
            crstring <- paste0(crstring, txt3, string[3])
        if(const[3])
            crconst <-  paste0(crconst,  txt3, const[3])
        if(stderror[3])
            crstderr <- paste0(crstderr, txt3, stderror[3])
        if(warn[3])
            crwarn <-   paste0(crwarn,   txt3, warn[3])
        if(error[3])
            crerror <-  paste0(crerror,  txt3, error[3])

        crnormal <- paste0(crnormal, "m")
        crnumber <- paste0(crnumber, "m")
        crnegnum <- paste0(crnegnum, "m")
        crdate   <- paste0(crdate,   "m")
        crstring <- paste0(crstring, "m")
        crconst  <- paste0(crconst,  "m")
        crstderr <- paste0(crstderr, "m")
        crwarn   <- paste0(crwarn,   "m")
        crerror  <- paste0(crerror,  "m")
    }

    .C("colorout_SetColors", crnormal, crnumber, crnegnum, crdate, crstring,
       crconst, crstderr, crwarn, crerror, as.integer(verbose), PACKAGE="colorout")
}

setOutputColors256 <- function(normal = 40, number = 214, negnum = 209, date = 179, string = 85,
                               const = 35, stderror = 33, warn = c(1, 0, 1),
                               error = c(1, 15), verbose = TRUE)
{
    number_to_ansi_color(normal, number, negnum, date, string, const,
                         stderror, warn, error, verbose, 255)
    return (invisible(NULL))
}

setOutputColors <- function(normal = 2, number = 3, negnum = 3, date = 3, string = 6,
                            const = 5, stderror = 4, warn = c(1, 0, 1),
                            error = c(1, 7), verbose = TRUE)
    
{
    number_to_ansi_color(normal, number, negnum, date, string, const,
                         stderror, warn, error, verbose, 8)
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
    cat("<html>\n<head>\n  <title>256 terminal emulator colors</title>\n</head>\n")
    cat("<body bgcolor=\"#000000\">\n")
    cat("\n<p>&nbsp;</p>\n\n")
    cat("<p><font color=\"#DDDDDD\">Hover the mouse over the table cells to see the color numbers:</font></p>\n")
    cat("\n<p>&nbsp;</p>\n\n")
    cat("<table>\n")
    cat("<tr height=\"20\">\n  ")
    for(i in 0:7){
        cat("<td width=\"20\", title=\"", i, " ", c256[i+1],
            "\" style=\"background: ", c256[i+1], "\"></td>", sep = "")
    }
    cat("\n</tr>\n<tr height=\"20\">\n  ")
    for(i in 8:15){
        cat("<td width=\"20\", title=\"", i, " ", c256[i+1],
            "\" style=\"background: ", c256[i+1], "\"></td>", sep = "")
    }
    cat("\n</tr>\n</table>\n")
    cat("\n<p>&nbsp;</p>\n\n")
    cat("<table>\n<tr height=\"20\">\n  ")
    for(red in 0:5){
        for(green in 0:5){
            for(blue in 0:5){
                i <- 16 + (36 * red) + (6 * green) + blue
                cat("<td width=\"20\", title=\"", i, " ", c256[i+1],
                    "\" style=\"background: ", c256[i+1], "\"></td>", sep = "")
            }
            cat("<td width=\"10\"></td>\n")
            if(green < 5) cat("  ")
        }
        cat("</tr>\n")
        if(red < 5) cat("<tr height=\"20\">\n")
    }
    cat("</table>\n")
    cat("\n<p>&nbsp;</p>\n\n")
    cat("<table>\n<tr height=\"20\">\n  ")
    for(i in 232:255){
        cat("<td width=\"20\", title=\"", i, " ", c256[i+1],
            "\" style=\"background: ", c256[i+1], "\"></td>", sep = "")
    }
    cat("\n</tr>\n</table>\n</body>\n</html>")
    sink()

    browseURL(outfile)

}
