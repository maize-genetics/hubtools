# ----
#' @title Embed HTML frames into Jupyter Notebooks
#'
#' @description
#' This function allows users to override the default height, width, and
#' scrolling options of \href{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe}{Inline Frame (iframe)}
#' elements. This can be used for any general \code{htmlwidgets} objects.
#'
#' @param h
#' An \code{htmlwidget} object.
#' @param height
#' Height of iframe element in pixels (px).
#' @param width
#' Width of iframe element in pixels (px).
#' @param scrollable
#' Provide a scrollbar if overfill is detected? Defaults to \code{TRUE}.
#'
#' @importFrom htmlwidgets saveWidget
#' @importFrom base64enc dataURI
#' @importFrom IRdisplay display_html
#'
#' @export
embedFrame <- function(h, height = NULL, width = NULL, scrollable = TRUE) {
    if (!is(h, "htmlwidget")) {
        stop("Object is not an HTML widget")
    }

    if (is.null(height)) height <- 600
    if (is.null(width)) width <- "99%"
    scroll <- ifelse(scrollable, "auto", "no")

    tmp <- tempfile(fileext = ".html")
    on.exit(unlink(tmp), add = TRUE)
    res <- htmlwidgets::saveWidget(h, tmp)

    html <- sprintf(
        '<iframe src="%s" width="%s" height="%s" id="igraph" scrolling=%s seamless="seamless" frameBorder="0"> </iframe>',
        base64enc::dataURI(mime = "text/html;charset=utf-8", file = tmp),
        width,
        height,
        scroll
    )
    IRdisplay::display_html(html)
}
