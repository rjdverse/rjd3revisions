theme_design <- function(x) {
    x <- flextable::border_remove(x)
    std_border <- flextable::fp_border_default(width = 4, color = "white")
    # x <- fontsize(x, size = 10, part = "all")
    x <- flextable::font(x, fontname = "Helvetica Neue", part = "all")
    x <- flextable::align(x, align = "center", part = "all")

    x <- flextable::bg(x, bg = "gray94", part = "body")
    x <- flextable::bg(x, bg = "gray74", part = "header")
    x <- flextable::bg(x, bg = "#7ca2ff", part = "footer")
    x <- flextable::color(x, color = "black", part = "all")

    x <- flextable::bg(x, j = 1L, bg = "gray74")
    x <- flextable::color(x, j = 1L, color = "black")

    x <- flextable::padding(x, padding = 6, part = "all")
    x <- flextable::border_outer(x, part = "all", border = std_border)
    x <- flextable::border_inner_h(x, border = std_border, part = "all")
    x <- flextable::border_inner_v(x, border = std_border, part = "all")
    x <- flextable::set_table_properties(x, layout = "autofit")
    return(x)
}

create_formula <- function(col, status) {
    return(as.formula(sprintf("~ grepl(x = `%s`, pattern = \"%s\")", col, status)))
}

format_column <- function(x, col) {
    x <- flextable::bg(x, create_formula(col, "Good"), col, bg = "#4CAF50")
    x <- flextable::bg(x, create_formula(col, "Uncertain"), col, bg = "#FFEB3B")
    x <- flextable::bg(x, create_formula(col, "Bad"), col, bg = "#ff3737")
    x <- flextable::bg(x, create_formula(col, "Severe"), col, bg = "#c10000")
    x <- flextable::bold(x, create_formula(col, "Severe"), col)
    x <- flextable::color(x, create_formula(col, "Severe"), col, color = "white")
    return(x)
}

format_table <- function(x, col = "Tests") {
    formatted_table <- cbind(
        data.frame(Tests = rownames(x)),
        as.data.frame(x)
    )
    colnames(formatted_table)[1] <- col
    return(formatted_table)
}
