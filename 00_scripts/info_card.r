info_card <- function(title, value,
                      main_icon = "chart-line",
                      bg_color = "default", text_color = "default") {
    
    div(
        class = "panel panel-default",
        style = "padding: 10px;",
        div(
            class = str_glue("panel-body bg-{bg_color} text-{text_color}"),
            p(class = "pull-right", icon(class = "fa", main_icon)),
            h4(title),
            h5(value)
        )
    )
    
}