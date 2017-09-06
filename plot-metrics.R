
requireNamespace("ggplot2")


plot.hierarchy = function(df) {
    plot = ggplot2::ggplot(df, ggplot2::aes(y = cc, x = deg, color = deg)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth()
    return(plot)
}
