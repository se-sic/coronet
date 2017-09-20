
requireNamespace("ggplot2")

#plots the maximum indegree one or more projects as a bar diagram
metrics.plot.hub.indegree = function(df) {
    plot = ggplot2::ggplot(df, ggplot2::aes(y = degree, x = project)) +
        ggplot2::geom_bar(stat="identity")
    return(plot)
}

metrics.plot.avg.outdegree = function(df) {
    plot = ggplot2::ggplot(df, ggplot2::aes(y = avg.degree, x = project)) +
        ggplot2::geom_bar(stat="identity")
    return(plot)
}

metrics.plot.node.degrees = function(df) {
    plot = ggplot2::ggplot(df, ggplot2::aes(y = degree, x = name)) +
        ggplot2::geom_bar(stat="identity")
    return(plot)
}

metrics.plot.density = function(df) {
    plot = ggplot2::ggplot(df, ggplot2::aes(y = density, x = project)) +
        ggplot2::geom_bar(stat="identity")
    return(plot)
}

metrics.plot.avg.pathlength = function(df) {
    plot = ggplot2::ggplot(df, ggplot2::aes(y = avg.papthlength, x = project)) +
        ggplot2::geom_bar(stat="identity")
    return(plot)
}

metrics.plot.clustering.coeff = function(df) {
    plot = ggplot2::ggplot(df, ggplot2::aes(y = clustering.coeff, x = project)) +
        ggplot2::geom_bar(stat="identity")
    return(plot)
}

metrics.plot.modularity = function(df) {
    plot = ggplot2::ggplot(df, ggplot2::aes(y = modularity, x = project)) +
        ggplot2::geom_bar(stat="identity")
    return(plot)
}

metrics.plot.amount.nodes = function(df) {
    plot = ggplot2::ggplot(df, ggplot2::aes(y = amount.nodes, x = project)) +
        ggplot2::geom_bar(stat="identity")
    return(plot)
}

metrics.plot.smallworldness = function(df) {
    plot = ggplot2::ggplot(df, ggplot2::aes(y = smallworldness, x = project)) +
        ggplot2::geom_bar(stat="identity")
    return(plot)
}

metrics.plot.power.law.fitting = function(df) {

}

metrics.plot.hierarchy = function(df) {
    pred = predict(lm(cc ~ deg, data = df))
    plot = ggplot2::ggplot(df, ggplot2::aes(y = cc, x = deg)) +
        ggplot2::geom_point(ggplot2::aes(color = deg)) +
        ggplot2::geom_line(ggplot2::aes(y = pred))
    return(plot)
}

