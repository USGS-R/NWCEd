\name{annualethist3}
\alias{annualethist3}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("annualethist3")}
\format{
  The format is:
List of 9
 $ data       :'data.frame':	15 obs. of  2 variables:
  ..$ year: chr [1:15] "2000" "2001" "2002" "2003" ...
  ..$ data: num [1:15] 332 321 316 319 280 310 297 318 288 302 ...
 $ layers     :List of 1
  ..$ :Classes 'LayerInstance', 'Layer', 'ggproto' <ggproto object: Class LayerInstance, Layer>
    aes_params: list
    compute_aesthetics: function
    compute_geom_1: function
    compute_geom_2: function
    compute_position: function
    compute_statistic: function
    data: waiver
    draw_geom: function
    geom: <ggproto object: Class GeomBar, GeomRect, Geom>
        aesthetics: function
        default_aes: uneval
        draw_group: function
        draw_key: function
        draw_layer: function
        draw_panel: function
        extra_params: na.rm
        handle_na: function
        non_missing_aes: 
        parameters: function
        required_aes: x
        setup_data: function
        use_defaults: function
        super:  <ggproto object: Class GeomRect, Geom>
    geom_params: list
    inherit.aes: TRUE
    layer_data: function
    map_statistic: function
    mapping: NULL
    position: <ggproto object: Class PositionStack, Position>
        compute_layer: function
        compute_panel: function
        required_aes: 
        setup_data: function
        setup_params: function
        super:  <ggproto object: Class Position>
    print: function
    show.legend: NA
    stat: <ggproto object: Class StatBin, Stat>
        compute_group: function
        compute_layer: function
        compute_panel: function
        default_aes: uneval
        extra_params: na.rm
        non_missing_aes: 
        parameters: function
        required_aes: x
        retransform: TRUE
        setup_data: function
        setup_params: function
        super:  <ggproto object: Class Stat>
    stat_params: list
    subset: NULL
    super:  <ggproto object: Class Layer> 
 $ scales     :Classes 'ScalesList', 'ggproto' <ggproto object: Class ScalesList>
    add: function
    clone: function
    find: function
    get_scales: function
    has_scale: function
    input: function
    n: function
    non_position_scales: function
    scales: list
    super:  <ggproto object: Class ScalesList> 
 $ mapping    :List of 1
  ..$ x: symbol data
 $ theme      :List of 2
  ..$ panel.grid.major: list()
  .. ..- attr(*, "class")= chr [1:2] "element_blank" "element"
  ..$ panel.grid.minor: list()
  .. ..- attr(*, "class")= chr [1:2] "element_blank" "element"
  ..- attr(*, "class")= chr [1:2] "theme" "gg"
  ..- attr(*, "complete")= logi FALSE
  ..- attr(*, "validate")= logi FALSE
 $ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto' <ggproto object: Class CoordCartesian, Coord>
    aspect: function
    distance: function
    expand: TRUE
    is_linear: function
    labels: function
    limits: list
    range: function
    render_axis_h: function
    render_axis_v: function
    render_bg: function
    render_fg: function
    train: function
    transform: function
    super:  <ggproto object: Class CoordCartesian, Coord> 
 $ facet      :List of 1
  ..$ shrink: logi TRUE
  ..- attr(*, "class")= chr [1:2] "null" "facet"
 $ plot_env   :<environment: R_GlobalEnv> 
 $ labels     :List of 3
  ..$ y    : chr "Frequency"
  ..$ x    : chr "mm ET"
  ..$ title: chr "Band Width = 3"
 - attr(*, "class")= chr [1:2] "gg" "ggplot"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(annualethist3)
## maybe str(annualethist3) ; plot(annualethist3) ...
}
\keyword{datasets}
