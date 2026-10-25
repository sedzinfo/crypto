# Match a plotly chart to the light or dark mode of the app

plotly ignores the Bootstrap theme, so the backgrounds are made
transparent and the text and grid colors are switched to suit the mode,
on every axis of a chart with several panels.

## Usage

``` r
plot_theme(p, mode)
```

## Arguments

- p:

  A plotly object.

- mode:

  `"light"` or `"dark"`, the value of the dark mode toggle.

## Value

The plotly object `p`, with its layout changed.

## Examples

``` r
p <- plotly::plot_ly(x = 1:3, y = c(2, 1, 3), type = "scatter", mode = "lines")
crypto:::plot_theme(p, "dark")

{"x":{"visdat":{"c15c3ba9161c":["function () ","plotlyVisDat"]},"cur_data":"c15c3ba9161c","attrs":{"c15c3ba9161c":{"x":[1,2,3],"y":[2,1,3],"mode":"lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"paper_bgcolor":"rgba(0,0,0,0)","plot_bgcolor":"rgba(0,0,0,0)","font":{"color":"#dee2e6"},"xaxis":{"domain":[0,1],"automargin":true,"gridcolor":"rgba(255,255,255,0.1)","title":[]},"yaxis":{"domain":[0,1],"automargin":true,"gridcolor":"rgba(255,255,255,0.1)","title":[]},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[1,2,3],"y":[2,1,3],"mode":"lines","type":"scatter","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
