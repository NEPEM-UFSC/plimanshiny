ensure_js_css_libs <- function() {
  libs <- list(
    list(name = "d3", version = "7.8.5"), # d3.v7.min.js
    list(name = "nouislider", version = "15.7.0") # nouislider.min.js/.css
  )
  dest_dir <- "inst/app/www/libs"
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  # d3
  d3_file <- file.path(dest_dir, "d3.v7.min.js")
  if (!file.exists(d3_file)) {
    download.file("https://unpkg.com/d3@7.8.5/dist/d3.min.js", d3_file, mode = "wb")
  }
  # nouislider js
  noui_js <- file.path(dest_dir, "nouislider.min.js")
  if (!file.exists(noui_js)) {
    download.file("https://unpkg.com/nouislider@15.7.0/dist/nouislider.min.js", noui_js, mode = "wb")
  }
  # nouislider css
  noui_css <- file.path(dest_dir, "nouislider.min.css")
  if (!file.exists(noui_css)) {
    download.file("https://unpkg.com/nouislider@15.7.0/dist/nouislider.min.css", noui_css, mode = "wb")
  }
}

mod_histo_slider_ui <- function(id, data, width = "100%", height = "400px", n_bins = 100, digits = NULL) {
  ensure_js_css_libs()

  # Helper function to get the number of significant digits for small values
  get_significant_digits <- function(x) {
    if (x == 0){
      return(0)
    }
    if(x > 1){
      return(2)
    } else{
      sci_notation <- format(abs(x), scientific = TRUE)
      significant_part <- strsplit(sci_notation, "e")[[1]][1]
      nchar(gsub("[^0-9]", "", significant_part))
    }
  }
  if (is.null(digits)) {
    digits <- get_significant_digits(min(data, na.rm = TRUE))
  }

  ns <- NS(id)

  # Convert the initial numeric vector to a JSON array string
  data_json <- paste0("[", paste(data, collapse = ","), "]")

  tags$div(
    id = ns("histo_slider"),
    style = sprintf("width: %s; height: %s; display: block; box-sizing: border-box;", width, height),

    # Histogram and slider containers
    tags$div(id = ns("histo_slider_histogram"), style = "height:90%; width:100%;"),
    tags$div(id = ns("histo_slider_slider"), style = "height:10%; width:100%; margin-bottom: 50px"),

    # Include external JavaScript and CSS dependencies.
    # https://d3js.org/d3.v7.min.js
    # https://cdnjs.cloudflare.com/ajax/libs/noUiSlider/15.7.0/nouislider.min.css
    # https://cdnjs.cloudflare.com/ajax/libs/noUiSlider/15.7.0/nouislider.min.js
    tags$head(
      tags$script(src = "libs/d3.v7.min.js"),
      tags$link(rel = "stylesheet", href = "libs/nouislider.min.css"),
      tags$script(src = "libs/nouislider.min.js")
    ),

    # Custom CSS
    tags$style(HTML("
      .bar rect { stroke: none; }
      .axis text { font: 10px sans-serif; }
      .axis path, .axis line { fill: none; stroke: #000; shape-rendering: crispEdges; }
      .noUi-handle {
        background: rgba(40, 167, 69, 1) !important;
        border: none !important;
        border-radius: 50% !important;
        width: 20px !important;
        height: 20px !important;
        top: -2px !important;
        transition: transform 0.2s ease !important;
      }
      .noUi-handle:hover {
        transform: scale(1.5) !important;
      }
      .noUi-target .noUi-base {
        height: 10px !important;
      }
      .noUi-tooltip {
        top: auto !important;
        bottom: -40px !important;
      }
    ")),

    # JavaScript to create the histogram and slider
    tags$script(HTML(sprintf("
      (function() {
        var containerId = '%s';

        function createHistoSlider(containerId, data) {
          var histoDiv = document.getElementById(containerId + '_histogram');
          var sliderDiv = document.getElementById(containerId + '_slider');
                    .nice()!sliderDiv) return;
                    .range([height, 0]);
          var margin = {top: 20, right: 30, bottom: 30, left: 15};
          svg.append('g')= histoDiv.clientWidth || 600;
             .attr('transform', 'translate(0,' + height + ')')
             .call(d3.axisBottom(x).ticks(7));
          var containerH = histoDiv.clientHeight || 300;
          var bar = svg.selectAll('.bar')n.top - margin.bottom;
                       .data(bins)
                       .enter().append('g')
                       .attr('class', 'bar')
                       .attr('transform', function(d) { return 'translate(' + x(d.x0) + ',' + y(d.length) + ')'; });
                      .append('svg')
          bar.append('rect')'width', width + margin.left + margin.right)
             .attr('x', 0)r('height', height + margin.top + margin.bottom)
             .attr('width', function(d) { return x(d.x1) - x(d.x0); })
             .attr('height', function(d) { return height - y(d.length); })+ margin.top + ')');
             .attr('fill', 'blue');
          var xExtent = d3.extent(data);
          if (sliderDiv.noUiSlider) { sliderDiv.noUiSlider.destroy(); }th]);
          noUiSlider.create(sliderDiv, {omain()).thresholds(x.ticks(%d))(data);
            start: [xExtent[0], xExtent[1]],
            connect: true,Linear()
            range: {.domain([0, d3.max(bins, function(d) { return d.length; })])
              'min': xExtent[0],
              'max': xExtent[1]ght, 0]);
            },
            tooltips: true,
            format: {ransform', 'translate(0,' + height + ')')
              to: function (value) { return parseFloat(value.toFixed(%d)); },
              from: function (value) { return Number(value); }
            } bar = svg.selectAll('.bar')
          });          .data(bins)
                       .enter().append('g')
          function updateHighlights(range) {
            var minVal = parseFloat(range[0]),tion(d) { return 'translate(' + x(d.x0) + ',' + y(d.length) + ')'; });
                maxVal = parseFloat(range[1]);
            bar.select('rect')
              .attr('fill', function(d) {
                var mid = (d.x0 + d.x1) / 2;turn x(d.x1) - x(d.x0); })
                return (mid >= minVal && mid <= maxVal) ? 'rgba(40, 167,69, 1)' : 'rgba(40, 167,69, 0.3)';
              });r('fill', 'blue');
          }
          if (sliderDiv.noUiSlider) { sliderDiv.noUiSlider.destroy(); }
          updateHighlights(xExtent);v, {
            start: [xExtent[0], xExtent[1]],
          // Update highlights on every move...
          sliderDiv.noUiSlider.on('update', function (values) {
            updateHighlights(values);
          }); 'max': xExtent[1]
            },
          // Only send the values to Shiny when the slider is released.
          sliderDiv.noUiSlider.on('set', function (values) {
            if (window.Shiny) {ue) { return parseFloat(value.toFixed(%d)); },
              Shiny.setInputValue(containerId + '_range', {min: parseFloat(values[0]), max: parseFloat(values[1])}, {priority: 'event'});
            }
          });
        }
          function updateHighlights(range) {
        function init() {parseFloat(range[0]),
          createHistoSlider(containerId, %s);;
        }   bar.select('rect')
              .attr('fill', function(d) {
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', init);a(40, 167,69, 1)' : 'rgba(40, 167,69, 0.3)';
        } else {;
          init();
        }
          updateHighlights(xExtent);
        var resizeTimer;
        window.addEventListener('resize', function() {
          clearTimeout(resizeTimer);pdate', function (values) {
          resizeTimer = setTimeout(function() {
            createHistoSlider(containerId, %s);
          }, 200);
        });/ Only send the values to Shiny when the slider is released.
          sliderDiv.noUiSlider.on('set', function (values) {
        Shiny.addCustomMessageHandler(containerId + '_updateHistoData', function(newData) {
          createHistoSlider(containerId, newData);range', {min: parseFloat(values[0]), max: parseFloat(values[1])}, {priority: 'event'});
        }); }
      })(););
    ", ns("histo_slider"), n_bins, digits, data_json, data_json)))
  )
}       function init() {
          createHistoSlider(containerId, %s);
#' Histo Slider Server Module
mod_histo_slider_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$nsddEventListener('DOMContentLoaded', init);
        } else {
    # When the reactive data changes, send an update message to the client.
    observeEvent(data_reactive(), {
      new_data <- data_reactive()
      session$sendCustomMessage(paste0(ns("histo_slider"), "_updateHistoData"), new_data)
    })  window.addEventListener('resize', function() {
          clearTimeout(resizeTimer);
    # Return a reactive expression for the current slider range.
    reactive({eateHistoSlider(containerId, %s);
      input[["histo_slider_range"]]
    })  });
  })
}       Shiny.addCustomMessageHandler(containerId + '_updateHistoData', function(newData) {
          createHistoSlider(containerId, newData);
        });
      })();
    ", ns("histo_slider"), n_bins, digits, data_json, data_json)))
  )
}

#' Histo Slider Server Module
mod_histo_slider_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # When the reactive data changes, send an update message to the client.
    observeEvent(data_reactive(), {
      new_data <- data_reactive()
      session$sendCustomMessage(paste0(ns("histo_slider"), "_updateHistoData"), new_data)
    })

    # Return a reactive expression for the current slider range.
    reactive({
      input[["histo_slider_range"]]
    })
  })
}
