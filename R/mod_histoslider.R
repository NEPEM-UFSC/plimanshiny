mod_histo_slider_ui <- function(id, data, width = "100%", height = "400px", n_bins = 100, digits = NULL) {


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
          if (!histoDiv || !sliderDiv) return;

          var margin = {top: 20, right: 30, bottom: 30, left: 15};
          var containerW = histoDiv.clientWidth || 600;
          var width = containerW - margin.left - margin.right;

          var containerH = histoDiv.clientHeight || 300;
          var height = containerH - margin.top - margin.bottom;

          histoDiv.innerHTML = '';

          var svg = d3.select(histoDiv)
                      .append('svg')
                      .attr('width', width + margin.left + margin.right)
                      .attr('height', height + margin.top + margin.bottom)
                      .append('g')
                      .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

          var xExtent = d3.extent(data);
          var x = d3.scaleLinear().domain(xExtent).nice().range([0, width]);
          var bins = d3.bin().domain(x.domain()).thresholds(x.ticks(%d))(data);

          var y = d3.scaleLinear()
                    .domain([0, d3.max(bins, function(d) { return d.length; })])
                    .nice()
                    .range([height, 0]);

          svg.append('g')
             .attr('transform', 'translate(0,' + height + ')')
             .call(d3.axisBottom(x).ticks(7));

          var bar = svg.selectAll('.bar')
                       .data(bins)
                       .enter().append('g')
                       .attr('class', 'bar')
                       .attr('transform', function(d) { return 'translate(' + x(d.x0) + ',' + y(d.length) + ')'; });

          bar.append('rect')
             .attr('x', 0)
             .attr('width', function(d) { return x(d.x1) - x(d.x0); })
             .attr('height', function(d) { return height - y(d.length); })
             .attr('fill', 'blue');

          if (sliderDiv.noUiSlider) { sliderDiv.noUiSlider.destroy(); }
          noUiSlider.create(sliderDiv, {
            start: [xExtent[0], xExtent[1]],
            connect: true,
            range: {
              'min': xExtent[0],
              'max': xExtent[1]
            },
            tooltips: true,
            format: {
              to: function (value) { return parseFloat(value.toFixed(%d)); },
              from: function (value) { return Number(value); }
            }
          });

          function updateHighlights(range) {
            var minVal = parseFloat(range[0]),
                maxVal = parseFloat(range[1]);
            bar.select('rect')
              .attr('fill', function(d) {
                var mid = (d.x0 + d.x1) / 2;
                return (mid >= minVal && mid <= maxVal) ? 'rgba(40, 167,69, 1)' : 'rgba(40, 167,69, 0.3)';
              });
          }

          updateHighlights(xExtent);

          // Update highlights on every move...
          sliderDiv.noUiSlider.on('update', function (values) {
            updateHighlights(values);
          });

          // Only send the values to Shiny when the slider is released.
          sliderDiv.noUiSlider.on('set', function (values) {
            if (window.Shiny) {
              Shiny.setInputValue(containerId + '_range', {min: parseFloat(values[0]), max: parseFloat(values[1])}, {priority: 'event'});
            }
          });
        }

        function init() {
          createHistoSlider(containerId, %s);
        }

        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', init);
        } else {
          init();
        }

        var resizeTimer;
        window.addEventListener('resize', function() {
          clearTimeout(resizeTimer);
          resizeTimer = setTimeout(function() {
            createHistoSlider(containerId, %s);
          }, 200);
        });

        Shiny.addCustomMessageHandler(containerId + '_updateHistoData', function(newData) {
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
