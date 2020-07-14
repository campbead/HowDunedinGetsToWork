library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)


# load data
SA2 <- st_read('data/geospatial_data.shp') # read geospatial data
commute_pivot <- readRDS("data/commute_data") # read commuting data

# join data
commute_pivot_join <-left_join(SA2, commute_pivot, by="ID")

# remove outside of Dunedin
commute_pivot_join<- commute_pivot_join %>% filter(ID != "000100")

# functions
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft",
    "topleft"), pal, values, na.label = "NA", bins = 7, colors,
    opacity = 0.5, labels = NULL, labFormat = labelFormat(),
    title = NULL, className = "info legend", layerId = NULL,
    group = NULL, data = getMapData(map), decreasing = FALSE) {
    position <- match.arg(position)
    type <- "unknown"
    na.color <- NULL
    extra <- NULL
    if (!missing(pal)) {
        if (!missing(colors))
            stop("You must provide either 'pal' or 'colors' (not both)")
        if (missing(title) && inherits(values, "formula"))
            title <- deparse(values[[2]])
        values <- evalFormula(values, data)
        type <- attr(pal, "colorType", exact = TRUE)
        args <- attr(pal, "colorArgs", exact = TRUE)
        na.color <- args$na.color
        if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] ==
                0) {
            na.color <- NULL
        }
        if (type != "numeric" && !missing(bins))
            warning("'bins' is ignored because the palette type is not numeric")
        if (type == "numeric") {
            cuts <- if (length(bins) == 1)
                pretty(values, bins)
            else bins

            if (length(bins) > 2)
                if (!all(abs(diff(bins, differences = 2)) <=
                        sqrt(.Machine$double.eps)))
                    stop("The vector of breaks 'bins' must be equally spaced")
            n <- length(cuts)
            r <- range(values, na.rm = TRUE)
            cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
            n <- length(cuts)
            p <- (cuts - r[1])/(r[2] - r[1])
            extra <- list(p_1 = p[1], p_n = p[n])
            p <- c("", paste0(100 * p, "%"), "")
            if (decreasing == TRUE){
                colors <- pal(rev(c(r[1], cuts, r[2])))
                labels <- rev(labFormat(type = "numeric", cuts))
            }else{
                colors <- pal(c(r[1], cuts, r[2]))
                labels <- rev(labFormat(type = "numeric", cuts))
            }
            colors <- paste(colors, p, sep = " ", collapse = ", ")

        }
        else if (type == "bin") {
            cuts <- args$bins
            n <- length(cuts)
            mids <- (cuts[-1] + cuts[-n])/2
            if (decreasing == TRUE){
                colors <- pal(rev(mids))
                labels <- rev(labFormat(type = "bin", cuts))
            }else{
                colors <- pal(mids)
                labels <- labFormat(type = "bin", cuts)
            }

        }
        else if (type == "quantile") {
            p <- args$probs
            n <- length(p)
            cuts <- quantile(values, probs = p, na.rm = TRUE)
            mids <- quantile(values, probs = (p[-1] + p[-n])/2,
                na.rm = TRUE)
            if (decreasing == TRUE){
                colors <- pal(rev(mids))
                labels <- rev(labFormat(type = "quantile", cuts, p))
            }else{
                colors <- pal(mids)
                labels <- labFormat(type = "quantile", cuts, p)
            }
        }
        else if (type == "factor") {
            v <- sort(unique(na.omit(values)))
            colors <- pal(v)
            labels <- labFormat(type = "factor", v)
            if (decreasing == TRUE){
                colors <- pal(rev(v))
                labels <- rev(labFormat(type = "factor", v))
            }else{
                colors <- pal(v)
                labels <- labFormat(type = "factor", v)
            }
        }
        else stop("Palette function not supported")
        if (!any(is.na(values)))
            na.color <- NULL
    }
    else {
        if (length(colors) != length(labels))
            stop("'colors' and 'labels' must be of the same length")
    }
    legend <- list(colors = I(unname(colors)), labels = I(unname(labels)),
        na_color = na.color, na_label = na.label, opacity = opacity,
        position = position, type = type, title = title, extra = extra,
        layerId = layerId, className = className, group = group)
    invokeMethod(map, data, "addLegend", legend)
}

# mode to Score
Score_title <- function(trans_mode){
    if (trans_mode== "bike") {
        return("Cycling Score")
    } else if (trans_mode== "drive") {
        return("Driving Score")
    } else if (trans_mode== "bus") {
        return("Bus Score")
    } else if (trans_mode== "onfoot") {
        return("Walking and Jogging Score")
    } else{
        return("Working at Home Score")
    }
}

# mode to verb
mode_verb <- function(trans_mode){
    if (trans_mode== "bike") {
        return("cycling")
    } else if (trans_mode== "drive") {
        return("driving")
    } else if (trans_mode== "bus") {
        return("public bus")
    } else if (trans_mode== "onfoot") {
        return("walking or jogging")
    } else{
        return("working at home")
    }
}

# mode to verb
mode_verb_2 <- function(trans_mode){
    if (trans_mode== "bike") {
        return("cycle")
    } else if (trans_mode== "drive") {
        return("drive")
    } else if (trans_mode== "bus") {
        return("bus")
    } else if (trans_mode== "onfoot") {
        return("walk or jog")
    } else{
        return("working at home")
    }
}


first_load = TRUE
# commute type
commute_type <- c(
    "Driving" = "drive",
    "Public bus" = "bus",
    "Bicycle" = "bike",
    "On Foot" = "onfoot",
    "Work at home" = "wfh"
)

output_type <- c(
    "Commuting Score" = "score",
    "Commuting Percent" = "percent"
)

ui <- fluidPage(navbarPage(
    "How Dunedin Gets to Work",
    id = "nav",
    tabPanel(
        "Map",
        div(
            class = "outer",
            tags$head(# Include our custom CSS
                includeCSS("styles.css"),),

            leafletOutput("map", width = "100%", height = "100%"),

            absolutePanel(
                id = "controls",
                class = "panel panel-default",
                fixed = TRUE,
                draggable = FALSE,
                top = 60,
                left = "auto",
                right = 5,
                bottom = "auto",
                width = 300,
                height = "auto",
                h4(selectInput("commuteType", "Transportation Mode", commute_type,
                    width = 250)),
            )
        ),
        tags$div(
            id = "cite",
            'Dataset: ',
            tags$em('2018 Census Main means of travel to work by Statistical Area 2'),
            ' from Stats NZ.'
        )
    ),
    tabPanel(
        "What scores mean",
        mainPanel(
            h2("What scores mean"),
            p("Scores represent how much (or how little) commuters ",strong("living in a given area,"), " typically travel to work by a transportation mode (for example by bus or by driving) compared to the city average."),
            p(strong("Positive scores"), "indicate that commuters living in that area use that mode", strong("more than the city average.")),
            p(strong("Negative scores"), "indicate that commuters living in that area use that mode", strong("less than the city average.")),
            img(src = "score_example_1_edit.png", alt = "Postives scores are above city average, negative scores are below city average"),

            h3("Transportation modes"),
            p("Scores are grouped by transportation modes, for this analysis driving is considered to be driving 'a private car, truck or van' OR driving 'a company car, truck or van', OR being a 'passenger in a car, truck, van or company bus'."),
            p("Census results for commuting by train, ferry and  other commuting (e.g. taxi, motorbike) are not included because they are negligible in Dunedin."),


            h3("How scores are calculated"),
            p("For a given transportation mode, scores represent the difference between the number of commuters and the number of commuters that would be expected in that area, given a city average."),
            withMathJax(),
            helpText('$$S = W - T \\cdot A$$'),
            p("The score",
                em("(S)"),
                "is the number of workers",
                em("(W)"),
                "using the transportation mode from that area, minus the total",
                em("(T)"),
                "number of workers from that area, times the city average rate",
                em("(A)"),
                "for the transportation mode."),
            p("For example, if an area has 100 workers total",
                em("(T)"),
                "and 30 of them walk or jog to work",
                em("(W)"),
                "and the city average for workers walking or jogging to work",
                em("(A)"),
                "is 10%. Then the score",
                em("(S)"),
                "would be 20."
            ),
            helpText('$$S = 30 - 100 \\cdot 10 \\% = 20 $$'),
            p("If that same area only had 5 workers walking or jogging to work",
                em("(W)"),
                "then the score",
                em("(S)"),
                "would be -5."
            ),
            helpText('$$S = 5 - 100 \\cdot 10 \\% = -5 $$')

        )
    ),
    tabPanel(
        "About this app",
        mainPanel(
            h2("About this app"),
            p(em("How Dunedin Gets to Work"), "was built for the", a("There and back again",
                href = "https://www.stats.govt.nz/2018-census/there-and-back-again-data-visualisation-competition"),
                "data visualisation competition by", a("Stats NZ", href="https://www.stats.govt.nz/")
            ),
            p("The purpose of this app to allow residents of Dunedin to visualise commuting patterns within their city.",
                "I wanted to encourage visitors to reflect on their own transportation choices, while considering their neighbours and the wider community.",
                "I hope this project is a starting point for considering why residents make the choices they make around commuting."),
            p("This app was built using",
                a("Shiny from RStudio.", href= "https://shiny.rstudio.com/" ),
                "The interactive map is built using ",
                a("Leaflet for R.", href = "https://rstudio.github.io/leaflet/"),
                "Data processing and development are done using ",
                a("R", href = "https://www.r-project.org/"),
                "and the ",
                a("tidyverse packages.", href = "https://www.tidyverse.org/")
            ),
            p("This app is open source and available at",
                a("Github.",href = "https://github.com/campbead/HowDunedinGetsToWork") ),
            p("If you like this, please consider reaching out to me on Twitter ",
                a("(@campbead)", href = "https://twitter.com/campbead"),
                "or check my personal website: ",
                a("adam-campbell.com", href = "https://www.adam-campbell.com/")
            ),
            h3("Data"),
            p(a("2018 Census Main means of travel to work by Statistical Area 2", href = "https://datafinder.stats.govt.nz/table/104720-2018-census-main-means-of-travel-to-work-by-statistical-area-2/"),
                "by",
                a("Stats NZ", href = "https://www.stats.govt.nz/")),
            p(a("Statistical Area 2 2018 (generalised)", href = "https://datafinder.stats.govt.nz/layer/92212-statistical-area-2-2018-generalised/"),
                "by",
                a("Stats NZ", href = "https://www.stats.govt.nz/")),
            p(a("Territorial Authority 2018 (generalised)", href = "https://datafinder.stats.govt.nz/layer/92214-territorial-authority-2018-generalised/"),
                "by",
                a("Stats NZ", href = "https://www.stats.govt.nz/")),
            h3("Thanks"),
            p("Special thanks to Jon Bapst, Ian Bowles, and Grandy Li for providing very helpful feedback and suggestions for improvements."),
            p("Copyright (c) 2020 Adam J Campbell")
        )
    ),
    collapsible = TRUE
))

server <- function(input, output, session) {
    observe({
        leafletProxy("map") %>% clearPopups()
        travel_mode = input$commuteType
        event <- input$map_shape_click
        if (is.null(event))
            return()
        isolate({
            showPopup(event$id, event$lat, event$lng)
            updateMap(event$id, travel_mode)
        })
    })
    showPopup <- function(SA_num, lat, lng) {

        trans_mode <- input$commuteType

        total_commute_from_citywide <- commute_pivot %>% filter(direction == "from", type == "sum") %>% summarise(citywide = sum(value))
        total_commute_from_mode <- commute_pivot %>% filter(direction == "from", type == "sum", mode== trans_mode) %>% summarise(onmode = sum(value))
        pct_city <-total_commute_from_mode / total_commute_from_citywide

        # make title
        my_title = if (input$commuteType== "bike") {
            "Cycling Score"
        } else if (input$commuteType== "drive") {
            "Driving Score"
        } else if (input$commuteType== "bus") {
            "Bus Score"
        } else if (input$commuteType== "onfoot") {
            "Walking and Jogging Score"
        } else{
            "Working at Home Score"
        }

        diff <-commute_pivot_join %>%filter(ID == SA_num, direction == "from", mode == input$commuteType, type == "diff") %>%select(value) %>% st_drop_geometry()


        pct <- commute_pivot_join %>%
            filter(direction == "from", ID == SA_num, mode == input$commuteType, type == 'pct') %>%
            select(value) %>%
            st_drop_geometry()
        if (input$commuteType == "wfh") {
            sentence = paste(
                round(pct*100, digits = 1),
                "% of workers living here typically work at home, compared to ",
                round(pct_city*100, digits = 1),
                "% citywide.",
                sep ="")
        }
        else {
            sentence = paste(
                round(pct*100, digits = 1),
                "% of workers living here typically get to work by ",
                mode_verb(trans_mode),
                ", compared to ",
                round(pct_city*100, digits = 1),
                "% citywide.",
                sep ="")
        }


        content <- as.character(tagList(
            tags$h4(as.character(SA2 %>% filter(ID == SA_num) %>% select(ID_NAME) %>% st_drop_geometry())),
            tags$strong(my_title, ":", as.character(round(diff))),
            tags$p(sentence)
        ))




        leafletProxy("map") %>% addPopups(lng, lat, content, layerId = "ID")
    }

    updateMap <- function(SA_num, travel_mode)
    {
        value_range <- commute_pivot_join %>%filter(direction == "from", mode == travel_mode, type == "diff") %>%select(value) %>% st_drop_geometry()
        diff = max(value_range)+min(value_range)
        pale <- colorNumeric(
            palette = "RdBu",
            reverse = if (input$commuteType == "drive" ) {TRUE} else {FALSE},
            #reverse = TRUE,
            domain = if (abs(min(value_range))> abs(max(value_range)) ){c(-min(value_range),min(value_range))}else {c(-max(value_range),max(value_range))}
        )
        leafletProxy("map") %>%
            addPolygons(
                data = commute_pivot_join  %>% filter(direction == "from",  mode == travel_mode, type == "diff", ID == SA_num),
                color = "black",
                fill= FALSE,
                weight = 3,
                layerId="highlight",
                smoothFactor = 1,
                opacity = 1,
                #label = labels,
                #label = ~ID,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                fillOpacity = 0.7
            )
    }


    output$map <- renderLeaflet({

        # compute pallets
        value_range <- commute_pivot_join %>%filter(direction == "from", mode == input$commuteType, type == "diff") %>%select(value) %>% st_drop_geometry()
        diff = max(value_range)+min(value_range)
        pale_rev <- colorNumeric(
            palette = "RdBu",
            #reverse = if (input$commuteType == "drive" ) {FALSE} else {TRUE},
            reverse = TRUE,
            domain = if (abs(min(value_range))> abs(max(value_range)) ){c(-min(value_range)+diff,min(value_range)+diff)}else {c(-max(value_range)+diff,max(value_range)+diff)}
        )
        pale <- colorNumeric(
            palette = "RdBu",
            #reverse = if (input$commuteType == "drive" ) {TRUE} else {FALSE},
            reverse = FALSE,
            domain = if (abs(min(value_range))> abs(max(value_range)) ){c(-min(value_range),min(value_range))}else {c(-max(value_range),max(value_range))}
        )

        # make title
        my_title = if (input$commuteType== "bike") {
            "Cycling Score"
        } else if (input$commuteType== "drive") {
            "Driving Score"
        } else if (input$commuteType== "bus") {
            "Bus Score"
        } else if (input$commuteType== "onfoot") {
            "Walking and Jogging Score"
        } else{
            "Working at Home Score"
        }


        leaflet(data = commute_pivot_join  %>% filter(direction == "from",  mode == input$commuteType, type == "diff")) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                options = providerTileOptions(noWrap = TRUE)
            ) %>%

            clearPopups()  %>%

            setView(170.5, -45.88, zoom = 12) %>%  # Dunedin

            addPolygons(
                color = "grey",
                fillColor = ~pale(value),
                weight = 1,
                layerId=~ID,
                smoothFactor = 0.5,
                opacity = 1,
                #label = labels,
                #label = ~ID_NAME,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                fillOpacity = 0.7
            ) %>%
            addLegend_decreasing(
                position = "bottomright",
                pal = pale_rev,
                values = ~value,
                opacity = 0.7,
                title = my_title
                )
    })
}

shinyApp(ui, server)
