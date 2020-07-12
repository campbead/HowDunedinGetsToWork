#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)


# load data
SA2 <- st_read('data/geospatial_data.shp') #%>% # read data

#commute_data <- read.csv("../statsnz2018-census-main-means-of-travel-to-work-by-statistical-area-CSV/2018-census-main-means-of-travel-to-work-by-statistical-area.csv", na.strings = "-999")
#commute_data <- readRDS("../data/commute_clip")
#coming_going <- readRDS("../data/coming_going")

#coming_going_join <-
#  left_join(SA2, coming_going, by = c("ID" = "ID_WORK"))

#commute_pct <- readRDS("../data/commute_pct")
#commute_pct_join <-left_join(SA2, commute_pct, by="ID")

commute_pivot <- readRDS("data/commute_data")
commute_pivot_join <-left_join(SA2, commute_pivot, by="ID")

commute_pivot_join<- commute_pivot_join %>% filter(ID != "000100")

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


# commute type
commute_type <- c(
    "Driving (including carpooling)" = "drive",
    "Public bus" = "bus",
    "Bicycle" = "bike",
    "On Foot" = "onfoot",
    "Work at home" = "wfh"
)

output_type <- c(
    "Commuting Score" = "score",
    "Commuting Percent" = "percent"


)

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

mode_verb <- function(trans_mode){
    if (trans_mode== "bike") {
        return("cycling")
    } else if (trans_mode== "drive") {
        return("driving")
    } else if (trans_mode== "bus") {
        return("bussing")
    } else if (trans_mode== "onfoot") {
        return("walking or jogging")
    } else{
        return("working at home")
    }
}

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
                draggable = TRUE,
                top = 60,
                left = "auto",
                right = 20,
                bottom = "auto",
                width = 400,
                height = "auto",
                h3("How Dunedin gets to work based on where people live"),
                h4(selectInput("commuteType", "Transportation Mode", commute_type)),
                #h4(selectInput("outputType", "Output type", output_type)),
                h4(textOutput("name")),
                textOutput("percent"),
                h4(textOutput("score")),
                textOutput("sentence")
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
        "About",
        mainPanel(
            h2("About this App"),
            p(em("How Dunedin Gets to Work"), "was built for the", a("There and back again",
                href = "https://www.stats.govt.nz/2018-census/there-and-back-again-data-visualisation-competition"),
                "data visualisation competition by", a("Stats NZ", href="https://www.stats.govt.nz/")
            ),
            p("The purpose of this app to allow residents of Dunedin to visualise commuting paterns within thier city.",
                "I wanted to encourage visitors to reflect on their own transportation choices, while considering their neighbors and wider community.",
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
                a("Github.",href = "https://github.com/campbead/") ),
            p("If you like this, please consider reaching out to me on Twitter ",
                a("(@campbead)", href = "https://twitter.com/campbead"),
                "or check my personal website: ",
                a("adam-campbell.com", href = "https://www.adam-campbell.com/")
            ),
            p("- Adam J. Campbell"),


            #i(class="far fa-heart"),
            h4("Data"),
            p(a("2018 Census Main means of travel to work by Statistical Area 2", href = "https://datafinder.stats.govt.nz/table/104720-2018-census-main-means-of-travel-to-work-by-statistical-area-2/"),
                "by",
                a("Stats NZ", href = "https://www.stats.govt.nz/")),
            p(a("Statistical Area 2 2018 (generalised)", href = "https://datafinder.stats.govt.nz/layer/92212-statistical-area-2-2018-generalised/"),
                "by",
                a("Stats NZ", href = "https://www.stats.govt.nz/")),
            p(a("Territorial Authority 2018 (generalised)", href = "https://datafinder.stats.govt.nz/layer/92214-territorial-authority-2018-generalised/"),
                "by",
                a("Stats NZ", href = "https://www.stats.govt.nz/")),
            h4("Thanks"),
            p("Special thanks to Grandy Li and Jon Bapst for providing very helpful feedback and suggestions for improvements.")



        )
    )
))

server <- function(input, output, session) {


    observe({
        leafletProxy("map") %>% clearPopups()
        event <- input$map_shape_click
        if (is.null(event))
            return()
        isolate({
            #showPopup(event$id, event$lat, event$lng)
            #showTravel(event$id)
        })
    })
    showPopup <- function(SA_num, lat, lng) {
        selectedSA <- SA2 %>% filter(ID == SA_num)
        content <- as.character(tagList(
            tags$h4("Statistical Area:", as.character(selectedSA$ID_NAME),
                "num items: " , as.character(nrow(selectedSA)))
        ))
        leafletProxy("map") %>% addPopups(lng, lat, content, layerId = "ID")
    }

    showTravel <- function(SA_num){
        commuteType_server <- input$commuteType

        commute_subset <- commute_data %>%
            filter(commute_data$ID_RES == SA_num)
        display <- left_join(SA2, commute_subset, by=c("ID" = "ID_WORK")) %>%
            mutate(highlight_stroke = ifelse(ID == SA_num, 3, 1)) %>%
            mutate(stroke_color = ifelse(ID == SA_num, "black", "grey"))
        home_SA <- SA2 %>%
            filter(SA2$ID == SA_num)
        #display <- left_join(SA2, commute_subset, by="ID_RES")

        if (commuteType_server == "total") { display$active <- display$Total}
        else if (commuteType_server == "bus"){display$active <- display$Public_bus}
        else if (commuteType_server == "wfh"){display$active <- display$Work_at_home}
        else if (commuteType_server == "DrivePrivate"){display$active <- display$Drive_a_private_car_truck_or_van}
        else if (commuteType_server == "DriveCompany"){display$active <- display$Drive_a_company_car_truck_or_van}
        else if (commuteType_server == "passenger"){display$active <- display$Passenger_in_a_car_truck_van_or_company_bus}
        else if (commuteType_server == "train"){display$active <- display$Train}
        else if (commuteType_server == "bike"){display$active <- display$Bicycle}
        else if (commuteType_server == "walk"){display$active <- display$Walk_or_jog}
        else if (commuteType_server == "ferry"){display$active <- display$Ferry}
        else if (commuteType_server == "other"){display$active <- display$Other}

        pal <- colorNumeric(
            palette = "Oranges",
            domain = commute_subset$active)


        leafletProxy("map") %>%
            addPolygons(data = display,
                color = ~stroke_color,
                fillColor = ~pal(active),
                weight = ~highlight_stroke,
                layerId=~ID,
                smoothFactor = 0.5,
                opacity = 1.0,
                stroke = TRUE,
                fillOpacity = 0.5,
                label = ~ID_NAME,
                highlightOptions = highlightOptions(color = "white",
                    weight = 2,
                    bringToFront = TRUE)) %>%
            addLegend("bottomright",
                data = display,
                pal = pal,
                layerId = 'my_legend',
                values = ~active,
                #title = "foo",
                title =  as.character(tagList(
                    tags$h4("Number of", as.character(home_SA$ID_NAME), " residents working in highlighted areas"),

                )),
                opacity = 0.5)

    }

    output$map <- renderLeaflet({

        # compute pallets
        value_range <- commute_pivot_join %>%filter(direction == "from", mode == input$commuteType, type == "diff") %>%select(value) %>% st_drop_geometry()
        diff = max(value_range)+min(value_range)
        pale_rev <- colorNumeric(
            palette = "RdBu",
            reverse = TRUE,
            domain = if (abs(min(value_range))> abs(max(value_range)) ){c(-min(value_range)+diff,min(value_range)+diff)}else {c(-max(value_range)+diff,max(value_range)+diff)}
        )
        pale <- colorNumeric(
            palette = "RdBu",
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

            setView(170.5, -45.88, zoom = 12) %>%  # Dunedin


            addPolygons(
                color = "grey",
                fillColor = ~pale(value),
                weight = 1,
                layerId=~ID,
                smoothFactor = 0.5,
                opacity = 1,
                #label = labels,
                label = ~ID_NAME,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(color = "black",
                    weight = 2,
                    bringToFront = TRUE)
            ) %>%
            addLegend_decreasing(
                position = "bottomright",
                pal = pale_rev,
                values = ~value,
                opacity = 0.7,
                title = my_title
            )
    })


    output$barPlotfrom <- renderPlot({

        event <- input$map_shape_click
        if (is.null(event))
            return()
        SA <-event$id
        area_name <- SA2 %>% filter(ID == SA) %>% select(ID_NAME)%>%summarise( name = ID_NAME) %>% st_drop_geometry()

        commuting_percent <- rbind(commuting_percent_total, commute_percent(SA,area_name$name))
        commuting_percent <- commuting_percent%>%
            mutate(area = factor(area, levels = c("City wide", area_name)))

        ggplot(data = commuting_percent %>% filter(type == "from") , aes(x= colname, y=percent, fill=area)) +
            geom_col(position = "dodge") +
            theme_minimal() +
            ylab("% of work travel\nfrom area") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title.x=element_blank(),
                legend.position = c(0.6, 0.9),
                text = element_text(size=20))

    })
    output$sentence <- renderText({
        event <- input$map_shape_click
        trans_mode <- input$commuteType
        if (is.null(event))
            return()
        SA <-event$id

        score <- commute_pivot_join %>%
            filter(direction == "from", ID == SA, mode == trans_mode, type == 'diff') %>%
            select(value) %>%
            st_drop_geometry()
        sum <- commute_pivot_join %>%
            filter(direction == "from", ID == SA, mode == trans_mode, type == 'sum') %>%
            select(value) %>%
            st_drop_geometry()
        average <- commute_pivot_join %>%
            filter(direction == "from", ID == SA, mode == trans_mode, type == 'ifaverage') %>%
            select(value) %>%
            st_drop_geometry()
        pct <- commute_pivot_join %>%
            filter(direction == "from", ID == SA, mode == trans_mode, type == 'pct') %>%
            select(value) %>%
            st_drop_geometry()
        name <- commute_pivot_join %>%
            filter(direction == "from", ID == SA, mode == trans_mode, type == 'pct') %>%
            select(ID_NAME) %>%
            st_drop_geometry()

        if (trans_mode == "wfh"){
            sentence <- paste("The ",
                Score_title(trans_mode), " of ", round(score), " means that ", sum, " people living here typically ",
                " work at home, compared to a city wide average of ", round(average), " for an area with this many commuters.", sep ="")
        }
        else {
            sentence <- paste("The ",
                Score_title(trans_mode), " of ", round(score), " means that ", sum, " people living here typically ", mode_verb_2(trans_mode),
                " to work, compared to a city wide average of ", round(average), " for an area with this many commuters.", sep ="")

        }

        return(sentence)
    })

    output$name <- renderText({
        event <- input$map_shape_click
        if (is.null(event))
            return()
        SA <-event$id
        name <- commute_pivot_join %>%
            filter(direction == "from", ID == SA, mode == input$commuteType) %>%
            select(ID_NAME) %>%
            st_drop_geometry() %>%
            unique()
        return(paste(name))
    })

    output$score <- renderText({
        event <- input$map_shape_click
        trans_mode <- input$commuteType
        if (is.null(event))
            return()
        SA <-event$id
        score <- commute_pivot_join %>%
            filter(direction == "from", ID == SA, mode == trans_mode, type == 'diff') %>%
            select(value) %>%
            st_drop_geometry()
        return(paste(Score_title(trans_mode), ": ", round(score)))
    })

    output$percent <- renderText({
        event <- input$map_shape_click
        if (is.null(event))
            return()
        SA <-event$id
        trans_mode <- input$commuteType
        pct <- commute_pivot_join %>%
            filter(direction == "from", ID == SA, mode == input$commuteType, type == 'pct') %>%
            select(value) %>%
            st_drop_geometry()
        if (trans_mode == "wfh") {
            sentence = paste(round(pct*100, digits = 1), "% of workers living here typically work at home.", sep ="")
        }
        else {
            sentence = paste(round(pct*100, digits = 1), "% of workers living here typically get to work by ", mode_verb(trans_mode),".", sep ="")
        }
        return(sentence)
    })
}

shinyApp(ui, server)
