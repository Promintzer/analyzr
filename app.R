###################################
###########             ###########
###########   analyzr   ###########
###########             ###########
###################################





# Libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(ggthemes)
library(DT)
library(sqldf)



# Create example data and Workspace
dat_mtcars <- mtcars
dat_diamonds <- diamonds
dat_economics <- economics
wslist <- ls()



# Create number of tabs and layers to be supported
tabsnum <- 25
layers <- 10



# Layer data: necessary for UI Remover Layer and Filter (append layer is not affected)
layerdat <- as.data.frame(paste0(rep(1:tabsnum, each=layers), "_", rep(1:layers, times=tabsnum)))
names(layerdat) <- "Name"
layerdat$ID <- seq.int(nrow(layerdat))
layerdat$Tab <- rep(1:tabsnum, each=layers)
layerdat$Layer <- rep(1:layers, times=tabsnum)
layerdat$Flag_Close <- 0



# Themes
themes <- list(
  "Clean" = theme_clean(),
  "Dark" = theme_dark(),
  "Economist" = theme_economist(),
  "Excel" = theme_excel(),
  "Void" = theme_void(),
  "WSJ" = theme_wsj()
)

# Color scales
colorscales <- list(
  "Economist" = scale_color_economist(),
  "Excel" = scale_color_excel(),
  "WSJ" = scale_color_wsj()
)

# Fill scales
fillscales <- list(
  "Economist" = scale_fill_economist(),
  "Excel" = scale_fill_excel(),
  "WSJ" = scale_fill_wsj()
)



# Sidebar width
sbwidth <- "20px"



# Functions ----
# shinydashboardPlus::boxSidebar ----
# 1) -> Without the tooltip "More".
boxSidebarNew <- function (..., id = NULL, width = 50, background = "#333a40",
          startOpen = FALSE, icon = shiny::icon("cogs"))
{
  stopifnot(width >= 25 && width <= 100)
  toolbarTag <- shiny::tags$button(id = id, `data-background` = background,
                                   `data-width` = width, `data-widget` = "chat-pane-toggle",
                                   #`data-toggle` = "tooltip", `data-original-title` = "More",
                                   `data-start-open` = tolower(startOpen), type = "button",
                                   icon)
  contentTag <- shiny::tags$div(style = "z-index: 1; height: inherit;",
                                class = "direct-chat-contacts", shiny::tags$ul(class = "contacts-list",
                                                                               shiny::tags$li(...)))
  shiny::tagList(toolbarTag, contentTag)
}





# UI ----
ui <- dashboardPage(

  # Header
  dashboardHeader(

    # Width
    titleWidth = sbwidth,

    # Height
    tags$li(
      class = "dropdown",
      #downloadButton(outputId = "download_png", label = "PNG"),
      tags$style(".main-header {max-height: 20px}"),
      tags$style(".main-header .logo {height: 20px;}"),
      tags$style(".sidebar-toggle {height: 20px; padding-top: 1px !important;}"),
      tags$style(".navbar {min-height:20px !important}")
    )
  ),


  #dashboardSidebar(disable = TRUE, width = "3px"),
  dashboardSidebar(

    # Width
    width = sbwidth,

    # Hide toggle
    tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"))
  ),


  # Body
  dashboardBody(

    # CSS
    tags$head(tags$style(HTML('
      /* logo */
      .skin-blue .main-header .logo {background-color: #303030;}
      /* logo when hovered */
      .skin-blue .main-header .logo:hover {background-color: #303030;}
      /* navbar (rest of the header) */
      .skin-blue .main-header .navbar {background-color: #87CEFA;}
      /* main sidebar */
      .skin-blue .main-sidebar {background-color: #303030;}
      # /* header height */
      # .skin-blue .main-header {}
    '))),

    # Toolbox
    fluidRow(
      box(id = "toolbox", width = 12, title = NULL,

        # New tab button
        actionButton("add", "New Tab", icon = icon("plus-circle"))

        # # Save as
        # actionButton("save_as", "Save As", icon = icon("save")),
        #
        # # Open from source
        # actionButton("open", "Open", icon = icon("folder-open"))
      ),
      # Remove header from box
      tags$head(tags$style('#toolbox .box-header{display: none}'))
    ),

    # Tab panel
    fluidRow(
      # Style Tabs
      tags$style(HTML(".tabbable > .nav > li > a {color:black; width: 80px; height: 40px; text-align: center; margin-top:1px; margin-bottom:1px; padding-top:5px; padding-bottom:1px;}")),
      tabsetPanel(id = "plottabs")
    )
  )
)




# Server
server <- function(input, output, session){



  # Panel ----

  # UI Adder ----
  observeEvent(input$add, {
    appendTab(inputId = "plottabs", #select = TRUE,
      tab = tabPanel(

        # Title and Close-Button
        title = fluidRow(
          paste0("Tab", input$add),
          actionButton(inputId = paste0("close", input$add), label = NULL, icon = icon("times"), style='font-size:100%; padding-left:1px; padding-right:1px; padding-top:0px; padding-bottom:0px; margin-top:4px; margin-bottom:7px; margin-left:5px; margin-right:5px')
        ),

        # Data Selection
        tags$br(),
        selectInput(inputId = paste0("data_select", input$add), label = "Data Selection", choices = c("", wslist), selected = "", multiple = FALSE),


        # Show only, if data is selected
        conditionalPanel(

          # Condition
          condition = paste0("input.data_select", input$add, "!== ''"),

          # Add Filter
          actionButton(inputId = paste0("addfilter", input$add), label = "Add Filter", icon = icon("filter")),

          # Apply Filter
          actionButton(inputId = paste0("filter_apply", input$add), label = "Apply Filter", icon = icon("check-square")),

          # Filter
          fluidRow(
            box(title = "Filter", width = 12, collapsible = TRUE,
              div(id = paste0("filter_div", input$add))
            )
          ),

          # Add Layer
          actionButton(inputId = paste0("addlayer", input$add), label = "Add Layer", icon = icon("layer-group")),

          # Plot Parameters
          fluidRow(
            #tags$style(HTML(".tabbable > .nav > li > a {color:black; width: 80px; height: 40px; text-align: center; margin-top:1px; margin-bottom:1px; padding-top:5px; padding-bottom:1px;}")),
            tabBox(id = paste0("plottabbox", input$add), title = "Plot Parameters", width = 12)
          ),

          # Plot output
          # Somehow the spinner does not disappear after closing the tab, but continues loading on all other tabs.
          # withSpinner(plotOutput(outputId = paste0("plot", input$add)), type = 5, size = 0.5, id = input$add)
          fluidRow(
            box(
              title = "",
              width = 6,
              sidebar = boxSidebarNew(id = paste0("plotsidebar", input$add), icon = icon("gear"), width = 30,
                #menuItem(text = "Title and Axis", tabName = paste0("ps_titleaxis", input$add)),
                # # collapsible =
                #   textInput(inputId = paste0("ps_title", input$add), label = "Title", placeholder = "Title of the plot"),
                #   selectInput(inputId = paste0("ps_theme", input$add), label = "Theme", choices = c("<null>", names(themes)), selected = "<null>"),
                #   selectInput(inputId = paste0("ps_colorsc", input$add), label = "Color Scales", choices = c("<null>", names(colorscales)), selected = "<null>"),
                #   selectInput(inputId = paste0("plot_facet", input$add), label = "Multiple Plots by", choices = NULL)
                # )
                # conditionalPanel(
                #   condition = paste0("input.plotsidebar", input$add, " == 'ps_titleaxis", input$add, "'"),
                  textInput(inputId = paste0("ps_title", input$add), label = "Title", placeholder = "Title of the plot"),
                  textInput(inputId = paste0("ps_xlab", input$add), label = "X-Axis Label", placeholder = "Label for X-Axis"),
                  textInput(inputId = paste0("ps_ylab", input$add), label = "Y-Axis Label", placeholder = "Label for Y-Axis"),
                  selectInput(inputId = paste0("ps_theme", input$add), label = "Theme", choices = c("<null>", names(themes)), selected = "<null>"),
                  selectInput(inputId = paste0("ps_colorsc", input$add), label = "Color Scales", choices = c("<null>", names(colorscales)), selected = "<null>"),
                  selectInput(inputId = paste0("ps_fillsc", input$add), label = "Fill Scales", choices = c("<null>", names(fillscales)), selected = "<null>"),
                  selectInput(inputId = paste0("plot_facet", input$add), label = "Multiple Plots by", choices = NULL)
                # )
              ),
              plotOutput(outputId = paste0("plot", input$add))
            ),
            tabBox(id = paste0("tabbox", input$add), width = 6,
              tabPanel(title = "Data", div(style = 'overflow-x: scroll; overflow-y: scroll', dataTableOutput(outputId = paste0("table", input$add)))),
              tabPanel(title = "Summary", verbatimTextOutput(outputId = paste0("summary", input$add)))
            )
          )
        )
      )
    )
  })



  # UI Remover ----
  lapply(1:tabsnum, FUN = function(i){
    observeEvent(input[[paste0("close", i)]], {
      print(paste0("close", i))
      removeTab(inputId = "plottabs", target = input$plottabs)
    })
  })


  # UI Add Layer ----
  # i -> tab-count, addlayer-i -> layer-button-count
  lapply(1:tabsnum, FUN = function(i){
    observeEvent(input[[paste0("addlayer", i)]],{

      # Print
      print(paste0("add layer", i, "_", input[[paste0("addlayer", i)]]))

      # Append Tab
      appendTab(inputId = paste0("plottabbox", i), select = TRUE, tab = tabPanel(

        # Title
        title = fluidRow(
          paste0("Layer", input[[paste0("addlayer", i)]]),
          actionButton(inputId = paste0("closelayer", i, "_", input[[paste0("addlayer", i)]]), label = NULL, icon = icon("times"), style='font-size:100%; padding-left:1px; padding-right:1px; padding-top:0px; padding-bottom:0px; margin-top:4px; margin-bottom:7px; margin-left:5px; margin-right:5px')
        ),


        # # Title and Close-Button
        # title = fluidRow(
        #   paste0("Tab", input$add),
        #   actionButton(inputId = paste0("close", input$add), label = NULL, icon = icon("times"), style='font-size:100%; padding-left:1px; padding-right:1px; padding-top:0px; padding-bottom:0px; margin-top:4px; margin-bottom:7px; margin-left:5px; margin-right:5px')
        # ),


        # Plot Parameters
        fluidRow(
          column(width = 4,
            selectInput(inputId = paste0("plottype", i, "_", input[[paste0("addlayer", i)]]), label = "Plot Type", choices = NULL),
            conditionalPanel(
              condition = paste0("input.plottype", i, "_", input[[paste0("addlayer", i)]], "!== '<null>' && input.plottype", i, "_", input[[paste0("addlayer", i)]], "!== 'Histogram'"),
              selectInput(inputId = paste0("axisy_select", i, "_", input[[paste0("addlayer", i)]]), label = "Y-Axis", choices = NULL),
            ),
            conditionalPanel(
              condition = paste0("input.plottype", i, "_", input[[paste0("addlayer", i)]], "!== '<null>'"),
              selectInput(inputId = paste0("axisx_select", i, "_", input[[paste0("addlayer", i)]]), label = "X-Axis", choices = NULL)
            )
          ),
          column(width = 4,
            conditionalPanel(
              condition = paste0("input.plottype", i, "_", input[[paste0("addlayer", i)]], "== 'Scatter' || input.plottype", i, "_", input[[paste0("addlayer", i)]], "== 'Smooth' || input.plottype", i, "_", input[[paste0("addlayer", i)]], "== 'Line' || input.plottype", i, "_", input[[paste0("addlayer", i)]], "== 'Histogram' || input.plottype", i, "_", input[[paste0("addlayer", i)]], "== 'Bar'"),
              # Color
              tags$div(style = 'display: inline-block; vertical-align:middle; width: 250px;', selectInput(inputId = paste0("plot_color", i, "_", input[[paste0("addlayer", i)]]), label = "Color by", choices = NULL)),
              tags$div(style = 'display: inline-block; vertical-align:middle; width: 150px;', checkboxInput(inputId = paste0("plot_color_factor", i, "_", input[[paste0("addlayer", i)]]), label = "As Factor", value = FALSE))
            ),
            conditionalPanel(
              condition = paste0("input.plottype", i, "_", input[[paste0("addlayer", i)]], " == 'Scatter'"),
              # Shape
              selectInput(inputId = paste0("plot_shape", i, "_", input[[paste0("addlayer", i)]]), label = "Shape by", choices = NULL)
            ),
            conditionalPanel(
              condition = paste0("input.plottype", i, "_", input[[paste0("addlayer", i)]], " == 'Line'"),
              # Line Type
              selectInput(inputId = paste0("plot_linetype", i, "_", input[[paste0("addlayer", i)]]), label = "Line Type", choices = c("solid", "blank", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid")
            ),
            conditionalPanel(
              condition = paste0("input.plottype", i, "_", input[[paste0("addlayer", i)]], " == 'Scatter' || input.plottype", i, "_", input[[paste0("addlayer", i)]], "== 'Line'"),
              # Size
              tags$div(style = 'display: inline-block; vertical-align:middle; width: 250px;', selectInput(inputId = paste0("plot_size", i, "_", input[[paste0("addlayer", i)]]), label = "Size by", choices = NULL)),
              tags$div(style = 'display: inline-block; vertical-align:middle; width: 150px;', checkboxInput(inputId = paste0("plot_size_factor", i, "_", input[[paste0("addlayer", i)]]), label = "As Factor", value = FALSE))
            ),
            conditionalPanel(
              condition = paste0("input.plottype", i, "_", input[[paste0("addlayer", i)]], " == 'Bar' || input.plottype", i, "_", input[[paste0("addlayer", i)]], "== 'Histogram'"),
              # Fill
              selectInput(inputId = paste0("plot_fill", i, "_", input[[paste0("addlayer", i)]]), label = "Fill by", choices = NULL),
            ),
            conditionalPanel(
              condition = paste0("input.plottype", i, "_", input[[paste0("addlayer", i)]], " == 'Bar' || input.plottype", i, "_", input[[paste0("addlayer", i)]], "== 'Histogram'"),
              # Position
              selectInput(inputId = paste0("plot_position", i, "_", input[[paste0("addlayer", i)]]), label = "Position", choices = c("Stack","Dodge","Fill"), selected = "Stack")
            ),
            # Smooth
            conditionalPanel(
              condition = paste0("input.plottype", i, "_", input[[paste0("addlayer", i)]], " == 'Smooth'"),
              selectInput(inputId = paste0("smooth_method", i, "_", input[[paste0("addlayer", i)]]), label = "Method", choices = c("lm","loess","gam"), selected = "lm"),
              checkboxInput(inputId = paste0("smooth_se", i, "_", input[[paste0("addlayer", i)]]), label = "Confidence Int.", value = TRUE),
              numericInput(inputId = paste0("smooth_level", i, "_", input[[paste0("addlayer", i)]]), label = "Confidence Int. - Level", value = 0.95, min = 0.05, max = 0.99, step = 0.05),
              conditionalPanel(
                condition = paste0("input.smooth_method", i, "_", input[[paste0("addlayer", i)]], " == 'loess'"),
                numericInput(inputId = paste0("smooth_span", i, "_", input[[paste0("addlayer", i)]]), label = "Span", value = 0.50, min = 0.05, max = 1, step = 0.05)
              )
            )
          ),
          column(width = 4,
            # Histogram
            conditionalPanel(
              condition = paste0("input.plottype", i, "_", input[[paste0("addlayer", i)]], " == 'Histogram'"),
              numericInput(inputId = paste0("histogram_bins", i, "_", input[[paste0("addlayer", i)]]), label = "Bins", value = 30, step = 1)
            )
          )
        )
      ))
    })
  })



  # UI Layer Remover ----
  removerr <- reactiveValues()
  lapply(1:nrow(layerdat), FUN = function(i){
    observeEvent(input[[paste0("closelayer", layerdat$Name[i])]], {
      # Print info
      print(paste0("closelayer", layerdat$Name[i]))

      # Save removed layers in reactive value
      removerr[[ paste0(layerdat$Tab[i]) ]] <- c(removerr[[ paste0(layerdat$Tab[i]) ]], layerdat$Layer[i])

      # Remove tab
      #print(input[[paste0("plottabbox", layerdat$Tab[i])]])
      removeTab(inputId = paste0("plottabbox", layerdat$Tab[i]), target = input[[paste0("plottabbox", layerdat$Tab[i])]])
    })
  })


  # UI Filter ----
  lapply(1:tabsnum, FUN = function(i){
    observeEvent(input[[paste0("addfilter", i)]], {
      insertUI(

        # Location of the filter
        selector = paste0("#filter_div", i),

        # UI, - inside div to make removing easier
        ui = div(id = paste0("filter_div_remover", i, "_", input[[paste0("addfilter", i)]]),
          fluidRow(
            # Logical Operator
            column(width = 1,
              conditionalPanel(
                condition = paste0(input[[paste0("addfilter", i)]], " > 1"),
                selectInput(inputId = paste0("filter_oplog", i, "_", input[[paste0("addfilter", i)]]), label = paste0("Logical Op ", input[[paste0("addfilter", i)]]), choices = c("<null>", "and", "or"), selected = "<null>")
              )
            ),
            #
            column(width = 4,
              selectInput(inputId = paste0("filter", i, "_", input[[paste0("addfilter", i)]]), label = paste0("Filter ", input[[paste0("addfilter", i)]]), choices = c("<null>", colnames(get(input[[paste0("data_select", i)]]))), selected = "<null>")
            ),
            column(width = 1,
              selectInput(inputId = paste0("filter_operator", i, "_", input[[paste0("addfilter", i)]]), label = paste0("Operator ", input[[paste0("addfilter", i)]]), choices = c("<null>", "=", "<>", "<", ">", "<=", ">="), selected = "<null>")
            ),
            column(width = 4,
              textInput(inputId = paste0("filter_value", i, "_", input[[paste0("addfilter", i)]]), label = paste0("Value ", input[[paste0("addfilter", i)]]), value = NULL, placeholder = "use quotes for characters, e.g.: 'character'")
            ),
            column(width = 1,
              actionButton(inputId = paste0("filter_remover", i, "_", input[[paste0("addfilter", i)]]), label = NULL, icon = icon("times"), style = 'margin-top:25px; margin-left:-10px')
            )
          )
        )
      )
    })
  })


  # UI Filter Update / Remover ----
  lapply(1:nrow(layerdat), FUN = function(i){
    observeEvent(input[[paste0("filter_remover", layerdat$Name[i])]], {

      # Update
      updateSelectInput(session, inputId = paste0("filter_oplog", layerdat$Name[i]), selected = "<null>")
      updateSelectInput(session, inputId = paste0("filter", layerdat$Name[i]), choices = c("<null>", colnames(get(input[[paste0("data_select", layerdat$Tab[i])]]))), selected = "<null>")
      updateSelectInput(session, inputId = paste0("filter_operator", layerdat$Name[i]), selected = "<null>")
      updateTextInput(session, inputId = paste0("filter_value", layerdat$Name[i]), value = "")

      # # Remove
      # removeUI(
      #   selector = paste0("div#filter_div_remover", layerdat$Name[i]),
      #   multiple = TRUE
      # )
    })
  })


  # Update widgets data_select ----
  lapply(1:tabsnum, FUN = function(i){
    observeEvent(input[[paste0("data_select", i)]], {
      if(!is.null(input[[paste0("data_select", i)]])){
        if(input[[paste0("data_select", i)]] != ""){
          print("update widgets: data")

          # Facet
          updateSelectInput(session, inputId = paste0("plot_facet", i), choices = c("<null>", colnames(get(input[[paste0("data_select", i)]]))), selected = "<null>")
          # Theme & Colorscales
          updateSelectInput(session, inputId = paste0("ps_theme", i), selected = "<null>")
          updateSelectInput(session, inputId = paste0("ps_colorsc", i), selected = "<null>")
          updateSelectInput(session, inputId = paste0("ps_fillsc", i), selected = "<null>")
          # Title & Axis labels
          updateTextInput(session, inputId = paste0("ps_title", i), value = "")
          updateTextInput(session, inputId = paste0("ps_xlab", i), value = "")
          updateTextInput(session, inputId = paste0("ps_ylab", i), value = "")
        }
      }
    })
  })

  # Update widgets data_select OR layer ----
  lapply(1:tabsnum, FUN = function(i){
    data_or_layer <- reactive(list(input[[paste0("data_select", i)]], input[[paste0("addlayer", i)]]))
    observeEvent(data_or_layer(), {
      if(!is.null(input[[paste0("data_select", i)]])){
        if(input[[paste0("data_select", i)]] != ""){
          print("update widgets: data or layer")

          # Plot Type
          updateSelectInput(session, inputId = paste0("plottype", i, "_", input[[paste0("addlayer", i)]]), choices = c("<null>","Scatter", "Bar", "Line", "Histogram", "Smooth"), selected = "<null>")
          # Axis
          updateSelectInput(session, inputId = paste0("axisy_select", i, "_", input[[paste0("addlayer", i)]]), choices = c("<null>", colnames(get(input[[paste0("data_select", i)]]))), selected = "<null>")
          updateSelectInput(session, inputId = paste0("axisx_select", i, "_", input[[paste0("addlayer", i)]]), choices = c("<null>", colnames(get(input[[paste0("data_select", i)]]))), selected = "<null>")
          # Scatter
          updateSelectInput(session, inputId = paste0("plot_color", i, "_", input[[paste0("addlayer", i)]]), choices = c("<null>", colnames(get(input[[paste0("data_select", i)]]))), selected = "<null>")
          updateSelectInput(session, inputId = paste0("plot_shape", i, "_", input[[paste0("addlayer", i)]]), choices = c("<null>", colnames(get(input[[paste0("data_select", i)]]))), selected = "<null>")
          updateSelectInput(session, inputId = paste0("plot_size", i, "_", input[[paste0("addlayer", i)]]), choices = c("<null>", colnames(get(input[[paste0("data_select", i)]]))), selected = "<null>")
          # Bar
          updateSelectInput(session, inputId = paste0("plot_fill", i, "_", input[[paste0("addlayer", i)]]), choices = c("<null>", colnames(get(input[[paste0("data_select", i)]]))), selected = "<null>")
          updateSelectInput(session, inputId = paste0("plot_position", i, "_", input[[paste0("addlayer", i)]]), selected = "Stack")
          # Smooth
          updateSelectInput(session, inputId = paste0("smooth_method", i, "_", input[[paste0("addlayer", i)]]), selected = "lm")
          updateCheckboxInput(session, inputId = paste0("smooth_se", i, "_", input[[paste0("addlayer", i)]]), value = TRUE)
          updateNumericInput(session, inputId = paste0("smooth_level", i, "_", input[[paste0("addlayer", i)]]), value = 0.95)
          updateNumericInput(session, inputId = paste0("smooth_span", i, "_", input[[paste0("addlayer", i)]]), value = 0.50)
          # Line
          updateSelectInput(session, inputId = paste0("plot_linetype", i, "_", input[[paste0("addlayer", i)]]), selected = "solid")
          # Histogram
          updateNumericInput(session, inputId = paste0("histogram_bins", i, "_", input[[paste0("addlayer", i)]]), value = 30)
        }
      }
    })
  })

  # Update widgets filter data (update all filters) ----
  lapply(1:tabsnum, FUN = function(i){
    observeEvent(input[[paste0("data_select", i)]], {
      if(!is.null(input[[paste0("data_select", i)]])){
        if(input[[paste0("data_select", i)]] != ""){
          print("update widgets: filter all")
          lapply(1:input[[paste0("addfilter", i)]], FUN = function(j){
            #Filter
            updateSelectInput(session, inputId = paste0("filter_oplog", i, "_", j), selected = "<null>")
            updateSelectInput(session, inputId = paste0("filter", i, "_", j), choices = c("<null>", colnames(get(input[[paste0("data_select", i)]]))), selected = "<null>")
            updateSelectInput(session, inputId = paste0("filter_operator", i, "_", j), selected = "<null>")
            updateTextInput(session, inputId = paste0("filter_value", i, "_", j), value = "")
          })
        }
      }
    })
  })




  # Plot Output ----
  lapply(1:tabsnum, FUN = function(i){
    plotid <- paste0("plot", i)
    output[[plotid]] <- renderPlot({


      # Reactive values for the plot - functions ----

      # Function to make checks on input
      checker <- function(data, inputid, inputid_f){
        if(!is.null(inputid)){
        if(inputid != ""){
        if(inputid != "<null>"){
        if(inputid %in% colnames(datr())){
          if(inputid_f){
            as.factor(data()[,inputid])
          } else {
            data()[,inputid]
          }
        }}}}
      }
      # Function to make checks on input, list
      checker_list <- function(list, inputid){
        if(!is.null(inputid)){
        if(inputid != ""){
        if(inputid != "<null>"){
          list[[ inputid ]]
        }}}
      }
      # Function to make checks on filter
      checker_f <- function(inputid){
        if(!is.null(inputid)){
        if(inputid != ""){
        if(inputid != "<null>"){
          inputid
        }}}
      }


      # Reactive values for the plot ----

      # Data: If data is selected and apply filter
      # datr <- reactive({
      #   if(!is.null(input[[paste0("data_select", i)]])){
      #   if(input[[paste0("data_select", i)]] != ""){
      #     as.data.frame(get(input[[paste0("data_select", i)]]))
      #   }}
      # })


      # Filter - Reactive Values
      sql1 <- "select * from "
      sql2 <- input[[paste0("data_select", i)]]
      sql3 <- " where "
      sql4 <- reactiveValues()
      sql5 <- reactiveValues()
      sql6 <- reactiveValues()
      sql7 <- reactiveValues()



      # Create data with or without Filter
      data_or_filterapply <- reactive(list(input[[paste0("data_select", i)]], input[[paste0("filter_apply", i)]]))
      datr <- eventReactive(data_or_filterapply(), {

        #print("within event reactive")

        # Collect all filters
        filter_collect <- lapply(1:input[[paste0("addfilter", i)]], FUN = function(j){
          sql4[[paste0(i, "_", j)]] <- checker_f(inputid = input[[paste0("filter_oplog", i, "_", j)]])
          sql5[[paste0(i, "_", j)]] <- checker_f(inputid = input[[paste0("filter", i, "_", j)]])
          sql6[[paste0(i, "_", j)]] <- checker_f(inputid = input[[paste0("filter_operator", i, "_", j)]])
          sql7[[paste0(i, "_", j)]] <- checker_f(inputid = input[[paste0("filter_value", i, "_", j)]])
          if(is.null(sql4[[paste0(i, "_", j)]])){
            paste0(sql5[[paste0(i, "_", j)]], sql6[[paste0(i, "_", j)]], sql7[[paste0(i, "_", j)]])
          } else {
            paste0(sql4[[paste0(i, "_", j)]], " ", sql5[[paste0(i, "_", j)]], sql6[[paste0(i, "_", j)]], sql7[[paste0(i, "_", j)]])
          }
        })

        # Remove empty elements
        filter_collect <- filter_collect[lengths(filter_collect) > 0L]

        #print(filter_collect)
        #print(length(filter_collect))
        #print(filter_collect[1])
        #print(sql5[[paste0(i, "_1")]])
        #print(colnames(as.data.frame(get(input[[paste0("data_select", i)]]))))

        # Create final sql
        # If filter is empty or first filter does not exist
        if(length(filter_collect) == 0 | is.null(sql5[[paste0(i, "_1")]])){
          #print("31a")
          print(paste0(sql1, sql2))
          sql_final <- paste0(sql1, sql2)
        # If first filter is not set to one of the colnames (change of data). Alternative: debounce.
        } else if(!(sql5[[paste0(i, "_1")]] %in% colnames(as.data.frame(get(input[[paste0("data_select", i)]]))))){
          #print("31b")
          print(paste0(sql1, sql2))
          sql_final <- paste0(sql1, sql2)
        # Apply Filter
        } else {
          #print("32")
          print(paste0(sql1, sql2, sql3, paste0(filter_collect, collapse = " ")))
          sql_final <- paste0(sql1, sql2, sql3, paste0(filter_collect, collapse = " "))
        }

        #print(paste0(filter_collect, " "))
        #print(paste0(sql1, sql2, sql3, paste0(filter_collect, collapse = " ")))

        # Empty data and apply final sql
        sqldf()
        sqldf(sql_final)
      })


      #print(head(datr()))



      # Facet
      plot_facetr <- reactive({checker(data = datr, inputid = input[[paste0("plot_facet", i)]], inputid_f = FALSE)})

      # Theme
      plot_themer <- reactive({checker_list(list = themes, inputid = input[[paste0("ps_theme", i)]])})

      # Color Scale
      plot_colorscr <- reactive({checker_list(list = colorscales, inputid = input[[paste0("ps_colorsc", i)]])})

      # Fill Scale
      plot_fillscr <- reactive({checker_list(list = fillscales, inputid = input[[paste0("ps_fillsc", i)]])})




      # Plot construction ----
      p1 <- ggplot(data = datr())

      # Initial reactive Values
      axisxr          <- reactiveValues()
      axisyr          <- reactiveValues()
      plot_colorr     <- reactiveValues()
      plot_shaper     <- reactiveValues()
      plot_sizer      <- reactiveValues()
      plot_fillr      <- reactiveValues()


      # Geoms to be added
      adder <- lapply(1:input[[paste0("addlayer", i)]], FUN = function(j){

        # Set reactive values
        axisxr[[paste0(i, "_", j)]]           <- checker(data = datr, inputid = input[[paste0("axisx_select", i, "_", j)]], inputid_f = FALSE)
        axisyr[[paste0(i, "_", j)]]           <- checker(data = datr, inputid = input[[paste0("axisy_select", i, "_", j)]], inputid_f = FALSE)
        plot_colorr[[paste0(i, "_", j)]]      <- checker(data = datr, inputid = input[[paste0("plot_color", i, "_", j)]], inputid_f = input[[paste0("plot_color_factor", i, "_", j)]])
        plot_shaper[[paste0(i, "_", j)]]      <- checker(data = datr, inputid = input[[paste0("plot_shape", i, "_", j)]], inputid_f = TRUE)
        plot_sizer[[paste0(i, "_", j)]]       <- checker(data = datr, inputid = input[[paste0("plot_size", i, "_", j)]], inputid_f = input[[paste0("plot_size_factor", i, "_", j)]])
        plot_fillr[[paste0(i, "_", j)]]       <- checker(data = datr, inputid = input[[paste0("plot_fill", i, "_", j)]], inputid_f = FALSE)

        # Print (debug)
        # print(paste0("plottype", i, "_", j))
        # print(input[[paste0("plottype", i, "_", j)]])

        # print(axisyr[[paste0(i, "_", j)]])
        # print(axisxr[[paste0(i, "_", j)]])
        # print(plot_colorr[[paste0(i, "_", j)]])



        #############################################################
        # Message for adding FIRST layer
        if(j == 0){

          ggtitle(paste0("<Press 'Add Layer'-Button to add first layer.>"))

        } else

        #############################################################
        # Scatter
        if(
          input[[paste0("plottype", i, "_", j)]] == "Scatter" &&
          !is.null(axisxr[[paste0(i, "_", j)]]) &&
          !is.null(axisyr[[paste0(i, "_", j)]])
        ){

          geom_point(
            aes_string(
              x =       axisxr[[paste0(i, "_", j)]],
              y =       axisyr[[paste0(i, "_", j)]],
              color =   plot_colorr[[paste0(i, "_", j)]],
              shape =   plot_shaper[[paste0(i, "_", j)]],
              size =    plot_sizer[[paste0(i, "_", j)]]
            )
          )

        } else

        #############################################################
        # Bar - X & Y-Axis
        if(
          input[[paste0("plottype", i, "_", j)]] == "Bar" &&
          !is.null(axisyr[[paste0(i, "_", j)]]) &&
          !is.null(axisxr[[paste0(i, "_", j)]])
        ){

          geom_bar(
            aes_string(
              x =       axisxr[[paste0(i, "_", j)]],
              y =       axisyr[[paste0(i, "_", j)]],
              fill =    plot_fillr[[paste0(i, "_", j)]],
              color =   plot_colorr[[paste0(i, "_", j)]]
            ),
            position =  input[[paste0("plot_position", i, "_", j)]],
            stat =      "identity"
          )

        } else

        #############################################################
        # Bar - Y-Axis
        if(
          input[[paste0("plottype", i, "_", j)]] == "Bar" &&
          !is.null(axisyr[[paste0(i, "_", j)]])
        ){

          geom_bar(
            aes_string(
              y =       axisyr[[paste0(i, "_", j)]],
              fill =    plot_fillr[[paste0(i, "_", j)]],
              color =   plot_colorr[[paste0(i, "_", j)]]
            ),
            position =  input[[paste0("plot_position", i, "_", j)]]
          )

        } else

        #############################################################
        # Bar - X-Axis
        if(
          input[[paste0("plottype", i, "_", j)]] == "Bar" &&
          !is.null(axisxr[[paste0(i, "_", j)]])
        ){

          geom_bar(
            aes_string(
              x =       axisxr[[paste0(i, "_", j)]],
              fill =    plot_fillr[[paste0(i, "_", j)]],
              color =   plot_colorr[[paste0(i, "_", j)]]
            ),
            position =  input[[paste0("plot_position", i, "_", j)]]
          )

        } else

        #############################################################
        # Line
        if(
          input[[paste0("plottype", i, "_", j)]] == "Line" &&
          !is.null(axisyr[[paste0(i, "_", j)]]) &&
          !is.null(axisxr[[paste0(i, "_", j)]])
        ){

          geom_line(
            aes_string(
              x =     axisxr[[paste0(i, "_", j)]],
              y =     axisyr[[paste0(i, "_", j)]],
              color = plot_colorr[[paste0(i, "_", j)]],
              size =  plot_sizer[[paste0(i, "_", j)]]
            ),
            linetype = input[[paste0("plot_linetype", i, "_", j)]],
          )

        } else

        #############################################################
        # Histogram
        if(
          input[[paste0("plottype", i, "_", j)]] == "Histogram" &&
          !is.null(axisxr[[paste0(i, "_", j)]])
        ){

          geom_histogram(
            aes_string(
              x =       axisxr[[paste0(i, "_", j)]],
              fill =    plot_fillr[[paste0(i, "_", j)]],
              color =   plot_colorr[[paste0(i, "_", j)]]
            ),
            position =  input[[paste0("plot_position", i, "_", j)]],
            bins =      input[[paste0("histogram_bins", i, "_", j)]]
          )

        } else

        #############################################################
        # Smooth
        if(
          input[[paste0("plottype", i, "_", j)]] == "Smooth" &&
          !is.null(axisyr[[paste0(i, "_", j)]]) &&
          !is.null(axisxr[[paste0(i, "_", j)]])
        ){

          geom_smooth(
            aes_string(
              x =     axisxr[[paste0(i, "_", j)]],
              y =     axisyr[[paste0(i, "_", j)]],
              color = plot_colorr[[paste0(i, "_", j)]]
            ),
            method =  input[[paste0("smooth_method", i, "_", j)]],
            se =      input[[paste0("smooth_se", i, "_", j)]],
            level =   input[[paste0("smooth_level", i, "_", j)]],
            span =    input[[paste0("smooth_span", i, "_", j)]]
          )

        } else

        #############################################################
        # Message for selecting plot type and axis
        {
          ggtitle(paste0("<Select Plot Type and Axis for Layer ", j, ", or close it.>"))
        }
      })

      # print(adder)
      # print(removerr[[paste0(i)]])


      # Add geoms, but remove layers, if remover is not null.
      if(is.null(removerr[[paste0(i)]])){
        p1 <- p1+adder
      } else {
        p1 <- p1+adder[-c(removerr[[paste0(i)]])]
      }


      # Features not specific to layer
      # Facet
      if(!is.null(plot_facetr())){
        p1 <- p1 + facet_wrap(~ plot_facetr())
      }
      # Theme
      if(!is.null(plot_themer())){
        p1 <- p1 + plot_themer()
      }
      # Colorscale
      if(!is.null(plot_colorscr())){
        p1 <- p1 + plot_colorscr()
      }
      # Fillscale
      if(!is.null(plot_fillscr())){
        p1 <- p1 + plot_fillscr()
      }
      # Title
      if(!is.null(input[[paste0("ps_title", i)]])){
      if(input[[paste0("ps_title", i)]] != ""){
        p1 <- p1 + ggtitle(input[[paste0("ps_title", i)]])
      }}
      # Axis Label X
      if(!is.null(input[[paste0("ps_xlab", i)]])){
      if(input[[paste0("ps_xlab", i)]] != ""){
        p1 <- p1 + xlab(input[[paste0("ps_xlab", i)]])
      }}
      # Axis Label X
      if(!is.null(input[[paste0("ps_ylab", i)]])){
      if(input[[paste0("ps_ylab", i)]] != ""){
        p1 <- p1 + ylab(input[[paste0("ps_ylab", i)]])
      }}




      # Return final Plot
      return(p1)

    })
  })


  # Table data ----
  lapply(1:tabsnum, FUN = function(i){
    tableid <- paste0("table", i)
    output[[tableid]] <- renderDataTable(
      if(!is.null(input[[paste0("data_select", i)]])){
      if(input[[paste0("data_select", i)]] != ""){
        dattab <- head(x = as.data.frame(get(input[[paste0("data_select", i)]])), n = 100)
        datatable(dattab, options = list(pageLength = 5))
      }}
    )
  })

  # Summary ----
  lapply(1:tabsnum, FUN = function(i){
    sumid <- paste0("summary", i)
    output[[sumid]] <- renderPrint(
      if(!is.null(input[[paste0("data_select", i)]])){
      if(input[[paste0("data_select", i)]] != ""){
        summary(as.data.frame(get(input[[paste0("data_select", i)]])))
      }}
    )
  })

}


shinyApp(ui, server)























