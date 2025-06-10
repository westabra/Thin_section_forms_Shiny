library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(shinyjs)

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Include shinyjs for DOM manipulation
  
  # Include required CSS
  tags$head(
    tags$style(HTML("
      .modal-lg {
        width: 90%;
        max-width: 1200px;
      }
      .selected-image {
        max-height: 500px;
        max-width: 100%;
        display: block;
        margin: 0 auto;
      }
      .clickable-img {
        cursor: pointer;
        transition: transform 0.2s;
        margin: 10px;
      }
      .clickable-img:hover {
        transform: scale(1.05);
      }
      .thumbnail-container {
        position: relative;
        margin: 10px;
        display: inline-block;
      }
      .selected-overlay {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background-color: rgba(0, 123, 255, 0.3);
        border: 3px solid #007bff;
        pointer-events: none;
      }
      .mineral-selection {
        border: 1px solid #ddd;
        padding: 10px;
        margin-bottom: 10px;
        background-color: #f9f9f9;
      }
      .mineral-row {
        margin-bottom: 5px;
      }
      #mineralSelectPanel {
        max-height: 250px;
        overflow-y: auto;
      }
      .mineral-library-panel {
        background-color: #f8f9fa;
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 10px;
        margin-top: 10px;
      }
      .library-section {
        margin-bottom: 15px;
      }
      .library-title {
        font-weight: bold;
        background-color: #e9ecef;
        padding: 5px;
        margin-bottom: 5px;
      }
      .library-entry {
        cursor: pointer;
        padding: 3px 5px;
        border-bottom: 1px dashed #e0e0e0;
      }
      .library-entry:hover {
        background-color: #e9ecef;
      }
      .multi-image-container {
        display: flex; 
        flex-wrap: wrap; 
        justify-content: space-around; 
        padding: 10px; 
        background-color: #f0f0f0; 
        border-radius: 5px; 
        margin-bottom: 15px;
      }
      .debug-info {
        font-family: monospace;
        font-size: 12px;
        white-space: pre-wrap;
        max-height: 200px;
        overflow-y: auto;
        background-color: #f5f5f5;
        border: 1px solid #ddd;
        padding: 8px;
      }
      .version-info {
        font-size: 12px;
        color: #777;
        text-align: right;
        margin-top: 5px;
      }
    "))
  ),
  
  titlePanel(
    div(
      "Rock Image Viewer with Classification",
      div(class = "version-info", "Version 18 (2025-06-10)")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # User and timestamp info
      wellPanel(
        textOutput("userInfo"),
        textOutput("currentDateTime")
      ),
      
      # Panel for selecting multiple minerals
      wellPanel(
        h4("Select Minerals"),
        div(id = "mineralSelectPanel", uiOutput("mineralSelectUI"))
      ),
      
      # Dropdown for rock textures (loaded from CSV)
      selectInput("textureSelect", "Select Rock Texture:", choices = NULL),
      
      # Dropdown for grain sizes (loaded from CSV)
      selectInput("grainsizeSelect", "Select Grain Size:", choices = NULL),
      
      # Add a button to refresh or apply filters
      actionButton("applyBtn", "Apply Filters", class = "btn-primary"),
      
      # Multiple image selection
      hr(),
      h4("Selected Images"),
      textOutput("selectedImageCount"),
      actionButton("clearSelectedImages", "Clear Selections", class = "btn-sm btn-warning"),
      actionButton("openDescriptionForm", "Open Description Form", class = "btn-sm btn-success"),
      
      # Library management
      hr(),
      h4("Mineral Reference Library"),
      actionButton("useDefaultLibrary", "Use Default Library", class = "btn-sm btn-info"),
      actionButton("createCustomLibrary", "Create Simple Library", class = "btn-sm btn-warning"),
      verbatimTextOutput("libraryStatus")
    ),
    
    mainPanel(
      width = 9,
      # Container for displaying images
      div(style = "height: 500px; width: 100%; overflow-y: auto; background-color: #f5f5f5; padding: 10px;",
          uiOutput("imageContainer")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Display current user and time
  output$userInfo <- renderText({
    paste("Current User:", "westabraЧто")
  })
  
  output$currentDateTime <- renderText({
    "2025-06-10 14:27:45"  # Updated fixed date/time as provided
  })
  
  # Store loaded dropdown option data
  mineral_choices <- reactiveVal(NULL)
  texture_choices <- reactiveVal(NULL)
  grainsize_choices <- reactiveVal(NULL)
  
  # Store multiple selected images
  selected_images <- reactiveVal(c())
  
  # Store selected minerals
  selected_minerals <- reactiveVal(c())
  
  # Store descriptions of rocks (in a real app, this would connect to a database)
  rock_descriptions <- reactiveVal(list())
  
  # Store mineral library data
  mineral_library <- reactiveVal(NULL)
  library_status <- reactiveVal("Mineral reference library not loaded yet.")
  
  # Create a default library with common minerals
  createDefaultLibrary <- function() {
    default_data <- data.frame(
      Mineral = c(
        "Quartz", "Quartz", 
        "Biotite", "Biotite", 
        "Plagioclase", "Plagioclase", 
        "Muscovite", "Muscovite", 
        "Hornblende", "Hornblende", 
        "Olivine", "Olivine",
        "K-feldspar", "K-feldspar",
        "Pyroxene", "Pyroxene",
        "Calcite", "Calcite",
        "Garnet", "Garnet"
      ),
      Descriptions = c(
        "Colorless in PPL, low relief", 
        "First-order gray interference colors in XPL",
        "Strong pleochroism from light brown to dark brown in PPL", 
        "Bird's-eye extinction in XPL",
        "Polysynthetic twinning visible in XPL", 
        "Low relief in PPL, generally colorless",
        "Colorless in PPL with high birefringence", 
        "Bird's-eye extinction in XPL",
        "Green to brown pleochroism in PPL", 
        "Oblique extinction in XPL",
        "High relief in PPL, colorless to pale green", 
        "High order interference colors in XPL",
        "Carlsbad twinning in XPL",
        "Tartan or microcline twinning in some varieties",
        "Two good cleavages at 87-93 degrees",
        "High relief and straight extinction",
        "Extreme birefringence with rhombohedral cleavage",
        "Shows characteristic twinkling when rotating the stage",
        "Isotropic behavior in XPL",
        "High relief, rounded crystals"
      ),
      Rocks_type = c(
        "Common in all rock types",
        "Essential mineral in granites and metamorphic rocks",
        "Common in igneous rocks",
        "Major component in granites and gneisses",
        "Essential in most igneous rocks",
        "Common in volcanic and plutonic rocks",
        "Common in metamorphic rocks",
        "Important in schists and gneisses",
        "Common in intermediate to mafic igneous rocks",
        "Found in amphibolites and some metamorphic rocks",
        "Characteristic of mafic and ultramafic rocks",
        "Common in basalt and peridotite",
        "Common in granites and granitic rocks",
        "Major component of felsic igneous rocks",
        "Common in mafic and ultramafic rocks",
        "Major component in basalts and gabbros",
        "Main component of limestone and marble",
        "Present in many sedimentary and metamorphic rocks",
        "Common in metamorphic rocks",
        "Indicator of high-grade metamorphism"
      ),
      Reference = c(
        "Nesse, 2011",
        "Deer et al., 1992",
        "Mackenzie et al., 1982",
        "Nesse, 2011",
        "Blatt & Tracy, 1996",
        "Klein & Dutrow, 2007",
        "Deer et al., 1992",
        "Nesse, 2011",
        "Mackenzie et al., 1982",
        "Klein & Dutrow, 2007",
        "Deer et al., 1992",
        "Nesse, 2011",
        "Klein & Dutrow, 2007",
        "Deer et al., 1992",
        "Nesse, 2011",
        "Mackenzie et al., 1982",
        "Deer et al., 1992",
        "Nesse, 2011",
        "Klein & Dutrow, 2007",
        "Deer et al., 1992"
      ),
      stringsAsFactors = FALSE
    )
    
    return(default_data)
  }
  
  # Function to create a simpler custom library with fewer entries
  createSimpleLibrary <- function() {
    simple_data <- data.frame(
      Mineral = c(
        "Quartz", 
        "Biotite", 
        "Plagioclase", 
        "Muscovite", 
        "Hornblende", 
        "Olivine"
      ),
      Descriptions = c(
        "Colorless in PPL, low relief. First-order gray interference colors in XPL.", 
        "Strong pleochroism from light brown to dark brown in PPL. Bird's-eye extinction in XPL.",
        "Polysynthetic twinning visible in XPL. Low relief in PPL, generally colorless.", 
        "Colorless in PPL with high birefringence. Bird's-eye extinction in XPL.",
        "Green to brown pleochroism in PPL. Oblique extinction in XPL.", 
        "High relief in PPL, colorless to pale green. High order interference colors in XPL."
      ),
      Rocks_type = c(
        "Common in all rock types. Essential mineral in granites and metamorphic rocks.",
        "Common in igneous rocks. Major component in granites and gneisses.",
        "Essential in most igneous rocks. Common in volcanic and plutonic rocks.",
        "Common in metamorphic rocks. Important in schists and gneisses.",
        "Common in intermediate to mafic igneous rocks. Found in amphibolites.",
        "Characteristic of mafic and ultramafic rocks. Common in basalt and peridotite."
      ),
      Reference = c(
        "Nesse, 2011; Deer et al., 1992",
        "Mackenzie et al., 1982; Nesse, 2011",
        "Blatt & Tracy, 1996; Klein & Dutrow, 2007",
        "Deer et al., 1992; Nesse, 2011",
        "Mackenzie et al., 1982; Klein & Dutrow, 2007",
        "Deer et al., 1992; Nesse, 2011"
      ),
      stringsAsFactors = FALSE
    )
    
    # Save the simple library to a file
    tryCatch({
      # Create directory if it doesn't exist
      library_dir <- "data/mineral_libraries"
      if(!dir.exists(library_dir)) {
        dir.create(library_dir, recursive = TRUE)
      }
      
      # Save the file
      write.csv(simple_data, "data/mineral_libraries/simple_mineral_library.csv", row.names = FALSE)
      library_status(paste0("Created simple mineral library with ", nrow(simple_data), " entries."))
    }, error = function(e) {
      library_status(paste0("Error creating simple library: ", e$message))
    })
    
    return(simple_data)
  }
  
  # Try to use the default library instead of loading from file
  observeEvent(input$useDefaultLibrary, {
    default_lib <- createDefaultLibrary()
    mineral_library(default_lib)
    library_status(paste0("Using built-in default library with ", nrow(default_lib), " entries."))
  })
  
  # Create a simple custom library when the button is pressed
  observeEvent(input$createCustomLibrary, {
    simple_lib <- createSimpleLibrary()
    mineral_library(simple_lib)
    library_status(paste0("Created and loaded simple custom library with ", nrow(simple_lib), " entries."))
  })
  
  # Initially use default library to avoid any issues
  observe({
    # Create default library but don't use it yet
    default_lib <- createDefaultLibrary()
    
    # Try to load user's library first (safely)
    tryCatch({
      # List of paths to try
      paths_to_try <- c(
        "data/mineral_libraries/biblio_for_mineral_descriptions.csv",
        "data/biblio_for_mineral_descriptions.csv",
        "biblio_for_mineral_descriptions.csv",
        "data/mineral_libraries/simple_mineral_library.csv"
      )
      
      # Try each path in order
      loaded_successfully <- FALSE
      for(path in paths_to_try) {
        if(file.exists(path)) {
          # Try to load the file with multiple approaches
          tryCatch({
            library_data <- read.csv(path, stringsAsFactors = FALSE, fill = TRUE)
            mineral_library(library_data)
            library_status(paste0("Loaded mineral library from ", path, " with ", nrow(library_data), " entries."))
            loaded_successfully <- TRUE
            break
          }, error = function(e1) {
            tryCatch({
              library_data <- read.table(path, header = TRUE, sep = ",", fill = TRUE, quote = "\"")
              mineral_library(library_data)
              library_status(paste0("Loaded mineral library from ", path, " using read.table."))
              loaded_successfully <- TRUE
              break
            }, error = function(e2) {
              library_status(paste0("Could not load mineral library from ", path, ". Using default library."))
            })
          })
        }
      }
      
      # If no user library was loaded, use the default
      if(!loaded_successfully) {
        mineral_library(default_lib)
        library_status("Using default mineral library because no custom library could be loaded.")
      }
    }, error = function(e) {
      # If any unhandled error occurs, fall back to the default library
      mineral_library(default_lib)
      library_status("An error occurred while loading libraries. Using default library.")
    })
  })
  
  # Display library status
  output$libraryStatus <- renderText({
    library_status()
  })
  
  # Load data from CSV files when the app starts
  observe({
    # Load minerals from CSV (two columns)
    tryCatch({
      minerals <- suppressMessages(read_csv("data/minerals.csv"))
      m_choices <- setNames(minerals$mineral_id, minerals$mineral_name)
      mineral_choices(m_choices)
    }, error = function(e) {
      library_status(paste0(library_status(), "\nError loading minerals.csv: ", e$message))
    })
    
    # Load textures from CSV (two columns)
    tryCatch({
      textures <- suppressMessages(read_csv("data/textures.csv"))
      t_choices <- setNames(textures$texture_id, textures$texture_name)
      updateSelectInput(session, "textureSelect", choices = t_choices)
      texture_choices(t_choices)
    }, error = function(e) {
      library_status(paste0(library_status(), "\nError loading textures.csv: ", e$message))
    })
    
    # Load grain sizes from CSV (two columns)
    tryCatch({
      grainsizes <- suppressMessages(read_csv("data/grainsizes.csv"))
      g_choices <- setNames(grainsizes$grainsize_id, grainsizes$grainsize_name)
      updateSelectInput(session, "grainsizeSelect", choices = g_choices)
      grainsize_choices(g_choices)
    }, error = function(e) {
      library_status(paste0(library_status(), "\nError loading grainsizes.csv: ", e$message))
    })
  })
  
  # Helper function to get mineral name from ID
  getMineralNameFromId <- function(mineral_id) {
    choices <- mineral_choices()
    if(is.null(choices) || is.null(mineral_id) || mineral_id == "") return(NULL)
    
    # Find the name corresponding to the ID
    mineral_name <- names(which(choices == mineral_id))
    if(length(mineral_name) == 0) return(NULL)
    
    return(mineral_name)
  }
  
  # Helper function to find library entries for a mineral
  findLibraryEntriesForMineral <- function(mineral_name) {
    tryCatch({
      library_data <- mineral_library()
      if(is.null(library_data) || is.null(mineral_name)) return(NULL)
      
      # Convert mineral name to lowercase for case-insensitive matching
      mineral_name_lower <- tolower(mineral_name)
      
      # Find entries with matching mineral name (case-insensitive)
      if("Mineral" %in% names(library_data)) {
        matches <- which(tolower(library_data$Mineral) == mineral_name_lower)
        
        # If no exact match, try partial matching
        if(length(matches) == 0) {
          for(i in 1:nrow(library_data)) {
            if(!is.na(library_data$Mineral[i]) && 
               grepl(mineral_name_lower, tolower(library_data$Mineral[i]), fixed = TRUE)) {
              matches <- c(matches, i)
            }
          }
        }
        
        if(length(matches) == 0) return(NULL)
        
        return(library_data[matches, ])
      }
      
      return(NULL)
    }, error = function(e) {
      # If any error occurs while searching, just return NULL
      library_status(paste0(library_status(), "\nError finding library entries: ", e$message))
      return(NULL)
    })
  }
  
  # Create checkboxes for mineral selection
  output$mineralSelectUI <- renderUI({
    choices <- mineral_choices()
    if(is.null(choices)) return(NULL)
    
    checkboxes <- lapply(1:length(choices), function(i) {
      name <- names(choices)[i]
      value <- choices[i]
      div(class = "mineral-row",
          checkboxInput(
            inputId = paste0("mineral_", i),
            label = name,
            value = value %in% selected_minerals()
          )
      )
    })
    
    tagList(checkboxes)
  })
  
  # Update selected minerals when checkboxes change
  observe({
    choices <- mineral_choices()
    if(is.null(choices)) return()
    
    selected <- c()
    for(i in 1:length(choices)) {
      checkbox_id <- paste0("mineral_", i)
      if(!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
        selected <- c(selected, choices[i])
      }
    }
    selected_minerals(selected)
  })
  
  # Get list of image files from the www folder
  image_files <- tryCatch({
    if(dir.exists("www/images")) {
      list.files(path = "www/images", pattern = "\\.(jpg|jpeg|png|gif)$", full.names = FALSE)
    } else {
      # Create directory if it doesn't exist
      dir.create("www/images", recursive = TRUE, showWarnings = FALSE)
      character(0)
    }
  }, error = function(e) {
    library_status(paste0(library_status(), "\nError loading images: ", e$message))
    return(character(0))
  })
  
  # Display images in the container
  output$imageContainer <- renderUI({
    # Check if there are images to display
    if(length(image_files) == 0) {
      return(div(
        style = "text-align: center; padding: 20px;",
        h3("No images found"),
        p("Please add images to the www/images directory.")
      ))
    }
    
    # Create a list of image tags
    image_tags <- lapply(image_files, function(img) {
      # Check if this image is selected
      is_selected <- img %in% selected_images()
      selected_class <- if(is_selected) "selected-overlay" else NULL
      
      div(
        class = "thumbnail-container",
        tags$img(
          src = file.path("images", img),
          class = "clickable-img",
          style = "max-width: 100%; max-height: 250px; object-fit: contain;",
          onclick = sprintf("Shiny.setInputValue('clicked_image', '%s', {priority: 'event'});", img)
        ),
        if(is_selected) div(class = "selected-overlay"),
        tags$p(img, style = "text-align: center;")
      )
    })
    
    # Return the list of image tags
    tagList(image_tags)
  })
  
  # Show the number of images selected
  output$selectedImageCount <- renderText({
    count <- length(selected_images())
    if(count == 0) {
      "No images selected"
    } else {
      paste(count, "image(s) selected")
    }
  })
  
  # Handle image click event - toggle selection
  observeEvent(input$clicked_image, {
    img <- input$clicked_image
    current_selections <- selected_images()
    
    if(img %in% current_selections) {
      # Deselect the image
      current_selections <- current_selections[current_selections != img]
    } else {
      # Select the image
      current_selections <- c(current_selections, img)
    }
    
    selected_images(current_selections)
  })
  
  # Clear selected images
  observeEvent(input$clearSelectedImages, {
    selected_images(c())
  })
  
  # Function to create mineral library panel
  createMineralLibraryPanel <- function(mineral_id, position) {
    tryCatch({
      # Get mineral name from ID
      mineral_name <- getMineralNameFromId(mineral_id)
      if(is.null(mineral_name)) {
        return(div(
          class = "mineral-library-panel",
          p(paste("No mineral name found for ID:", mineral_id))
        ))
      }
      
      # Find library entries for this mineral
      entries <- findLibraryEntriesForMineral(mineral_name)
      if(is.null(entries) || nrow(entries) == 0) {
        return(div(
          class = "mineral-library-panel",
          p(paste("No library entries found for mineral:", mineral_name))
        ))
      }
      
      # Create sections for each column
      sections <- list()
      
      # Descriptions section
      if("Descriptions" %in% names(entries)) {
        descriptions <- unique(entries$Descriptions)
        descriptions <- descriptions[!is.na(descriptions) & descriptions != ""]
        
        if(length(descriptions) > 0) {
          sections$descriptions <- div(
            class = "library-section",
            div(class = "library-title", "Descriptions"),
            lapply(descriptions, function(desc) {
              div(
                class = "library-entry",
                onclick = sprintf(
                  "var textArea = document.getElementById('morphology_%s'); if(textArea) { if(textArea.value) textArea.value += '\\n'; textArea.value += '%s'; textArea.dispatchEvent(new Event('input')); }",
                  position, gsub("'", "\\\\'", desc)
                ),
                desc
              )
            })
          )
        }
      }
      
      # Rock types section
      if("Rocks_type" %in% names(entries)) {
        rock_types <- unique(entries$Rocks_type)
        rock_types <- rock_types[!is.na(rock_types) & rock_types != ""]
        
        if(length(rock_types) > 0) {
          sections$rock_types <- div(
            class = "library-section",
            div(class = "library-title", "Rock Types"),
            lapply(rock_types, function(rt) {
              div(
                class = "library-entry",
                onclick = sprintf(
                  "var textArea = document.getElementById('morphology_%s'); if(textArea) { if(textArea.value) textArea.value += '\\n'; textArea.value += '%s'; textArea.dispatchEvent(new Event('input')); }",
                  position, gsub("'", "\\\\'", rt)
                ),
                rt
              )
            })
          )
        }
      }
      
      # References section
      if("Reference" %in% names(entries)) {
        references <- unique(entries$Reference)
        references <- references[!is.na(references) & references != ""]
        
        if(length(references) > 0) {
          sections$references <- div(
            class = "library-section",
            div(class = "library-title", "References"),
            lapply(references, function(ref) {
              div(
                class = "library-entry",
                onclick = sprintf(
                  "var textArea = document.getElementById('morphology_%s'); if(textArea) { if(textArea.value) textArea.value += '\\n'; textArea.value += '[%s]'; textArea.dispatchEvent(new Event('input')); }",
                  position, gsub("'", "\\\\'", ref)
                ),
                ref
              )
            })
          )
        }
      }
      
      # Return the complete panel if we have sections
      if(length(sections) > 0) {
        div(
          class = "mineral-library-panel",
          h4(paste("Reference Information for", mineral_name)),
          do.call(tagList, sections)
        )
      } else {
        div(
          class = "mineral-library-panel",
          p("No reference information available for this mineral.")
        )
      }
    }, error = function(e) {
      # Return a simple error message if anything goes wrong
      library_status(paste0(library_status(), "\nError in library panel: ", e$message))
      div(
        class = "mineral-library-panel",
        p("Error loading reference information. Please try again.")
      )
    })
  }
  
  # Open description form with multiple selected images
  observeEvent(input$openDescriptionForm, {
    imgs <- selected_images()
    
    if(length(imgs) < 1) {
      showNotification("Please select at least one image first!", type = "error")
      return()
    }
    
    # Display modal with the selected images and form
    showModal(
      modalDialog(
        title = "Description of the Rock. Thin Section",
        size = "l",
        
        # Display selected images
        div(class = "multi-image-container",
            lapply(imgs, function(img) {
              tags$div(class = "text-center",
                       style = "margin: 5px;",
                       tags$img(src = file.path("images", img), 
                                style = "max-height: 200px; max-width: 200px;"),
                       tags$p(img, style = "font-size: smaller;")
              )
            })
        ),
        
        # Top row fields - moved up as requested
        fluidRow(
          column(3, textInput("thinSectionNo", "Thin Section No:")),
          column(3, textInput("rockName", "Rock Name:")),
          column(3, selectInput("selectedTexture", "Texture:", choices = texture_choices())),
          column(3, selectInput("selectedGrainsize", "Grain Size:", choices = grainsize_choices()))
        ),
        
        hr(),
        
        # Mineral fields - scrollable
        h4("Minerals"),
        div(style = "max-height: 400px; overflow-y: auto;",
            uiOutput("mineralFormFields")
        ),
        
        # Total percentage display
        div(style = "margin-top: 10px; margin-bottom: 10px; font-weight: bold;",
            textOutput("totalMineralPercent")
        ),
        
        hr(),
        
        # Additional description
        textAreaInput("rockDescription", "Additional Description:", rows = 4),
        
        footer = tagList(
          actionButton("saveDescription", "Save Description", class = "btn-success"),
          modalButton("Close")
        ),
        easyClose = TRUE
      )
    )
  })
  
  # Generate the mineral form fields for the modal
  output$mineralFormFields <- renderUI({
    # Get number of selected minerals from checkboxes (min 3, max 10)
    num_minerals <- max(3, min(10, length(selected_minerals())))
    
    # Create mineral selection fields
    mineral_fields <- lapply(1:num_minerals, function(i) {
      # Get the mineral_id for this position
      mineral_id <- if(i <= length(selected_minerals())) selected_minerals()[i] else ""
      
      # Get mineral name for this ID
      mineral_name <- names(which(mineral_choices() == mineral_id))
      if(length(mineral_name) == 0) mineral_name <- ""
      
      div(class = "mineral-selection",
          h4(paste("Mineral", i)),
          fluidRow(
            column(4, 
                   selectInput(inputId = paste0("descMineral_", i), 
                               label = "Type:", 
                               choices = mineral_choices(),
                               selected = mineral_id)
            ),
            column(2, 
                   numericInput(inputId = paste0("descMineral_percent_", i), 
                                label = "Percentage (%):", 
                                value = 0,
                                min = 0, max = 100)
            ),
            column(6,
                   textAreaInput(inputId = paste0("morphology_", i),
                                 label = "Morphology, features, genesis:",
                                 rows = 3,
                                 placeholder = "Description of mineral characteristics")
            )
          ),
          # Add mineral library panel
          uiOutput(paste0("libraryPanel_", i))
      )
    })
    
    tagList(mineral_fields)
  })
  
  # Create dynamic library panels for each mineral position
  observe({
    # For each possible mineral position
    for(i in 1:10) {
      local({
        local_i <- i
        mineral_id_name <- paste0("descMineral_", local_i)
        
        # When the mineral selection changes, update the library panel
        observeEvent(input[[mineral_id_name]], {
          selected_mineral_id <- input[[mineral_id_name]]
          output_id <- paste0("libraryPanel_", local_i)
          
          # Update the library panel
          output[[output_id]] <- renderUI({
            if(!is.null(selected_mineral_id) && selected_mineral_id != "") {
              createMineralLibraryPanel(selected_mineral_id, local_i)
            } else {
              div(class = "mineral-library-panel",
                  p("Select a mineral to see reference information"))
            }
          })
        }, ignoreNULL = FALSE, ignoreInit = FALSE)
      })
    }
  })
  
  # Calculate and display total mineral percentage
  output$totalMineralPercent <- renderText({
    total <- 0
    for(i in 1:10) {
      percent_id <- paste0("descMineral_percent_", i)
      if(!is.null(input[[percent_id]])) {
        total <- total + input[[percent_id]]
      }
    }
    
    color <- if(total == 100) "green" else if(total > 100) "red" else "orange"
    HTML(paste0("<span style='color:", color, ";'>Total Mineral Percentage: ", total, "% ", 
                if(total != 100) "(should be 100%)" else "(✓)", "</span>"))
  })
  
  # Save description of the rock
  observeEvent(input$saveDescription, {
    # Get the currently selected images
    imgs <- selected_images()
    
    if(length(imgs) > 0) {
      # Collect mineral data
      minerals <- list()
      for(i in 1:10) {
        mineral_id_name <- paste0("descMineral_", i)
        percent_id <- paste0("descMineral_percent_", i)
        morphology_id <- paste0("morphology_", i)
        
        if(!is.null(input[[mineral_id_name]]) && !is.null(input[[percent_id]]) && input[[percent_id]] > 0) {
          selected_mineral <- input[[mineral_id_name]]
          
          minerals[[i]] <- list(
            id = selected_mineral,
            percent = input[[percent_id]],
            morphology = if(!is.null(input[[morphology_id]])) input[[morphology_id]] else ""
          )
        }
      }
      
      # Create a list of description data
      desc_data <- list(
        thinSectionNo = input$thinSectionNo,
        rockName = input$rockName,
        minerals = minerals,
        texture = input$selectedTexture,
        grainsize = input$selectedGrainsize,
        description = input$rockDescription,
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        user = "westabraЧто"
      )
      
      # Update the stored descriptions for each selected image
      all_desc <- rock_descriptions()
      for(img in imgs) {
        all_desc[[img]] <- desc_data
      }
      rock_descriptions(all_desc)
      
      # Show a confirmation message and close the modal
      removeModal()
      showNotification(paste("Description saved for", length(imgs), "image(s)!"), type = "message")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)