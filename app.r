library(shiny)

ui <- fluidPage(
  headerPanel("PREZICEREA PREȚULUI CASEI"),
  sidebarPanel(
    selectInput("area", "Alegeți zona",
                list("Zona construită 1",
                     "Zona super construită 4",
                     "Zona de teren 3",
                     "Zona de covor 2")),
    textInput("area_id", "Introduceți id-ul zonei pe care îl puteți vedea mai sus", ""),
    textInput("location_id", "Introduceți codul poștal al locației", ""),
    textInput("bhk", "Câte dormitoare doriți", ""),
    textInput("sqft", "Introduceți suprafața totală în mp", ""),
    textInput("bath", "Câte băi doriți", ""),
    textInput("balcony", "Câte balcoane doriți", ""),
    actionButton('go', "Prezice")
  ),
  mainPanel(
    sidebarPanel(width = 25,
                 headerPanel("PREȚUL CASEI ESTE"),
                 textOutput("value")
    ))
  
)

server <- function(input, output) {
  
  predictData = reactiveValues()
  observeEvent(input$go, {
    
    data = read.csv("app.csv")
    View(data)
    summary(data)
    str(data)
    
    is.factor(data$area_type)
    is.factor(data$location)
    
    data$area_type = as.factor(data$area_type)
    data$location = as.factor(data$location)
    
    str(data$area_type)
    str(data$location)
    str(data)
    
    usableData = data[, c("area_type", "location", "size", "total_sqft", "bath", "balcony", "price")]
    head(usableData)
    summary(usableData)
    
    cleanedDF = na.omit(usableData)
    summary(cleanedDF)
    str(cleanedDF)
    View(cleanedDF)
    
    area.type = sapply(cleanedDF$area_type, as.numeric)
    View(area.type)
    
    data.location = sapply(cleanedDF$location, as.numeric)
    View(data.location)
    
    second_final = cbind(cleanedDF, area_id = area.type)
    View(second_final)
    
    second_main_dataset = cbind(second_final, location_id = data.location)
    View(second_main_dataset)
    
    Main_data_set = second_main_dataset[, c("area_type", "area_id", "location", "location_id", "size", "total_sqft", "bath", "balcony", "price")]
    View(Main_data_set)
    
    inputDF = Main_data_set[, c("area_id", "location_id", "size", "total_sqft", "bath", "balcony", "price")]
    
    View(inputDF)
    
    
    predictData$myarea_id <- as.numeric(input$area_id)
    predictData$myloaction_id <- as.numeric(input$location_id)
    predictData$mybhk <- as.numeric(input$bhk)
    predictData$mysqft <- as.numeric(input$sqft)
    predictData$mybath <- as.numeric(input$bath)
    predictData$mybalcony <- as.numeric(input$balcony)
    
    newPredict = data.frame(area_id = predictData$myarea_id, location_id = predictData$myloaction_id,
                            size = predictData$mybhk, total_sqft = predictData$mysqft,
                            bath = predictData$mybath, balcony = predictData$mybalcony)
    
    model = lm(price ~ area_id + location_id + size + total_sqft + bath + balcony,
               data = inputDF, weights = 1 / inputDF$price ^ 1.9)
    
    predictData$op = predict(model, newPredict)
  })
  
  output$value <- renderPrint({ predictData$op })
}

shinyApp(ui, server)
