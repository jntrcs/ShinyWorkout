#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinyjs)
workouts = read_csv("Workouts.csv") %>% filter(!Archived)
warmups = workouts %>% filter(Muscle_Category=="Warmup")
exercises= workouts %>% filter(!Muscle_Category=="Warmup")

choose_new_exercise <- function(current_workout, last_category=NULL){
    #Every week from the debut, I want the upper limits of the workout to increase by 4%
    
    if(nrow(current_workout>=2)){
    weekly_multiplier =
        1+as.numeric(difftime(Sys.Date(), lubridate::ymd("2021-03-27"), units="days"))/7 * .04
    }else weekly_multiplier=1
    
    difficulty = runif(1) * weekly_multiplier
    if (nrow(current_workout)<2){
        warmups %>% filter(!Workout %in%current_workout$Workout) %>%
            slice_sample(n=1) %>%
            mutate(Duration = round((Max-Min)*difficulty+Min),
                   Difficulty = (Duration)/(Max),
                   Time=Sys.time(),
                   Points = Difficulty * Intensity)
    }else{
        exercises %>% filter(Muscle_Category != last_category) %>%
            slice_sample(n=1) %>%
            mutate(Duration = round((Max-Min)*difficulty+Min),
                   Difficulty = (Duration)/(Max),
                   Time=Sys.time(),
                   Points = Difficulty * Intensity)
    }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),

    # Application title
    titlePanel("Jackson's Custom Workout"),
    tabsetPanel(                
        tabPanel(title="Workout",
                 actionButton("NewWorkout", "New Workout"),
                 uiOutput("WorkoutInstructions"),
                 uiOutput("WorkoutButtons"),
                 tableOutput("WorkoutTable"),
                 textOutput("SavedMessage"),
                 uiOutput("EndWorkout")),
        tabPanel(title="Analysis", 
                 checkboxGroupInput("MuscleGroups", "Included Muscles", choices = unique(workouts$Muscle_Category),
                                    selected=unique(workouts$Muscle_Category), inline=T, width="100%"),
                 plotOutput("Performance")
                 )
        )

    # Sidebar with a slider input for number of bins 

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    shinyjs::hide("WorkoutButtons")
    shinyjs::hide("WorkoutInstructions")
    hide("SavedMessage")
    hide("EndWorkout")
    
    current_workout<-reactiveVal()
    current_exercise<-reactiveVal()
    pause_time<-reactiveVal()
    unpause_time<-reactiveVal()
    seconds_paused<-reactiveVal(0)
    previous_pause_lengths <- reactiveVal(0)
    paused<-reactiveVal(FALSE)
    
    observeEvent(input$NewWorkout, {
        shinyjs::hide(id="NewWorkout")
        shinyjs::show(id="WorkoutButtons")
        shinyjs::show(id="WorkoutInstructions")
        shinyjs::hide("Unpause")
        hide("SaveWorkout")
        hide("SavedMessage")
        seconds_paused(0)
        previous_pause_lengths(0)
        paused(FALSE)
        pause_time(NULL)
        unpause_time(NULL)
        current_workout(tibble(Workout=character(), Intensity=numeric(), Muscle_Category=character(),
                               Duration = numeric(),
                               Difficulty=numeric(), Points=numeric()))
        current_exercise(choose_new_exercise(current_workout()))
    })
    
    output$WorkoutButtons<-renderUI({
        fluidRow(actionButton("NextExercise", "Next Exercise"),
                 actionButton("Pause", "Pause Workout"),
                 actionButton("Unpause", "Unpause Workout"),
                 actionButton("EndWorkoutRecord", "Record and End Workout"),
                 actionButton("EndWorkoutAbandon", "Skip and End Workout"))
    })
    
    output$WorkoutInstructions<-renderUI({
        workout = current_exercise()
        invalidateLater(1000, session)
        
            if (paused()){
                seconds_paused(difftime(Sys.time(), pause_time(), units="secs"))
            }
    
            elapsed = round(difftime(Sys.time(), workout$Time, units="secs") - previous_pause_lengths() -
                                seconds_paused())
        list(h1(workout$Workout), 
             h1(paste(workout$Duration, if_else(workout$Type=="Time", "Seconds", "Reps"))),
             h1(paste("Time:", elapsed)))
    })
    


    observeEvent(input$Pause, {
        pause_time(Sys.time())
        paused(TRUE)
        hide("Pause")
        show("Unpause")
    })
    
    observeEvent(input$Unpause, {
        #unpause_time(Sys.time())
        previous_pause_lengths(previous_pause_lengths() + difftime(Sys.time(), pause_time(), units="secs"))
        seconds_paused(0)
        paused(FALSE)
        hide("Unpause")
        show("Pause")
    })
    
    output$WorkoutTable<-renderTable({
        current_workout()
    })
    
    observeEvent(input$NextExercise, {
        hide("Unpause")
        show("Pause")
        paused(FALSE)
        seconds_paused(0)
        pause_time(NULL)
        unpause_time(NULL)
        previous_pause_lengths(0)
        finished_ex = current_exercise() %>% 
            select(Workout, Intensity, Muscle_Category, Duration, Difficulty, Points)
        current_workout(bind_rows(current_workout(), finished_ex))
        current_exercise(choose_new_exercise(current_workout(), finished_ex$Muscle_Category))
    })
    
    observeEvent(input$EndWorkoutRecord, {
        finished_ex = current_exercise() %>% 
            select(Workout, Intensity, Muscle_Category, Duration, Difficulty, Points)
        current_workout(bind_rows(current_workout(), finished_ex))
        end_workout()
    })
    
    observeEvent(input$EndWorkoutAbandon, {
        end_workout()
    })
    
    output$SavedMessage<-renderText({"Saved!"})
    
    output$EndWorkout<-renderUI({
        list(actionButton("SaveWorkout", "Save Workout"))
    })
    
    end_workout<-function(){
        hide("WorkoutButtons")
        hide("WorkoutInstructions")
        show("EndWorkout")
        show("NewWorkout")
        show("SaveWorkout")
    }
    
    observeEvent(input$SaveWorkout, {
        hide("SaveWorkout")
        show("SavedMessage")
        all_workouts = read_csv("WorkoutHistory.csv")
        all_workouts %>% bind_rows(
            current_workout() %>% mutate(ID = sample.int(200000000, 1),
                                       Date=Sys.Date(),
                                       Workout = Workout, 
                                       Duration = Duration,
                                       Difficulty = Difficulty, .keep="none")
        ) %>% write_csv("WorkoutHistory.csv")

    })
    
    output$Performance<-renderPlot({
        workout_history = 
            read_csv("WorkoutHistory.csv") %>% drop_na() %>%
            left_join(workouts) %>%
            mutate(Points = Difficulty * Intensity) %>%
            filter(Muscle_Category %in% input$MuscleGroups)
        ggplot(workout_history, aes(x=Date, y=Points, fill=Muscle_Category))+
            geom_col()+
            scale_fill_discrete(NULL)+
            theme_minimal()+
            xlab(NULL)+
            ylab("Intensity")
    })
    

   
}

# Run the application 
shinyApp(ui = ui, server = server)
