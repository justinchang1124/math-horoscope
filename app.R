#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)

#' whether x is a character of length n
#'
#' @param x [object]
#' @param n [int] not checked
#' @returns [boolean]
is_str <- function(x, n = 1L)
{
  is.character(x) && length(x) == n
}

#' whether fun(x) is TRUE for all x in X
#' note: succeeds if x is length 0
#'
#' @param X [vector] not checked
#' @param FUN [function] not checked
#' @returns [boolean]
all_fun_true <- function(X, FUN)
{
  all(vapply(X, FUN, FALSE))
}

is_options_obj <- function(x)
{
  is.list(x) && is.character(unlist(x))
}

is_data_card <- function(x)
{
  members <- c(
    "detail",
    "image",
    "caption",
    "prompt",
    "options",
    "footnote"
  )

  is.list(x) && identical(names(x), members) &&
    is_str(x[["detail"]]) && is_str(x[["image"]]) &&
    is_str(x[["caption"]]) && is_str(x[["prompt"]]) &&
    is_options_obj(x[["options"]]) && is_str(x[["footnote"]])
}

data_cards <- list(
  "Philosopher" = list(
    "detail" = "Welcome to the equivalent of a Buzzfeed quiz about mathematical philosophy - answer as many or as few times as you like.",
    "image" = "start.jpg",
    "caption" = "",
    "prompt" = "Do numbers exist?",
    "options" = list(
      "Yes" = "Realist",
      "No" = "Nominalist"
    ),
    "footnote" = "By numbers, we refer to positive integers: the prime example of mathematical objects. Zero, negative numbers, irrational numbers, complex numbers, geometric figures, etc. are all debatable as well, but that is outside the scope of this horoscope."
  ),
  "Realist" = list(
    "detail" = "",
    "image" = "",
    "caption" = "",
    "prompt" = "Were numbers discovered or invented?",
    "options" = list(
      "Discovered" = "Epistemic Realist",
      "Invented" = "Idealist"
    ),
    "footnote" = "More formally, we ask whether mathematical objects exist outside of time and space. If they do, they cannot be causally influenced by anything we do and therefore cannot be invented. This is a major epistemological challenge; how do we access acausal concepts?"
  ),
  "Epistemic Realist" = list(
    "detail" = "",
    "image" = "",
    "caption" = "",
    "prompt" = "How do we discover math?",
    "options" = list(
      "Inductively (Science)" = "Naturalist",
      "Deductively (Mind)" = "platonist"
    ),
    "footnote" = "The epistemological Integration Challenge asks how we can access acausal objects in order to reason about them. We might also ask how we can access concrete objects; the answer requires a better understanding of consciousness."
  ),
  "Naturalist" = list(
    "detail" = "",
    "image" = "quine.jpg",
    "caption" = "W. V. O. Quine",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "platonist" = list(
    "detail" = "",
    "image" = "godel.jpg",
    "caption" = "Kurt Godel",
    "prompt" = "(A) Are numbers more real than the senses? (B) Can you logically derive numbers?",
    "options" = list(
      "A" = "Platonist",
      "B" = "Logicist",
      "Both" = "Platonic Logicist"
    ),
    "footnote" = ""
  ),
  "Platonist" = list(
    "detail" = "",
    "image" = "Plato_Aristotle.webp",
    "caption" = "Plato, Aristotle",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "Platonic Logicist" = list(
    "detail" = "",
    "image" = "",
    "caption" = "",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "Logicist" = list(
    "detail" = "",
    "image" = "frege.jpg",
    "caption" = "Gottlob Frege",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "Idealist" = list(
    "detail" = "",
    "image" = "",
    "caption" = "",
    "prompt" = "Where does math happen?",
    "options" = c(
      "Representation" = "Formalist",
      "Thought" = "Mentalist"
    ),
    "footnote" = "More specifically, what do we call mathematics: the concrete representations we create (textbooks, papers, computers) or the thoughts in our minds?"
  ),
  "Formalist" = list(
    "detail" = "If you also believe math has no relation to truth, you are a game formalist. After all, the best move in a chess game has no intrinsic truth value.",
    "image" = "",
    "caption" = "",
    "prompt" = "Can mathematical thought be nonlinear?",
    "options" = list(
      "Yes" = "Diagrammaticist",
      "No" = "Sententialist"
    ),
    "footnote" = "Nonlinearity implies the existence of mathematical arguments that can be processed in multiple ways."
  ),
  "Diagrammaticist" = list(
    "detail" = "Dr. Shin (below) developed a revolutionary system of diagrammatic reasoning in 1995, therefore overturning sententenialism (the idea that mathematical thought must be linear).",
    "image" = "shin.jpg",
    "caption" = "Sun Joo Shin",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "Sententialist" = list(
    "detail" = "",
    "image" = "hilbert.jpg",
    "caption" = "David Hilbert",
    "prompt" = "",
    "options" = list(),
    "image" = "",
    "footnote" = ""
  ),
  "Mentalist" = list(
    "detail" = "",
    "image" = "",
    "caption" = "",
    "prompt" = "Is space a fundamental human intuition?",
    "options" = list(
      "Yes" = "Transcendental Idealist",
      "No" = "Intuitionist"
    ),
    "footnote" = "By space, we mean the ability to imagine things with relative position. So visual impairment does not exclude spatial intuition."
  ),
  "Transcendental Idealist" = list(
    "detail" = "",
    "image" = "kant.jpg",
    "caption" = "Immanuel Kant",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "Intuitionist" = list(
    "detail" = "",
    "image" = "brouwer.jpeg",
    "caption" = "L. E. J. Brouwer",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "Nominalist" = list(
    "detail" = "",
    "image" = "",
    "caption" = "",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "Model Structuralist" = list(
    "detail" = "",
    "image" = "benacerraf.jpg",
    "caption" = "Paul Benacerraf",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "Fictionalist" = list(
    "detail" = "",
    "image" = "rosen.jpg",
    "caption" = "Gideon Rosen",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "Bedrocker" = list(
    "detail" = "",
    "image" = "",
    "caption" = "",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  )
)

all_fun_true(data_cards, is_data_card)

# Define UI for application that draws a histogram
ui <- function(request)
{
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Mathematical Philosophy Horoscope", titleWidth = "100%"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      tags$head(includeCSS("app_styling.css")),
      column(12, align = "center", uiOutput("card")),
      actionButton("undo", "Undo", icon = icon("undo"), width = "100%", style =
                     "color: white; background-color: #C90016; border-color: #00356B"),
      actionButton("reset", "Reset", icon = icon("trash"), width = "100%", style =
                     "color: white; background-color: black; border-color: #00356B")
    )
  )
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  path <- reactiveVal("Philosopher")

  observeEvent(input$undo, {
    n <- length(path())
    if (n > 1)
      path(path()[-length(path())])
  })

  observeEvent(input$reset, {
    path("Philosopher")
  })

  observeEvent(input$user_picker, {
    current <- tail(path(), 1)
    data_card <- data_cards[[current]]
    card_options <- data_card[["options"]]
    path(c(path(), card_options[[input$user_picker]]))
  })

  output$card <- renderUI({
    current <- tail(path(), 1)
    data_card <- data_cards[[current]]
    card_options <- data_card[["options"]]

    image <- NULL
    image_path <- data_card[["image"]]
    if (image_path != "")
      image <- img(src = image_path, align = "center", width = "50%")

    prompt_choices <- NULL
    if (length(card_options) > 0)
    {
      prompt_choices <- checkboxGroupButtons(
        inputId = "user_picker",
        label = data_card[["prompt"]],
        choices = names(card_options),
        status = "primary",
        individual = TRUE,
        justified = TRUE
      )

      prompt_choices$children[[3]]$children[[1]] <- HTML(gsub(
        'role="group"',
        'role="group" style="padding: 5px"',
        prompt_choices$children[[3]]$children[[1]]
      ))
    }

    footnote <- NULL
    if (data_card[["footnote"]] != "")
      footnote <- HTML(sprintf("<i>%s</i>", data_card[["footnote"]]))

    detail <- NULL
    if (data_card[["detail"]] != "")
      detail <- HTML(sprintf("<div>%s</div>", data_card[["detail"]]))

    caption <- NULL
    if (data_card[["caption"]] != "")
      caption <- HTML(sprintf("<div><i>%s</i></div>", data_card[["caption"]]))

    spacing <- NULL
    if (!is.null(prompt_choices) && (!is.null(detail) || !is.null(caption)))
      spacing <- HTML("<br><br>")

    box(
      detail,
      image,
      caption,
      spacing,
      prompt_choices,
      footnote,
      title = HTML(sprintf("<b>%s</b>", current)),
      collapsible = FALSE,
      width = "100%"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
