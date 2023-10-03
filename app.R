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
    "detail" = "Welcome! Pretend this is a Buzzfeed quiz about mathematical philosophy.",
    "image" = "start.jpg",
    "caption" = "Relax and have fun!",
    "prompt" = "Do numbers exist?",
    "options" = list(
      "Yes" = "Realist",
      "No" = "Nominalist"
    ),
    "footnote" = "By numbers, we refer to positive integers like one or five. Zero, negative numbers, irrational numbers, complex numbers, geometric figures, etc. are for later."
  ),
  "Realist" = list(
    "detail" = "You are a realist. Although you've never seen a number (or any other mathematical concept), you know they're out there somewhere.",
    "image" = "apple.svg",
    "caption" = "I see five apples, two apples, and three apples. You know what I don't see? The abstract concepts of five, two, and three.",
    "prompt" = "Is mathematics discovered or invented?",
    "options" = list(
      "Discovered" = "Epistemic Realist",
      "Invented" = "Idealist"
    ),
    "footnote" = "In other words: do mathematical objects exist outside of time and space? If they do, they cannot be causally influenced by anything we do and must be discovered, not invented. But if mathematical objects are acausal concepts, how do we access them? This is a major epistemological challenge."
  ),
  "Epistemic Realist" = list(
    "detail" = "You are an epistemic realist. You believe that one plus one is, has, and always will be two. Therefore, mathematicians are geographers, not inventors.",
    "image" = "patent.webp",
    "caption" = "The US Patent Office agrees, preventing the patent of pure mathematics. This also prevented software programs from being patented until 1968.",
    "prompt" = "How do we discover math?",
    "options" = list(
      "Inductively (Science)" = "Naturalist",
      "Deductively (Mind)" = "platonist"
    ),
    "footnote" = "The epistemological Integration Challenge asks how we can access acausal objects in order to reason about them. We might also ask how we can access concrete objects - a question that's been around for as long as the idea of consciousness has."
  ),
  "Naturalist" = list(
    "detail" = "You are a naturalist. To you, mathematics exists to serve empiricism: the hard sciences first and foremost. A better system can always dethrone mathematics in the same way that mathematics dethroned rational metaphysics: by being a better tool for physicists, chemists, biologists, and so forth.",
    "image" = "quine.jpg",
    "caption" = "W. V. O. Quine (1908 CE - 2000 CE)",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "platonist" = list(
    "detail" = "You are a platonist (lowercase 'p'). You treat mathematical objects with as much respect as real-world objects. Your views align with a plurality of modern mathematicians.",
    "image" = "godel.jpg",
    "caption" = "Kurt Godel (1906 CE - 1978 CE)",
    "prompt" = "(A) Are numbers more real than the senses? (B) Can you logically derive numbers?",
    "options" = list(
      "A" = "Platonist",
      "B" = "Logicist",
      "Both" = "Platonic Logicist"
    ),
    "footnote" = ""
  ),
  "Platonist" = list(
    "detail" = "You are a Platonist (uppercase 'P'). You view mathematical objects as pure, abstract truths: more reliable than the fallible senses.",
    "image" = "Plato_Aristotle.webp",
    "caption" = "Plato (428/427 BCE - 348/347 BCE), Aristotle (384 BCE - 322 BCE)",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "Platonic Logicist" = list(
    "detail" = "You are a Platonist (uppercase 'P') and a logicist. You view mathematical objects as pure and logical: more trustworthy than the fallible senses. Additionally, you believe that mathematics can be entirely derived from logical reasoning. That's an ambitious claim!",
    "image" = "",
    "caption" = "",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "Logicist" = list(
    "detail" = "You are a logicist. You believe that mathematics can be entirely derived from logical reasoning. Unfortunately, Godel's Incompleteness Theorem prevents this for any system capable of basic arithmetic.",
    "image" = "frege.jpg",
    "caption" = "Gottlob Frege (1848-1925)",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  ),
  "Idealist" = list(
    "detail" = "You are an idealist. Num",
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
    "detail" = "You are a sententialist. To you, pictures and diagrams are aids in understanding, but bring mathematicians no closer to the truth.",
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
    "prompt" = "So why does math work?",
    "options" = list(
      "It lets us structure experiences." = "Model Structuralist",
      "It's a useful fiction." = "Fictionalist"
    ),
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
    "detail" = "You view mathematicians as novelists, although you appreciate the rarity of plot holes in their stories.",
    "image" = "rosen.jpg",
    "caption" = "Gideon Rosen",
    "prompt" = "Would treating math as fiction improve society?",
    "options" = list("Yes" = "Bedrocker"),
    "footnote" = ""
  ),
  "Bedrocker" = list(
    "detail" = "You speak in careful and concrete terms. You preface your thoughts about fiction with 'If the events of this work had occurred in real life ...'",
    "image" = "bedrock.png",
    "caption" = "Your thoughts are as finite and immovable as Minecraft bedrock.",
    "prompt" = "",
    "options" = list(),
    "footnote" = ""
  )
)

all_fun_true(data_cards, is_data_card)

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

add_font_size <- function(msg, font_size)
{
  HTML(sprintf("<div style='font-size: %spx'>%s</div>", font_size, msg))
}

server <- function(input, output) {
  visits <- reactiveVal("Philosopher")

  observeEvent(input$undo, {
    visited <- visits()
    n <- length(visited)
    if (n > 1)
      visits(visited[-n])
  })

  observeEvent(input$reset, {
    visits("Philosopher")
  })

  observeEvent(input$buttons, {
    visited <- visits()
    current <- tail(visited, 1)
    data_card <- data_cards[[current]]
    card_options <- data_card[["options"]]
    visits(c(visited, card_options[[input$buttons]]))
  })

  output$card <- renderUI({
    visited <- visits()
    current <- tail(visited, 1)
    data_card <- data_cards[[current]]

    detail <- NULL
    if (data_card[["detail"]] != "")
      detail <- add_font_size(sprintf("%s<br><br>", data_card[["detail"]]), 18)

    image <- NULL
    image_path <- data_card[["image"]]
    if (image_path != "")
      image <- img(src = image_path, align = "center", width = "50%")

    caption <- NULL
    if (data_card[["caption"]] != "")
      caption <- add_font_size(tags$i(data_card[["caption"]]), 18)

    # BUTTONS
    buttons <- NULL
    button_choices <- names(data_card[["options"]])
    if (length(button_choices) > 0)
    {
      buttons <- checkboxGroupButtons(
        inputId = "buttons",
        label = add_font_size(data_card[["prompt"]], 18),
        choices = button_choices,
        status = "primary",
        individual = TRUE,
        justified = TRUE
      )

      buttons$children[[3]]$children[[1]] <- HTML(gsub(
        'role="group"',
        'role="group" style="padding: 5px"',
        buttons$children[[3]]$children[[1]]
      ))
    }

    spacing2 <- NULL
    if (!is.null(buttons) && !is.null(image))
      spacing2 <- HTML("<br><br>")

    footnote <- NULL
    if (data_card[["footnote"]] != "")
      footnote <- add_font_size(tags$i(data_card[["footnote"]]), 18)

    box(
      detail,
      image,
      caption,
      spacing2,
      buttons,
      footnote,
      title = HTML(sprintf("<div style='font-size: 36px'>Perspective: <b>%s</b></div>", current)),
      collapsible = FALSE,
      width = "100%"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
