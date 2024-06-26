# Zuerst die benötigten Bibliotheken laden
library(shiny)
library(ggplot2)
library(MASS)

# Benutzeroberfläche der Shiny-App definieren
ui <- shinyUI(fluidPage(withMathJax(),
                        titlePanel(span("t-Test versus F-Test", style="color:white"),
                                   windowTitle="t-Test versus F-Test"),
  
      sidebarLayout(position="right",
        sidebarPanel(
          wellPanel(style = "background-color: #FFFFFF;", h5("Bedienfenster"),
          sliderInput("n", "Beobachtungsumfang \\( (N) \\)", value = 30, min = 10, max = 500, step = 1),
          sliderInput("alpha", "Signifikanzniveau \\( (\\%) \\)", value = 0.05, min = 0.01, max = 0.50, step = 0.01),
          numericInput("beta1_null", "Nullhypothese für \\( \\beta_{1} \\)", value = 0.33, step = 0.01),
          numericInput("beta2_null", "Nullhypothese für \\( \\beta_{2} \\)", value = 0.33, step = 0.01),
          br(),
          sliderInput("rho", "Korrelationskoeffizient von \\( \\beta_{1} \\) und \\( \\beta_{2} \\)", min = -1, max = 1, value = 0, step = 0.01),
          br(),
          shinysky::actionButton('generate', 'Stichprobenerzeugung', styleclass='success'),
          downloadButton('downloadPlot', 'Grafik herunterladen')),
          br(),
          br(),
          
      wellPanel(p(strong("Redaktion"), style='margin-bottom:1px;color:black;'),
                HTML("<p style='margin-bottom:1px;color:black;'>Programmierung: Luca Grün</p>"),
                p("Text: Ludwig von Auer", style="color:black"),
                HTML("<a , style='margin-bottom:1px;color:black;' href = 'https://www.uni-trier.de/index.php?id=50126' target='_blank'>Professur für Finanzwissenschaft</a>"),
                p("Fachbereich IV - Volkswirtschaftslehre", style = 'margin-bottom:1px;color:black;'),
                p("Universität Trier", style="color:black"),
                p(strong("Lehrbuch"), style = 'margin-bottom:1px; color:black;'),
                HTML("<p style = 'color:black;'>Auer, L. <a href = 'https://www.uni-trier.de/index.php?id=15929' target='_blank'><img src='buch_klein2.jpg' style='float: right;'></a>von (2023)<br>
                    <a href = 'https://www.uni-trier.de/index.php?id=15929' target='_blank' style='color:black'>Ökonometrie - eine Einführung<br>
                    8. Auflage, Springer-Gabler<a/> </p>"),
                
                p(strong("Arbeitsbuch"), style = 'margin-bottom:1px; color:black;'),
                HTML("<p style = 'color:black;'><a href = 'https://www.uni-trier.de/index.php?id=15929' target='_blank'><img src='übuch_klein2.jpg' style='float: right;'></a>Auer, L. von, Hoffmann, S., Kranz, T. (2024)<br>
                    <a href = 'https://www.uni-trier.de/index.php?id=15929' target='_blank' style='color:black'>Ökonometrie - Das R Arbeitsbuch<br>
                    2. Auflage, Springer-Gabler<a/> </p>"),
                
                br(),
                br(),
                HTML('<div class="btn-group dropup">
                                <a class="btn btn-info dropdown-toggle" data-toggle="dropdown" href="#">
                                Weitere Animationen
                                <span class="caret"></span>
                                </a>
                                <ul class="dropdown-menu">
                                <p style="margin-bottom:1px;"><a href="https://oekonometrie.shinyapps.io/Stoergroessen/" target="_blank">&nbsp; Störgrößen</a></p>
                                <p style="margin-bottom:1px;color:black;"><a href="https://oekonometrie.shinyapps.io/WiederholteStichproben/" target="_blank">&nbsp; KQ-Schätzer</p>
                                <a href="https://oekonometrie.shinyapps.io/Intervallschaetzer/" target="_blank">&nbsp; Intervallschätzer</a>
                                <p style="margin-bottom:1px;"><a href="https://oekonometrie.shinyapps.io/t-Test/" target="_blank">&nbsp; t-Test</a></p>
                                <p style="margin-bottom:1px;" >&nbsp; t-Test vs F-Test</p>
								
                                </ul>
                                </div>')),
      list(tags$head(tags$style("body {background-color: #6d6d6d; }")))
        ),
    
    mainPanel(
      wellPanel(wellPanel(style = "background-color: #FFFFFF;",plotOutput("plot",height = "550px"))),
      wellPanel(style = "background-color: #DEEBF7;",tabsetPanel(type = "pills",                                                        
                                                                tabPanel(h5("Was wird veranschaulicht?"),p("Eine Nullhypothese mit zwei Linearkombinationen 
macht einen \\( F \\)-Test erforderlich. Könnte man auch alternativ jeden 
der beiden Parameter mit jeweils einem \\( t \\)-Test individuell überprüfen? 
Das aus der Stichprobe ermittelte Wertepaar wird hier den aus dem \\( t \\)-Test 
ermittelten Akzeptanzintervallen und der Akzeptanzregion des \\( F \\)-Tests gegenübergestellt. 
In der Animation können Sie den Einfluss des Signifikanzniveaus, des Beobachtungsumfangs, 
des Korrelationskoeffizenten der Parameter und der zu überprüfenden Hypothese auf die 
Breite der Akzeptanzintervalle und die Größe und Form der Akzeptanzregion studieren.", style="color:black") ),
                                                                 
                                                                tabPanel(h5("Was zeigt die Anfangseinstellung?"), 
                                                                          p(HTML("<p style='color:black;'>Die Animation orientiert sich an dem Dünger-Beispiel des Lehrbuchs. 
Für jede Parzelle \\( i \\) wird der logarithmierte Gersten-Output \\( y_{i} \\) durch den logarithmierten
Phosphat-Input \\( x_{1i} \\) und den logarithmierten Stickstoff-Input \\( x_{2i} \\) erklärt: 
$$y_{i}=α+β_{1}x_{1i}+β_{2}x_{2i}+u_{i}$$ 
In der Anfangseinstellung wurde eine Stichprobe mit 30 Beobachtungen genereiert. 
Es wurde ein simultaner \\( F \\)-Test und zwei individuelle \\( t \\)-Tests 
mit der Nullhypothese \\( H_{0}: \\beta_{1} = \\beta_{2} = 0,33 \\) auf einem
Signifikanzniveau von \\( 5\\% \\) durchgeführt.</p>"), 
                                                                            HTML('<p style="color:black;">Die KQ-Schätzung der 
generierten Stichprobe liefert in der Anfangseinstellung die Schätzwerte \\(	 \\hat{\\beta_{1}} = 1,886 \\) und \\(	 \\hat{\\beta_{1}} = -0,396 \\).
Dieses Wertepaar ist in der Grafik durch den schwarzen Punkt markiert.</p>'), 
                                                                            
                                                                            HTML('<p style="color:black;">Das weiße Rechteck ergibt sich 
durch die Breiten der Akzeptanzintervalle der individuellen 
\\( t \\)-Tests auf der horizontalen und vertikalen Achse und die graue Ellipse 
kennzeichnet die Akzeptanzregion des simultanen \\( F \\)-Tests. Sollte der Punkt 
innerhalb des weißen Rechtecks liegen, kann die \\( H_{0} \\) im Zuge der individuellen 
\\( t \\)-Tests nicht verworfen werden. Sollte der Punkt innerhalb der grauen Ellipse liegen,  
kann die \\( H_{0} \\) im Zuge des \\( F \\)-Tests nicht verworfen werden.</p>'),
                                                                          
                                                                            HTML('<p style="color:black;">Da das Wertepaar der 
Stichprobe in der Anfangseinstellung sowohl innerhalb des Rechtecks 
als auch innerhalb der Ellipse liegt, kann die \\( H_{0} \\) hier nicht verworfen werden.</p>') )
                                                                          
                                                                 ),
                                                                 
                                                                 tabPanel(h5("Benutzungshinweise"), 
                                                                          p(HTML("Sie können auch einen eigenen \\( t \\)- und \\( F \\)-Test durchführen. Im Bedienfenster
sehen Sie verschiedene Schieber <img src='slider.jpg'>, mit denen Sie die
Parameterwerte des \\( t \\)- und \\( F \\)-Tests verändern können. Klicken Sie dafür mit der
linken Maustaste auf den entsprechenden Schieber und bewegen sie ihn nach
rechts oder links. Klicken Sie anschließend wieder auf den Knopf <img src='stich.jpg'>."),
                                                                            
                                                                            HTML('<ul><li style="color:black;">Sie können die Nullhypothese selbst festlegen. 
Zu diesem Zweck
können Sie im Bedienfester unter der Überschrift „Nullhypothese für \\(
\\beta_{1} \\)" oder „Nullhypothese für \\(
\\beta_{2} \\)" den voreingestellten Wert \\( q=0,33 \\) durch den von
Ihnen gewünschten Wert \\( q \\) ersetzen. Erst wenn Sie den Knopf  <img
src="stich.jpg"> drücken, wird eine neue Stichprobe erzeugt und der von Ihnen
eingestellte \\( t \\)- und \\( F \\)-Test wird durchgeführt. </li></ul>'),
                                                                            HTML('<ul><li style="color:black;">Mit dem <strong>Signifikanzniveau-Schieber</strong> 
stellen Sie ein, wie groß die
Wahrscheinlichkeit sein soll, dass es in Ihrem Test trotz Gültigkeit der
Nullhypothese zu einer Ablehnung dieser Nullhypothese kommt. Eine Erhöhung des 
Signifikanzniveaus verkleinert die Breite der Akzeptanzintervalle und 
die Größe der Akzeptanzregion. (Achsenabschnitte beachten!) </li></ul>'), 
                                                                            HTML('<ul><li style="color:black;">Der <strong>Beobachtungsumfang-Schieber</strong> 
gibt an, wie
viele Beobachtungen der Stichprobe zugrunde liegen. Eine Erhöhung des 
Stichprobenumfangs verkleinert die Breite der Akzeptanzintervalle und 
die Größe der Akzeptanzregion. (Achsenabschnitte beachten!) </li></ul>'), 
                                                                            HTML('<ul><li style="color:black;">Der <strong>Korrelationskoeffizient-Schieber</strong> 
gibt an, wie hoch der Korrelationskoeffizient ziwschen 
\\( \\beta_{1} \\) und \\( \\beta_{2} \\) ist. </li></ul>'), 
                                                                            HTML('<ul><li style="color:black;"> Um die aktuelle Grafik in einer jpg-Datei zu
speichern, klicken
Sie das Feld <img src="download.jpg"> an.</li></ul>'), 
                                                                            HTML("<p style='color:black;'>Um Animationen
zu anderen ökonometrischen Themen zu sehen, klicken Sie bitte auf <img src =
'info.jpg'>.</p>")), style="color:black"),
                                                                 
                                                                 tabPanel(h5("Details"),
                                                                          p(HTML('<p style="color:black;">Der Stichprobenerzeugung liegen die 
„wahren" Parameterwerte \\( \\alpha=1 \\) , \\( \\beta_{1}=0,6 \\) und 
\\( \\beta_{2}=0,3 \\) zugrunde. Die  \\( x_{1i} \\)-Werte werden aus einer Normalverteilung mit Erwartungswert \\( E(x_{1i})=3,2
\\) und Varianz \\( σ_{x_{1}}=0,01 \\) und die \\( x_{2i} \\)-Werte aus einer Normalverteilung mit Erwartungswert \\( E(x_{2i})=4,3
\\) und Varianz \\( σ_{x_{2}}=0,13 \\) erzeugt.</p>'),HTML("<p
style='color:black;'>Die entsprechenden R-Skripte für die Reproduktion dieser
Seite sind unter folgendem Link aufrufbar: <a href='https://github.com/Oekonometrie-Lernen/t-Test-versus-F-Test' target='_blank'>R
Codes.</a></p>")
                                                                            
      ))
      )),
    )
  )
)
)

# Serverlogik der Shiny-App definieren
server <- function(input, output) {
  generate_sample <- reactiveVal(NULL)
  
  generate_data <- function() {
    n <- isolate(input$n)
    alpha <- isolate(input$alpha)
    beta1_null <- isolate(input$beta1_null)
    beta2_null <- isolate(input$beta2_null)
    rho <- isolate(input$rho)
    
    # Wahre Parameterwerte
    true_beta1 <- 0.6
    true_beta2 <- 0.3
    true_alpha <- 1
    
    # Stichprobe generieren
    set.seed(123)
    
    mean_X1 <- 3.2258
    var_X1 <- 0.0077
    mean_X2 <- 4.3241
    var_X2 <- 0.1269
    
    X1 <- rnorm(n, mean = mean_X1, sd = sqrt(var_X1))
    X2 <- rnorm(n, mean = mean_X2, sd = sqrt(var_X2))
    X <- cbind(1, X1, X2)
    beta <- c(true_alpha, true_beta1, true_beta2)
    y <- X %*% beta + rnorm(n)
    
    # OLS Schätzer berechnen
    ols <- lm(y ~ X - 1)
    betas <- coef(ols)
    beta1_hat <- betas[2]
    beta2_hat <- betas[3]
    
    # Standardabweichungen der Schätzer
    se_beta1 <- summary(ols)$coefficients[2, 2]
    se_beta2 <- summary(ols)$coefficients[3, 2]
    
    # t-Tests durchführen
    t_beta1 <- (beta1_hat - beta1_null) / se_beta1
    t_beta2 <- (beta2_hat - beta2_null) / se_beta2
    crit_t <- qt(1 - alpha / 2, df = n - 3)
    
    # F-Test durchführen
    SSR_restricted <- sum((y - X %*% c(betas[1], beta1_null, beta2_null))^2)
    SSR_unrestricted <- sum(residuals(ols)^2)
    F_stat <- ((SSR_restricted - SSR_unrestricted) / 2) / (SSR_unrestricted / (n - 3))
    crit_F <- qf(1 - alpha, df1 = 2, df2 = n - 3)
    
    # Kovarianzmatrix der Schätzer berechnen
    cov_matrix <- summary(ols)$cov.unscaled * summary(ols)$sigma^2
    cov_matrix[2, 3] <- cov_matrix[3, 2] <- rho * se_beta1 * se_beta2
    
    # Akzeptanzintervalle berechnen
    t_acceptance_region <- data.frame(
      x = c(beta1_null - crit_t * se_beta1, beta1_null + crit_t * se_beta1),
      y = c(beta2_null - crit_t * se_beta2, beta2_null + crit_t * se_beta2)
    )
    
    # Ellipse für F-Test zeichnen
    eigen_values <- eigen(cov_matrix[2:3, 2:3])
    ellipse_radius_x <- sqrt(crit_F * eigen_values$values[1])
    ellipse_radius_y <- sqrt(crit_F * eigen_values$values[2])
    angle <- atan(eigen_values$vectors[2, 1] / eigen_values$vectors[1, 1])
    
    theta <- seq(0, 2 * pi, length.out = 100)
    ellipse <- data.frame(
      x = beta1_null + ellipse_radius_x * cos(theta) * cos(angle) - ellipse_radius_y * sin(theta) * sin(angle),
      y = beta2_null + ellipse_radius_x * cos(theta) * sin(angle) + ellipse_radius_y * sin(theta) * cos(angle)
    )
    
    list(beta1_hat = beta1_hat, beta2_hat = beta2_hat, t_acceptance_region = t_acceptance_region, ellipse = ellipse)
  }
  
  generate_sample(generate_data())
  
  observeEvent(input$generate, {
    generate_sample(generate_data())
  })
  
  output$plot <- renderPlot({
    sample <- generate_sample()
    
    ggplot() +
      # Region außerhalb des Rechtecks und innerhalb des Plotbereichs blassrot
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "lightcoral", alpha = 0.2) +
      # Region innerhalb des Rechtecks und außerhalb der Ellipse blassrot
      annotate("rect", xmin = sample$t_acceptance_region$x[1], xmax = sample$t_acceptance_region$x[2], ymin = sample$t_acceptance_region$y[1], ymax = sample$t_acceptance_region$y[2], fill = "white") +
      annotate("polygon", x = sample$ellipse$x, y = sample$ellipse$y, fill = "grey", alpha = 0.2) +
      # Punkte und Formen zeichnen
      geom_point(aes(x = sample$beta1_hat, y = sample$beta2_hat), color = "black", size = 3) +
      geom_rect(data = sample$t_acceptance_region, aes(xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]), fill = NA, color = "black") +
      geom_path(data = sample$ellipse, aes(x = x, y = y), color = "black") +
      labs(x = expression(hat(beta)[1]), y = expression(hat(beta)[2]), title = "Akzeptanzregionen für t-Tests und F-Test") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 20)
      )
  })
}

# Shiny-App starten
shinyApp(ui = ui, server = server)