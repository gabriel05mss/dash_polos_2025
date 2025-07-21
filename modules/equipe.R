equipeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
        .card-container {
          display: flex;
          flex-wrap: wrap;
          gap: 20px;
          justify-content: center;
        }

        .member-card {
          border: 1px solid #ccc;
          border-radius: 12px;
          padding: 20px;
          width: 300px;
          text-align: center;
          box-shadow: 2px 2px 6px rgba(0,0,0,0.1);
        }

        .member-name {
          font-size: 20px;
          font-weight: bold;
        }

        .member-role {
          font-size: 15px;
          color: #555;
          margin-bottom: 15px;
          list-style-type: none;
          padding-left: 0;
        }

        .member-role li::before {
          content: '• ';
          color: #1E90FF;
          font-weight: bold;
        }

        .icon-btn {
          display: inline-block;
          margin: 5px;
          padding: 10px 12px;
          border-radius: 50%;
          background-color: #1E90FF;
          color: white !important;
          text-decoration: none;
          font-size: 18px;
          transition: background-color 0.3s;
        }

        .icon-btn:hover {
          background-color: #0d6efd;
        }
      "))
    ),
    
    h2("Nossa Equipe", align = "center"),
    
    div(class = "card-container",
        
        # Membro 1
        div(class = "member-card",
            div(class = "member-name", "Andrezza Cristina Santos"),
            tags$ul(class = "member-role",
                    tags$li("Pesquisador extensionista")
            ),
            a(icon("linkedin"), href = "https://www.linkedin.com/in/andrezza-cristina-santos-batista-b7828723a", class = "icon-btn", target = "_blank"),
            a(icon("file-alt"), href = "https://lattes.cnpq.br/9300533003335560", class = "icon-btn", target = "_blank"),
            a(icon("envelope"), href = "andrezza-II@hotmail.com", class = "icon-btn")
        ),
        # Membro 2
        div(class = "member-card",
            div(class = "member-name", "Caique Izidoro Alvarenga"),
            tags$ul(class = "member-role",
                    tags$li("Pesquisador extensionista")
            ),
            a(icon("linkedin"), href = "https://linkedin.com/in/caiqueizidr", class = "icon-btn", target = "_blank"),
            a(icon("file-alt"), href = "https://lattes.cnpq.br/9300533003335560", class = "icon-btn", target = "_blank"),
            a(icon("envelope"), href = "caiqueizidoro369@gmail.com", class = "icon-btn")
        ),
        # Membro 3
        div(class = "member-card",
            div(class = "member-name", "Diogo Gomes Pires"),
            tags$ul(class = "member-role",
                    tags$li("Pesquisador extensionista")
            ),
            a(icon("linkedin"), href = "https://www.linkedin.com/in/diogo-gomes-pires-802076206", class = "icon-btn", target = "_blank"),
            a(icon("file-alt"), href = "http://lattes.cnpq.br/5116149150044825", class = "icon-btn", target = "_blank"),
            a(icon("envelope"), href = "diogogomes1109@gmail.com", class = "icon-btn")
        ),
        
        # Membro 4
        div(class = "member-card",
            div(class = "member-name", "Gabriel Moreira da Silva"),
            tags$ul(class = "member-role",
                    tags$li("Cientista de Dados"),
                    tags$li("Analista de BI")
            ),
            a(icon("linkedin"), href = "https://www.linkedin.com/in/gabriel-moreira-73341a23b/", class = "icon-btn", target = "_blank"),
            a(icon("file-alt"), href = "http://lattes.cnpq.br/987654321", class = "icon-btn", target = "_blank"),
            a(icon("envelope"), href = "gmoreira.silva2005@gmail.com", class = "icon-btn")
        ),
        # Membro 5
        div(class = "member-card",
            div(class = "member-name", "Irlana dos Santos"),
            tags$ul(class = "member-role",
                    tags$li("Pesquisador extensionista")
            ),
            a(icon("linkedin"), href = "https://www.linkedin.com/in/irlana-silva-905457185", class = "icon-btn", target = "_blank"),
            a(icon("file-alt"), href = "https://lattes.cnpq.br/9300533003335560", class = "icon-btn", target = "_blank"),
            a(icon("envelope"), href = "irlannasilva1720@gmail.com", class = "icon-btn")
        )
        
        #pra colocar mais só copiar a colar a div toda, NÃO ESQUECER DA VIRGULA DEPOIS DAS DIVs
    )
  )
}



equipeServer <- function(input, output, session, dados) {

}
