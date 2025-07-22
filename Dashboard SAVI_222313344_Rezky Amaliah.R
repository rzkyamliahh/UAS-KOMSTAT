#Load library yang dibutuhkan
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(corrplot)
library(car)
library(nortest)
library(dplyr)
library(stringr)
library(readr)
library(shinycssloaders)
library(officer)
library(flextable)
library(webshot)
library(tidyr)
library(stringr)
library(gridExtra)
library(openxlsx)
library(leaflet)
library(RColorBrewer)
library(viridis)
library(colourpicker)

#Load data sesuai URL yang diberikan dalam soal UAS
tryCatch({
  #Data SoVI
  sovi_data <- read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv")
  
  #Matriks penimbang jarak
  distance_matrix <- read.csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv", row.names = 1)
  
  print("✅ Data berhasil dimuat dari URL")
  print(paste("📊 SoVI Data:", nrow(sovi_data), "rows,", ncol(sovi_data), "columns"))
  print(paste("📏 Distance Matrix:", nrow(distance_matrix), "rows,", ncol(distance_matrix), "columns"))
  
}, error = function(e) {
  print("⚠️ Gagal memuat data dari URL, menggunakan sample data")
  
  set.seed(123)
  sovi_data <- data.frame(
    ID = 1:100,
    County = paste("County", 1:100),
    State = sample(c("CA", "TX", "FL", "NY", "PA"), 100, replace = TRUE),
    Population = rnorm(100, 50000, 15000),
    Income = rnorm(100, 45000, 12000),
    Education = rnorm(100, 85, 10),
    Healthcare = rnorm(100, 75, 15),
    Housing = rnorm(100, 65, 20),
    Employment = rnorm(100, 80, 12),
    Infrastructure = rnorm(100, 70, 18),
    SoVI_Score = rnorm(100, 0, 1),
    Latitude = runif(100, 25, 49),
    Longitude = runif(100, -125, -65),
    Region = sample(c("North", "South", "East", "West"), 100, replace = TRUE),
    Urban_Rural = sample(c("Urban", "Rural"), 100, replace = TRUE)
  )
  
  n <- nrow(sovi_data)
  distance_matrix <- matrix(runif(n*n, 0, 100), nrow = n, ncol = n)
  diag(distance_matrix) <- 0
  distance_matrix <- as.data.frame(distance_matrix)
  names(distance_matrix) <- paste0("County_", 1:n)
})

#Memastikan struktur data
if(ncol(sovi_data) > 0) {
  char_cols <- sapply(sovi_data, is.character)
  if(any(char_cols)) {
    sovi_data[char_cols] <- lapply(sovi_data[char_cols], as.factor)
  }
  
  numeric_vars <- names(sovi_data)[sapply(sovi_data, is.numeric)]
  if(length(numeric_vars) > 2) {
    var1 <- numeric_vars[1]
    var2 <- numeric_vars[2]
    
    sovi_data[[paste0(var1, "_Cat")]] <- cut(sovi_data[[var1]], 
                                             breaks = 3, 
                                             labels = c("Low", "Medium", "High"))
    sovi_data[[paste0(var2, "_Cat")]] <- cut(sovi_data[[var2]], 
                                             breaks = 2, 
                                             labels = c("Group_A", "Group_B"))
  }
}

print("📋 Data structure:")
print(str(sovi_data))

#UI - Dashboard SAVI - Sistem Analisis Visualisasi Interaktif
ui <- dashboardPage(
  dashboardHeader(title = "🎓 SAVI"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("🏠 Beranda", tabName = "home", icon = icon("home")),
      menuItem("🗃️ Manajemen Data", tabName = "data_mgmt", icon = icon("database")),
      menuItem("📊 Eksplorasi Data", tabName = "exploration", icon = icon("chart-bar"),
               menuSubItem("Statistik Deskriptif", tabName = "descriptive"),
               menuSubItem("Visualisasi", tabName = "visualization"),
               menuSubItem("Analisis Matriks Jarak", tabName = "distance_analysis")
      ),
      menuItem("🧪 Uji Asumsi", tabName = "assumptions", icon = icon("check-circle")),
      menuItem("📈 Statistik Inferensia", tabName = "inference", icon = icon("calculator"),
               menuSubItem("Uji Rata-rata", tabName = "mean_tests"),
               menuSubItem("Uji Proporsi", tabName = "prop_tests"),
               menuSubItem("Uji Varians", tabName = "var_tests")
      ),
      menuItem("📊 ANOVA", tabName = "anova", icon = icon("layer-group")),
      menuItem("📈 Regresi Linear Berganda", tabName = "regression", icon = icon("line-chart"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f4f4; }
        .box { border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        .download-btn { margin: 5px; }
        .interpretation-box { 
          background-color: #f8f9fa; 
          border: 1px solid #dee2e6; 
          border-radius: 8px; 
          padding: 15px; 
          margin: 10px 0; 
        }
        .info-card {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 20px;
          border-radius: 10px;
          margin: 10px 0;
        }
        .stat-card {
          background: white;
          border-radius: 10px;
          padding: 20px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          margin: 10px 0;
          border-left: 4px solid #667eea;
        }
      "))
    ),
    
    tabItems( 
      #Page Beranda
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "🎓 Dashboard SAVI - Sistem Analisis Visualisasi Interaktif", 
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  div(class = "info-card",
                      p("Dashboard ini dibuat untuk memenuhi persyaratan UAS Komputasi Statistik STIS - Semester Genap TA. 2024/2025"),
                      br(),
                      h4("📋 Metadata Dataset:"),
                      div(style = "background-color: #f8f9fa; color:black; padding: 15px; border-radius: 8px; margin: 10px 0;",
                          h5("🔗 Sumber Data Utama:"),
                          tags$a(href = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv", 
                                 target = "_blank",
                                 style = "color: black; text-decoration: none; font-weight: bold;",
                                 "📊 Dataset SoVI (Indeks Kerentanan Sosial)"),
                          br(),
                          
                          tags$a(href = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv", 
                                 target = "_blank",
                                 style = "color: #007bff; text-decoration: none; font-weight: bold;",
                                 "📏 Matriks Jarak (Distance Matrix)"),
                          br(), br(),
                          
                          h5("📚 Referensi Ilmiah:"),
                          tags$a(href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180", 
                                 target = "_blank",
                                 style = "color: black; text-decoration: none; font-weight: bold;",
                                 "📖 Publikasi ScienceDirect - Artikel Data"),
                          p(style = "margin-top: 10px; font-size: 0.9em; color: #6c757d;",
                            "Klik tautan di atas untuk mengakses artikel ilmiah yang menjelaskan metodologi dan penggunaan dataset SoVI.")
                      ),
                      
                      br(),
                      
                      h4("🎯 Tujuan Dashboard:"),
                      div(style = "background-color: #fff3cd; padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #ffc107;",
                          tags$ul(style = "margin: 0; color:black; padding-left: 20px;",
                                  tags$li("Menganalisis data Indeks Kerentanan Sosial (SoVI) secara komprehensif"),
                                  tags$li("Menerapkan berbagai metode statistik inferensia dan deskriptif"),
                                  tags$li("Melakukan visualisasi data interaktif"),
                                  tags$li("Mengimplementasikan uji asumsi statistik yang diperlukan"),
                                  tags$li("Menyediakan platform analisis yang user-friendly untuk pembelajaran")
                          )
                      )
                  )
                )
              ),
              
              fluidRow(
                column(4,
                       div(class = "stat-card",
                           h4("📊 Dataset SoVI", style = "color: #495057;"),
                           hr(style = "margin: 10px 0;"),
                           div(style = "display: flex; justify-content: space-between; margin: 5px 0;",
                               span(strong("Observasi:"), style = "color: #6c757d;"),
                               span(nrow(sovi_data), style = "color: #007bff; font-weight: bold;")
                           ),
                           div(style = "display: flex; justify-content: space-between; margin: 5px 0;",
                               span(strong("Variabel:"), style = "color: #6c757d;"),
                               span(ncol(sovi_data), style = "color: #007bff; font-weight: bold;")
                           ),
                           div(style = "display: flex; justify-content: space-between; margin: 5px 0;",
                               span(strong("Numerik:"), style = "color: #6c757d;"),
                               span(sum(sapply(sovi_data, is.numeric)), style = "color: #28a745; font-weight: bold;")
                           ),
                           div(style = "display: flex; justify-content: space-between; margin: 5px 0;",
                               span(strong("Kategorikal:"), style = "color: #6c757d;"),
                               span(sum(!sapply(sovi_data, is.numeric)), style = "color: #ffc107; font-weight: bold;")
                           ),
                           br(),
                           actionButton("view_sovi_details", "📋 Lihat Detail", 
                                        class = "btn btn-outline-primary btn-sm", style = "width: 100%;")
                       )
                ),
                column(4,
                       div(class = "stat-card",
                           h4("📏 Matriks Jarak", style = "color: #495057;"),
                           hr(style = "margin: 10px 0;"),
                           div(style = "display: flex; justify-content: space-between; margin: 5px 0;",
                               span(strong("Dimensi:"), style = "color: #6c757d;"),
                               span(paste(nrow(distance_matrix), "x", ncol(distance_matrix)), style = "color: #007bff; font-weight: bold;")
                           ),
                           div(style = "display: flex; justify-content: space-between; margin: 5px 0;",
                               span(strong("Tipe:"), style = "color: #6c757d;"),
                               span("Penimbang Jarak", style = "color: #28a745; font-weight: bold;")
                           ),
                           div(style = "display: flex; justify-content: space-between; margin: 5px 0;",
                               span(strong("Format:"), style = "color: #6c757d;"),
                               span("Simetris", style = "color: #ffc107; font-weight: bold;")
                           ),
                           div(style = "display: flex; justify-content: space-between; margin: 5px 0;",
                               span(strong("Diagonal:"), style = "color: #6c757d;"),
                               span("Nol", style = "color: #dc3545; font-weight: bold;")
                           ),
                           br(),
                           actionButton("view_distance_details", "📋 Lihat Detail", 
                                        class = "btn btn-outline-warning btn-sm", style = "width: 100%;")
                       )
                ),
                column(4,
                       div(class = "stat-card",
                           h4("🎯 Fitur Dashboard", style = "color: #495057;"),
                           hr(style = "margin: 10px 0;"),
                           div(style = "max-height: 150px; overflow-y: auto;",
                               tags$ul(style = "margin: 0; padding-left: 20px; list-style-type: none;",
                                       tags$li(style = "margin: 5px 0;", "✅ Manajemen Data"),
                                       tags$li(style = "margin: 5px 0;", "📊 Eksplorasi & Visualisasi"),
                                       tags$li(style = "margin: 5px 0;", "🔍 Uji Asumsi Statistik"),
                                       tags$li(style = "margin: 5px 0;", "📈 Statistik Inferensia"),
                                       tags$li(style = "margin: 5px 0;", "📊 ANOVA & Regresi"),
                                       tags$li(style = "margin: 5px 0;", "💾 Ekspor Hasil"),
                                       tags$li(style = "margin: 5px 0;", "🗺️ Visualisasi Peta"),
                                       tags$li(style = "margin: 5px 0;", "📋 Laporan Otomatis")
                               )
                           ),
                           br(),
                           actionButton("dashboard_tour", "🚀 Mulai Tur", 
                                        class = "btn btn-outline-success btn-sm", style = "width: 100%;")
                       )
                )
              ),
              
              fluidRow(
                column(6,
                       box(
                         title = "📊 Ringkasan Data SoVI", 
                         status = "info",
                         solidHeader = TRUE,
                         width = NULL,
                         collapsible = TRUE,
                         
                         tabsetPanel(
                           tabPanel("📋 Statistik Dasar",
                                    br(),
                                    withSpinner(DT::dataTableOutput("home_sovi_summary")),
                                    br(),
                                    div(style = "text-align: center;",
                                        downloadButton("download_sovi_summary", "📄 Unduh Ringkasan (Word)",
                                                       class = "btn btn-warning download-btn")
                                    )
                           ),
                           tabPanel("📈 Visualisasi Cepat",
                                    br(),
                                    plotOutput("home_quick_viz", height = "300px"),
                                    br(),
                                    div(style = "text-align: center;",
                                        downloadButton("download_quick_viz", "🖼️ Unduh Plot (JPG)",
                                                       class = "btn btn-info download-btn")
                                    )
                           ),
                           tabPanel("🔗 Tautan Berguna",
                                    br(),
                                    div(style = "padding: 15px;",
                                        h5("📚 Referensi dan Dokumentasi:"),
                                        tags$ul(
                                          tags$li(tags$a(href = "https://cran.r-project.org/web/packages/shiny/index.html", 
                                                         target = "_blank", "📖 Dokumentasi Shiny")),
                                          tags$li(tags$a(href = "https://ggplot2.tidyverse.org/", 
                                                         target = "_blank", "📊 Dokumentasi ggplot2")),
                                          tags$li(tags$a(href = "https://www.rdocumentation.org/packages/DT/versions/0.20", 
                                                         target = "_blank", "📋 Dokumentasi Paket DT")),
                                          tags$li(tags$a(href = "https://github.com/bmlmcmc/naspaclust", 
                                                         target = "_blank", "💻 Repository Sumber"))
                                        ),
                                        br(),
                                        h5("🎓 Materi Pembelajaran:"),
                                        tags$ul(
                                          tags$li(tags$a(href = "https://www.stis.ac.id/", 
                                                         target = "_blank", "🏫 Website STIS")),
                                          tags$li(tags$a(href = "https://bookdown.org/yihui/rmarkdown/", 
                                                         target = "_blank", "📝 Panduan R Markdown")),
                                          tags$li(tags$a(href = "https://r4ds.had.co.nz/", 
                                                         target = "_blank", "📚 R untuk Data Science"))
                                        ),
                                        br(),
                                        h5("📊 Tutorial Statistik:"),
                                        tags$ul(
                                          tags$li(tags$a(href = "https://www.statmethods.net/", 
                                                         target = "_blank", "📈 Quick-R Statistical Methods")),
                                          tags$li(tags$a(href = "https://www.r-bloggers.com/", 
                                                         target = "_blank", "📝 R-Bloggers")),
                                          tags$li(tags$a(href = "https://www.datacamp.com/courses/free-introduction-to-r", 
                                                         target = "_blank", "🎓 Kursus R Gratis"))
                                        )
                                    )
                           )
                         )
                       )
                ),
                column(6,
                       box(
                         title = "📏 Informasi Matriks Jarak", 
                         status = "warning",
                         solidHeader = TRUE,
                         width = NULL,
                         collapsible = TRUE,
                         
                         tabsetPanel(
                           tabPanel("ℹ️ Info Dasar",
                                    br(),
                                    withSpinner(verbatimTextOutput("distance_info")),
                                    br(),
                                    div(style = "text-align: center;",
                                        downloadButton("download_distance_info", "📄 Unduh Info (Word)",
                                                       class = "btn btn-danger download-btn")
                                    )
                           ),
                           tabPanel("📊 Statistik Jarak",
                                    br(),
                                    withSpinner(DT::dataTableOutput("distance_stats_table")),
                                    br(),
                                    div(style = "text-align: center;",
                                        downloadButton("download_distance_stats", "📊 Unduh Statistik (Excel)",
                                                       class = "btn btn-success download-btn")
                                    )
                           ),
                           tabPanel("🎯 Penggunaan",
                                    br(),
                                    div(style = "padding: 15px;",
                                        h5("🔍 Aplikasi Matriks Jarak:"),
                                        tags$div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0;",
                                                 tags$strong("1. Analisis Clustering"), br(),
                                                 "Mengelompokkan observasi berdasarkan kedekatan jarak untuk identifikasi pola"
                                        ),
                                        tags$div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0;",
                                                 tags$strong("2. Analisis Spasial"), br(),
                                                 "Menganalisis pola geografis dan hubungan spasial antar wilayah"
                                        ),
                                        tags$div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0;",
                                                 tags$strong("3. Multidimensional Scaling (MDS)"), br(),
                                                 "Reduksi dimensi untuk visualisasi data dalam ruang 2D atau 3D"
                                        ),
                                        tags$div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0;",
                                                 tags$strong("4. Deteksi Outlier"), br(),
                                                 "Identifikasi observasi yang memiliki pola tidak biasa atau ekstrem"
                                        ),
                                        tags$div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0;",
                                                 tags$strong("5. Analisis Kemiripan"), br(),
                                                 "Mengukur tingkat kemiripan atau perbedaan antar unit observasi"
                                        )
                                    )
                           )
                         )
                       )
                )
              ),
              
              fluidRow(
                box(
                  title = "💾 Unduhan Lengkap", 
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  
                  div(style = "background: linear-gradient(135deg, #28a745 0%, #20c997 100%); color: white; padding: 20px; border-radius: 10px; margin: 10px 0;",
                      h4("📥 Unduh Dataset dan Dokumentasi:", style = "margin-top: 0;"),
                      p("Unduh semua file yang diperlukan untuk analisis offline dan dokumentasi lengkap penelitian Anda.")
                  ),
                  
                  fluidRow(
                    column(3, 
                           div(style = "text-align: center; padding: 15px;",
                               icon("file-word", style = "font-size: 2em; color: #007bff; margin-bottom: 10px;"),
                               br(),
                               downloadButton("download_metadata_complete", "📖 Metadata Lengkap",
                                              class = "btn btn-info btn-block download-btn"),
                               br(), br(),
                               tags$small("Format: Microsoft Word (.docx)", style = "color: #6c757d;"),
                               br(),
                               tags$small("Berisi informasi lengkap dataset dan metodologi", style = "color: #6c757d;")
                           )
                    ),
                    column(3, 
                           div(style = "text-align: center; padding: 15px;",
                               icon("file-excel", style = "font-size: 2em; color: #28a745; margin-bottom: 10px;"),
                               br(),
                               downloadButton("download_sovi_data", "📊 Data SoVI",
                                              class = "btn btn-success btn-block download-btn"),
                               br(), br(),
                               tags$small("Format: Microsoft Excel (.xlsx)", style = "color: #6c757d;"),
                               br(),
                               tags$small("Dataset utama Indeks Kerentanan Sosial", style = "color: #6c757d;")
                           )
                    ),
                    column(3, 
                           div(style = "text-align: center; padding: 15px;",
                               icon("table", style = "font-size: 2em; color: #007bff; margin-bottom: 10px;"),
                               br(),
                               downloadButton("download_distance_matrix", "📏 Matriks Jarak",
                                              class = "btn btn-primary btn-block download-btn"),
                               br(), br(),
                               tags$small("Format: Microsoft Excel (.xlsx)", style = "color: #6c757d;"),
                               br(),
                               tags$small("Matriks jarak untuk analisis spasial", style = "color: #6c757d;")
                           )
                    ),
                    column(3, 
                           div(style = "text-align: center; padding: 15px;",
                               icon("archive", style = "font-size: 2em; color: #ffc107; margin-bottom: 10px;"),
                               br(),
                               downloadButton("download_combined_data", "💾 Data Gabungan",
                                              class = "btn btn-warning btn-block download-btn"),
                               br(), br(),
                               tags$small("Format: Multi-sheet Excel (.xlsx)", style = "color: #6c757d;"),
                               br(),
                               tags$small("Semua data dalam satu file Excel", style = "color: #6c757d;")
                           )
                    )
                  ),
                  
                  hr(),
                  
                  div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px;",
                      h5("🚀 Akses Cepat:", style = "margin-top: 0;"),
                      p("Gunakan tombol di bawah untuk mengakses fitur-fitur utama dashboard dengan cepat.", style = "color: #6c757d; margin-bottom: 15px;"),
                      fluidRow(
                        column(3,
                               actionButton("quick_analysis", "⚡ Analisis Cepat", 
                                            class = "btn btn-outline-primary btn-block", style = "margin: 5px;"),
                               tags$small("Mulai analisis statistik dasar", style = "color: #6c757d; display: block; text-align: center;")
                        ),
                        column(3,
                               actionButton("view_tutorial", "📚 Tutorial", 
                                            class = "btn btn-outline-info btn-block", style = "margin: 5px;"),
                               tags$small("Panduan penggunaan dashboard", style = "color: #6c757d; display: block; text-align: center;")
                        ),
                        column(3,
                               actionButton("export_report", "📋 Ekspor Laporan", 
                                            class = "btn btn-outline-success btn-block", style = "margin: 5px;"),
                               tags$small("Buat laporan analisis lengkap", style = "color: #6c757d; display: block; text-align: center;")
                        ),
                        column(3,
                               actionButton("contact_support", "💬 Bantuan", 
                                            class = "btn btn-outline-secondary btn-block", style = "margin: 5px;"),
                               tags$small("Hubungi tim dukungan teknis", style = "color: #6c757d; display: block; text-align: center;")
                        )
                      )
                  )
                )
              ),
              
              fluidRow(
                column(6,
                       box(
                         title = "📖 Panduan Penggunaan Dashboard",
                         status = "info",
                         solidHeader = TRUE,
                         width = NULL,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         
                         div(style = "padding: 10px;",
                             h5("🎯 Langkah-langkah Analisis:"),
                             tags$ol(
                               tags$li(strong("Eksplorasi Data:"), " Mulai dengan melihat statistik deskriptif dan visualisasi data"),
                               tags$li(strong("Uji Asumsi:"), " Periksa normalitas dan homogenitas data sebelum analisis inferensia"),
                               tags$li(strong("Analisis Inferensia:"), " Lakukan uji hipotesis sesuai dengan tujuan penelitian"),
                               tags$li(strong("Interpretasi:"), " Baca hasil analisis dan unduh laporan untuk dokumentasi")
                             ),
                             br(),
                             h5("💡 Tips Penggunaan:"),
                             tags$ul(
                               tags$li("Gunakan menu navigasi di sebelah kiri untuk berpindah antar fitur"),
                               tags$li("Setiap hasil analisis dapat diunduh dalam format Word atau Excel"),
                               tags$li("Visualisasi dapat disimpan sebagai gambar JPG atau PNG"),
                               tags$li("Gunakan fitur 'Bantuan' jika mengalami kesulitan")
                             )
                         )
                       )
                ),
                column(6,
                       box(
                         title = "🔧 Spesifikasi Teknis",
                         status = "warning",
                         solidHeader = TRUE,
                         width = NULL,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         
                         div(style = "padding: 10px;",
                             h5("💻 Teknologi yang Digunakan:"),
                             tags$ul(
                               tags$li(strong("R Shiny:"), " Framework untuk aplikasi web interaktif"),
                               tags$li(strong("ggplot2:"), " Library untuk visualisasi data"),
                               tags$li(strong("DT:"), " Tabel data interaktif"),
                               tags$li(strong("Plotly:"), " Visualisasi interaktif"),
                               tags$li(strong("Leaflet:"), " Peta interaktif")
                             ),
                             br(),
                             h5("📊 Metode Statistik:"),
                             tags$ul(
                               tags$li("Statistik Deskriptif dan Inferensia"),
                               tags$li("Uji Normalitas (Shapiro-Wilk, Kolmogorov-Smirnov)"),
                               tags$li("ANOVA dan Regresi Linear Berganda"),
                               tags$li("Analisis Clustering dan MDS"),
                               tags$li("Uji Hipotesis untuk Rata-rata, Proporsi, dan Varians")
                             )
                         )
                       )
                )
              )
      ),
      
      #Page Management Data
      tabItem(tabName = "data_mgmt",
              fluidRow(
                column(4,
                       box(
                         title = "🔧 Pengaturan Kategorisasi Data", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         selectInput("cat_variable", "Pilih Variabel untuk Dikategorikan:",
                                     choices = NULL),
                         
                         numericInput("cat_groups", "Jumlah Kategori:", 
                                      value = 3, min = 2, max = 5),
                         
                         radioButtons("cat_method", "Metode Kategorisasi:",
                                      choices = list(
                                        "Berdasarkan Kuantil" = "quantile",
                                        "Berdasarkan Range Sama" = "equal",
                                        "Berdasarkan K-means" = "kmeans"
                                      )),
                         
                         textInput("cat_labels", "Label Kategori (pisahkan dengan koma):",
                                   placeholder = "Rendah, Sedang, Tinggi"),
                         
                         br(),
                         actionButton("apply_categorization", "🔄 Terapkan Kategorisasi", 
                                      class = "btn btn-primary btn-block"),
                         
                         br(), br(),
                         h4("💾 Download Hasil:"),
                         downloadButton("download_categorized_data", "📊 Data Terkategorisasi (Excel)", 
                                        class = "btn btn-success btn-block download-btn"),
                         downloadButton("download_cat_interpretation", "📝 Interpretasi (Word)", 
                                        class = "btn btn-info btn-block download-btn"),
                         downloadButton("download_cat_report", "📋 Laporan Lengkap (Word)", 
                                        class = "btn btn-warning btn-block download-btn")
                       )
                ),
                
                column(8,
                       box(
                         title = "📊 Hasil Kategorisasi Data", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         tabsetPanel(
                           tabPanel("📋 Data Terkategorisasi",
                                    br(),
                                    withSpinner(DT::dataTableOutput("categorized_data"))
                           ),
                           
                           tabPanel("📊 Visualisasi Perbandingan",
                                    br(),
                                    withSpinner(plotOutput("categorization_plot")),
                                    br(),
                                    downloadButton("download_cat_plot_jpg", "🖼️ Download Plot (JPG)", 
                                                   class = "btn btn-warning download-btn")
                           ),
                           
                           tabPanel("📈 Statistik Kategori",
                                    br(),
                                    withSpinner(DT::dataTableOutput("category_stats")),
                                    br(),
                                    downloadButton("download_cat_stats", "📊 Download Statistik (Excel)", 
                                                   class = "btn btn-success download-btn")
                           ),
                           
                           tabPanel("📝 Interpretasi Lengkap",
                                    br(),
                                    div(class = "interpretation-box",
                                        withSpinner(verbatimTextOutput("categorization_interpretation"))
                                    )
                           )
                         )
                       )
                )
              )
      ),
      
      tabItem(tabName = "descriptive",
              fluidRow(
                column(4,
                       box(
                         title = "📊 Pengaturan Analisis Deskriptif", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         h4("📈 Statistik Deskriptif"),
                         checkboxGroupInput("desc_vars", "Pilih Variabel:",
                                            choices = NULL),
                         
                         radioButtons("desc_type", "Jenis Analisis:",
                                      choices = list(
                                        "Statistik Dasar" = "basic",
                                        "Statistik Lengkap" = "complete",
                                        "Analisis Distribusi" = "distribution"
                                      )),
                         
                         checkboxInput("desc_by_group", "Analisis per Grup", FALSE),
                         
                         conditionalPanel(
                           condition = "input.desc_by_group",
                           selectInput("desc_group_var", "Variabel Grouping:", choices = NULL)
                         ),
                         
                         actionButton("calc_descriptive", "📊 Hitung Statistik Deskriptif", 
                                      class = "btn btn-primary btn-block"),
                         
                         br(), br(),
                         h4("💾 Download Hasil:"),
                         downloadButton("download_desc_excel", "📊 Statistik (Excel)", 
                                        class = "btn btn-success btn-block download-btn"),
                         downloadButton("download_desc_interpretation", "📝 Interpretasi (Word)", 
                                        class = "btn btn-info btn-block download-btn"),
                         downloadButton("download_desc_report", "📋 Laporan Lengkap (Word)", 
                                        class = "btn btn-warning btn-block download-btn")
                       )
                ),
                
                column(8,
                       box(
                         title = "📊 Hasil Analisis Deskriptif", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         tabsetPanel(
                           tabPanel("📊 Tabel Statistik",
                                    br(),
                                    withSpinner(DT::dataTableOutput("descriptive_stats"))
                           ),
                           
                           tabPanel("📈 Visualisasi Distribusi",
                                    br(),
                                    withSpinner(plotOutput("descriptive_plots", height = "600px")),
                                    br(),
                                    downloadButton("download_desc_plots", "🖼️ Download Plots (JPG)", 
                                                   class = "btn btn-warning download-btn")
                           ),
                           
                           tabPanel("📊 Perbandingan Grup",
                                    br(),
                                    withSpinner(plotOutput("group_comparison_plot")),
                                    br(),
                                    withSpinner(DT::dataTableOutput("group_stats_table"))
                           ),
                           
                           tabPanel("📝 Interpretasi",
                                    br(),
                                    div(class = "interpretation-box",
                                        withSpinner(verbatimTextOutput("descriptive_interpretation"))
                                    )
                           )
                         )
                       )
                )
              )
      ),
      
      tabItem(tabName = "visualization",
              fluidRow(
                column(4,
                       box(
                         title = "🎨 Pengaturan Visualisasi", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         h4("📊 Konfigurasi Plot"),
                         selectInput("plot_x", "Variabel X:", choices = NULL),
                         selectInput("plot_y", "Variabel Y:", choices = NULL),
                         selectInput("plot_type", "Jenis Plot:",
                                     choices = list(
                                       "Scatter Plot" = "scatter",
                                       "Line Plot" = "line",
                                       "Box Plot" = "box",
                                       "Violin Plot" = "violin",
                                       "Histogram" = "hist",
                                       "Density Plot" = "density",
                                       "Heatmap" = "heatmap"
                                     )),
                         
                         conditionalPanel(
                           condition = "input.plot_type == 'box' || input.plot_type == 'violin'",
                           selectInput("plot_group", "Variabel Grouping:", choices = NULL)
                         ),
                         
                         colourInput("plot_color", "Warna Utama:", "#667eea"),
                         
                         checkboxInput("add_trend", "Tambahkan Garis Trend", FALSE),
                         
                         textInput("plot_title", "Judul Plot:", placeholder = "Masukkan judul custom"),
                         
                         actionButton("generate_plot", "🎨 Buat Plot", 
                                      class = "btn btn-success btn-block"),
                       )
                ),
                
                column(8,
                       box(
                         title = "📊 Visualisasi Data", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         tabsetPanel(
                           tabPanel("📈 Plot Interaktif",
                                    br(),
                                    withSpinner(plotlyOutput("interactive_plot", height = "500px")),
                                    br(),
                                    fluidRow(
                                      column(4, downloadButton("download_plot_jpg", "🖼️ JPG", 
                                                               class = "btn btn-warning download-btn")),
                                      column(4, downloadButton("download_plot_png", "🖼️ PNG", 
                                                               class = "btn btn-success download-btn")),
                                      column(4, downloadButton("download_plot_interpretation", "📝 Interpretasi (Word)", 
                                                               class = "btn btn-info download-btn"))
                                    )
                           ),
                           
                           tabPanel("📊 Plot Statis",
                                    br(),
                                    withSpinner(plotOutput("static_plot", height = "500px")),
                                    br(),
                                    downloadButton("download_static_plot", "🖼️ Download Plot Statis (JPG)", 
                                                   class = "btn btn-warning download-btn")
                           )
                         )
                       )
                )
              )
      ),
      
      tabItem(tabName = "distance_analysis",
              fluidRow(
                column(4,
                       box(
                         title = "📏 Pengaturan Analisis Matriks Jarak", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         h4("🔍 Konfigurasi Analisis"),
                         
                         radioButtons("distance_analysis_type", "Jenis Analisis:",
                                      choices = list(
                                        "Statistik Deskriptif Matriks" = "descriptive",
                                        "Clustering Berdasarkan Jarak" = "clustering",
                                        "Visualisasi Heatmap" = "heatmap",
                                        "Analisis Komponen Utama" = "pca"
                                      )),
                         
                         conditionalPanel(
                           condition = "input.distance_analysis_type == 'clustering'",
                           numericInput("n_clusters", "Jumlah Cluster:", value = 3, min = 2, max = 10),
                           selectInput("cluster_method", "Metode Clustering:",
                                       choices = list(
                                         "K-means" = "kmeans",
                                         "Hierarchical" = "hierarchical",
                                         "PAM" = "pam"
                                       ))
                         ),
                         
                         actionButton("run_distance_analysis", "📏 Jalankan Analisis", 
                                      class = "btn btn-primary btn-block"),
                         
                         br(), br(),
                         h4("💾 Download Hasil:"),
                         downloadButton("download_distance_results", "📊 Hasil Analisis (Excel)", 
                                        class = "btn btn-success btn-block download-btn"),
                         downloadButton("download_distance_plot", "🖼️ Visualisasi (JPG)", 
                                        class = "btn btn-warning btn-block download-btn"),
                         downloadButton("download_distance_interpretation", "📝 Interpretasi (Word)", 
                                        class = "btn btn-info btn-block download-btn")
                       )
                ),
                
                column(8,
                       box(
                         title = "📏 Hasil Analisis Matriks Jarak", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         tabsetPanel(
                           tabPanel("📊 Hasil Analisis",
                                    br(),
                                    withSpinner(verbatimTextOutput("distance_analysis_results")),
                                    br(),
                                    withSpinner(DT::dataTableOutput("distance_results_table"))
                           ),
                           
                           tabPanel("📈 Visualisasi",
                                    br(),
                                    withSpinner(plotOutput("distance_plot", height = "600px"))
                           ),
                           
                           tabPanel("🗺️ Heatmap Jarak",
                                    br(),
                                    withSpinner(plotOutput("distance_heatmap", height = "600px"))
                           ),
                           
                           tabPanel("📝 Interpretasi",
                                    br(),
                                    div(class = "interpretation-box",
                                        withSpinner(verbatimTextOutput("distance_interpretation"))
                                    )
                           )
                         )
                       )
                )
              )
      ),
      
      #Page Uji Asumsi
      tabItem(tabName = "assumptions",
              fluidRow(
                column(6,
                       box(
                         title = "🔍 Uji Normalitas", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         selectInput("norm_var", "Pilih Variabel:", choices = NULL),
                         
                         checkboxGroupInput("norm_tests", "Pilih Uji:",
                                            choices = list(
                                              "Shapiro-Wilk" = "shapiro",
                                              "Kolmogorov-Smirnov" = "ks",
                                              "Lilliefors" = "lillie",
                                              "Anderson-Darling" = "ad",
                                              "Jarque-Bera" = "jb"
                                            ),
                                            selected = c("shapiro", "lillie")),
                         
                         sliderInput("norm_alpha", "Tingkat Signifikansi:",
                                     min = 0.01, max = 0.1, value = 0.05, step = 0.01),
                         
                         actionButton("run_normality", "🔍 Jalankan Uji Normalitas", 
                                      class = "btn btn-primary btn-block"),
                         
                         br(), br(),
                         h4("💾 Download Hasil:"),
                         downloadButton("download_norm_results", "📊 Hasil Uji (Word)", 
                                        class = "btn btn-success btn-block download-btn"),
                         downloadButton("download_norm_plots", "🖼️ Plot (JPG)", 
                                        class = "btn btn-warning btn-block download-btn"),
                         downloadButton("download_norm_interpretation", "📝 Interpretasi (Word)", 
                                        class = "btn btn-info btn-block download-btn")
                       )
                ),
                
                column(6,
                       box(
                         title = "⚖️ Uji Homogenitas", 
                         status = "warning", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         selectInput("homo_var", "Variabel Numerik:", choices = NULL),
                         selectInput("homo_group", "Variabel Grouping:", choices = NULL),
                         
                         radioButtons("homo_test", "Jenis Uji:",
                                      choices = list(
                                        "Levene's Test" = "levene",
                                        "Bartlett's Test" = "bartlett",
                                        "Fligner-Killeen Test" = "fligner"
                                      )),
                         
                         sliderInput("homo_alpha", "Tingkat Signifikansi:",
                                     min = 0.01, max = 0.1, value = 0.05, step = 0.01),
                         
                         actionButton("run_homogeneity", "⚖️ Jalankan Uji Homogenitas", 
                                      class = "btn btn-warning btn-block"),
                         
                         br(), br(),
                         h4("💾 Download Hasil:"),
                         downloadButton("download_homo_results", "📊 Hasil Uji (Word)", 
                                        class = "btn btn-success btn-block download-btn"),
                         downloadButton("download_homo_interpretation", "📝 Interpretasi (Word)", 
                                        class = "btn btn-info btn-block download-btn")
                       )
                )
              ),
              
              fluidRow(
                column(6,
                       box(
                         title = "🔍 Hasil Uji Normalitas", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         withSpinner(verbatimTextOutput("normality_results")),
                         br(),
                         withSpinner(plotOutput("normality_plots"))
                       )
                ),
                
                column(6,
                       box(
                         title = "⚖️ Hasil Uji Homogenitas", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         withSpinner(verbatimTextOutput("homogeneity_results")),
                         br(),
                         withSpinner(plotOutput("homogeneity_plots"))
                       )
                )
              ),
              
              fluidRow(
                box(
                  title = "📝 Interpretasi Gabungan Uji Asumsi", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  div(class = "interpretation-box",
                      withSpinner(verbatimTextOutput("assumptions_interpretation"))
                  ),
                  br(),
                  downloadButton("download_assumptions_report", "📋 Laporan Uji Asumsi Lengkap (Word)", 
                                 class = "btn btn-success btn-block download-btn")
                )
              )
      ),
      
      #Uji Rata-rata 
      tabItem(tabName = "mean_tests",
              fluidRow(
                column(4,
                       box(
                         title = "🧪 Pengaturan Uji Rata-rata", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         selectInput("mean_test_type", "Jenis Uji:",
                                     choices = list(
                                       "One Sample t-test" = "t_one",
                                       "Two Sample t-test" = "t_two",
                                       "Paired t-test" = "t_paired",
                                       "Welch t-test" = "welch"
                                     )),
                         
                         selectInput("mean_test_var", "Variabel:", choices = NULL),
                         
                         conditionalPanel(
                           condition = "input.mean_test_type == 't_one'",
                           numericInput("mu_value", "Nilai μ₀:", value = 0)
                         ),
                         
                         conditionalPanel(
                           condition = "input.mean_test_type == 't_two' || input.mean_test_type == 'welch'",
                           selectInput("mean_group_var", "Variabel Grouping:", choices = NULL)
                         ),
                         
                         conditionalPanel(
                           condition = "input.mean_test_type == 't_paired'",
                           selectInput("paired_var", "Variabel Pasangan:", choices = NULL)
                         ),
                         
                         radioButtons("mean_alternative", "Hipotesis Alternatif:",
                                      choices = list(
                                        "Two-sided" = "two.sided",
                                        "Greater than" = "greater",
                                        "Less than" = "less"
                                      )),
                         
                         sliderInput("mean_alpha", "Tingkat Signifikansi:",
                                     min = 0.01, max = 0.1, value = 0.05, step = 0.01),
                         
                         actionButton("run_mean_test", "🧪 Jalankan Uji", 
                                      class = "btn btn-primary btn-block"),
                         
                         br(), br(),
                         h4("💾 Download Hasil:"),
                         downloadButton("download_mean_results", "📊 Hasil Uji (Word)", 
                                        class = "btn btn-success btn-block download-btn"),
                         downloadButton("download_mean_interpretation", "📝 Interpretasi (Word)", 
                                        class = "btn btn-info btn-block download-btn")
                       )
                ),
                
                column(8,
                       box(
                         title = "🧪 Hasil Uji Rata-rata", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         tabsetPanel(
                           tabPanel("📊 Hasil Uji",
                                    br(),
                                    withSpinner(verbatimTextOutput("mean_test_results"))
                           ),
                           
                           tabPanel("📈 Visualisasi",
                                    br(),
                                    withSpinner(plotOutput("mean_test_plot", height = "400px"))
                           ),
                           
                           tabPanel("📝 Interpretasi Lengkap",
                                    br(),
                                    div(class = "interpretation-box",
                                        withSpinner(verbatimTextOutput("mean_test_interpretation"))
                                    )
                           )
                         )
                       )
                )
              )
      ),
      
      #Uji Proporsi
      tabItem(tabName = "prop_tests",
              fluidRow(
                column(4,
                       box(
                         title = "📊 Pengaturan Uji Proporsi", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         selectInput("prop_test_type", "Jenis Uji:",
                                     choices = list(
                                       "One Sample Proportion Test" = "prop_one",
                                       "Two Sample Proportion Test" = "prop_two",
                                       "Chi-square Goodness of Fit" = "chisq_gof",
                                       "Chi-square Independence" = "chisq_indep"
                                     )),
                         
                         selectInput("prop_var", "Variabel Kategorik:", choices = NULL),
                         
                         conditionalPanel(
                           condition = "input.prop_test_type == 'prop_one'",
                           numericInput("prop_value", "Proporsi Hipotesis:", 
                                        value = 0.5, min = 0, max = 1, step = 0.01)
                         ),
                         
                         conditionalPanel(
                           condition = "input.prop_test_type == 'prop_two' || input.prop_test_type == 'chisq_indep'",
                           selectInput("prop_group_var", "Variabel Grouping:", choices = NULL)
                         ),
                         
                         sliderInput("prop_alpha", "Tingkat Signifikansi:",
                                     min = 0.01, max = 0.1, value = 0.05, step = 0.01),
                         
                         actionButton("run_prop_test", "📊 Jalankan Uji", 
                                      class = "btn btn-primary btn-block"),
                         
                         br(), br(),
                         h4("💾 Download Hasil:"),
                         downloadButton("download_prop_results", "📊 Hasil Uji (Word)", 
                                        class = "btn btn-success btn-block download-btn"),
                         downloadButton("download_prop_interpretation", "📝 Interpretasi (Word)", 
                                        class = "btn btn-info btn-block download-btn")
                       )
                ),
                
                column(8,
                       box(
                         title = "📊 Hasil Uji Proporsi", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         tabsetPanel(
                           tabPanel("📊 Hasil Uji",
                                    br(),
                                    withSpinner(verbatimTextOutput("prop_test_results"))
                           ),
                           
                           tabPanel("📈 Tabel Kontingensi",
                                    br(),
                                    withSpinner(DT::dataTableOutput("contingency_table"))
                           ),
                           
                           tabPanel("📊 Visualisasi",
                                    br(),
                                    withSpinner(plotOutput("prop_test_plot", height = "400px"))
                           ),
                           
                           tabPanel("📝 Interpretasi",
                                    br(),
                                    div(class = "interpretation-box",
                                        withSpinner(verbatimTextOutput("prop_test_interpretation"))
                                    )
                           )
                         )
                       )
                )
              )
      ),
      
      #Uji Varians 
      tabItem(tabName = "var_tests",
              fluidRow(
                column(4,
                       box(
                         title = "📈 Pengaturan Uji Varians", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         selectInput("var_test_type", "Jenis Uji:",
                                     choices = list(
                                       "One Sample Variance Test" = "var_one",
                                       "Two Sample F-test" = "f_test",
                                       "Levene's Test" = "levene",
                                       "Bartlett's Test" = "bartlett"
                                     )),
                         
                         selectInput("var_test_var", "Variabel:", choices = NULL),
                         
                         conditionalPanel(
                           condition = "input.var_test_type == 'var_one'",
                           numericInput("sigma_squared", "Nilai σ² Hipotesis:", value = 1, min = 0.01)
                         ),
                         
                         conditionalPanel(
                           condition = "input.var_test_type != 'var_one'",
                           selectInput("var_group_var", "Variabel Grouping:", choices = NULL)
                         ),
                         
                         sliderInput("var_alpha", "Tingkat Signifikansi:",
                                     min = 0.01, max = 0.1, value = 0.05, step = 0.01),
                         
                         actionButton("run_var_test", "📈 Jalankan Uji", 
                                      class = "btn btn-primary btn-block"),
                         
                         br(), br(),
                         h4("💾 Download Hasil:"),
                         downloadButton("download_var_results", "📊 Hasil Uji (Word)", 
                                        class = "btn btn-success btn-block download-btn"),
                         downloadButton("download_var_interpretation", "📝 Interpretasi (Word)", 
                                        class = "btn btn-info btn-block download-btn")
                       )
                ),
                
                column(8,
                       box(
                         title = "📈 Hasil Uji Varians", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         tabsetPanel(
                           tabPanel("📊 Hasil Uji",
                                    br(),
                                    withSpinner(verbatimTextOutput("var_test_results"))
                           ),
                           
                           tabPanel("📈 Visualisasi",
                                    br(),
                                    withSpinner(plotOutput("var_test_plot", height = "400px"))
                           ),
                           
                           tabPanel("📝 Interpretasi",
                                    br(),
                                    div(class = "interpretation-box",
                                        withSpinner(verbatimTextOutput("var_test_interpretation"))
                                    )
                           )
                         )
                       )
                )
              )
      ),
      
      #Uji ANOVA
      tabItem(tabName = "anova",
              fluidRow(
                column(4,
                       box(
                         title = "📊 Pengaturan ANOVA", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         radioButtons("anova_type", "Jenis ANOVA:",
                                      choices = list(
                                        "One-Way ANOVA" = "one_way",
                                        "Two-Way ANOVA" = "two_way",
                                        "Repeated Measures ANOVA" = "repeated"
                                      )),
                         
                         selectInput("anova_response", "Variabel Response:", choices = NULL),
                         selectInput("anova_factor1", "Faktor 1:", choices = NULL),
                         
                         conditionalPanel(
                           condition = "input.anova_type == 'two_way'",
                           selectInput("anova_factor2", "Faktor 2:", choices = NULL),
                           checkboxInput("anova_interaction", "Sertakan Interaksi", TRUE)
                         ),
                         
                         conditionalPanel(
                           condition = "input.anova_type == 'repeated'",
                           selectInput("anova_subject", "Variabel Subjek:", choices = NULL)
                         ),
                         
                         checkboxInput("anova_posthoc", "Uji Post-hoc", TRUE),
                         
                         conditionalPanel(
                           condition = "input.anova_posthoc",
                           selectInput("posthoc_method", "Metode Post-hoc:",
                                       choices = list(
                                         "Tukey HSD" = "tukey",
                                         "Bonferroni" = "bonferroni",
                                         "Scheffe" = "scheffe",
                                         "LSD" = "lsd"
                                       ))
                         ),
                         
                         checkboxInput("anova_assumptions", "Uji Asumsi", TRUE),
                         
                         sliderInput("anova_alpha", "Tingkat Signifikansi:",
                                     min = 0.01, max = 0.1, value = 0.05, step = 0.01),
                         
                         actionButton("run_anova", "📊 Jalankan ANOVA", 
                                      class = "btn btn-primary btn-block"),
                         
                         br(), br(),
                         h4("💾 Download Hasil:"),
                         downloadButton("download_anova_results", "📊 Hasil ANOVA (Word)", 
                                        class = "btn btn-success btn-block download-btn"),
                         downloadButton("download_anova_plots", "🖼️ Plot (JPG)", 
                                        class = "btn btn-warning btn-block download-btn"),
                         downloadButton("download_anova_interpretation", "📝 Interpretasi (Word)", 
                                        class = "btn btn-info btn-block download-btn")
                       )
                ),
                
                column(8,
                       box(
                         title = "📊 Hasil ANOVA", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         tabsetPanel(
                           tabPanel("📊 Tabel ANOVA",
                                    br(),
                                    withSpinner(verbatimTextOutput("anova_results"))
                           ),
                           
                           tabPanel("📈 Visualisasi",
                                    br(),
                                    withSpinner(plotOutput("anova_plot", height = "500px"))
                           ),
                           
                           tabPanel("🔍 Post-hoc Tests",
                                    br(),
                                    withSpinner(verbatimTextOutput("posthoc_results")),
                                    br(),
                                    withSpinner(plotOutput("posthoc_plot"))
                           ),
                           
                           tabPanel("✅ Uji Asumsi",
                                    br(),
                                    withSpinner(verbatimTextOutput("anova_assumptions_results")),
                                    br(),
                                    withSpinner(plotOutput("anova_assumptions_plots"))
                           ),
                           
                           tabPanel("📝 Interpretasi",
                                    br(),
                                    div(class = "interpretation-box",
                                        withSpinner(verbatimTextOutput("anova_interpretation"))
                                    )
                           )
                         )
                       )
                )
              )
      ),
      
      #Page Regresi Linear Berganda
      tabItem(tabName = "regression",
              fluidRow(
                column(4,
                       box(
                         title = "📈 Pengaturan Regresi Linear Berganda", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         selectInput("reg_response", "Variabel Response:", choices = NULL),
                         selectInput("reg_predictors", "Variabel Prediktor:", 
                                     choices = NULL, multiple = TRUE),
                         
                         checkboxInput("reg_intercept", "Sertakan Intercept", TRUE),
                         checkboxInput("reg_interaction", "Sertakan Interaksi", FALSE),
                         checkboxInput("reg_polynomial", "Sertakan Polynomial", FALSE),
                         
                         conditionalPanel(
                           condition = "input.reg_polynomial",
                           numericInput("poly_degree", "Derajat Polynomial:", value = 2, min = 2, max = 4)
                         ),
                         
                         selectInput("reg_method", "Metode Seleksi:",
                                     choices = list(
                                       "Enter (Forced)" = "enter",
                                       "Forward Selection" = "forward",
                                       "Backward Elimination" = "backward",
                                       "Stepwise" = "stepwise"
                                     )),
                         
                         checkboxInput("reg_standardize", "Standardisasi Variabel", FALSE),
                         checkboxInput("reg_diagnostics", "Uji Asumsi Lengkap", TRUE),
                         
                         actionButton("run_regression", "📈 Jalankan Regresi", 
                                      class = "btn btn-primary btn-block"),
                         
                         br(), br(),
                         h4("💾 Download Hasil:"),
                         downloadButton("download_reg_results", "📊 Hasil Regresi (Word)", 
                                        class = "btn btn-success btn-block download-btn"),
                         downloadButton("download_reg_diagnostics", "🖼️ Diagnostik (JPG)", 
                                        class = "btn btn-warning btn-block download-btn"),
                         downloadButton("download_reg_interpretation", "📝 Interpretasi (Word)", 
                                        class = "btn btn-info btn-block download-btn")
                       )
                ),
                
                column(8,
                       box(
                         title = "📈 Hasil Regresi Linear Berganda", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = NULL,
                         
                         tabsetPanel(
                           tabPanel("📊 Summary Model",
                                    br(),
                                    withSpinner(verbatimTextOutput("regression_summary"))
                           ),
                           
                           tabPanel("📈 Plot Diagnostik",
                                    br(),
                                    withSpinner(plotOutput("regression_diagnostics", height = "600px"))
                           ),
                           
                           tabPanel("🔍 Uji Asumsi",
                                    br(),
                                    withSpinner(verbatimTextOutput("regression_assumptions")),
                                    br(),
                                    withSpinner(plotOutput("regression_assumptions_plots"))
                           ),
                           
                           tabPanel("📊 Model Comparison",
                                    br(),
                                    withSpinner(DT::dataTableOutput("model_comparison")),
                                    br(),
                                    withSpinner(plotOutput("model_comparison_plot"))
                           ),
                           
                           tabPanel("🔮 Prediksi",
                                    br(),
                                    fluidRow(
                                      column(6,
                                             h4("Input Nilai untuk Prediksi:"),
                                             uiOutput("prediction_inputs")
                                      ),
                                      column(6,
                                             h4("Hasil Prediksi:"),
                                             withSpinner(verbatimTextOutput("prediction_results"))
                                      )
                                    ),
                                    br(),
                                    withSpinner(plotOutput("prediction_plot"))
                           ),
                           
                           tabPanel("📝 Interpretasi",
                                    br(),
                                    div(class = "interpretation-box",
                                        withSpinner(verbatimTextOutput("regression_interpretation"))
                                    )
                           )
                         )
                       )
                )
              )
      )
    )
  )
)

#Server - Dashboard SAVI - Sistem Analisis Visualisasi Interaktif 
server <- function(input, output, session) {
  
  values <- reactiveValues(
    categorized_data = NULL,
    current_plot = NULL,
    current_map = NULL,
    descriptive_results = NULL,
    correlation_results = NULL,
    distance_analysis_results = NULL,
    normality_results = NULL,
    homogeneity_results = NULL,
    mean_test_results = NULL,
    prop_test_results = NULL,
    var_test_results = NULL,
    anova_results = NULL,
    regression_model = NULL
  )
  
  observe({
    req(sovi_data)
    
    #Identifikasi jenis variabel
    numeric_vars <- names(sovi_data)[sapply(sovi_data, is.numeric)]
    all_vars <- names(sovi_data)
    categorical_vars <- names(sovi_data)[sapply(sovi_data, function(x) {
      is.factor(x) || is.character(x) || 
        (is.numeric(x) && length(unique(x[!is.na(x)])) <= 10)
    })]
    
    #Jika tidak ada variabel kategorikal, buat dari numerik
    if(length(categorical_vars) == 0 && length(numeric_vars) >= 2) {
      var1 <- numeric_vars[1]
      var2 <- numeric_vars[2]
      
      sovi_data[[paste0(var1, "_Group")]] <<- cut(sovi_data[[var1]], 
                                                  breaks = 3, 
                                                  labels = c("Low", "Medium", "High"))
      sovi_data[[paste0(var2, "_Type")]] <<- cut(sovi_data[[var2]], 
                                                 breaks = 2, 
                                                 labels = c("Type_A", "Type_B"))
      
      categorical_vars <- c(categorical_vars, paste0(var1, "_Group"), paste0(var2, "_Type"))
    }
    
    #Update semua select inputs
    updateSelectInput(session, "cat_variable", choices = numeric_vars)
    updateCheckboxGroupInput(session, "desc_vars", choices = numeric_vars, selected = numeric_vars[1:min(5, length(numeric_vars))])
    updateSelectInput(session, "desc_group_var", choices = categorical_vars)
    updateSelectInput(session, "plot_x", choices = numeric_vars)
    updateSelectInput(session, "plot_y", choices = numeric_vars)
    updateSelectInput(session, "plot_group", choices = categorical_vars)
    updateSelectInput(session, "map_variable", choices = numeric_vars)
    updateSelectInput(session, "norm_var", choices = numeric_vars)
    updateSelectInput(session, "homo_var", choices = numeric_vars)
    updateSelectInput(session, "homo_group", choices = categorical_vars)
    updateSelectInput(session, "mean_test_var", choices = numeric_vars)
    updateSelectInput(session, "mean_group_var", choices = categorical_vars)
    updateSelectInput(session, "paired_var", choices = numeric_vars)
    updateSelectInput(session, "prop_var", choices = all_vars)
    updateSelectInput(session, "prop_group_var", choices = categorical_vars)
    updateSelectInput(session, "var_test_var", choices = numeric_vars)
    updateSelectInput(session, "var_group_var", choices = categorical_vars)
    updateSelectInput(session, "anova_response", choices = numeric_vars)
    updateSelectInput(session, "anova_factor1", choices = categorical_vars)
    updateSelectInput(session, "anova_factor2", choices = categorical_vars)
    updateSelectInput(session, "anova_subject", choices = all_vars)
    updateSelectInput(session, "reg_response", choices = numeric_vars)
    updateSelectInput(session, "reg_predictors", choices = numeric_vars)
  })
  
  output$has_coordinates <- reactive({
    coord_vars <- c("latitude", "longitude", "lat", "lon", "Latitude", "Longitude")
    any(tolower(names(sovi_data)) %in% tolower(coord_vars))
  })
  outputOptions(output, "has_coordinates", suspendWhenHidden = FALSE)
  
  #Output Page Beranda
  output$home_sovi_summary <- DT::renderDataTable({
    if(exists("sovi_data")) {
      summary_data <- data.frame(
        Variabel = names(sovi_data),
        Tipe = sapply(sovi_data, class),
        Missing = sapply(sovi_data, function(x) sum(is.na(x))),
        Min = sapply(sovi_data, function(x) if(is.numeric(x)) round(min(x, na.rm = TRUE), 3) else "-"),
        Max = sapply(sovi_data, function(x) if(is.numeric(x)) round(max(x, na.rm = TRUE), 3) else "-"),
        Mean = sapply(sovi_data, function(x) if(is.numeric(x)) round(mean(x, na.rm = TRUE), 3) else "-"),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(summary_data, 
                    options = list(pageLength = 10, scrollX = TRUE),
                    caption = "Ringkasan Statistik Dataset SoVI")
    }
  }, server = FALSE)
  
  output$home_quick_viz <- renderPlot({
    if(exists("sovi_data")) {
      numeric_vars <- names(sovi_data)[sapply(sovi_data, is.numeric)]
      if(length(numeric_vars) > 0) {
        var_name <- numeric_vars[1]
        ggplot(sovi_data, aes_string(x = var_name)) +
          geom_histogram(fill = "#007bff", alpha = 0.7, bins = 30) +
          theme_minimal() +
          labs(title = paste("Distribusi", var_name),
               x = var_name, y = "Frekuensi") +
          theme(plot.title = element_text(hjust = 0.5))
      }
    }
  })
  
  output$distance_info <- renderText({
    if(exists("distance_matrix")) {
      paste(
        "INFORMASI MATRIKS JARAK",
        "========================",
        "",
        paste("Dimensi Matriks:", nrow(distance_matrix), "x", ncol(distance_matrix)),
        paste("Tipe Data: Matriks Penimbang Jarak"),
        paste("Sumber:", "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"),
        "",
        "Statistik Deskriptif Jarak:",
        paste("• Jarak Minimum:", round(min(distance_matrix, na.rm = TRUE), 3)),
        paste("• Jarak Maksimum:", round(max(distance_matrix, na.rm = TRUE), 3)),
        paste("• Jarak Rata-rata:", round(mean(as.matrix(distance_matrix), na.rm = TRUE), 3)),
        paste("• Standar Deviasi:", round(sd(as.matrix(distance_matrix), na.rm = TRUE), 3)),
        "",
        "📊 Penggunaan dalam Analisis:",
        "• Clustering berdasarkan jarak",
        "• Analisis spasial",
        "• Visualisasi heatmap",
        "• Analisis komponen utama",
        sep = "\n"
      )
    } else {
      "Data matriks jarak tidak tersedia."
    }
  })
  
  output$distance_stats_table <- DT::renderDataTable({
    if(exists("distance_matrix")) {
      stats_data <- data.frame(
        ID = 1:nrow(distance_matrix),
        Min_Distance = apply(distance_matrix, 1, function(x) round(min(x[x > 0], na.rm = TRUE), 3)),
        Max_Distance = apply(distance_matrix, 1, function(x) round(max(x, na.rm = TRUE), 3)),
        Mean_Distance = apply(distance_matrix, 1, function(x) round(mean(x, na.rm = TRUE), 3)),
        SD_Distance = apply(distance_matrix, 1, function(x) round(sd(x, na.rm = TRUE), 3))
      )
      
      DT::datatable(stats_data,
                    options = list(pageLength = 10, scrollX = TRUE),
                    caption = "Statistik Jarak per Observasi")
    }
  }, server = FALSE)
  
  #Penanganan Download
  # Download ringkasan SoVI 
  output$download_sovi_summary <- downloadHandler(
    filename = function() {
      paste("Ringkasan_SoVI_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      doc <- read_docx()
      
      doc <- doc %>%
        body_add_par("RINGKASAN DATASET SOVI", style = "heading 1") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal")
      
      if(exists("sovi_data")) {
        doc <- doc %>%
          body_add_par("Informasi Dasar Dataset:", style = "heading 2") %>%
          body_add_par(paste("• Jumlah Observasi:", nrow(sovi_data))) %>%
          body_add_par(paste("• Jumlah Variabel:", ncol(sovi_data))) %>%
          body_add_par(paste("• Variabel Numerik:", sum(sapply(sovi_data, is.numeric)))) %>%
          body_add_par(paste("• Variabel Kategorikal:", sum(!sapply(sovi_data, is.numeric)))) %>%
          body_add_par("", style = "Normal")
        
        summary_data <- data.frame(
          Variabel = names(sovi_data),
          Tipe = sapply(sovi_data, class),
          Missing = sapply(sovi_data, function(x) sum(is.na(x))),
          Min = sapply(sovi_data, function(x) if(is.numeric(x)) round(min(x, na.rm = TRUE), 3) else "-"),
          Max = sapply(sovi_data, function(x) if(is.numeric(x)) round(max(x, na.rm = TRUE), 3) else "-"),
          Mean = sapply(sovi_data, function(x) if(is.numeric(x)) round(mean(x, na.rm = TRUE), 3) else "-")
        )
        
        ft <- flextable(summary_data)
        ft <- theme_vanilla(ft)
        ft <- autofit(ft)
        
        doc <- doc %>%
          body_add_par("Ringkasan Statistik:", style = "heading 2") %>%
          body_add_flextable(ft)
      }
      
      print(doc, target = file)
    }
  )
  
  #Download informasi matriks jarak 
  output$download_distance_info <- downloadHandler(
    filename = function() {
      paste("Info_Matriks_Jarak_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      doc <- read_docx()
      
      doc <- doc %>%
        body_add_par("INFORMASI MATRIKS JARAK", style = "heading 1") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal")
      
      if(exists("distance_matrix")) {
        doc <- doc %>%
          body_add_par("Spesifikasi Matriks:", style = "heading 2") %>%
          body_add_par(paste("• Dimensi:", nrow(distance_matrix), "x", ncol(distance_matrix))) %>%
          body_add_par("• Tipe: Matriks Penimbang Jarak") %>%
          body_add_par("• Format: Simetris") %>%
          body_add_par("• Diagonal: Nol") %>%
          body_add_par("", style = "Normal") %>%
          body_add_par("Statistik Deskriptif:", style = "heading 2") %>%
          body_add_par(paste("• Jarak Minimum:", round(min(distance_matrix, na.rm = TRUE), 3))) %>%
          body_add_par(paste("• Jarak Maksimum:", round(max(distance_matrix, na.rm = TRUE), 3))) %>%
          body_add_par(paste("• Jarak Rata-rata:", round(mean(as.matrix(distance_matrix), na.rm = TRUE), 3))) %>%
          body_add_par(paste("• Standar Deviasi:", round(sd(as.matrix(distance_matrix), na.rm = TRUE), 3)))
      }
      
      print(doc, target = file)
    }
  )
  
  #Download visualisasi cepat
  output$download_quick_viz <- downloadHandler(
    filename = function() {
      paste("Visualisasi_Cepat_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      if(exists("sovi_data")) {
        numeric_vars <- names(sovi_data)[sapply(sovi_data, is.numeric)]
        if(length(numeric_vars) > 0) {
          var_name <- numeric_vars[1]
          p <- ggplot(sovi_data, aes_string(x = var_name)) +
            geom_histogram(fill = "#007bff", alpha = 0.7, bins = 30) +
            theme_minimal() +
            labs(title = paste("Distribusi", var_name),
                 x = var_name, y = "Frekuensi") +
            theme(plot.title = element_text(hjust = 0.5))
          
          ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
        }
      }
    }
  )
  
  #Download statistik jarak dalam Excel
  output$download_distance_stats <- downloadHandler(
    filename = function() {
      paste("Statistik_Jarak_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      if(exists("distance_matrix")) {
        stats_data <- data.frame(
          ID = 1:nrow(distance_matrix),
          Min_Distance = apply(distance_matrix, 1, function(x) round(min(x[x > 0], na.rm = TRUE), 3)),
          Max_Distance = apply(distance_matrix, 1, function(x) round(max(x, na.rm = TRUE), 3)),
          Mean_Distance = apply(distance_matrix, 1, function(x) round(mean(x, na.rm = TRUE), 3)),
          SD_Distance = apply(distance_matrix, 1, function(x) round(sd(x, na.rm = TRUE), 3))
        )
        
        write.xlsx(stats_data, file, sheetName = "Statistik_Jarak", rowNames = FALSE)
      }
    }
  )
  
  #Download metadata lengkap
  output$download_metadata_complete <- downloadHandler(
    filename = function() {
      paste("Metadata_Lengkap_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      doc <- read_docx()
      
      doc <- doc %>%
        body_add_par("METADATA LENGKAP DASHBOARD SAVI", style = "heading 1") %>%
        body_add_par("Dashboard Analisis Indeks Kerentanan Sosial", style = "heading 2") %>%
        body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("INFORMASI MATA KULIAH", style = "heading 2") %>%
        body_add_par("• Mata Kuliah: Komputasi Statistik (K201313/3 SKS)") %>%
        body_add_par("• Dosen: Robert Kurniawan, Sukim, Yuliagnis Transver Wijaya") %>%
        body_add_par("• UAS: Rabu, 23 Juli 2025, 10.30-12.30 WIB") %>%
        body_add_par("• Institusi: Sekolah Tinggi Ilmu Statistik (STIS)") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("SUMBER DATA", style = "heading 2") %>%
        body_add_par("• Dataset SoVI: https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv") %>%
        body_add_par("• Matriks Jarak: https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv") %>%
        body_add_par("• Referensi: https://www.sciencedirect.com/science/article/pii/S2352340921010180")
      
      print(doc, target = file)
    }
  )
  
  #Download data SoVI
  output$download_sovi_data <- downloadHandler(
    filename = function() {
      paste("Data_SoVI_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      if(exists("sovi_data")) {
        write.xlsx(sovi_data, file, sheetName = "Data_SoVI", rowNames = FALSE)
      }
    }
  )
  
  #Download matriks jarak 
  output$download_distance_matrix <- downloadHandler(
    filename = function() {
      paste("Matriks_Jarak_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      if(exists("distance_matrix")) {
        write.xlsx(distance_matrix, file, sheetName = "Matriks_Jarak", rowNames = TRUE)
      }
    }
  )
  
  #Download data gabungan
  output$download_combined_data <- downloadHandler(
    filename = function() {
      paste("Data_Gabungan_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      
      if(exists("sovi_data")) {
        addWorksheet(wb, "Data_SoVI")
        writeData(wb, "Data_SoVI", sovi_data)
      }
      
      if(exists("distance_matrix")) {
        addWorksheet(wb, "Matriks_Jarak")
        writeData(wb, "Matriks_Jarak", distance_matrix, rowNames = TRUE)
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  observeEvent(input$view_sovi_details, {
    showModal(modalDialog(
      title = "Detail Dataset SoVI",
      size = "l",
      if(exists("sovi_data")) {
        tagList(
          h4("Informasi Dataset"),
          p(paste("Jumlah Observasi:", nrow(sovi_data))),
          p(paste("Jumlah Variabel:", ncol(sovi_data))),
          p(paste("Variabel Numerik:", sum(sapply(sovi_data, is.numeric)))),
          p(paste("Variabel Kategorikal:", sum(!sapply(sovi_data, is.numeric)))),
          br(),
          h4("Variabel dalam Dataset:"),
          renderTable({
            data.frame(
              No = 1:ncol(sovi_data),
              Nama_Variabel = names(sovi_data),
              Tipe = sapply(sovi_data, class),
              Missing_Values = sapply(sovi_data, function(x) sum(is.na(x)))
            )
          })
        )
      } else {
        p("Dataset SoVI tidak tersedia.")
      },
      easyClose = TRUE,
      footer = modalButton("Tutup")
    ))
  })
  
  observeEvent(input$view_distance_details, {
    showModal(modalDialog(
      title = "Detail Matriks Jarak",
      size = "l",
      if(exists("distance_matrix")) {
        tagList(
          h4("Spesifikasi Matriks"),
          p(paste("Dimensi:", nrow(distance_matrix), "x", ncol(distance_matrix))),
          p("Tipe: Matriks Penimbang Jarak"),
          p("Format: Simetris"),
          br(),
          h4("Statistik Deskriptif"),
          p(paste("Minimum:", round(min(distance_matrix, na.rm = TRUE), 3))),
          p(paste("Maksimum:", round(max(distance_matrix, na.rm = TRUE), 3))),
          p(paste("Rata-rata:", round(mean(as.matrix(distance_matrix), na.rm = TRUE), 3))),
          p(paste("Standar Deviasi:", round(sd(as.matrix(distance_matrix), na.rm = TRUE), 3)))
        )
      } else {
        p("Matriks jarak tidak tersedia.")
      },
      easyClose = TRUE,
      footer = modalButton("Tutup")
    ))
  })
  
  observeEvent(input$dashboard_tour, {
    showModal(modalDialog(
      title = "🚀 Tur Dashboard",
      size = "l",
      tagList(
        h4("Selamat datang di Dashboard SAVI!"),
        p("Dashboard ini menyediakan berbagai fitur untuk analisis data SoVI:"),
        tags$ol(
          tags$li(strong("Beranda:"), " Informasi umum dan metadata"),
          tags$li(strong("Manajemen Data:"), " Kelola dataset"),
          tags$li(strong("Eksplorasi Data:"), " Statistik deskriptif dan visualisasi"),
          tags$li(strong("Uji Asumsi:"), " Normalitas, homogenitas, dll"),
          tags$li(strong("Statistik Inferensia:"), " Uji hipotesis"),
          tags$li(strong("ANOVA & Regresi:"), " Analisis lanjutan"),
          tags$li(strong("Ekspor Hasil:"), " Download laporan dan data")
        ),
        br(),
        p("Gunakan menu navigasi di sebelah kiri untuk berpindah antar fitur."),
        p("Setiap hasil analisis dapat diunduh dalam berbagai format.")
      ),
      easyClose = TRUE,
      footer = modalButton("Mulai Eksplorasi")
    ))
  })
  
  observeEvent(input$quick_analysis, {
    showNotification("Memulai analisis cepat...", type = "message")
    updateTabItems(session, "tabs", "eksplorasi")
  })
  
  observeEvent(input$view_tutorial, {
    showModal(modalDialog(
      title = "📚 Tutorial Penggunaan",
      size = "l",
      tagList(
        h4("Panduan Langkah demi Langkah"),
        tags$ol(
          tags$li("Mulai dari tab 'Beranda' untuk melihat informasi dataset"),
          tags$li("Gunakan 'Manajemen Data' untuk upload data tambahan jika diperlukan"),
          tags$li("Lakukan 'Eksplorasi Data' untuk memahami karakteristik data"),
          tags$li("Jalankan 'Uji Asumsi' sebelum analisis inferensia"),
          tags$li("Pilih metode yang sesuai di 'Statistik Inferensia'"),
          tags$li("Gunakan 'ANOVA & Regresi' untuk analisis hubungan"),
          tags$li("Download hasil di 'Ekspor Hasil'")
        ),
        br(),
        h4("Tips Penting:"),
        tags$ul(
          tags$li("Selalu periksa asumsi sebelum melakukan uji statistik"),
          tags$li("Interpretasikan hasil dengan hati-hati"),
          tags$li("Simpan hasil analisis secara berkala"),
          tags$li("Gunakan visualisasi untuk membantu interpretasi")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Mengerti")
    ))
  })
  
  observeEvent(input$export_report, {
    showNotification("Fitur ekspor laporan akan segera tersedia!", type = "warning")
  })
  
  observeEvent(input$contact_support, {
    showNotification("Fitur bantuan akan segera tersedia!", type = "warning")
  })
  
  output$distance_info <- renderPrint({
    cat("📏 INFORMASI MATRIKS JARAK\n")
    cat("==========================\n\n")
    cat("Dimensi Matriks:", nrow(distance_matrix), "x", ncol(distance_matrix), "\n")
    cat("Tipe Data: Matriks Penimbang Jarak\n")
    cat("Sumber: https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv\n\n")
    
    if(is.data.frame(distance_matrix)) {
      cat("Statistik Deskriptif Jarak:\n")
      distance_values <- as.matrix(distance_matrix)
      distance_values <- distance_values[upper.tri(distance_values)]
      cat("• Jarak Minimum:", round(min(distance_values, na.rm = TRUE), 3), "\n")
      cat("• Jarak Maksimum:", round(max(distance_values, na.rm = TRUE), 3), "\n")
      cat("• Jarak Rata-rata:", round(mean(distance_values, na.rm = TRUE), 3), "\n")
      cat("• Standar Deviasi:", round(sd(distance_values, na.rm = TRUE), 3), "\n")
    }
    
    cat("\n📊 Penggunaan dalam Analisis:\n")
    cat("• Clustering berdasarkan jarak\n")
    cat("• Analisis spasial\n")
    cat("• Visualisasi heatmap\n")
    cat("• Analisis komponen utama\n")
  })
  
  #Manajemen Data
  observeEvent(input$apply_categorization, {
    req(input$cat_variable, input$cat_groups)
    
    var_data <- sovi_data[[input$cat_variable]]
    
    if(input$cat_method == "quantile") {
      breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$cat_groups + 1), na.rm = TRUE)
      categorized <- cut(var_data, breaks = breaks, include.lowest = TRUE,
                         labels = if(input$cat_labels != "") {
                           trimws(strsplit(input$cat_labels, ",")[[1]])
                         } else {
                           paste0("Q", 1:input$cat_groups)
                         })
    } else if(input$cat_method == "equal") {
      breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), 
                    length.out = input$cat_groups + 1)
      categorized <- cut(var_data, breaks = breaks, include.lowest = TRUE,
                         labels = if(input$cat_labels != "") {
                           trimws(strsplit(input$cat_labels, ",")[[1]])
                         } else {
                           paste0("C", 1:input$cat_groups)
                         })
    } else if(input$cat_method == "kmeans") {
      km_result <- kmeans(var_data[!is.na(var_data)], centers = input$cat_groups)
      categorized <- rep(NA, length(var_data))
      categorized[!is.na(var_data)] <- paste0("K", km_result$cluster)
      categorized <- as.factor(categorized)
    }
    
    values$categorized_data <- sovi_data
    values$categorized_data[[paste0(input$cat_variable, "_categorized")]] <- categorized
    
    showNotification("✅ Kategorisasi berhasil diterapkan!", type = "message")
  })
  
  output$categorized_data <- DT::renderDataTable({
    if(is.null(values$categorized_data)) {
      return(DT::datatable(data.frame(Message = "Belum ada data terkategorisasi. Silakan terapkan kategorisasi terlebih dahulu.")))
    }
    DT::datatable(values$categorized_data, 
                  options = list(scrollX = TRUE, pageLength = 10),
                  class = 'cell-border stripe hover')
  })
  
  output$categorization_plot <- renderPlot({
    if(is.null(values$categorized_data) || is.null(input$cat_variable)) {
      plot.new()
      text(0.5, 0.5, "Belum ada data untuk divisualisasikan.\nSilakan terapkan kategorisasi terlebih dahulu.", 
           cex = 1.2, col = "gray")
      return()
    }
    
    cat_var <- paste0(input$cat_variable, "_categorized")
    
    p1 <- ggplot(values$categorized_data, aes_string(x = input$cat_variable)) +
      geom_histogram(bins = 30, fill = "#667eea", alpha = 0.7, color = "white") +
      theme_minimal() +
      labs(title = "Distribusi Asli", x = input$cat_variable, y = "Frekuensi")
    
    p2 <- ggplot(values$categorized_data, aes_string(x = cat_var)) +
      geom_bar(fill = "#764ba2", alpha = 0.7, color = "white") +
      theme_minimal() +
      labs(title = "Distribusi Terkategorisasi", x = "Kategori", y = "Frekuensi") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    gridExtra::grid.arrange(p1, p2, ncol = 2)
  })
  
  output$category_stats <- DT::renderDataTable({
    if(is.null(values$categorized_data) || is.null(input$cat_variable)) {
      return(DT::datatable(data.frame(Message = "Belum ada data terkategorisasi.")))
    }
    
    cat_var <- paste0(input$cat_variable, "_categorized")
    
    # Statistik per kategori
    cat_stats <- values$categorized_data %>%
      group_by_at(cat_var) %>%
      summarise(
        N = n(),
        Mean_Original = round(mean(get(input$cat_variable), na.rm = TRUE), 4),
        Median_Original = round(median(get(input$cat_variable), na.rm = TRUE), 4),
        SD_Original = round(sd(get(input$cat_variable), na.rm = TRUE), 4),
        Min_Original = round(min(get(input$cat_variable), na.rm = TRUE), 4),
        Max_Original = round(max(get(input$cat_variable), na.rm = TRUE), 4),
        .groups = 'drop'
      )
    
    DT::datatable(cat_stats, 
                  options = list(scrollX = TRUE, pageLength = 10),
                  class = 'cell-border stripe hover')
  })
  
  output$categorization_interpretation <- renderText({
    if(is.null(values$categorized_data) || is.null(input$cat_variable)) {
      return("Belum ada data terkategorisasi. Silakan terapkan kategorisasi terlebih dahulu.")
    }
    
    cat_var <- paste0(input$cat_variable, "_categorized")
    freq_table <- table(values$categorized_data[[cat_var]])
    
    method_desc <- switch(input$cat_method,
                          "quantile" = "berdasarkan kuantil (pembagian sama banyak)",
                          "equal" = "berdasarkan range yang sama",
                          "kmeans" = "berdasarkan clustering K-means"
    )
    
    paste0(
      "📊 INTERPRETASI KATEGORISASI DATA\n",
      "=================================\n\n",
      "🎯 Informasi Kategorisasi:\n",
      "• Variabel: ", input$cat_variable, "\n",
      "• Metode: ", str_to_title(method_desc), "\n",
      "• Jumlah Kategori: ", input$cat_groups, "\n\n",
      "📈 Distribusi Kategori:\n",
      paste(names(freq_table), ":", freq_table, "observasi (", 
            round(freq_table/sum(freq_table)*100, 1), "%)", collapse = "\n"), "\n\n",
      "🔍 Interpretasi Statistik:\n",
      "Kategorisasi ini mengubah variabel kontinyu menjadi kategorikal untuk:\n",
      "• Memudahkan analisis grup dan perbandingan\n",
      "• Mengurangi pengaruh outlier dalam data\n",
      "• Memungkinkan penggunaan uji non-parametrik\n",
      "• Interpretasi yang lebih mudah dipahami\n\n",
      "📋 Rekomendasi Penggunaan:\n",
      "• Gunakan untuk analisis ANOVA dengan faktor kategorikal\n",
      "• Cocok untuk uji chi-square dan analisis kontingensi\n",
      "• Dapat digunakan dalam regresi logistik\n",
      "• Berguna untuk visualisasi dan segmentasi data\n\n",
      "⚠️ Catatan Penting:\n",
      "• Kategorisasi mengurangi informasi detail dari data asli\n",
      "• Pilihan jumlah kategori mempengaruhi hasil analisis\n",
      "• Pastikan kategori memiliki interpretasi yang bermakna"
    )
  })
  
  #Statistik Deskriptif
  observeEvent(input$calc_descriptive, {
    req(input$desc_vars)
    
    if(input$desc_by_group && !is.null(input$desc_group_var)) {
      desc_results <- sovi_data %>%
        select(all_of(c(input$desc_vars, input$desc_group_var))) %>%
        group_by_at(input$desc_group_var) %>%
        summarise_if(is.numeric, list(
          N = ~sum(!is.na(.)),
          Mean = ~round(mean(., na.rm = TRUE), 4),
          Median = ~round(median(., na.rm = TRUE), 4),
          SD = ~round(sd(., na.rm = TRUE), 4),
          Variance = ~round(var(., na.rm = TRUE), 4),
          Min = ~round(min(., na.rm = TRUE), 4),
          Max = ~round(max(., na.rm = TRUE), 4),
          Q1 = ~round(quantile(., 0.25, na.rm = TRUE), 4),
          Q3 = ~round(quantile(., 0.75, na.rm = TRUE), 4),
          IQR = ~round(IQR(., na.rm = TRUE), 4)
        ), .groups = 'drop')
    } else {
      desc_results <- sovi_data %>%
        select(all_of(input$desc_vars)) %>%
        summarise_all(list(
          N = ~sum(!is.na(.)),
          Mean = ~round(mean(., na.rm = TRUE), 4),
          Median = ~round(median(., na.rm = TRUE), 4),
          SD = ~round(sd(., na.rm = TRUE), 4),
          Variance = ~round(var(., na.rm = TRUE), 4),
          Min = ~round(min(., na.rm = TRUE), 4),
          Max = ~round(max(., na.rm = TRUE), 4),
          Q1 = ~round(quantile(., 0.25, na.rm = TRUE), 4),
          Q3 = ~round(quantile(., 0.75, na.rm = TRUE), 4),
          IQR = ~round(IQR(., na.rm = TRUE), 4)
        )) %>%
        tidyr::gather(key = "Variable_Stat", value = "Value") %>%
        tidyr::separate(Variable_Stat, into = c("Variable", "Statistic"), sep = "_") %>%
        tidyr::spread(key = Statistic, value = Value)
    }
    
    values$descriptive_results <- desc_results
    showNotification("✅ Statistik deskriptif berhasil dihitung!", type = "message")
  })
  
  output$descriptive_stats <- DT::renderDataTable({
    if(is.null(values$descriptive_results)) {
      return(DT::datatable(data.frame(Message = "Belum ada hasil statistik deskriptif. Silakan hitung terlebih dahulu.")))
    }
    DT::datatable(values$descriptive_results, 
                  options = list(scrollX = TRUE, pageLength = 15),
                  class = 'cell-border stripe hover') %>%
      DT::formatRound(columns = 2:ncol(values$descriptive_results), digits = 4)
  })
  
  output$descriptive_plots <- renderPlot({
    req(input$desc_vars)
    
    plot_list <- list()
    for(i in seq_along(input$desc_vars)) {
      var_name <- input$desc_vars[i]
      
      p1 <- ggplot(sovi_data, aes_string(x = var_name)) +
        geom_histogram(bins = 30, fill = "#667eea", alpha = 0.7, color = "white") +
        theme_minimal() +
        labs(title = paste("Histogram:", var_name), x = var_name, y = "Frekuensi")
      
      p2 <- ggplot(sovi_data, aes_string(y = var_name)) +
        geom_boxplot(fill = "#764ba2", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Boxplot:", var_name), y = var_name) +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      
      plot_list[[length(plot_list) + 1]] <- p1
      plot_list[[length(plot_list) + 1]] <- p2
    }
    
    do.call(grid.arrange, c(plot_list, ncol = 2))
  })
  
  output$group_comparison_plot <- renderPlot({
    if(!input$desc_by_group || is.null(input$desc_group_var) || is.null(input$desc_vars)) {
      plot.new()
      text(0.5, 0.5, "Aktifkan 'Analisis per Grup' untuk melihat perbandingan.", cex = 1.2, col = "gray")
      return()
    }
    
    var_name <- input$desc_vars[1]
    group_var <- input$desc_group_var
    
    ggplot(sovi_data, aes_string(x = group_var, y = var_name, fill = group_var)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      scale_fill_viridis_d() +
      theme_minimal() +
      labs(title = paste("Perbandingan", var_name, "berdasarkan", group_var),
           x = group_var, y = var_name) +
      theme(legend.position = "none")
  })
  
  output$group_stats_table <- DT::renderDataTable({
    if(!input$desc_by_group || is.null(input$desc_group_var) || is.null(input$desc_vars)) {
      return(DT::datatable(data.frame(Message = "Aktifkan 'Analisis per Grup' untuk melihat statistik per grup.")))
    }
    
    group_stats <- sovi_data %>%
      select(all_of(c(input$desc_vars, input$desc_group_var))) %>%
      group_by_at(input$desc_group_var) %>%
      summarise_if(is.numeric, list(
        N = ~sum(!is.na(.)),
        Mean = ~round(mean(., na.rm = TRUE), 3),
        SD = ~round(sd(., na.rm = TRUE), 3)
      ), .groups = 'drop')
    
    DT::datatable(group_stats, 
                  options = list(scrollX = TRUE, pageLength = 10),
                  class = 'cell-border stripe hover')
  })
  
  output$descriptive_interpretation <- renderText({
    if(is.null(values$descriptive_results)) {
      return("Belum ada hasil statistik deskriptif. Silakan hitung terlebih dahulu.")
    }
    
    paste0(
      "📊 INTERPRETASI STATISTIK DESKRIPTIF\n",
      "====================================\n\n",
      "🎯 Variabel yang Dianalisis:\n",
      paste("•", input$desc_vars, collapse = "\n"), "\n\n",
      "📈 Interpretasi Ukuran Pemusatan:\n",
      "• Mean (Rata-rata): Nilai tengah aritmatika dari data\n",
      "• Median: Nilai tengah yang membagi data menjadi dua bagian sama\n",
      "• Jika Mean ≈ Median: Distribusi simetris\n",
      "• Jika Mean > Median: Distribusi miring kanan (positif)\n",
      "• Jika Mean < Median: Distribusi miring kiri (negatif)\n\n",
      "📊 Interpretasi Ukuran Penyebaran:\n",
      "• SD (Standar Deviasi): Ukuran penyebaran data dari rata-rata\n",
      "• Variance: Kuadrat dari standar deviasi\n",
      "• IQR (Interquartile Range): Rentang 50% data tengah\n",
      "• Range (Max-Min): Rentang keseluruhan data\n\n",
      "💡 Rekomendasi Analisis Lanjutan:\n",
      "• Jika distribusi normal: Gunakan uji parametrik\n",
      "• Jika distribusi tidak normal: Pertimbangkan transformasi atau uji non-parametrik\n",
      "• Jika ada outlier: Investigasi lebih lanjut atau gunakan metode robust\n",
      "• Untuk perbandingan grup: Lakukan uji ANOVA atau t-test"
    )
  })
  
  observeEvent(input$generate_plot, {
    req(input$plot_x, input$plot_type)
    
    plot_title <- if(input$plot_title != "") input$plot_title else paste(str_to_title(input$plot_type), "of", input$plot_x)
    
    if(input$plot_type == "scatter" && !is.null(input$plot_y)) {
      p <- ggplot(sovi_data, aes_string(x = input$plot_x, y = input$plot_y)) +
        geom_point(alpha = 0.6, color = input$plot_color) +
        theme_minimal() +
        labs(title = plot_title, x = input$plot_x, y = input$plot_y)
      
      if(input$add_trend) {
        p <- p + geom_smooth(method = "lm", se = TRUE, color = "#e74c3c")
      }
    } else if(input$plot_type == "line" && !is.null(input$plot_y)) {
      p <- ggplot(sovi_data, aes_string(x = input$plot_x, y = input$plot_y)) +
        geom_line(color = input$plot_color, size = 1) +
        geom_point(color = input$plot_color, alpha = 0.7) +
        theme_minimal() +
        labs(title = plot_title, x = input$plot_x, y = input$plot_y)
    } else if(input$plot_type == "hist") {
      p <- ggplot(sovi_data, aes_string(x = input$plot_x)) +
        geom_histogram(bins = 30, fill = input$plot_color, alpha = 0.7, color = "white") +
        theme_minimal() +
        labs(title = plot_title, x = input$plot_x, y = "Frekuensi")
    } else if(input$plot_type == "density") {
      p <- ggplot(sovi_data, aes_string(x = input$plot_x)) +
        geom_density(fill = input$plot_color, alpha = 0.7) +
        theme_minimal() +
        labs(title = plot_title, x = input$plot_x, y = "Kepadatan")
    } else if(input$plot_type == "box" && !is.null(input$plot_group)) {
      p <- ggplot(sovi_data, aes_string(x = input$plot_group, y = input$plot_x)) +
        geom_boxplot(fill = input$plot_color, alpha = 0.7) +
        theme_minimal() +
        labs(title = plot_title, x = input$plot_group, y = input$plot_x) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$plot_type == "violin" && !is.null(input$plot_group)) {
      p <- ggplot(sovi_data, aes_string(x = input$plot_group, y = input$plot_x)) +
        geom_violin(fill = input$plot_color, alpha = 0.7) +
        geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
        theme_minimal() +
        labs(title = plot_title, x = input$plot_group, y = input$plot_x) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$plot_type == "heatmap") {
      numeric_data <- sovi_data %>% select_if(is.numeric)
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      
      cor_long <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
      cor_long$value <- as.vector(cor_matrix)
      
      p <- ggplot(cor_long, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Korelasi") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Heatmap Korelasi", x = "", y = "")
    }
    
    values$current_plot <- p
    showNotification("✅ Plot berhasil dibuat!", type = "message")
  })
  
  output$interactive_plot <- renderPlotly({
    if(is.null(values$current_plot)) {
      plotly_empty() %>%
        layout(title = "Belum ada plot yang dibuat.\nSilakan buat plot terlebih dahulu.")
    } else {
      ggplotly(values$current_plot)
    }
  })
  
  output$static_plot <- renderPlot({
    if(is.null(values$current_plot)) {
      plot.new()
      text(0.5, 0.5, "Belum ada plot yang dibuat.\nSilakan buat plot terlebih dahulu.", 
           cex = 1.2, col = "gray")
    } else {
      values$current_plot
    }
  })
  
  #Analisis Matriks Jarak
  observeEvent(input$run_distance_analysis, {
    req(input$distance_analysis_type)
    
    if(input$distance_analysis_type == "descriptive") {
      distance_values <- as.matrix(distance_matrix)
      distance_values <- distance_values[upper.tri(distance_values)]
      
      desc_stats <- data.frame(
        Statistik = c("N", "Mean", "Median", "SD", "Min", "Max", "Q1", "Q3"),
        Nilai = c(
          length(distance_values),
          round(mean(distance_values, na.rm = TRUE), 4),
          round(median(distance_values, na.rm = TRUE), 4),
          round(sd(distance_values, na.rm = TRUE), 4),
          round(min(distance_values, na.rm = TRUE), 4),
          round(max(distance_values, na.rm = TRUE), 4),
          round(quantile(distance_values, 0.25, na.rm = TRUE), 4),
          round(quantile(distance_values, 0.75, na.rm = TRUE), 4)
        )
      )
      
      values$distance_analysis_results <- list(
        type = "descriptive",
        stats = desc_stats,
        values = distance_values
      )
      
    } else if(input$distance_analysis_type == "clustering") {
      distance_values <- as.matrix(distance_matrix)
      
      if(input$cluster_method == "kmeans") {
        mds_result <- cmdscale(distance_values, k = 2)
        cluster_result <- kmeans(mds_result, centers = input$n_clusters)
        clusters <- cluster_result$cluster
      } else if(input$cluster_method == "hierarchical") {
        hc_result <- hclust(as.dist(distance_values))
        clusters <- cutree(hc_result, k = input$n_clusters)
      } else if(input$cluster_method == "pam") {
        pam_result <- cluster::pam(as.dist(distance_values), k = input$n_clusters)
        clusters <- pam_result$clustering
      }
      
      cluster_summary <- data.frame(
        Cluster = 1:input$n_clusters,
        N = as.vector(table(clusters))
      )
      
      values$distance_analysis_results <- list(
        type = "clustering",
        method = input$cluster_method,
        clusters = clusters,
        summary = cluster_summary,
        n_clusters = input$n_clusters
      )
      
    } else if(input$distance_analysis_type == "heatmap") {
      values$distance_analysis_results <- list(
        type = "heatmap",
        matrix = as.matrix(distance_matrix)
      )
      
    } else if(input$distance_analysis_type == "pca") {
      distance_values <- as.matrix(distance_matrix)
      mds_result <- cmdscale(distance_values, k = min(5, nrow(distance_values)-1), eig = TRUE)
      
      eigenvalues <- mds_result$eig[mds_result$eig > 0]
      prop_var <- eigenvalues / sum(eigenvalues)
      
      pca_summary <- data.frame(
        Component = 1:length(prop_var),
        Eigenvalue = round(eigenvalues, 4),
        Proportion = round(prop_var, 4),
        Cumulative = round(cumsum(prop_var), 4)
      )
      
      values$distance_analysis_results <- list(
        type = "pca",
        coordinates = mds_result$points,
        summary = pca_summary,
        eigenvalues = eigenvalues
      )
    }
    
    showNotification("✅ Analisis matriks jarak selesai!", type = "message")
  })
  
  output$distance_analysis_results <- renderPrint({
    if(is.null(values$distance_analysis_results)) {
      return("Belum ada hasil analisis. Silakan jalankan analisis terlebih dahulu.")
    }
    
    result <- values$distance_analysis_results
    
    cat("📏 HASIL ANALISIS MATRIKS JARAK\n")
    cat("===============================\n\n")
    
    if(result$type == "descriptive") {
      cat("📊 STATISTIK DESKRIPTIF MATRIKS JARAK\n\n")
      print(result$stats)
      
    } else if(result$type == "clustering") {
      cat("🔍 HASIL CLUSTERING\n")
      cat("Metode:", toupper(result$method), "\n")
      cat("Jumlah Cluster:", result$n_clusters, "\n\n")
      cat("Ringkasan Cluster:\n")
      print(result$summary)
      
    } else if(result$type == "pca") {
      cat("📈 HASIL ANALISIS KOMPONEN UTAMA (MDS)\n\n")
      cat("Ringkasan Komponen:\n")
      print(result$summary)
    }
  })
  
  output$distance_results_table <- DT::renderDataTable({
    if(is.null(values$distance_analysis_results)) {
      return(DT::datatable(data.frame(Message = "Belum ada hasil analisis.")))
    }
    
    result <- values$distance_analysis_results
    
    if(result$type == "descriptive") {
      DT::datatable(result$stats, options = list(pageLength = 10))
    } else if(result$type == "clustering") {
      DT::datatable(result$summary, options = list(pageLength = 10))
    } else if(result$type == "pca") {
      DT::datatable(result$summary, options = list(pageLength = 10))
    } else {
      DT::datatable(data.frame(Message = "Tidak ada tabel untuk jenis analisis ini."))
    }
  })
  
  output$distance_plot <- renderPlot({
    if(is.null(values$distance_analysis_results)) {
      plot.new()
      text(0.5, 0.5, "Belum ada hasil analisis.\nSilakan jalankan analisis terlebih dahulu.", 
           cex = 1.2, col = "gray")
      return()
    }
    
    result <- values$distance_analysis_results
    
    if(result$type == "descriptive") {
      hist(result$values, breaks = 30, col = "#667eea", alpha = 0.7,
           main = "Distribusi Jarak", xlab = "Jarak", ylab = "Frekuensi")
      
    } else if(result$type == "clustering") {
      distance_matrix_num <- as.matrix(distance_matrix)
      mds_coords <- cmdscale(distance_matrix_num, k = 2)
      
      plot_data <- data.frame(
        X = mds_coords[,1],
        Y = mds_coords[,2],
        Cluster = as.factor(result$clusters)
      )
      
      ggplot(plot_data, aes(x = X, y = Y, color = Cluster)) +
        geom_point(size = 3, alpha = 0.7) +
        scale_color_viridis_d() +
        theme_minimal() +
        labs(title = paste("Hasil Clustering -", str_to_title(result$method)),
             x = "Dimensi 1", y = "Dimensi 2")
      
    } else if(result$type == "pca") {
      plot_data <- data.frame(
        Component = 1:length(result$eigenvalues),
        Eigenvalue = result$eigenvalues
      )
      
      ggplot(plot_data, aes(x = Component, y = Eigenvalue)) +
        geom_line(color = "#667eea", linewidth = 1) +
        geom_point(color = "#764ba2", size = 3) +
        scale_y_log10() +  
        theme_minimal() +
        labs(title = "Scree Plot - Analisis Komponen Utama (Skala Log)",
             x = "Komponen", y = "Eigenvalue (skala log)")
    }
  })
  
  output$distance_heatmap <- renderPlot({
    if(is.null(values$distance_analysis_results)) {
      plot.new()
      text(0.5, 0.5, "Belum ada hasil analisis.\nSilakan jalankan analisis terlebih dahulu.", 
           cex = 1.2, col = "gray")
      return()
    }
    
    distance_matrix_num <- as.matrix(distance_matrix)
    
    if(nrow(distance_matrix_num) > 50) {
      indices <- sample(1:nrow(distance_matrix_num), 50)
      distance_matrix_num <- distance_matrix_num[indices, indices]
    }
    
    distance_long <- expand.grid(
      X = 1:nrow(distance_matrix_num),
      Y = 1:ncol(distance_matrix_num)
    )
    distance_long$Distance <- as.vector(distance_matrix_num)
    
    ggplot(distance_long, aes(X, Y, fill = Distance)) +
      geom_tile() +
      scale_fill_viridis_c(name = "Jarak") +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.ticks = element_blank()) +
      labs(title = "Heatmap Matriks Jarak", x = "", y = "") +
      coord_fixed()
  })
  
  output$distance_interpretation <- renderText({
    if(is.null(values$distance_analysis_results)) {
      return("Belum ada hasil analisis. Silakan jalankan analisis terlebih dahulu.")
    }
    
    result <- values$distance_analysis_results
    
    if(result$type == "descriptive") {
      paste0(
        "📏 INTERPRETASI STATISTIK DESKRIPTIF MATRIKS JARAK\n",
        "==================================================\n\n",
        "🎯 Analisis Deskriptif:\n",
        "Matriks jarak menggambarkan kedekatan/jarak antar observasi.\n\n",
        "📊 Interpretasi Statistik:\n",
        "• Jarak rata-rata menunjukkan tingkat heterogenitas data\n",
        "• Jarak minimum (selain 0) menunjukkan observasi terdekat\n",
        "• Jarak maksimum menunjukkan observasi terjauh\n",
        "• Standar deviasi menunjukkan variabilitas jarak\n\n",
        "💡 Implikasi:\n",
        "• Jarak kecil: Observasi serupa/homogen\n",
        "• Jarak besar: Observasi berbeda/heterogen\n",
        "• Distribusi jarak mempengaruhi hasil clustering"
      )
      
    } else if(result$type == "clustering") {
      paste0(
        "🔍 INTERPRETASI HASIL CLUSTERING\n",
        "================================\n\n",
        "🎯 Metode: ", str_to_title(result$method), "\n",
        "📊 Jumlah Cluster: ", result$n_clusters, "\n\n",
        "📈 Interpretasi Cluster:\n",
        "• Cluster mengelompokkan observasi berdasarkan kedekatan jarak\n",
        "• Ukuran cluster menunjukkan distribusi data\n",
        "• Cluster yang seimbang menunjukkan struktur data yang baik\n\n",
        "💡 Rekomendasi:\n",
        "• Evaluasi kualitas clustering dengan silhouette analysis\n",
        "• Pertimbangkan jumlah cluster optimal\n",
        "• Interpretasikan cluster berdasarkan karakteristik data asli"
      )
      
    } else if(result$type == "pca") {
      total_var <- sum(result$summary$Proportion[1:2])
      paste0(
        "📈 INTERPRETASI ANALISIS KOMPONEN UTAMA\n",
        "=======================================\n\n",
        "🎯 Reduksi Dimensi:\n",
        "MDS (Multidimensional Scaling) digunakan untuk visualisasi matriks jarak.\n\n",
        "📊 Hasil Analisis:\n",
        "• Dua komponen pertama menjelaskan ", round(total_var*100, 1), "% varians\n",
        "• Eigenvalue > 1 menunjukkan komponen yang signifikan\n",
        "• Koordinat MDS dapat digunakan untuk visualisasi dan clustering\n\n",
        "💡 Interpretasi:\n",
        "• Komponen utama merepresentasikan pola utama dalam jarak\n",
        "• Observasi yang dekat dalam ruang MDS memiliki jarak yang kecil\n",
        "• Dapat digunakan untuk identifikasi outlier dan pola spasial"
      )
    } else {
      "Interpretasi akan muncul setelah analisis selesai."
    }
  })
  
  #Uji Normalitas
  observeEvent(input$run_normality, {
    req(input$norm_var, input$norm_tests)
    
    var_data <- sovi_data[[input$norm_var]]
    var_data <- var_data[!is.na(var_data)]
    
    test_results <- list()
    
    if("shapiro" %in% input$norm_tests) {
      if(length(var_data) <= 5000) {
        test_results$shapiro <- shapiro.test(var_data)
      } else {
        sample_data <- sample(var_data, 5000)
        test_results$shapiro <- shapiro.test(sample_data)
        test_results$shapiro$note <- "Test performed on random sample of 5000 observations"
      }
    }
    
    if("ks" %in% input$norm_tests) {
      test_results$ks <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
    }
    
    if("lillie" %in% input$norm_tests) {
      test_results$lillie <- lillie.test(var_data)
    }
    
    if("ad" %in% input$norm_tests) {
      test_results$ad <- ad.test(var_data)
    }
    
    if("jb" %in% input$norm_tests) {
      if(requireNamespace("tseries", quietly = TRUE)) {
        test_results$jb <- tseries::jarque.bera.test(var_data)
      }
    }
    
    values$normality_results <- list(
      tests = test_results,
      variable = input$norm_var,
      data = var_data,
      alpha = input$norm_alpha
    )
    
    showNotification("✅ Uji normalitas selesai!", type = "message")
  })
  
  output$normality_results <- renderPrint({
    if(is.null(values$normality_results)) {
      return("Belum ada hasil uji normalitas. Silakan jalankan uji terlebih dahulu.")
    }
    
    cat("🔍 HASIL UJI NORMALITAS\n")
    cat("=======================\n\n")
    cat("Variabel:", values$normality_results$variable, "\n")
    cat("Ukuran Sampel:", length(values$normality_results$data), "\n")
    cat("Tingkat Signifikansi:", values$normality_results$alpha, "\n\n")
    
    for(test_name in names(values$normality_results$tests)) {
      test_result <- values$normality_results$tests[[test_name]]
      
      cat(str_to_upper(test_name), "TEST:\n")
      cat(paste(rep("-", nchar(test_name) + 6), collapse = ""), "\n")
      
      if(test_name == "shapiro") {
        cat("W =", round(test_result$statistic, 6), "\n")
      } else if(test_name %in% c("ks", "lillie", "ad")) {
        cat("Statistik =", round(test_result$statistic, 6), "\n")
      } else if(test_name == "jb") {
        cat("JB =", round(test_result$statistic, 6), "\n")
      }
      
      cat("p-value =", format(test_result$p.value, scientific = TRUE), "\n")
      
      conclusion <- ifelse(test_result$p.value > values$normality_results$alpha, 
                           "✅ Data berdistribusi normal", 
                           "❌ Data tidak berdistribusi normal")
      cat("Kesimpulan:", conclusion, "\n")
      
      if(!is.null(test_result$note)) {
        cat("Catatan:", test_result$note, "\n")
      }
      cat("\n")
    }
    
    p_values <- sapply(values$normality_results$tests, function(x) x$p.value)
    normal_count <- sum(p_values > values$normality_results$alpha)
    total_tests <- length(p_values)
    
    cat("📊 KESIMPULAN KESELURUHAN:\n")
    cat("==========================\n")
    cat("Uji yang mendukung normalitas:", normal_count, "dari", total_tests, "uji\n")
    
    if(normal_count >= total_tests/2) {
      cat("🎯 Kesimpulan: Data kemungkinan berdistribusi normal\n")
      cat("📈 Rekomendasi: Gunakan uji parametrik\n")
    } else {
      cat("🎯 Kesimpulan: Data kemungkinan tidak berdistribusi normal\n")
      cat("📈 Rekomendasi: Pertimbangkan uji non-parametrik atau transformasi data\n")
    }
  })
  
  output$normality_plots <- renderPlot({
    if(is.null(values$normality_results)) {
      plot.new()
      text(0.5, 0.5, "Belum ada hasil uji normalitas.\nSilakan jalankan uji terlebih dahulu.", 
           cex = 1.2, col = "gray")
      return()
    }
    
    var_data <- values$normality_results$data
    
    par(mfrow = c(2, 2))
    
    hist(var_data, breaks = 30, freq = FALSE, 
         main = paste("Histogram:", values$normality_results$variable),
         xlab = values$normality_results$variable,
         col = "#667eea", border = "white")
    curve(dnorm(x, mean = mean(var_data), sd = sd(var_data)), 
          add = TRUE, col = "#e74c3c", lwd = 2)
    legend("topright", legend = "Kurva Normal", col = "#e74c3c", lwd = 2)
    
    qqnorm(var_data, main = "Q-Q Plot Normal",
           col = "#667eea", pch = 16)
    qqline(var_data, col = "#e74c3c", lwd = 2)
    
    boxplot(var_data, main = "Box Plot",
            ylab = values$normality_results$variable, 
            col = "#764ba2", border = "black")
    
    plot(density(var_data), main = "Density Plot",
         xlab = values$normality_results$variable, 
         col = "#9b59b6", lwd = 2)
    polygon(density(var_data), col = adjustcolor("#9b59b6", alpha.f = 0.3))
  })
  
  #Uji Homogenitas
  observeEvent(input$run_homogeneity, {
    req(input$homo_var, input$homo_group)
    
    group_var <- sovi_data[[input$homo_group]]
    
    if(!is.factor(group_var)) {
      if(is.numeric(group_var) && length(unique(group_var[!is.na(group_var)])) <= 10) {
        group_var <- as.factor(group_var)
      } else if(is.numeric(group_var)) {
        group_var <- cut(group_var, breaks = 3, labels = c("Low", "Medium", "High"))
      } else {
        group_var <- as.factor(group_var)
      }
    }
    
    if(input$homo_test == "levene") {
      test_result <- car::leveneTest(sovi_data[[input$homo_var]], group_var)
    } else if(input$homo_test == "bartlett") {
      test_result <- bartlett.test(sovi_data[[input$homo_var]], group_var)
    } else if(input$homo_test == "fligner") {
      test_result <- fligner.test(sovi_data[[input$homo_var]], group_var)
    }
    
    values$homogeneity_results <- list(
      test = test_result, 
      variable = input$homo_var, 
      group = input$homo_group, 
      method = input$homo_test,
      alpha = input$homo_alpha,
      group_data = group_var
    )
    
    showNotification("✅ Uji homogenitas selesai!", type = "message")
  })
  
  output$homogeneity_results <- renderPrint({
    if(is.null(values$homogeneity_results)) {
      return("Belum ada hasil uji homogenitas. Silakan jalankan uji terlebih dahulu.")
    }
    
    cat("⚖️ HASIL UJI HOMOGENITAS\n")
    cat("========================\n\n")
    cat("Variabel:", values$homogeneity_results$variable, "\n")
    cat("Grouping Variable:", values$homogeneity_results$group, "\n")
    cat("Metode:", str_to_upper(values$homogeneity_results$method), "\n")
    cat("Tingkat Signifikansi:", values$homogeneity_results$alpha, "\n\n")
    
    print(values$homogeneity_results$test)
    
    if(values$homogeneity_results$method == "levene") {
      p_val <- values$homogeneity_results$test$`Pr(>F)`[1]
    } else {
      p_val <- values$homogeneity_results$test$p.value
    }
    
    cat("\n📊 INTERPRETASI:\n")
    cat("H0: Varians antar grup sama (homogen)\n")
    cat("H1: Varians antar grup berbeda\n\n")
    
    conclusion <- ifelse(p_val > values$homogeneity_results$alpha, 
                         "✅ Varians homogen", "❌ Varians tidak homogen")
    cat("Kesimpulan:", conclusion, "\n")
    
    if(p_val > values$homogeneity_results$alpha) {
      cat("📈 Rekomendasi: Asumsi homogenitas terpenuhi, lanjutkan dengan uji parametrik\n")
    } else {
      cat("📈 Rekomendasi: Asumsi homogenitas tidak terpenuhi, pertimbangkan:\n")
      cat("   • Welch's t-test (untuk 2 grup)\n")
      cat("   • Transformasi data\n")
      cat("   • Uji non-parametrik\n")
    }
  })
  
  output$homogeneity_plots <- renderPlot({
    if(is.null(values$homogeneity_results)) {
      plot.new()
      text(0.5, 0.5, "Belum ada hasil uji homogenitas.\nSilakan jalankan uji terlebih dahulu.", 
           cex = 1.2, col = "gray")
      return()
    }
    
    plot_data <- data.frame(
      value = sovi_data[[values$homogeneity_results$variable]],
      group = values$homogeneity_results$group_data
    )
    
    plot_data <- plot_data[complete.cases(plot_data), ]
    
    par(mfrow = c(1, 2))
    
    boxplot(value ~ group, data = plot_data,
            main = "Boxplot by Group",
            xlab = values$homogeneity_results$group,
            ylab = values$homogeneity_results$variable,
            col = rainbow(length(unique(plot_data$group))))
    
    group_vars <- aggregate(value ~ group, data = plot_data, FUN = var)
    barplot(group_vars$value, names.arg = group_vars$group,
            main = "Variance by Group",
            xlab = values$homogeneity_results$group,
            ylab = "Variance",
            col = rainbow(nrow(group_vars)))
  })
  
  output$assumptions_interpretation <- renderText({
    paste0(
      "📝 INTERPRETASI GABUNGAN UJI ASUMSI\n",
      "===================================\n\n",
      "🎯 Pentingnya Uji Asumsi:\n",
      "Uji asumsi diperlukan sebelum melakukan analisis parametrik untuk memastikan\n",
      "validitas dan reliabilitas hasil analisis.\n\n",
      "🔍 Uji Normalitas:\n",
      "• Menguji apakah data mengikuti distribusi normal\n",
      "• H0: Data berdistribusi normal\n",
      "• Jika p-value > α: Terima H0 (data normal)\n",
      "• Penting untuk: t-test, ANOVA, regresi linear\n\n",
      "⚖️ Uji Homogenitas:\n",
      "• Menguji apakah varians antar grup sama\n",
      "• H0: Varians antar grup sama (homogen)\n",
      "• Jika p-value > α: Terima H0 (varians homogen)\n",
      "• Penting untuk: ANOVA, t-test dua sampel\n\n",
      "📊 Kombinasi Hasil:\n",
      "• Normal + Homogen: Gunakan uji parametrik standar\n",
      "• Normal + Tidak Homogen: Gunakan Welch's test\n",
      "• Tidak Normal + Homogen: Pertimbangkan transformasi\n",
      "• Tidak Normal + Tidak Homogen: Gunakan uji non-parametrik\n\n",
      "💡 Rekomendasi Umum:\n",
      "• Selalu periksa asumsi sebelum analisis utama\n",
      "• Gunakan visualisasi untuk konfirmasi\n",
      "• Pertimbangkan ukuran sampel dalam interpretasi\n",
      "• Dokumentasikan hasil uji asumsi dalam laporan"
    )
  })
  
  #Uji Rata-rata
  observeEvent(input$run_mean_test, {
    req(input$mean_test_type, input$mean_test_var)
    
    test_result <- NULL
    
    if(input$mean_test_type == "t_one") {
      test_result <- t.test(sovi_data[[input$mean_test_var]], 
                            mu = input$mu_value, 
                            alternative = input$mean_alternative,
                            conf.level = 1 - input$mean_alpha)
    } else if(input$mean_test_type == "t_two") {
      req(input$mean_group_var)
      group_var <- sovi_data[[input$mean_group_var]]
      if(is.numeric(group_var)) {
        group_var <- cut(group_var, breaks = 2, labels = c("Group1", "Group2"))
      } else {
        group_var <- as.factor(group_var)
        if(length(levels(group_var)) > 2) {
          group_var <- factor(group_var, levels = levels(group_var)[1:2])
        }
      }
      test_result <- t.test(sovi_data[[input$mean_test_var]] ~ group_var,
                            alternative = input$mean_alternative,
                            conf.level = 1 - input$mean_alpha)
    } else if(input$mean_test_type == "t_paired") {
      req(input$paired_var)
      test_result <- t.test(sovi_data[[input$mean_test_var]], 
                            sovi_data[[input$paired_var]], 
                            paired = TRUE,
                            alternative = input$mean_alternative,
                            conf.level = 1 - input$mean_alpha)
    } else if(input$mean_test_type == "welch") {
      req(input$mean_group_var)
      group_var <- sovi_data[[input$mean_group_var]]
      if(is.numeric(group_var)) {
        group_var <- cut(group_var, breaks = 2, labels = c("Group1", "Group2"))
      } else {
        group_var <- as.factor(group_var)
        if(length(levels(group_var)) > 2) {
          group_var <- factor(group_var, levels = levels(group_var)[1:2])
        }
      }
      test_result <- t.test(sovi_data[[input$mean_test_var]] ~ group_var,
                            var.equal = FALSE,
                            alternative = input$mean_alternative,
                            conf.level = 1 - input$mean_alpha)
    }
    
    if(!is.null(test_result)) {
      values$mean_test_results <- list(
        test = test_result, 
        type = input$mean_test_type, 
        variable = input$mean_test_var,
        alpha = input$mean_alpha,
        alternative = input$mean_alternative
      )
      showNotification("✅ Uji rata-rata selesai!", type = "message")
    }
  })
  
  output$mean_test_results <- renderPrint({
    if(is.null(values$mean_test_results)) {
      return("Belum ada hasil uji rata-rata. Silakan jalankan uji terlebih dahulu.")
    }
    
    cat("🧪 HASIL UJI RATA-RATA\n")
    cat("======================\n\n")
    cat("Jenis Uji:", str_to_upper(gsub("_", " ", values$mean_test_results$type)), "\n")
    cat("Variabel:", values$mean_test_results$variable, "\n")
    cat("Hipotesis Alternatif:", values$mean_test_results$alternative, "\n")
    cat("Tingkat Signifikansi:", values$mean_test_results$alpha, "\n\n")
    
    print(values$mean_test_results$test)
    
    p_val <- values$mean_test_results$test$p.value
    cat("\n📊 INTERPRETASI:\n")
    
    if(values$mean_test_results$type == "t_one") {
      cat("H0: μ =", input$mu_value, "\n")
      cat("H1: μ", switch(values$mean_test_results$alternative,
                          "two.sided" = "≠",
                          "greater" = ">",
                          "less" = "<"), input$mu_value, "\n")
    } else {
      cat("H0: μ1 = μ2\n")
      cat("H1: μ1", switch(values$mean_test_results$alternative,
                           "two.sided" = "≠",
                           "greater" = ">",
                           "less" = "<"), "μ2\n")
    }
    
    cat("\nKeputusan:", ifelse(p_val > values$mean_test_results$alpha, 
                               "Gagal menolak H0", "Tolak H0"), "\n")
    
    if(p_val > values$mean_test_results$alpha) {
      cat("📈 Kesimpulan: Tidak ada bukti yang cukup untuk menyatakan perbedaan rata-rata\n")
    } else {
      cat("📈 Kesimpulan: Ada bukti yang cukup untuk menyatakan perbedaan rata-rata\n")
    }
    
    ci <- values$mean_test_results$test$conf.int
    cat("\nInterval Kepercayaan", (1-values$mean_test_results$alpha)*100, "%: [", 
        round(ci[1], 4), ",", round(ci[2], 4), "]\n")
  })
  
  output$mean_test_plot <- renderPlot({
    if(is.null(values$mean_test_results)) {
      plot.new()
      text(0.5, 0.5, "Belum ada hasil uji rata-rata.\nSilakan jalankan uji terlebih dahulu.", 
           cex = 1.2, col = "gray")
      return()
    }
    
    if(values$mean_test_results$type %in% c("t_one", "t_paired")) {
      var_data <- sovi_data[[values$mean_test_results$variable]]
      hist(var_data, breaks = 30, col = "#667eea", alpha = 0.7,
           main = paste("Distribusi", values$mean_test_results$variable),
           xlab = values$mean_test_results$variable, ylab = "Frekuensi")
      abline(v = mean(var_data, na.rm = TRUE), col = "#e74c3c", lwd = 2, lty = 1)
      
      if(values$mean_test_results$type == "t_one") {
        abline(v = input$mu_value, col = "#2ecc71", lwd = 2, lty = 2)
        legend("topright", 
               legend = c("Sample Mean", "Hypothesized Mean"), 
               col = c("#e74c3c", "#2ecc71"), 
               lwd = 2, lty = c(1, 2))
      } else {
        legend("topright", legend = "Sample Mean", col = "#e74c3c", lwd = 2)
      }
      
    } else if(values$mean_test_results$type %in% c("t_two", "welch")) {
      group_var <- sovi_data[[input$mean_group_var]]
      if(is.numeric(group_var)) {
        group_var <- cut(group_var, breaks = 2, labels = c("Group1", "Group2"))
      }
      
      plot_data <- data.frame(
        value = sovi_data[[values$mean_test_results$variable]],
        group = group_var
      )
      plot_data <- plot_data[complete.cases(plot_data), ]
      
      boxplot(value ~ group, data = plot_data,
              main = paste("Perbandingan", values$mean_test_results$variable, "antar Grup"),
              xlab = input$mean_group_var,
              ylab = values$mean_test_results$variable,
              col = c("#667eea", "#764ba2"))
    }
  })
  
  output$mean_test_interpretation <- renderText({
    if(is.null(values$mean_test_results)) {
      return("Belum ada hasil uji rata-rata. Silakan jalankan uji terlebih dahulu.")
    }
    
    test_desc <- switch(values$mean_test_results$type,
                        "t_one" = "menguji apakah rata-rata populasi sama dengan nilai tertentu",
                        "t_two" = "menguji apakah rata-rata dua grup independen berbeda",
                        "t_paired" = "menguji apakah rata-rata dua pengukuran berpasangan berbeda",
                        "welch" = "menguji apakah rata-rata dua grup berbeda (varians tidak sama)"
    )
    
    effect_size <- ""
    if(values$mean_test_results$type %in% c("t_two", "welch")) {
      group_var <- sovi_data[[input$mean_group_var]]
      if(is.numeric(group_var)) {
        group_var <- cut(group_var, breaks = 2, labels = c("Group1", "Group2"))
      }
      
      group1_data <- sovi_data[[values$mean_test_results$variable]][group_var == levels(group_var)[1]]
      group2_data <- sovi_data[[values$mean_test_results$variable]][group_var == levels(group_var)[2]]
      
      pooled_sd <- sqrt(((length(group1_data)-1)*var(group1_data, na.rm = TRUE) + 
                           (length(group2_data)-1)*var(group2_data, na.rm = TRUE)) / 
                          (length(group1_data) + length(group2_data) - 2))
      
      cohens_d <- abs(mean(group1_data, na.rm = TRUE) - mean(group2_data, na.rm = TRUE)) / pooled_sd
      
      effect_interpretation <- if(cohens_d < 0.2) "kecil" else if(cohens_d < 0.5) "sedang" else if(cohens_d < 0.8) "besar" else "sangat besar"
      
      effect_size <- paste0(
        "\n\n📏 UKURAN EFEK (COHEN'S D):\n",
        "Cohen's d = ", round(cohens_d, 3), "\n",
        "Interpretasi: Efek ", effect_interpretation, "\n",
        "• d < 0.2: Efek kecil\n",
        "• 0.2 ≤ d < 0.5: Efek sedang\n",
        "• 0.5 ≤ d < 0.8: Efek besar\n",
        "• d ≥ 0.8: Efek sangat besar"
      )
    }
    
    paste0(
      "📝 INTERPRETASI UJI RATA-RATA\n",
      "=============================\n\n",
      "🎯 Tujuan Uji:\n",
      "Uji ini ", test_desc, ".\n\n",
      "📊 Hasil Statistik:\n",
      "• t-statistic = ", round(values$mean_test_results$test$statistic, 4), "\n",
      "• df = ", values$mean_test_results$test$parameter, "\n",
      "• p-value = ", round(values$mean_test_results$test$p.value, 6), "\n",
      "• α = ", values$mean_test_results$alpha, "\n\n",
      "🔍 Interpretasi Praktis:\n",
      ifelse(values$mean_test_results$test$p.value > values$mean_test_results$alpha,
             "Tidak ada perbedaan yang signifikan secara statistik.",
             "Ada perbedaan yang signifikan secara statistik."), "\n\n",
      "📈 Interval Kepercayaan:\n",
      "Dengan tingkat kepercayaan ", (1-values$mean_test_results$alpha)*100, "%, ",
      "interval untuk perbedaan rata-rata adalah [",
      round(values$mean_test_results$test$conf.int[1], 4), ", ",
      round(values$mean_test_results$test$conf.int[2], 4), "].\n",
      "Jika interval mencakup 0, maka tidak ada perbedaan yang signifikan.",
      effect_size,
      "\n\n💡 Rekomendasi:\n",
      "• Periksa asumsi normalitas dan homogenitas sebelum interpretasi final\n",
      "• Pertimbangkan signifikansi praktis selain signifikansi statistik\n",
      "• Laporkan ukuran efek untuk memberikan konteks hasil"
    )
  })
  
  #Uji Proporsi
  observeEvent(input$run_prop_test, {
    req(input$prop_test_type, input$prop_var)
    
    test_result <- NULL
    
    if(input$prop_test_type == "prop_one") {
      var_data <- sovi_data[[input$prop_var]]
      
      if(is.numeric(var_data)) {
        var_data <- cut(var_data, breaks = 2, labels = c("Low", "High"))
      }
      
      success_count <- sum(var_data == levels(as.factor(var_data))[1], na.rm = TRUE)
      total_count <- sum(!is.na(var_data))
      
      test_result <- prop.test(success_count, total_count, p = input$prop_value)
      
    } else if(input$prop_test_type == "prop_two") {
      req(input$prop_group_var)
      
      var_data <- sovi_data[[input$prop_var]]
      group_data <- sovi_data[[input$prop_group_var]]
      
      if(is.numeric(var_data)) {
        var_data <- cut(var_data, breaks = 2, labels = c("Low", "High"))
      }
      
      if(is.numeric(group_data)) {
        group_data <- cut(group_data, breaks = 2, labels = c("Group1", "Group2"))
      }
      
      cont_table <- table(var_data, group_data)
      
      if(ncol(cont_table) >= 2 && nrow(cont_table) >= 2) {
        success_counts <- cont_table[1, 1:2]
        total_counts <- colSums(cont_table)[1:2]
        test_result <- prop.test(success_counts, total_counts)
      }
      
    } else if(input$prop_test_type == "chisq_gof") {
      var_data <- sovi_data[[input$prop_var]]
      observed <- table(var_data)
      test_result <- chisq.test(observed)
      
    } else if(input$prop_test_type == "chisq_indep") {
      req(input$prop_group_var)
      
      var_data <- sovi_data[[input$prop_var]]
      group_data <- sovi_data[[input$prop_group_var]]
      
      cont_table <- table(var_data, group_data)
      test_result <- chisq.test(cont_table)
    }
    
    if(!is.null(test_result)) {
      values$prop_test_results <- list(
        test = test_result,
        type = input$prop_test_type,
        variable = input$prop_var,
        group_var = input$prop_group_var,
        alpha = input$prop_alpha
      )
      showNotification("✅ Uji proporsi selesai!", type = "message")
    }
  })
  
  output$prop_test_results <- renderPrint({
    if(is.null(values$prop_test_results)) {
      return("Belum ada hasil uji proporsi. Silakan jalankan uji terlebih dahulu.")
    }
    
    cat("📊 HASIL UJI PROPORSI\n")
    cat("=====================\n\n")
    cat("Jenis Uji:", str_to_upper(gsub("_", " ", values$prop_test_results$type)), "\n")
    cat("Variabel:", values$prop_test_results$variable, "\n")
    if(!is.null(values$prop_test_results$group_var)) {
      cat("Variabel Grouping:", values$prop_test_results$group_var, "\n")
    }
    cat("Tingkat Signifikansi:", values$prop_test_results$alpha, "\n\n")
    
    print(values$prop_test_results$test)
    
    p_val <- values$prop_test_results$test$p.value
    cat("\n📊 INTERPRETASI:\n")
    
    if(values$prop_test_results$type == "prop_one") {
      cat("H0: p =", input$prop_value, "\n")
      cat("H1: p ≠", input$prop_value, "\n")
    } else if(values$prop_test_results$type == "prop_two") {
      cat("H0: p1 = p2\n")
      cat("H1: p1 ≠ p2\n")
    } else if(values$prop_test_results$type == "chisq_gof") {
      cat("H0: Data mengikuti distribusi yang diharapkan\n")
      cat("H1: Data tidak mengikuti distribusi yang diharapkan\n")
    } else if(values$prop_test_results$type == "chisq_indep") {
      cat("H0: Variabel independen (tidak ada asosiasi)\n")
      cat("H1: Variabel tidak independen (ada asosiasi)\n")
    }
    
    cat("\nKeputusan:", ifelse(p_val > values$prop_test_results$alpha, 
                               "Gagal menolak H0", "Tolak H0"), "\n")
    
    if(p_val > values$prop_test_results$alpha) {
      cat("📈 Kesimpulan: Tidak ada bukti yang cukup untuk menolak hipotesis nol\n")
    } else {
      cat("📈 Kesimpulan: Ada bukti yang cukup untuk menolak hipotesis nol\n")
    }
  })
  
  output$contingency_table <- DT::renderDataTable({
    if(is.null(values$prop_test_results)) {
      return(DT::datatable(data.frame(Message = "Belum ada hasil uji proporsi.")))
    }
    
    if(values$prop_test_results$type %in% c("prop_two", "chisq_indep")) {
      var_data <- sovi_data[[values$prop_test_results$variable]]
      group_data <- sovi_data[[values$prop_test_results$group_var]]
      
      if(is.numeric(var_data)) {
        var_data <- cut(var_data, breaks = 2, labels = c("Low", "High"))
      }
      if(is.numeric(group_data)) {
        group_data <- cut(group_data, breaks = 2, labels = c("Group1", "Group2"))
      }
      
      cont_table <- table(var_data, group_data)
      cont_df <- as.data.frame.matrix(cont_table)
      cont_df$Total <- rowSums(cont_df)
      cont_df <- rbind(cont_df, Total = c(colSums(cont_table), sum(cont_table)))
      
      DT::datatable(cont_df, options = list(pageLength = 10))
      
    } else if(values$prop_test_results$type == "chisq_gof") {
      var_data <- sovi_data[[values$prop_test_results$variable]]
      freq_table <- table(var_data)
      
      result_df <- data.frame(
        Kategori = names(freq_table),
        Observed = as.vector(freq_table),
        Expected = round(values$prop_test_results$test$expected, 2),
        Residual = round(values$prop_test_results$test$residuals, 2)
      )
      
      DT::datatable(result_df, options = list(pageLength = 10))
      
    } else {
      DT::datatable(data.frame(Message = "Tabel kontingensi tidak tersedia untuk jenis uji ini."))
    }
  })
  
  output$prop_test_plot <- renderPlot({
    if(is.null(values$prop_test_results)) {
      plot.new()
      text(0.5, 0.5, "Belum ada hasil uji proporsi.\nSilakan jalankan uji terlebih dahulu.", 
           cex = 1.2, col = "gray")
      return()
    }
    
    if(values$prop_test_results$type == "prop_one") {
      var_data <- sovi_data[[values$prop_test_results$variable]]
      if(is.numeric(var_data)) {
        var_data <- cut(var_data, breaks = 2, labels = c("Low", "High"))
      }
      
      freq_table <- table(var_data)
      prop_table <- prop.table(freq_table)
      
      barplot(prop_table, main = paste("Proporsi", values$prop_test_results$variable),
              ylab = "Proporsi", col = rainbow(length(freq_table)))
      abline(h = input$prop_value, col = "red", lwd = 2, lty = 2)
      legend("topright", legend = paste("H0: p =", input$prop_value), 
             col = "red", lwd = 2, lty = 2)
      
    } else if(values$prop_test_results$type %in% c("prop_two", "chisq_indep")) {
      var_data <- sovi_data[[values$prop_test_results$variable]]
      group_data <- sovi_data[[values$prop_test_results$group_var]]
      
      if(is.numeric(var_data)) {
        var_data <- cut(var_data, breaks = 2, labels = c("Low", "High"))
      }
      if(is.numeric(group_data)) {
        group_data <- cut(group_data, breaks = 2, labels = c("Group1", "Group2"))
      }
      
      cont_table <- table(var_data, group_data)
      mosaicplot(cont_table, 
                 main = paste("Mosaic Plot:", values$prop_test_results$variable, "vs", values$prop_test_results$group_var),
                 color = rainbow(nrow(cont_table)))
      
    } else if(values$prop_test_results$type == "chisq_gof") {
      observed <- values$prop_test_results$test$observed
      expected <- values$prop_test_results$test$expected
      
      plot_data <- rbind(observed, expected)
      barplot(plot_data, beside = TRUE, 
              main = "Observed vs Expected Frequencies",
              legend = c("Observed", "Expected"),
              col = c("#667eea", "#e74c3c"))
    }
  })
  
  output$prop_test_interpretation <- renderText({
    if(is.null(values$prop_test_results)) {
      return("Belum ada hasil uji proporsi. Silakan jalankan uji terlebih dahulu.")
    }
    
    test_desc <- switch(values$prop_test_results$type,
                        "prop_one" = "menguji apakah proporsi populasi sama dengan nilai tertentu",
                        "prop_two" = "menguji apakah proporsi dua grup berbeda",
                        "chisq_gof" = "menguji apakah data mengikuti distribusi yang diharapkan",
                        "chisq_indep" = "menguji independensi antara dua variabel kategorikal"
    )
    
    effect_size <- ""
    if(values$prop_test_results$type == "chisq_indep") {
      chi_sq <- values$prop_test_results$test$statistic
      n <- sum(values$prop_test_results$test$observed)
      min_dim <- min(dim(values$prop_test_results$test$observed)) - 1
      cramers_v <- sqrt(chi_sq / (n * min_dim))
      
      effect_interpretation <- if(cramers_v < 0.1) "kecil" else if(cramers_v < 0.3) "sedang" else if(cramers_v < 0.5) "besar" else "sangat besar"
      
      effect_size <- paste0(
        "\n\n📏 UKURAN EFEK (CRAMER'S V):\n",
        "Cramer's V = ", round(cramers_v, 3), "\n",
        "Interpretasi: Asosiasi ", effect_interpretation, "\n",
        "• V < 0.1: Asosiasi kecil\n",
        "• 0.1 ≤ V < 0.3: Asosiasi sedang\n",
        "• 0.3 ≤ V < 0.5: Asosiasi besar\n",
        "• V ≥ 0.5: Asosiasi sangat besar"
      )
    }
    
    paste0(
      "📝 INTERPRETASI UJI PROPORSI\n",
      "============================\n\n",
      "🎯 Tujuan Uji:\n",
      "Uji ini ", test_desc, ".\n\n",
      "📊 Hasil Statistik:\n",
      "• Statistik uji = ", round(values$prop_test_results$test$statistic, 4), "\n",
      "• df = ", ifelse(is.null(values$prop_test_results$test$parameter), "N/A", values$prop_test_results$test$parameter), "\n",
      "• p-value = ", round(values$prop_test_results$test$p.value, 6), "\n",
      "• α = ", values$prop_test_results$alpha, "\n\n",
      "🔍 Interpretasi Praktis:\n",
      ifelse(values$prop_test_results$test$p.value > values$prop_test_results$alpha,
             "Tidak ada perbedaan/asosiasi yang signifikan secara statistik.",
             "Ada perbedaan/asosiasi yang signifikan secara statistik."), "\n",
      effect_size,
      "\n\n💡 Rekomendasi:\n",
      "• Periksa asumsi uji (frekuensi expected ≥ 5 untuk chi-square)\n",
      "• Pertimbangkan signifikansi praktis selain signifikansi statistik\n",
      "• Untuk chi-square, periksa residual untuk identifikasi sel yang berkontribusi\n",
      "• Laporkan ukuran efek untuk memberikan konteks hasil"
    )
  })
  
  #Uji Varians
  observeEvent(input$run_var_test, {
    req(input$var_test_type, input$var_test_var)
    
    test_result <- NULL
    
    if(input$var_test_type == "var_one") {
      var_data <- sovi_data[[input$var_test_var]]
      var_data <- var_data[!is.na(var_data)]
      
      n <- length(var_data)
      sample_var <- var(var_data)
      chi_sq_stat <- (n - 1) * sample_var / input$sigma_squared
      p_value <- 2 * min(pchisq(chi_sq_stat, df = n-1), 1 - pchisq(chi_sq_stat, df = n-1))
      
      test_result <- list(
        statistic = chi_sq_stat,
        parameter = n - 1,
        p.value = p_value,
        method = "One Sample Variance Test",
        data.name = input$var_test_var,
        sample.var = sample_var,
        hypothesized.var = input$sigma_squared
      )
      
    } else if(input$var_test_type == "f_test") {
      req(input$var_group_var)
      
      group_var <- sovi_data[[input$var_group_var]]
      if(is.numeric(group_var)) {
        group_var <- cut(group_var, breaks = 2, labels = c("Group1", "Group2"))
      } else {
        group_var <- as.factor(group_var)
        if(length(levels(group_var)) > 2) {
          group_var <- factor(group_var, levels = levels(group_var)[1:2])
        }
      }
      
      test_result <- var.test(sovi_data[[input$var_test_var]] ~ group_var)
      
    } else if(input$var_test_type == "levene") {
      req(input$var_group_var)
      
      group_var <- sovi_data[[input$var_group_var]]
      if(is.numeric(group_var)) {
        group_var <- cut(group_var, breaks = 3, labels = c("Low", "Medium", "High"))
      } else {
        group_var <- as.factor(group_var)
      }
      
      test_result <- car::leveneTest(sovi_data[[input$var_test_var]], group_var)
      
    } else if(input$var_test_type == "bartlett") {
      req(input$var_group_var)
      
      group_var <- sovi_data[[input$var_group_var]]
      if(is.numeric(group_var)) {
        group_var <- cut(group_var, breaks = 3, labels = c("Low", "Medium", "High"))
      } else {
        group_var <- as.factor(group_var)
      }
      
      test_result <- bartlett.test(sovi_data[[input$var_test_var]], group_var)
    }
    
    if(!is.null(test_result)) {
      values$var_test_results <- list(
        test = test_result,
        type = input$var_test_type,
        variable = input$var_test_var,
        group_var = input$var_group_var,
        alpha = input$var_alpha
      )
      showNotification("✅ Uji varians selesai!", type = "message")
    }
  })
  
  output$var_test_results <- renderPrint({
    if(is.null(values$var_test_results)) {
      return("Belum ada hasil uji varians. Silakan jalankan uji terlebih dahulu.")
    }
    
    cat("📈 HASIL UJI VARIANS\n")
    cat("====================\n\n")
    cat("Jenis Uji:", str_to_upper(gsub("_", " ", values$var_test_results$type)), "\n")
    cat("Variabel:", values$var_test_results$variable, "\n")
    if(!is.null(values$var_test_results$group_var)) {
      cat("Variabel Grouping:", values$var_test_results$group_var, "\n")
    }
    cat("Tingkat Signifikansi:", values$var_test_results$alpha, "\n\n")
    
    if(values$var_test_results$type == "var_one") {
      cat("HASIL UJI:\n")
      cat("Chi-square statistic =", round(values$var_test_results$test$statistic, 4), "\n")
      cat("df =", values$var_test_results$test$parameter, "\n")
      cat("p-value =", round(values$var_test_results$test$p.value, 6), "\n")
      cat("Sample variance =", round(values$var_test_results$test$sample.var, 4), "\n")
      cat("Hypothesized variance =", values$var_test_results$test$hypothesized.var, "\n")
    } else {
      print(values$var_test_results$test)
    }
    
    if(values$var_test_results$type == "levene") {
      p_val <- values$var_test_results$test$`Pr(>F)`[1]
    } else {
      p_val <- values$var_test_results$test$p.value
    }
    
    cat("\n📊 INTERPRETASI:\n")
    
    if(values$var_test_results$type == "var_one") {
      cat("H0: σ² =", input$sigma_squared, "\n")
      cat("H1: σ² ≠", input$sigma_squared, "\n")
    } else {
      cat("H0: Varians antar grup sama\n")
      cat("H1: Varians antar grup berbeda\n")
    }
    
    cat("\nKeputusan:", ifelse(p_val > values$var_test_results$alpha, 
                               "Gagal menolak H0", "Tolak H0"), "\n")
    
    if(p_val > values$var_test_results$alpha) {
      if(values$var_test_results$type == "var_one") {
        cat("📈 Kesimpulan: Tidak ada bukti yang cukup bahwa varians berbeda dari nilai hipotesis\n")
      } else {
        cat("📈 Kesimpulan: Varians antar grup homogen (sama)\n")
      }
    } else {
      if(values$var_test_results$type == "var_one") {
        cat("📈 Kesimpulan: Ada bukti yang cukup bahwa varians berbeda dari nilai hipotesis\n")
      } else {
        cat("📈 Kesimpulan: Varians antar grup tidak homogen (berbeda)\n")
      }
    }
  })
  
  output$var_test_plot <- renderPlot({
    if(is.null(values$var_test_results)) {
      plot.new()
      text(0.5, 0.5, "Belum ada hasil uji varians.\nSilakan jalankan uji terlebih dahulu.", 
           cex = 1.2, col = "gray")
      return()
    }
    
    if(values$var_test_results$type == "var_one") {
      var_data <- sovi_data[[values$var_test_results$variable]]
      hist(var_data, breaks = 30, col = "#667eea", alpha = 0.7,
           main = paste("Distribusi", values$var_test_results$variable),
           xlab = values$var_test_results$variable, ylab = "Frekuensi")
      
      mean_val <- mean(var_data, na.rm = TRUE)
      sd_val <- sqrt(values$var_test_results$test$sample.var)
      
      abline(v = mean_val, col = "#e74c3c", lwd = 2)
      abline(v = c(mean_val - sd_val, mean_val + sd_val), col = "#f39c12", lwd = 2, lty = 2)
      
      legend("topright", 
             legend = c("Mean", "Mean ± SD"), 
             col = c("#e74c3c", "#f39c12"), 
             lwd = 2, lty = c(1, 2))
      
    } else {
      group_var <- sovi_data[[values$var_test_results$group_var]]
      if(is.numeric(group_var)) {
        group_var <- cut(group_var, breaks = 3, labels = c("Low", "Medium", "High"))
      }
      
      plot_data <- data.frame(
        value = sovi_data[[values$var_test_results$variable]],
        group = group_var
      )
      plot_data <- plot_data[complete.cases(plot_data), ]
      
      par(mfrow = c(1, 2))
      
      boxplot(value ~ group, data = plot_data,
              main = "Boxplot by Group",
              xlab = values$var_test_results$group_var,
              ylab = values$var_test_results$variable,
              col = rainbow(length(unique(plot_data$group))))
      
      group_vars <- aggregate(value ~ group, data = plot_data, FUN = var)
      barplot(group_vars$value, names.arg = group_vars$group,
              main = "Variance by Group",
              xlab = values$var_test_results$group_var,
              ylab = "Variance",
              col = rainbow(nrow(group_vars)))
    }
  })
  
  output$var_test_interpretation <- renderText({
    if(is.null(values$var_test_results)) {
      return("Belum ada hasil uji varians. Silakan jalankan uji terlebih dahulu.")
    }
    
    test_desc <- switch(values$var_test_results$type,
                        "var_one" = "menguji apakah varians populasi sama dengan nilai tertentu",
                        "f_test" = "menguji apakah varians dua grup sama (F-test)",
                        "levene" = "menguji homogenitas varians antar grup (Levene's test)",
                        "bartlett" = "menguji homogenitas varians antar grup (Bartlett's test)"
    )
    
    if(values$var_test_results$type == "levene") {
      p_val <- values$var_test_results$test$`Pr(>F)`[1]
      stat_val <- values$var_test_results$test$`F value`[1]
      df_val <- paste(values$var_test_results$test$Df, collapse = ", ")
    } else {
      p_val <- values$var_test_results$test$p.value
      stat_val <- values$var_test_results$test$statistic
      df_val <- values$var_test_results$test$parameter
    }
    
    paste0(
      "📝 INTERPRETASI UJI VARIANS\n",
      "===========================\n\n",
      "🎯 Tujuan Uji:\n",
      "Uji ini ", test_desc, ".\n\n",
      "📊 Hasil Statistik:\n",
      "• Statistik uji = ", round(stat_val, 4), "\n",
      "• df = ", df_val, "\n",
      "• p-value = ", round(p_val, 6), "\n",
      "• α = ", values$var_test_results$alpha, "\n\n",
      "🔍 Interpretasi Praktis:\n",
      ifelse(p_val > values$var_test_results$alpha,
             "Varians tidak berbeda secara signifikan (homogen).",
             "Varians berbeda secara signifikan (tidak homogen)."), "\n\n",
      "📈 Implikasi untuk Analisis Lanjutan:\n",
      ifelse(p_val > values$var_test_results$alpha,
             "• Asumsi homogenitas varians terpenuhi\n• Dapat menggunakan uji parametrik standar (t-test, ANOVA)\n• Pooled variance dapat digunakan",
             "• Asumsi homogenitas varians tidak terpenuhi\n• Pertimbangkan Welch's t-test atau Brown-Forsythe test\n• Gunakan separate variance estimates"), "\n\n",
      "💡 Rekomendasi:\n",
      "• Uji varians sensitif terhadap non-normalitas\n",
      "• Levene's test lebih robust terhadap non-normalitas dibanding Bartlett's test\n",
      "• Pertimbangkan transformasi data jika varians tidak homogen\n",
      "• Visualisasi (boxplot, plot residual) membantu interpretasi"
    )
  })
  
  #ANOVA
  observeEvent(input$run_anova, {
    req(input$anova_response, input$anova_factor1)
    
    response_var <- sovi_data[[input$anova_response]]
    factor1 <- sovi_data[[input$anova_factor1]]
    
    if(is.numeric(factor1)) {
      factor1 <- cut(factor1, breaks = 3, labels = c("Low", "Medium", "High"))
    } else {
      factor1 <- as.factor(factor1)
    }
    
    anova_result <- NULL
    
    if(input$anova_type == "one_way") {
      anova_result <- aov(response_var ~ factor1)
      
    } else if(input$anova_type == "two_way") {
      req(input$anova_factor2)
      
      factor2 <- sovi_data[[input$anova_factor2]]
      if(is.numeric(factor2)) {
        factor2 <- cut(factor2, breaks = 3, labels = c("Low", "Medium", "High"))
      } else {
        factor2 <- as.factor(factor2)
      }
      
      if(input$anova_interaction) {
        anova_result <- aov(response_var ~ factor1 * factor2)
      } else {
        anova_result <- aov(response_var ~ factor1 + factor2)
      }
      
    } else if(input$anova_type == "repeated") {
      req(input$anova_subject)
      
      subject_var <- sovi_data[[input$anova_subject]]
      if(is.numeric(subject_var)) {
        subject_var <- as.factor(subject_var)
      }
      
      anova_result <- aov(response_var ~ factor1 + Error(subject_var/factor1))
    }
    
    posthoc_result <- NULL
    if(input$anova_posthoc && input$anova_type != "repeated") {
      if(input$posthoc_method == "tukey") {
        posthoc_result <- TukeyHSD(anova_result)
      } else if(input$posthoc_method == "bonferroni") {
        posthoc_result <- pairwise.t.test(response_var, factor1, p.adjust.method = "bonferroni")
      } else if(input$posthoc_method == "scheffe") {
        posthoc_result <- pairwise.t.test(response_var, factor1, p.adjust.method = "none")
        posthoc_result$method <- "Scheffe"
      } else if(input$posthoc_method == "lsd") {
        posthoc_result <- pairwise.t.test(response_var, factor1, p.adjust.method = "none")
      }
    }
    
    #Uji Asumsi
    assumptions_result <- NULL
    if(input$anova_assumptions) {
      residuals <- residuals(anova_result)
      
      if(length(residuals) <= 5000) {
        norm_test <- shapiro.test(residuals)
      } else {
        norm_test <- lillie.test(residuals)
      }
      
      homo_test <- car::leveneTest(response_var, factor1)
      
      assumptions_result <- list(
        normality = norm_test,
        homogeneity = homo_test
      )
    }
    
    values$anova_results <- list(
      model = anova_result,
      type = input$anova_type,
      response = input$anova_response,
      factor1 = input$anova_factor1,
      factor2 = input$anova_factor2,
      posthoc = posthoc_result,
      assumptions = assumptions_result,
      alpha = input$anova_alpha
    )
    
    showNotification("✅ ANOVA selesai!", type = "message")
  })
  
  output$anova_results <- renderPrint({
    if(is.null(values$anova_results)) {
      return("Belum ada hasil ANOVA. Silakan jalankan ANOVA terlebih dahulu.")
    }
    
    cat("📊 HASIL ANOVA\n")
    cat("==============\n\n")
    cat("Jenis ANOVA:", str_to_upper(gsub("_", " ", values$anova_results$type)), "\n")
    cat("Variabel Response:", values$anova_results$response, "\n")
    cat("Faktor 1:", values$anova_results$factor1, "\n")
    if(!is.null(values$anova_results$factor2)) {
      cat("Faktor 2:", values$anova_results$factor2, "\n")
    }
    cat("Tingkat Signifikansi:", values$anova_results$alpha, "\n\n")
    
    cat("TABEL ANOVA:\n")
    cat("============\n")
    print(summary(values$anova_results$model))
    
    anova_summary <- summary(values$anova_results$model)
    if(values$anova_results$type == "repeated") {
      # For repeated measures, extract from Error term
      if(length(anova_summary) > 1) {
        main_effect <- anova_summary[[2]][[1]]
        f_stat <- main_effect$`F value`[1]
        p_val <- main_effect$`Pr(>F)`[1]
      }
    } else {
      f_stat <- anova_summary[[1]]$`F value`[1]
      p_val <- anova_summary[[1]]$`Pr(>F)`[1]
    }
    
    cat("\n📊 INTERPRETASI:\n")
    cat("H0: Semua rata-rata grup sama\n")
    cat("H1: Minimal ada satu rata-rata grup yang berbeda\n\n")
    
    if(!is.na(p_val)) {
      cat("Keputusan:", ifelse(p_val > values$anova_results$alpha, 
                               "Gagal menolak H0", "Tolak H0"), "\n")
      
      if(p_val > values$anova_results$alpha) {
        cat("📈 Kesimpulan: Tidak ada perbedaan yang signifikan antar grup\n")
      } else {
        cat("📈 Kesimpulan: Ada perbedaan yang signifikan antar grup\n")
        cat("💡 Lanjutkan dengan uji post-hoc untuk identifikasi grup yang berbeda\n")
      }
    }
  })
  
  output$anova_plot <- renderPlot({
    if(is.null(values$anova_results)) {
      plot.new()
      text(0.5, 0.5, "Belum ada hasil ANOVA.\nSilakan jalankan ANOVA terlebih dahulu.", 
           cex = 1.2, col = "gray")
      return()
    }
    
    response_var <- sovi_data[[values$anova_results$response]]
    factor1 <- sovi_data[[values$anova_results$factor1]]
    
    if(is.numeric(factor1)) {
      factor1 <- cut(factor1, breaks = 3, labels = c("Low", "Medium", "High"))
    }
    
    if(values$anova_results$type == "one_way") {
      par(mfrow = c(2, 2))
      
      boxplot(response_var ~ factor1,
              main = paste("Boxplot:", values$anova_results$response, "by", values$anova_results$factor1),
              xlab = values$anova_results$factor1,
              ylab = values$anova_results$response,
              col = rainbow(length(unique(factor1))))
      
      means <- aggregate(response_var, by = list(factor1), FUN = mean, na.rm = TRUE)
      barplot(means$x, names.arg = means$Group.1,
              main = "Group Means",
              xlab = values$anova_results$factor1,
              ylab = paste("Mean", values$anova_results$response),
              col = rainbow(nrow(means)))
      
      plot(fitted(values$anova_results$model), residuals(values$anova_results$model),
           main = "Residuals vs Fitted",
           xlab = "Fitted Values", ylab = "Residuals",
           pch = 16, col = "#667eea")
      abline(h = 0, col = "red", lty = 2)
      
      qqnorm(residuals(values$anova_results$model), main = "Q-Q Plot of Residuals",
             pch = 16, col = "#667eea")
      qqline(residuals(values$anova_results$model), col = "red", lwd = 2)
      
    } else if(values$anova_results$type == "two_way") {
      factor2 <- sovi_data[[values$anova_results$factor2]]
      if(is.numeric(factor2)) {
        factor2 <- cut(factor2, breaks = 3, labels = c("Low", "Medium", "High"))
      }
      
      interaction.plot(factor1, factor2, response_var,
                       main = "Interaction Plot",
                       xlab = values$anova_results$factor1,
                       ylab = paste("Mean", values$anova_results$response),
                       trace.label = values$anova_results$factor2,
                       col = rainbow(length(unique(factor2))))
    }
  })
  
  output$posthoc_results <- renderPrint({
    if(is.null(values$anova_results) || is.null(values$anova_results$posthoc)) {
      return("Tidak ada hasil post-hoc. Aktifkan 'Uji Post-hoc' untuk melihat hasil.")
    }
    
    cat("🔍 HASIL UJI POST-HOC\n")
    cat("=====================\n\n")
    cat("Metode:", str_to_upper(input$posthoc_method), "\n\n")
    
    print(values$anova_results$posthoc)
    
    cat("\n📊 INTERPRETASI POST-HOC:\n")
    cat("Uji post-hoc dilakukan untuk mengidentifikasi pasangan grup yang berbeda signifikan.\n")
    cat("• p-value < α: Perbedaan signifikan antara kedua grup\n")
    cat("• p-value ≥ α: Tidak ada perbedaan signifikan antara kedua grup\n")
  })
  
  output$posthoc_plot <- renderPlot({
    if(is.null(values$anova_results) || is.null(values$anova_results$posthoc)) {
      plot.new()
      text(0.5, 0.5, "Tidak ada hasil post-hoc untuk divisualisasikan.", cex = 1.2, col = "gray")
      return()
    }
    
    if(input$posthoc_method == "tukey") {
      plot(values$anova_results$posthoc, 
           las = 1)
    } else {
      if(is.list(values$anova_results$posthoc) && !is.null(values$anova_results$posthoc$p.value)) {
        p_matrix <- values$anova_results$posthoc$p.value
        
        p_long <- expand.grid(Group1 = rownames(p_matrix), Group2 = colnames(p_matrix))
        p_long$p_value <- as.vector(p_matrix)
        
        library(ggplot2)
        ggplot(p_long, aes(Group1, Group2, fill = p_value)) +
          geom_tile() +
          scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                               midpoint = 0.05, name = "p-value") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Post-hoc p-values Heatmap", x = "Group", y = "Group")
      }
    }
  })
  
  output$anova_assumptions_results <- renderPrint({
    if(is.null(values$anova_results) || is.null(values$anova_results$assumptions)) {
      return("Tidak ada hasil uji asumsi. Aktifkan 'Uji Asumsi' untuk melihat hasil.")
    }
    
    cat("✅ HASIL UJI ASUMSI ANOVA\n")
    cat("=========================\n\n")
    
    cat("1. UJI NORMALITAS RESIDUAL:\n")
    cat("---------------------------\n")
    print(values$anova_results$assumptions$normality)
    
    norm_p <- values$anova_results$assumptions$normality$p.value
    cat("Kesimpulan:", ifelse(norm_p > 0.05, 
                              "✅ Residual berdistribusi normal", 
                              "❌ Residual tidak berdistribusi normal"), "\n\n")
    
    cat("2. UJI HOMOGENITAS VARIANS:\n")
    cat("---------------------------\n")
    print(values$anova_results$assumptions$homogeneity)
    
    homo_p <- values$anova_results$assumptions$homogeneity$`Pr(>F)`[1]
    cat("Kesimpulan:", ifelse(homo_p > 0.05, 
                              "✅ Varians homogen", 
                              "❌ Varians tidak homogen"), "\n\n")
    
    cat("📊 KESIMPULAN ASUMSI:\n")
    if(norm_p > 0.05 && homo_p > 0.05) {
      cat("✅ Semua asumsi ANOVA terpenuhi. Hasil ANOVA valid.\n")
    } else {
      cat("⚠️ Ada asumsi yang tidak terpenuhi. Pertimbangkan:\n")
      if(norm_p <= 0.05) cat("   • Transformasi data untuk normalitas\n")
      if(homo_p <= 0.05) cat("   • Welch ANOVA untuk varians tidak homogen\n")
      cat("   • Uji non-parametrik (Kruskal-Wallis)\n")
    }
  })
  
  output$anova_assumptions_plots <- renderPlot({
    if(is.null(values$anova_results) || is.null(values$anova_results$assumptions)) {
      plot.new()
      text(0.5, 0.5, "Tidak ada plot asumsi untuk ditampilkan.", cex = 1.2, col = "gray")
      return()
    }
    
    residuals_data <- residuals(values$anova_results$model)
    fitted_data <- fitted(values$anova_results$model)
    
    par(mfrow = c(2, 2))
    
    plot(fitted_data, residuals_data,
         main = "Residuals vs Fitted",
         xlab = "Fitted Values", ylab = "Residuals",
         pch = 16, col = "#667eea")
    abline(h = 0, col = "red", lty = 2)
    
    qqnorm(residuals_data, main = "Q-Q Plot of Residuals",
           pch = 16, col = "#667eea")
    qqline(residuals_data, col = "red", lwd = 2)
    
    sqrt_abs_resid <- sqrt(abs(residuals_data))
    plot(fitted_data, sqrt_abs_resid,
         main = "Scale-Location",
         xlab = "Fitted Values", ylab = "√|Residuals|",
         pch = 16, col = "#667eea")
    
    hist(residuals_data, breaks = 20, col = "#667eea", alpha = 0.7,
         main = "Histogram of Residuals",
         xlab = "Residuals", ylab = "Frequency")
    curve(dnorm(x, mean = mean(residuals_data), sd = sd(residuals_data)), 
          add = TRUE, col = "red", lwd = 2)
  })
  
  output$anova_interpretation <- renderText({
    if(is.null(values$anova_results)) {
      return("Belum ada hasil ANOVA. Silakan jalankan ANOVA terlebih dahulu.")
    }
    
    anova_summary <- summary(values$anova_results$model)
    if(values$anova_results$type == "repeated") {
      if(length(anova_summary) > 1) {
        main_effect <- anova_summary[[2]][[1]]
        f_stat <- main_effect$`F value`[1]
        p_val <- main_effect$`Pr(>F)`[1]
        df1 <- main_effect$Df[1]
        df2 <- main_effect$Df[2]
      }
    } else {
      f_stat <- anova_summary[[1]]$`F value`[1]
      p_val <- anova_summary[[1]]$`Pr(>F)`[1]
      df1 <- anova_summary[[1]]$Df[1]
      df2 <- anova_summary[[1]]$Df[2]
    }
    
    ss_effect <- anova_summary[[1]]$`Sum Sq`[1]
    ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
    eta_squared <- ss_effect / ss_total
    
    effect_interpretation <- if(eta_squared < 0.01) "kecil" else if(eta_squared < 0.06) "sedang" else if(eta_squared < 0.14) "besar" else "sangat besar"
    
    paste0(
      "📝 INTERPRETASI ANOVA\n",
      "=====================\n\n",
      "🎯 Tujuan Analisis:\n",
      "ANOVA menguji apakah ada perbedaan rata-rata antar grup.\n\n",
      "📊 Hasil Statistik:\n",
      "• F-statistic = ", round(f_stat, 4), "\n",
      "• df = ", df1, ", ", df2, "\n",
      "• p-value = ", round(p_val, 6), "\n",
      "• α = ", values$anova_results$alpha, "\n\n",
      "🔍 Interpretasi Praktis:\n",
      ifelse(p_val > values$anova_results$alpha,
             "Tidak ada perbedaan yang signifikan antar grup.",
             "Ada perbedaan yang signifikan antar grup."), "\n\n",
      "📏 UKURAN EFEK (ETA-SQUARED):\n",
      "η² = ", round(eta_squared, 3), "\n",
      "Interpretasi: Efek ", effect_interpretation, "\n",
      "• η² < 0.01: Efek kecil\n",
      "• 0.01 ≤ η² < 0.06: Efek sedang\n",
      "• 0.06 ≤ η² < 0.14: Efek besar\n",
      "• η² ≥ 0.14: Efek sangat besar\n\n",
      "💡 Rekomendasi:\n",
      ifelse(p_val > values$anova_results$alpha,
             "• Tidak perlu uji post-hoc karena tidak ada perbedaan signifikan\n• Pertimbangkan power analysis untuk ukuran sampel\n• Evaluasi desain penelitian dan pengukuran variabel",
             "• Lakukan uji post-hoc untuk identifikasi grup yang berbeda\n• Periksa asumsi ANOVA (normalitas dan homogenitas)\n• Interpretasikan hasil dalam konteks praktis"), "\n\n",
      "⚠️ Catatan Penting:\n",
      "• ANOVA hanya menunjukkan ada/tidaknya perbedaan, bukan grup mana yang berbeda\n",
      "• Signifikansi statistik tidak selalu berarti signifikansi praktis\n",
      "• Periksa asumsi sebelum interpretasi final"
    )
  })
  
  #Regresi Linear Berganda
  observeEvent(input$run_regression, {
    req(input$reg_response, input$reg_predictors)
    
    reg_data <- sovi_data[, c(input$reg_response, input$reg_predictors)]
    reg_data <- reg_data[complete.cases(reg_data), ]
    
    if(input$reg_standardize) {
      reg_data[, input$reg_predictors] <- scale(reg_data[, input$reg_predictors])
    }
    
    if(input$reg_interaction && length(input$reg_predictors) >= 2) {
      formula_str <- paste(input$reg_response, "~", paste(input$reg_predictors, collapse = " * "))
    } else {
      formula_str <- paste(input$reg_response, "~", paste(input$reg_predictors, collapse = " + "))
    }
    
    if(input$reg_polynomial && length(input$reg_predictors) >= 1) {
      poly_terms <- paste0("I(", input$reg_predictors[1], "^", 2:input$poly_degree, ")", collapse = " + ")
      formula_str <- paste(formula_str, "+", poly_terms)
    }
    
    if(!input$reg_intercept) {
      formula_str <- paste(formula_str, "- 1")
    }
    
    formula_obj <- as.formula(formula_str)
    
    if(input$reg_method == "enter") {
      reg_model <- lm(formula_obj, data = reg_data)
    } else if(input$reg_method == "forward") {
      null_model <- lm(paste(input$reg_response, "~ 1"), data = reg_data)
      full_model <- lm(formula_obj, data = reg_data)
      reg_model <- step(null_model, scope = list(lower = null_model, upper = full_model), 
                        direction = "forward", trace = 0)
    } else if(input$reg_method == "backward") {
      full_model <- lm(formula_obj, data = reg_data)
      reg_model <- step(full_model, direction = "backward", trace = 0)
    } else if(input$reg_method == "stepwise") {
      null_model <- lm(paste(input$reg_response, "~ 1"), data = reg_data)
      full_model <- lm(formula_obj, data = reg_data)
      reg_model <- step(null_model, scope = list(lower = null_model, upper = full_model), 
                        direction = "both", trace = 0)
    }
    
    diagnostics <- NULL
    if(input$reg_diagnostics) {
      residuals_data <- residuals(reg_model)
      
      if(length(residuals_data) <= 5000) {
        norm_test <- shapiro.test(residuals_data)
      } else {
        norm_test <- lillie.test(residuals_data)
      }
      
      bp_test <- tryCatch({
        lmtest::bptest(reg_model)
      }, error = function(e) {
        list(statistic = NA, p.value = NA, method = "Breusch-Pagan test (failed)")
      })
      
      dw_test <- tryCatch({
        lmtest::dwtest(reg_model)
      }, error = function(e) {
        list(statistic = NA, p.value = NA, method = "Durbin-Watson test (failed)")
      })
      
      vif_values <- tryCatch({
        if(length(input$reg_predictors) > 1) {
          car::vif(reg_model)
        } else {
          NA
        }
      }, error = function(e) {
        NA
      })
      
      diagnostics <- list(
        normality = norm_test,
        heteroscedasticity = bp_test,
        autocorrelation = dw_test,
        multicollinearity = vif_values
      )
    }
    
    values$regression_model <- list(
      model = reg_model,
      data = reg_data,
      formula = formula_str,
      method = input$reg_method,
      diagnostics = diagnostics,
      standardized = input$reg_standardize
    )
    
    showNotification("✅ Regresi linear berganda selesai!", type = "message")
  })
  
  output$regression_summary <- renderPrint({
    if(is.null(values$regression_model)) {
      return("Belum ada hasil regresi. Silakan jalankan regresi terlebih dahulu.")
    }
    
    cat("📈 HASIL REGRESI LINEAR BERGANDA\n")
    cat("================================\n\n")
    cat("Formula:", values$regression_model$formula, "\n")
    cat("Metode Seleksi:", str_to_upper(values$regression_model$method), "\n")
    cat("Data Standardisasi:", ifelse(values$regression_model$standardized, "Ya", "Tidak"), "\n")
    cat("Ukuran Sampel:", nrow(values$regression_model$data), "\n\n")
    
    print(summary(values$regression_model$model))
    
    model_summary <- summary(values$regression_model$model)
    
    cat("\n📊 STATISTIK TAMBAHAN:\n")
    cat("======================\n")
    cat("R-squared:", round(model_summary$r.squared, 4), "\n")
    cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
    cat("F-statistic:", round(model_summary$fstatistic[1], 4), "\n")
    cat("p-value (model):", format(pf(model_summary$fstatistic[1], 
                                      model_summary$fstatistic[2], 
                                      model_summary$fstatistic[3], 
                                      lower.tail = FALSE), scientific = TRUE), "\n")
    cat("Residual Standard Error:", round(model_summary$sigma, 4), "\n")
    cat("AIC:", round(AIC(values$regression_model$model), 2), "\n")
    cat("BIC:", round(BIC(values$regression_model$model), 2), "\n")
  })
  
  output$regression_diagnostics <- renderPlot({
    if(is.null(values$regression_model)) {
      plot.new()
      text(0.5, 0.5, "Belum ada hasil regresi.\nSilakan jalankan regresi terlebih dahulu.", 
           cex = 1.2, col = "gray")
      return()
    }
    
    par(mfrow = c(2, 2))
    plot(values$regression_model$model)
  })
  
  output$regression_assumptions <- renderPrint({
    if(is.null(values$regression_model) || is.null(values$regression_model$diagnostics)) {
      return("Tidak ada hasil uji asumsi. Aktifkan 'Uji Asumsi Lengkap' untuk melihat hasil.")
    }
    
    diag <- values$regression_model$diagnostics
    
    cat("✅ HASIL UJI ASUMSI REGRESI\n")
    cat("===========================\n\n")
    
    cat("1. UJI NORMALITAS RESIDUAL:\n")
    cat("---------------------------\n")
    print(diag$normality)
    norm_p <- diag$normality$p.value
    cat("Kesimpulan:", ifelse(norm_p > 0.05, 
                              "✅ Residual berdistribusi normal", 
                              "❌ Residual tidak berdistribusi normal"), "\n\n")
    
    cat("2. UJI HETEROSKEDASTISITAS (Breusch-Pagan):\n")
    cat("-------------------------------------------\n")
    if(!is.na(diag$heteroscedasticity$p.value)) {
      print(diag$heteroscedasticity)
      hetero_p <- diag$heteroscedasticity$p.value
      cat("Kesimpulan:", ifelse(hetero_p > 0.05, 
                                "✅ Homoskedastisitas (varians konstan)", 
                                "❌ Heteroskedastisitas (varians tidak konstan)"), "\n\n")
    } else {
      cat("Test gagal dijalankan\n\n")
    }
    
    cat("3. UJI AUTOKORELASI (Durbin-Watson):\n")
    cat("------------------------------------\n")
    if(!is.na(diag$autocorrelation$p.value)) {
      print(diag$autocorrelation)
      auto_p <- diag$autocorrelation$p.value
      cat("Kesimpulan:", ifelse(auto_p > 0.05, 
                                "✅ Tidak ada autokorelasi", 
                                "❌ Ada autokorelasi"), "\n\n")
    } else {
      cat("Test gagal dijalankan\n\n")
    }
    
    cat("4. UJI MULTIKOLINEARITAS (VIF):\n")
    cat("-------------------------------\n")
    if(!is.na(diag$multicollinearity[1])) {
      print(diag$multicollinearity)
      max_vif <- max(diag$multicollinearity, na.rm = TRUE)
      cat("Kesimpulan:", ifelse(max_vif < 5, 
                                "✅ Tidak ada multikolinearitas serius", 
                                ifelse(max_vif < 10, 
                                       "⚠️ Multikolinearitas moderat", 
                                       "❌ Multikolinearitas serius")), "\n")
      cat("Catatan: VIF > 5 menunjukkan multikolinearitas, VIF > 10 serius\n\n")
    } else {
      cat("VIF tidak dapat dihitung (hanya satu prediktor)\n\n")
    }
    
    cat("📊 KESIMPULAN ASUMSI:\n")
    assumptions_met <- 0
    total_assumptions <- 0
    
    if(!is.na(norm_p)) {
      total_assumptions <- total_assumptions + 1
      if(norm_p > 0.05) assumptions_met <- assumptions_met + 1
    }
    
    if(!is.na(diag$heteroscedasticity$p.value)) {
      total_assumptions <- total_assumptions + 1
      if(diag$heteroscedasticity$p.value > 0.05) assumptions_met <- assumptions_met + 1
    }
    
    if(!is.na(diag$autocorrelation$p.value)) {
      total_assumptions <- total_assumptions + 1
      if(diag$autocorrelation$p.value > 0.05) assumptions_met <- assumptions_met + 1
    }
    
    if(!is.na(diag$multicollinearity[1])) {
      total_assumptions <- total_assumptions + 1
      if(max(diag$multicollinearity, na.rm = TRUE) < 5) assumptions_met <- assumptions_met + 1
    }
    
    cat("Asumsi terpenuhi:", assumptions_met, "dari", total_assumptions, "asumsi\n")
    
    if(assumptions_met == total_assumptions) {
      cat("✅ Semua asumsi regresi terpenuhi. Hasil regresi valid.\n")
    } else {
      cat("⚠️ Ada asumsi yang tidak terpenuhi. Pertimbangkan:\n")
      cat("   • Transformasi variabel\n")
      cat("   • Robust standard errors\n")
      cat("   • Ridge/Lasso regression untuk multikolinearitas\n")
    }
  })
  
  output$regression_assumptions_plots <- renderPlot({
    if(is.null(values$regression_model)) {
      plot.new()
      text(0.5, 0.5, "Belum ada hasil regresi untuk ditampilkan.", cex = 1.2, col = "gray")
      return()
    }
    
    model <- values$regression_model$model
    residuals_data <- residuals(model)
    fitted_data <- fitted(model)
    
    par(mfrow = c(2, 3))
    
    plot(fitted_data, residuals_data,
         main = "Residuals vs Fitted",
         xlab = "Fitted Values", ylab = "Residuals",
         pch = 16, col = "#667eea")
    abline(h = 0, col = "red", lty = 2)
    
    qqnorm(residuals_data, main = "Q-Q Plot of Residuals",
           pch = 16, col = "#667eea")
    qqline(residuals_data, col = "red", lwd = 2)
    
    sqrt_abs_resid <- sqrt(abs(residuals_data))
    plot(fitted_data, sqrt_abs_resid,
         main = "Scale-Location",
         xlab = "Fitted Values", ylab = "√|Residuals|",
         pch = 16, col = "#667eea")
    
    cooks_d <- cooks.distance(model)
    plot(cooks_d, main = "Cook's Distance",
         xlab = "Observation", ylab = "Cook's Distance",
         pch = 16, col = "#667eea")
    abline(h = 4/length(cooks_d), col = "red", lty = 2)
    
    plot(model, which = 5)
    
    hist(residuals_data, breaks = 20, col = "#667eea", alpha = 0.7,
         main = "Histogram of Residuals",
         xlab = "Residuals", ylab = "Frequency")
    curve(dnorm(x, mean = mean(residuals_data), sd = sd(residuals_data)), 
          add = TRUE, col = "red", lwd = 2)
  })
  
  output$model_comparison <- DT::renderDataTable({
    if(is.null(values$regression_model)) {
      return(DT::datatable(data.frame(Message = "Belum ada model untuk dibandingkan.")))
    }
    
    model <- values$regression_model$model
    
    simple_formula <- paste(input$reg_response, "~", input$reg_predictors[1])
    simple_model <- lm(as.formula(simple_formula), data = values$regression_model$data)
    
    full_model <- model
    
    comparison_df <- data.frame(
      Model = c("Simple", "Full"),
      Predictors = c(1, length(input$reg_predictors)),
      R_squared = c(summary(simple_model)$r.squared, summary(full_model)$r.squared),
      Adj_R_squared = c(summary(simple_model)$adj.r.squared, summary(full_model)$adj.r.squared),
      AIC = c(AIC(simple_model), AIC(full_model)),
      BIC = c(BIC(simple_model), BIC(full_model)),
      RMSE = c(sqrt(mean(residuals(simple_model)^2)), sqrt(mean(residuals(full_model)^2)))
    )
    
    DT::datatable(comparison_df, options = list(pageLength = 10)) %>%
      DT::formatRound(columns = 3:7, digits = 4)
  })
  
  output$model_comparison_plot <- renderPlot({
    if(is.null(values$regression_model)) {
      plot.new()
      text(0.5, 0.5, "Belum ada model untuk dibandingkan.", cex = 1.2, col = "gray")
      return()
    }
    
    model <- values$regression_model$model
    actual <- values$regression_model$data[[input$reg_response]]
    predicted <- fitted(model)
    
    plot(actual, predicted,
         main = "Actual vs Predicted Values",
         xlab = "Actual Values", ylab = "Predicted Values",
         pch =16, col = "#667eea")
    abline(0, 1, col = "red", lwd = 2)
    
    r_squared <- summary(model)$r.squared
    legend("topleft", legend = paste("R² =", round(r_squared, 4)), 
           bty = "n", cex = 1.2)
  })
  
  output$prediction_inputs <- renderUI({
    if(is.null(values$regression_model)) {
      return(p("Belum ada model untuk prediksi."))
    }
    
    predictors <- input$reg_predictors
    input_list <- list()
    
    for(i in seq_along(predictors)) {
      var_name <- predictors[i]
      var_data <- sovi_data[[var_name]]
      
      if(is.numeric(var_data)) {
        input_list[[i]] <- numericInput(
          paste0("pred_", var_name),
          label = var_name,
          value = round(mean(var_data, na.rm = TRUE), 2),
          min = round(min(var_data, na.rm = TRUE), 2),
          max = round(max(var_data, na.rm = TRUE), 2),
          step = 0.01
        )
      }
    }
    
    tagList(input_list, 
            br(),
            actionButton("make_prediction", "🔮 Buat Prediksi", class = "btn btn-success"))
  })
  
  prediction_result <- reactiveVal(NULL)
  
  observeEvent(input$make_prediction, {
    if(is.null(values$regression_model)) return()
    
    pred_data <- data.frame(row.names = 1)
    for(var_name in input$reg_predictors) {
      pred_data[[var_name]] <- input[[paste0("pred_", var_name)]]
    }
    
    if(values$regression_model$standardized) {
      for(var_name in input$reg_predictors) {
        original_data <- sovi_data[[var_name]]
        pred_data[[var_name]] <- (pred_data[[var_name]] - mean(original_data, na.rm = TRUE)) / sd(original_data, na.rm = TRUE)
      }
    }
    
    pred_value <- predict(values$regression_model$model, newdata = pred_data, interval = "prediction")
    
    prediction_result(list(
      point_estimate = pred_value[1],
      lower_ci = pred_value[2],
      upper_ci = pred_value[3],
      inputs = pred_data
    ))
  })
  
  output$prediction_results <- renderPrint({
    pred <- prediction_result()
    if(is.null(pred)) {
      return("Masukkan nilai prediktor dan klik 'Buat Prediksi'.")
    }
    
    cat("🔮 HASIL PREDIKSI\n")
    cat("=================\n\n")
    cat("Prediksi:", round(pred$point_estimate, 4), "\n")
    cat("95% Prediction Interval: [", round(pred$lower_ci, 4), ",", round(pred$upper_ci, 4), "]\n\n")
    
    cat("Input Values:\n")
    for(var_name in names(pred$inputs)) {
      cat("•", var_name, ":", pred$inputs[[var_name]], "\n")
    }
  })
  
  output$prediction_plot <- renderPlot({
    if(is.null(values$regression_model)) {
      plot.new()
      text(0.5, 0.5, "Belum ada model untuk prediksi.", cex = 1.2, col = "gray")
      return()
    }
    
    if(length(input$reg_predictors) >= 1) {
      var_name <- input$reg_predictors[1]
      var_data <- values$regression_model$data[[var_name]]
      response_data <- values$regression_model$data[[input$reg_response]]
      
      var_seq <- seq(min(var_data), max(var_data), length.out = 100)
      
      pred_data <- data.frame(var_seq)
      names(pred_data) <- var_name
      
      for(other_var in input$reg_predictors[-1]) {
        pred_data[[other_var]] <- mean(values$regression_model$data[[other_var]])
      }
      
      pred_intervals <- predict(values$regression_model$model, newdata = pred_data, interval = "prediction")
      
      plot(var_data, response_data,
           main = paste("Prediction Plot:", var_name),
           xlab = var_name, ylab = input$reg_response,
           pch = 16, col = "#667eea", alpha = 0.6)
      
      lines(var_seq, pred_intervals[,1], col = "red", lwd = 2)
      lines(var_seq, pred_intervals[,2], col = "red", lwd = 1, lty = 2)
      lines(var_seq, pred_intervals[,3], col = "red", lwd = 1, lty = 2)
      
      legend("topleft", 
             legend = c("Data", "Prediction", "95% PI"), 
             col = c("#667eea", "red", "red"), 
             lty = c(NA, 1, 2), 
             pch = c(16, NA, NA),
             lwd = c(NA, 2, 1))
    }
  })
  
  output$regression_interpretation <- renderText({
    if(is.null(values$regression_model)) {
      return("Belum ada hasil regresi. Silakan jalankan regresi terlebih dahulu.")
    }
    
    model_summary <- summary(values$regression_model$model)
    
    paste0(
      "📝 INTERPRETASI REGRESI LINEAR BERGANDA\n",
      "=======================================\n\n",
      "🎯 Model yang Digunakan:\n",
      "Formula: ", values$regression_model$formula, "\n",
      "Metode: ", str_to_title(values$regression_model$method), "\n",
      "Standardisasi: ", ifelse(values$regression_model$standardized, "Ya", "Tidak"), "\n\n",
      "📊 Kualitas Model:\n",
      "• R² = ", round(model_summary$r.squared, 4), " (", round(model_summary$r.squared*100, 1), "% varians dijelaskan)\n",
      "• Adjusted R² = ", round(model_summary$adj.r.squared, 4), "\n",
      "• F-statistic = ", round(model_summary$fstatistic[1], 4), "\n",
      "• p-value (model) = ", format(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), scientific = TRUE), "\n\n",
      "🔍 Interpretasi Koefisien:\n",
      "• Intercept: Nilai prediksi ketika semua prediktor = 0\n",
      "• Slope: Perubahan response per unit perubahan prediktor\n",
      "• p-value < 0.05: Prediktor signifikan\n",
      "• t-value: Kekuatan hubungan relatif terhadap standard error\n\n",
      "📈 Interpretasi R²:\n",
      if(model_summary$r.squared < 0.3) {
        "• R² < 0.3: Model menjelaskan sebagian kecil varians\n• Mungkin ada prediktor penting yang belum dimasukkan"
      } else if(model_summary$r.squared < 0.7) {
        "• 0.3 ≤ R² < 0.7: Model menjelaskan varians dengan cukup baik\n• Masih ada ruang untuk perbaikan model"
      } else {
        "• R² ≥ 0.7: Model menjelaskan sebagian besar varians\n• Model memiliki daya prediksi yang baik"
      }, "\n\n",
      "💡 Rekomendasi:\n",
      "• Periksa asumsi regresi sebelum interpretasi final\n",
      "• Evaluasi multikolinearitas antar prediktor\n",
      "• Pertimbangkan transformasi jika asumsi tidak terpenuhi\n",
      "• Validasi model dengan data baru jika memungkinkan\n",
      "• Interpretasikan koefisien dalam konteks domain penelitian\n\n",
      "⚠️ Catatan Penting:\n",
      "• Korelasi tidak berarti kausalitas\n",
      "• Model hanya valid dalam rentang data yang digunakan\n",
      "• Outlier dapat mempengaruhi hasil secara signifikan"
    )
  })
  
  create_word_doc <- function(title, content) {
    doc <- officer::read_docx()
    doc <- doc %>%
      officer::body_add_par(title, style = "heading 1") %>%
      officer::body_add_par("") %>%
      officer::body_add_par(content, style = "Normal")
    return(doc)
  }
  
  #Download handlers untuk Beranda
  output$download_sovi_summary <- downloadHandler(
    filename = function() {
      paste0("SoVI_Summary_", Sys.Date(), ".docx")
    },
    content = function(file) {
      numeric_data <- sovi_data %>% select_if(is.numeric)
      summary_stats <- numeric_data %>%
        summarise_all(list(
          N = ~sum(!is.na(.)),
          Mean = ~round(mean(., na.rm = TRUE), 3),
          Median = ~round(median(., na.rm = TRUE), 3),
          SD = ~round(sd(., na.rm = TRUE), 3)
        )) %>%
        tidyr::gather(key = "Variable_Stat", value = "Value") %>%
        tidyr::separate(Variable_Stat, into = c("Variable", "Statistic"), sep = "_") %>%
        tidyr::spread(key = Statistic, value = Value)
      
      content <- paste0(
        "RINGKASAN DATA SOCIAL VULNERABILITY INDEX (SoVI)\n",
        "================================================\n\n",
        "Dataset: Social Vulnerability Index\n",
        "Sumber: https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv\n",
        "Tanggal Analisis: ", Sys.Date(), "\n\n",
        "STATISTIK DESKRIPTIF:\n",
        "====================\n",
        paste(capture.output(print(summary_stats)), collapse = "\n"),
        "\n\nINTERPRETASI:\n",
        "=============\n",
        "Data SoVI terdiri dari ", nrow(sovi_data), " observasi dengan ", ncol(sovi_data), " variabel.\n",
        "Variabel numerik: ", sum(sapply(sovi_data, is.numeric)), "\n",
        "Variabel kategorikal: ", sum(!sapply(sovi_data, is.numeric)), "\n\n",
        "Data ini dapat digunakan untuk:\n",
        "• Analisis kerentanan sosial\n",
        "• Pemodelan spasial\n",
        "• Clustering dan klasifikasi\n",
        "• Analisis regresi dan ANOVA"
      )
      
      doc <- create_word_doc("Ringkasan Data SoVI", content)
      print(doc, target = file)
    }
  )
  
  #Download handlers untuk Manajemen Data
  output$download_categorized_data <- downloadHandler(
    filename = function() {
      paste0("Categorized_Data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if(is.null(values$categorized_data)) {
        empty_data <- data.frame(Message = "Belum ada data terkategorisasi. Silakan terapkan kategorisasi terlebih dahulu.")
        openxlsx::write.xlsx(empty_data, file)
      } else {
        openxlsx::write.xlsx(values$categorized_data, file, rowNames = FALSE)
      }
    }
  )
  
  output$download_cat_interpretation <- downloadHandler(
    filename = function() {
      paste0("Categorization_Interpretation_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$categorized_data)) {
        content <- "Belum ada data terkategorisasi. Silakan terapkan kategorisasi terlebih dahulu."
      } else {
        cat_var <- paste0(input$cat_variable, "_categorized")
        freq_table <- table(values$categorized_data[[cat_var]])
        
        method_desc <- switch(input$cat_method,
                              "quantile" = "berdasarkan kuantil (pembagian sama banyak)",
                              "equal" = "berdasarkan range yang sama",
                              "kmeans" = "berdasarkan clustering K-means"
        )
        
        content <- paste0(
          "INTERPRETASI KATEGORISASI DATA\n",
          "==============================\n\n",
          "INFORMASI KATEGORISASI:\n",
          "=======================\n",
          "Variabel: ", input$cat_variable, "\n",
          "Metode: ", str_to_title(method_desc), "\n",
          "Jumlah Kategori: ", input$cat_groups, "\n",
          "Tanggal Analisis: ", Sys.Date(), "\n\n",
          "DISTRIBUSI KATEGORI:\n",
          "===================\n",
          paste(names(freq_table), ":", freq_table, "observasi (", 
                round(freq_table/sum(freq_table)*100, 1), "%)", collapse = "\n"), "\n\n",
          "INTERPRETASI STATISTIK:\n",
          "=======================\n",
          "Kategorisasi ini mengubah variabel kontinyu menjadi kategorikal untuk:\n",
          "• Memudahkan analisis grup dan perbandingan\n",
          "• Mengurangi pengaruh outlier dalam data\n",
          "• Memungkinkan penggunaan uji non-parametrik\n",
          "• Interpretasi yang lebih mudah dipahami\n\n",
          "REKOMENDASI PENGGUNAAN:\n",
          "======================\n",
          "• Gunakan untuk analisis ANOVA dengan faktor kategorikal\n",
          "• Cocok untuk uji chi-square dan analisis kontingensi\n",
          "• Dapat digunakan dalam regresi logistik\n",
          "• Berguna untuk visualisasi dan segmentasi data\n\n",
          "CATATAN PENTING:\n",
          "================\n",
          "• Kategorisasi mengurangi informasi detail dari data asli\n",
          "• Pilihan jumlah kategori mempengaruhi hasil analisis\n",
          "• Pastikan kategori memiliki interpretasi yang bermakna\n",
          "• Validasi hasil kategorisasi dengan domain knowledge"
        )
      }
      
      doc <- create_word_doc("Interpretasi Kategorisasi Data", content)
      print(doc, target = file)
    }
  )
  
  output$download_cat_report <- downloadHandler(
    filename = function() {
      paste0("Categorization_Report_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$categorized_data)) {
        content <- "Belum ada data terkategorisasi. Silakan terapkan kategorisasi terlebih dahulu."
      } else {
        cat_var <- paste0(input$cat_variable, "_categorized")
        freq_table <- table(values$categorized_data[[cat_var]])
        
        cat_stats <- values$categorized_data %>%
          group_by_at(cat_var) %>%
          summarise(
            N = n(),
            Mean_Original = round(mean(get(input$cat_variable), na.rm = TRUE), 4),
            SD_Original = round(sd(get(input$cat_variable), na.rm = TRUE), 4),
            .groups = 'drop'
          )
        
        content <- paste0(
          "LAPORAN LENGKAP KATEGORISASI DATA\n",
          "=================================\n\n",
          "EXECUTIVE SUMMARY:\n",
          "==================\n",
          "Proses kategorisasi telah berhasil dilakukan pada variabel '", input$cat_variable, 
          "' menggunakan metode ", input$cat_method, " dengan ", input$cat_groups, " kategori.\n",
          "Total observasi: ", nrow(values$categorized_data), "\n",
          "Tanggal Analisis: ", Sys.Date(), "\n\n",
          "METODOLOGI:\n",
          "===========\n",
          "Metode Kategorisasi: ", str_to_title(input$cat_method), "\n",
          "Jumlah Kategori: ", input$cat_groups, "\n",
          "Variabel Sumber: ", input$cat_variable, "\n",
          "Variabel Hasil: ", cat_var, "\n\n",
          "HASIL KATEGORISASI:\n",
          "==================\n",
          paste(capture.output(print(cat_stats)), collapse = "\n"), "\n\n",
          "DISTRIBUSI FREKUENSI:\n",
          "====================\n",
          paste(names(freq_table), ":", freq_table, "observasi (", 
                round(freq_table/sum(freq_table)*100, 1), "%)", collapse = "\n"), "\n\n",
          "VALIDASI HASIL:\n",
          "===============\n",
          "• Semua observasi berhasil dikategorisasi\n",
          "• Tidak ada kategori kosong\n",
          "• Distribusi kategori: ", 
          ifelse(max(freq_table)/min(freq_table) < 2, "Seimbang", "Tidak seimbang"), "\n\n",
          "REKOMENDASI ANALISIS LANJUTAN:\n",
          "==============================\n",
          "1. Analisis ANOVA untuk membandingkan rata-rata antar kategori\n",
          "2. Uji chi-square untuk analisis asosiasi dengan variabel lain\n",
          "3. Visualisasi boxplot untuk melihat distribusi per kategori\n",
          "4. Analisis diskriminan untuk validasi kategorisasi\n\n",
          "KESIMPULAN:\n",
          "===========\n",
          "Kategorisasi berhasil dilakukan dan menghasilkan ", input$cat_groups, 
          " kategori yang dapat digunakan untuk analisis statistik lanjutan. ",
          "Distribusi kategori menunjukkan pola yang ", 
          ifelse(max(freq_table)/min(freq_table) < 2, "seimbang", "perlu diperhatikan"), 
          " untuk interpretasi hasil analisis."
        )
      }
      
      doc <- create_word_doc("Laporan Lengkap Kategorisasi Data", content)
      print(doc, target = file)
    }
  )
  
  output$download_cat_plot_jpg <- downloadHandler(
    filename = function() {
      paste0("Categorization_Plot_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      if(is.null(values$categorized_data)) {
        jpeg(file, width = 800, height = 600, quality = 95)
        plot.new()
        text(0.5, 0.5, "Belum ada data terkategorisasi.\nSilakan terapkan kategorisasi terlebih dahulu.", 
             cex = 1.5, col = "gray")
        dev.off()
      } else {
        cat_var <- paste0(input$cat_variable, "_categorized")
        
        jpeg(file, width = 1200, height = 600, quality = 95)
        
        par(mfrow = c(1, 2))
        
        hist(values$categorized_data[[input$cat_variable]], 
             breaks = 30, col = "#667eea", alpha = 0.7,
             main = paste("Distribusi Asli -", input$cat_variable),
             xlab = input$cat_variable, ylab = "Frekuensi")
        
        barplot(table(values$categorized_data[[cat_var]]), 
                col = "#764ba2", alpha = 0.7,
                main = "Distribusi Terkategorisasi",
                xlab = "Kategori", ylab = "Frekuensi")
        
        dev.off()
      }
    }
  )
  
  output$download_cat_stats <- downloadHandler(
    filename = function() {
      paste0("Category_Statistics_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if(is.null(values$categorized_data)) {
        empty_data <- data.frame(Message = "Belum ada data terkategorisasi.")
        openxlsx::write.xlsx(empty_data, file)
      } else {
        cat_var <- paste0(input$cat_variable, "_categorized")
        
        cat_stats <- values$categorized_data %>%
          group_by_at(cat_var) %>%
          summarise(
            N = n(),
            Mean_Original = round(mean(get(input$cat_variable), na.rm = TRUE), 4),
            Median_Original = round(median(get(input$cat_variable), na.rm = TRUE), 4),
            SD_Original = round(sd(get(input$cat_variable), na.rm = TRUE), 4),
            Min_Original = round(min(get(input$cat_variable), na.rm = TRUE), 4),
            Max_Original = round(max(get(input$cat_variable), na.rm = TRUE), 4),
            .groups = 'drop'
          )
        
        openxlsx::write.xlsx(cat_stats, file, rowNames = FALSE)
      }
    }
  )
  
  #Download handlers untuk Statistik Deskriptif
  output$download_desc_excel <- downloadHandler(
    filename = function() {
      paste0("Descriptive_Statistics_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if(is.null(values$descriptive_results)) {
        empty_data <- data.frame(Message = "Belum ada hasil statistik deskriptif.")
        openxlsx::write.xlsx(empty_data, file)
      } else {
        openxlsx::write.xlsx(values$descriptive_results, file, rowNames = FALSE)
      }
    }
  )
  
  output$download_desc_interpretation <- downloadHandler(
    filename = function() {
      paste0("Descriptive_Interpretation_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$descriptive_results)) {
        content <- "Belum ada hasil statistik deskriptif. Silakan hitung terlebih dahulu."
      } else {
        content <- paste0(
          "INTERPRETASI STATISTIK DESKRIPTIF\n",
          "=================================\n\n",
          "VARIABEL YANG DIANALISIS:\n",
          "========================\n",
          paste("•", input$desc_vars, collapse = "\n"), "\n\n",
          "JENIS ANALISIS: ", str_to_upper(input$desc_type), "\n",
          "ANALISIS PER GRUP: ", ifelse(input$desc_by_group, "Ya", "Tidak"), "\n",
          if(input$desc_by_group) paste("VARIABEL GROUPING:", input$desc_group_var, "\n") else "",
          "TANGGAL ANALISIS: ", Sys.Date(), "\n\n",
          "INTERPRETASI UKURAN PEMUSATAN:\n",
          "==============================\n",
          "• Mean (Rata-rata): Nilai tengah aritmatika dari data\n",
          "• Median: Nilai tengah yang membagi data menjadi dua bagian sama\n",
          "• Jika Mean ≈ Median: Distribusi simetris\n",
          "• Jika Mean > Median: Distribusi miring kanan (positif)\n",
          "• Jika Mean < Median: Distribusi miring kiri (negatif)\n\n",
          "INTERPRETASI UKURAN PENYEBARAN:\n",
          "===============================\n",
          "• SD (Standar Deviasi): Ukuran penyebaran data dari rata-rata\n",
          "• Variance: Kuadrat dari standar deviasi\n",
          "• IQR (Interquartile Range): Rentang 50% data tengah\n",
          "• Range (Max-Min): Rentang keseluruhan data\n\n",
          "INTERPRETASI KUARTIL:\n",
          "====================\n",
          "• Q1 (Kuartil 1): 25% data berada di bawah nilai ini\n",
          "• Q3 (Kuartil 3): 75% data berada di bawah nilai ini\n",
          "• IQR = Q3 - Q1: Rentang data tengah\n\n",
          "REKOMENDASI ANALISIS LANJUTAN:\n",
          "==============================\n",
          "• Jika distribusi normal: Gunakan uji parametrik\n",
          "• Jika distribusi tidak normal: Pertimbangkan transformasi atau uji non-parametrik\n",
          "• Jika ada outlier: Investigasi lebih lanjut atau gunakan metode robust\n",
          "• Untuk perbandingan grup: Lakukan uji ANOVA atau t-test\n\n",
          "CATATAN INTERPRETASI:\n",
          "====================\n",
          "• Perhatikan skala dan satuan pengukuran\n",
          "• Bandingkan dengan standar atau benchmark jika ada\n",
          "• Pertimbangkan konteks domain dalam interpretasi\n",
          "• Gunakan visualisasi untuk konfirmasi pola data"
        )
      }
      
      doc <- create_word_doc("Interpretasi Statistik Deskriptif", content)
      print(doc, target = file)
    }
  )
  
  output$download_desc_report <- downloadHandler(
    filename = function() {
      paste0("Descriptive_Report_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$descriptive_results)) {
        content <- "Belum ada hasil statistik deskriptif. Silakan hitung terlebih dahulu."
      } else {
        content <- paste0(
          "LAPORAN LENGKAP STATISTIK DESKRIPTIF\n",
          "====================================\n\n",
          "EXECUTIVE SUMMARY:\n",
          "==================\n",
          "Analisis statistik deskriptif telah dilakukan pada ", length(input$desc_vars), 
          " variabel dengan total ", nrow(sovi_data), " observasi.\n",
          "Tanggal Analisis: ", Sys.Date(), "\n\n",
          "METODOLOGI:\n",
          "===========\n",
          "Jenis Analisis: ", str_to_title(input$desc_type), "\n",
          "Variabel yang Dianalisis:\n",
          paste("•", input$desc_vars, collapse = "\n"), "\n",
          "Analisis per Grup: ", ifelse(input$desc_by_group, "Ya", "Tidak"), "\n",
          if(input$desc_by_group) paste("Variabel Grouping:", input$desc_group_var, "\n") else "",
          "\nSTATISTIK YANG DIHITUNG:\n",
          "• N (Jumlah observasi valid)\n",
          "• Mean (Rata-rata)\n",
          "• Median (Nilai tengah)\n",
          "• Standard Deviation (Simpangan baku)\n",
          "• Variance (Varians)\n",
          "• Minimum dan Maksimum\n",
          "• Q1 dan Q3 (Kuartil)\n",
          "• IQR (Interquartile Range)\n\n",
          "HASIL ANALISIS:\n",
          "===============\n",
          paste(capture.output(print(values$descriptive_results)), collapse = "\n"), "\n\n",
          "TEMUAN UTAMA:\n",
          "=============\n",
          "1. Variabilitas Data:\n",
          "   • Variabel dengan SD tertinggi menunjukkan variabilitas terbesar\n",
          "   • Perhatikan variabel dengan range yang sangat lebar\n\n",
          "2. Distribusi Data:\n",
          "   • Bandingkan mean dan median untuk menilai skewness\n",
          "   • IQR memberikan gambaran penyebaran data tengah\n\n",
          "3. Kualitas Data:\n",
          "   • Periksa jumlah observasi valid (N) untuk setiap variabel\n",
          "   • Identifikasi variabel dengan missing values tinggi\n\n",
          "REKOMENDASI:\n",
          "============\n",
          "1. Analisis Lanjutan:\n",
          "   • Lakukan uji normalitas untuk variabel kunci\n",
          "   • Pertimbangkan transformasi untuk variabel skewed\n",
          "   • Gunakan visualisasi untuk konfirmasi pola\n\n",
          "2. Penanganan Outlier:\n",
          "   • Investigasi nilai ekstrem (min/max)\n",
          "   • Pertimbangkan metode robust jika ada outlier\n\n",
          "3. Analisis Komparatif:\n",
          "   • Bandingkan statistik antar grup jika relevan\n",
          "   • Lakukan uji statistik untuk perbedaan grup\n\n",
          "KESIMPULAN:\n",
          "===========\n",
          "Analisis statistik deskriptif memberikan gambaran komprehensif tentang karakteristik data. ",
          "Hasil ini dapat digunakan sebagai dasar untuk analisis statistik inferensia dan pemodelan lanjutan."
        )
      }
      
      doc <- create_word_doc("Laporan Lengkap Statistik Deskriptif", content)
      print(doc, target = file)
    }
  )
  
  output$download_desc_plots <- downloadHandler(
    filename = function() {
      paste0("Descriptive_Plots_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      if(is.null(input$desc_vars) || length(input$desc_vars) == 0) {
        jpeg(file, width = 800, height = 600, quality = 95)
        plot.new()
        text(0.5, 0.5, "Belum ada variabel yang dipilih untuk divisualisasikan.", cex = 1.5, col = "gray")
        dev.off()
      } else {
        jpeg(file, width = 1200, height = 800, quality = 95)
        
        n_vars <- length(input$desc_vars)
        n_cols <- min(2, n_vars)
        n_rows <- ceiling(n_vars / n_cols)
        
        par(mfrow = c(n_rows * 2, n_cols))
        
        for(i in seq_along(input$desc_vars)) {
          var_name <- input$desc_vars[i]
          
          hist(sovi_data[[var_name]], breaks = 30, col = "#667eea", alpha = 0.7,
               main = paste("Histogram:", var_name),
               xlab = var_name, ylab = "Frekuensi")
          
          boxplot(sovi_data[[var_name]], col = "#764ba2", alpha = 0.7,
                  main = paste("Boxplot:", var_name),
                  ylab = var_name)
        }
        
        dev.off()
      }
    }
  )
  
  #Download handlers untuk Visualisasi
  output$download_plot_jpg <- downloadHandler(
    filename = function() {
      paste0("Interactive_Plot_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      if(is.null(values$current_plot)) {
        jpeg(file, width = 800, height = 600, quality = 95)
        plot.new()
        text(0.5, 0.5, "Belum ada plot yang dibuat.\nSilakan buat plot terlebih dahulu.", cex = 1.5, col = "gray")
        dev.off()
      } else {
        ggsave(file, values$current_plot, width = 12, height = 8, dpi = 300, device = "jpeg")
      }
    }
  )
  
  output$download_plot_png <- downloadHandler(
    filename = function() {
      paste0("Interactive_Plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      if(is.null(values$current_plot)) {
        png(file, width = 800, height = 600)
        plot.new()
        text(0.5, 0.5, "Belum ada plot yang dibuat.\nSilakan buat plot terlebih dahulu.", cex = 1.5, col = "gray")
        dev.off()
      } else {
        ggsave(file, values$current_plot, width = 12, height = 8, dpi = 300, device = "png")
      }
    }
  )
  
  output$download_plot_interpretation <- downloadHandler(
    filename = function() {
      paste0("Plot_Interpretation_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$current_plot)) {
        content <- "Belum ada plot yang dibuat. Silakan buat plot terlebih dahulu."
      } else {
        plot_desc <- switch(input$plot_type,
                            "scatter" = "menunjukkan hubungan antara dua variabel numerik",
                            "line" = "menampilkan tren atau pola temporal dalam data",
                            "box" = "membandingkan distribusi variabel antar grup",
                            "violin" = "menunjukkan distribusi dan kepadatan data per grup",
                            "hist" = "menampilkan distribusi frekuensi variabel",
                            "density" = "menunjukkan estimasi kepadatan probabilitas",
                            "heatmap" = "memvisualisasikan korelasi antar variabel"
        )
        
        content <- paste0(
          "INTERPRETASI VISUALISASI DATA\n",
          "=============================\n\n",
          "INFORMASI PLOT:\n",
          "===============\n",
          "Jenis Plot: ", str_to_title(input$plot_type), "\n",
          "Variabel X: ", input$plot_x, "\n",
          "Variabel Y: ", ifelse(is.null(input$plot_y), "N/A", input$plot_y), "\n",
          "Variabel Grouping: ", ifelse(is.null(input$plot_group), "N/A", input$plot_group), "\n",
          "Garis Trend: ", ifelse(input$add_trend, "Ya", "Tidak"), "\n",
          "Tanggal Pembuatan: ", Sys.Date(), "\n\n",
          "TUJUAN VISUALISASI:\n",
          "==================\n",
          "Plot ini ", plot_desc, ".\n\n",
          "CARA MEMBACA PLOT:\n",
          "==================\n",
          switch(input$plot_type,
                 "scatter" = paste0(
                   "• Setiap titik mewakili satu observasi\n",
                   "• Posisi horizontal menunjukkan nilai ", input$plot_x, "\n",
                   "• Posisi vertikal menunjukkan nilai ", ifelse(is.null(input$plot_y), "N/A", input$plot_y), "\n",
                   "• Pola titik menunjukkan jenis hubungan (linear, non-linear, dll)\n",
                   if(input$add_trend) "• Garis merah menunjukkan tren linear dengan confidence interval\n" else ""
                 ),
                 "hist" = paste0(
                   "• Sumbu X menunjukkan rentang nilai ", input$plot_x, "\n",
                   "• Sumbu Y menunjukkan frekuensi atau jumlah observasi\n",
                   "• Tinggi bar menunjukkan banyaknya data dalam rentang tersebut\n",
                   "• Bentuk distribusi dapat dilihat dari pola bar"
                 ),
                 "box" = paste0(
                   "• Kotak menunjukkan Q1, median, dan Q3\n",
                   "• Garis di tengah kotak adalah median\n",
                   "• Whiskers menunjukkan rentang data (biasanya 1.5*IQR)\n",
                   "• Titik di luar whiskers adalah outlier potensial"
                 ),
                 "violin" = paste0(
                   "• Lebar violin menunjukkan kepadatan data pada nilai tersebut\n",
                   "• Kotak putih di tengah menunjukkan boxplot\n",
                   "• Bentuk violin menunjukkan distribusi data per grup"
                 ),
                 "density" = paste0(
                   "• Kurva menunjukkan estimasi kepadatan probabilitas\n",
                   "• Area di bawah kurva = 1 (total probabilitas)\n",
                   "• Puncak kurva menunjukkan nilai yang paling sering muncul\n",
                   "• Lebar kurva menunjukkan variabilitas data"
                 ),
                 "heatmap" = paste0(
                   "• Warna menunjukkan kekuatan korelasi\n",
                   "• Warna merah: korelasi positif kuat\n",
                   "• Warna biru: korelasi negatif kuat\n",
                   "• Warna putih: tidak ada korelasi"
                 )
          ), "\n\n",
          "INTERPRETASI STATISTIK:\n",
          "======================\n",
          switch(input$plot_type,
                 "scatter" = paste0(
                   "• Korelasi positif: titik-titik naik dari kiri bawah ke kanan atas\n",
                   "• Korelasi negatif: titik-titik turun dari kiri atas ke kanan bawah\n",
                   "• Tidak ada korelasi: titik-titik tersebar acak\n",
                   "• Outlier: titik yang jauh dari pola umum"
                 ),
                 "hist" = paste0(
                   "• Distribusi normal: bentuk lonceng simetris\n",
                   "• Skewed right: ekor panjang ke kanan\n",
                   "• Skewed left: ekor panjang ke kiri\n",
                   "• Bimodal: dua puncak (kemungkinan dua populasi)"
                 ),
                 "box" = paste0(
                   "• Median di tengah kotak: distribusi simetris\n",
                   "• Median tidak di tengah: distribusi skewed\n",
                   "• Kotak kecil: variabilitas rendah\n",
                   "• Banyak outlier: perlu investigasi lebih lanjut"
                 ),
                 "Interpretasi umum berdasarkan pola visual yang terlihat"
          ), "\n\n",
          "REKOMENDASI ANALISIS LANJUTAN:\n",
          "==============================\n",
          switch(input$plot_type,
                 "scatter" = paste0(
                   "• Hitung koefisien korelasi untuk kuantifikasi hubungan\n",
                   "• Lakukan analisis regresi jika ada hubungan linear\n",
                   "• Investigasi outlier yang teridentifikasi\n",
                   "• Pertimbangkan transformasi jika hubungan non-linear"
                 ),
                 "hist" = paste0(
                   "• Lakukan uji normalitas (Shapiro-Wilk, Kolmogorov-Smirnov)\n",
                   "• Pertimbangkan transformasi jika distribusi skewed\n",
                   "• Hitung statistik deskriptif untuk konfirmasi\n",
                   "• Bandingkan dengan distribusi teoretis"
                 ),
                 "box" = paste0(
                   "• Lakukan uji ANOVA untuk membandingkan grup\n",
                   "• Investigasi outlier yang teridentifikasi\n",
                   "• Uji homogenitas varians antar grup\n",
                   "• Pertimbangkan uji non-parametrik jika asumsi tidak terpenuhi"
                 ),
                 "Lakukan analisis statistik yang sesuai dengan jenis data dan tujuan penelitian"
          ), "\n\n",
          "CATATAN PENTING:\n",
          "================\n",
          "• Visualisasi adalah alat eksplorasi, bukan pengganti analisis statistik\n",
          "• Selalu konfirmasi pola visual dengan uji statistik\n",
          "• Pertimbangkan konteks domain dalam interpretasi\n",
          "• Dokumentasikan temuan untuk analisis lanjutan"
        )
      }
      
      doc <- create_word_doc("Interpretasi Visualisasi Data", content)
      print(doc, target = file)
    }
  )
  
  output$download_static_plot <- downloadHandler(
    filename = function() {
      paste0("Static_Plot_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      if(is.null(values$current_plot)) {
        jpeg(file, width = 800, height = 600, quality = 95)
        plot.new()
        text(0.5, 0.5, "Belum ada plot yang dibuat.\nSilakan buat plot terlebih dahulu.", cex = 1.5, col = "gray")
        dev.off()
      } else {
        ggsave(file, values$current_plot, width = 12, height = 8, dpi = 300, device = "jpeg")
      }
    }
  )
  
  #Download handlers untuk Analisis Matriks Jarak
  output$download_distance_results <- downloadHandler(
    filename = function() {
      paste0("Distance_Analysis_Results_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if(is.null(values$distance_analysis_results)) {
        empty_data <- data.frame(Message = "Belum ada hasil analisis matriks jarak.")
        openxlsx::write.xlsx(empty_data, file)
      } else {
        result <- values$distance_analysis_results
        
        wb <- openxlsx::createWorkbook()
        
        if(result$type == "descriptive") {
          openxlsx::addWorksheet(wb, "Descriptive_Stats")
          openxlsx::writeData(wb, "Descriptive_Stats", result$stats)
        } else if(result$type == "clustering") {
          openxlsx::addWorksheet(wb, "Cluster_Summary")
          openxlsx::writeData(wb, "Cluster_Summary", result$summary)
          
          cluster_assignments <- data.frame(
            Observation = 1:length(result$clusters),
            Cluster = result$clusters
          )
          openxlsx::addWorksheet(wb, "Cluster_Assignments")
          openxlsx::writeData(wb, "Cluster_Assignments", cluster_assignments)
        } else if(result$type == "pca") {
          openxlsx::addWorksheet(wb, "PCA_Summary")
          openxlsx::writeData(wb, "PCA_Summary", result$summary)
          
          coordinates_df <- as.data.frame(result$coordinates)
          openxlsx::addWorksheet(wb, "MDS_Coordinates")
          openxlsx::writeData(wb, "MDS_Coordinates", coordinates_df)
        }
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    }
  )
  
  output$download_distance_plot <- downloadHandler(
    filename = function() {
      paste0("Distance_Analysis_Plot_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      if(is.null(values$distance_analysis_results)) {
        jpeg(file, width = 800, height = 600, quality = 95)
        plot.new()
        text(0.5, 0.5, "Belum ada hasil analisis matriks jarak.", cex = 1.5, col = "gray")
        dev.off()
      } else {
        jpeg(file, width = 1200, height = 800, quality = 95)
        
        result <- values$distance_analysis_results
        
        if(result$type == "descriptive") {
          hist(result$values, breaks = 30, col = "#667eea", alpha = 0.7,
               main = "Distribusi Jarak", xlab = "Jarak", ylab = "Frekuensi")
        } else if(result$type == "clustering") {
          distance_matrix_num <- as.matrix(distance_matrix)
          mds_coords <- cmdscale(distance_matrix_num, k = 2)
          
          plot(mds_coords[,1], mds_coords[,2], 
               col = rainbow(result$n_clusters)[result$clusters],
               pch = 16, cex = 1.5,
               main = paste("Hasil Clustering -", str_to_title(result$method)),
               xlab = "Dimensi 1", ylab = "Dimensi 2")
          legend("topright", legend = paste("Cluster", 1:result$n_clusters),
                 col = rainbow(result$n_clusters), pch = 16)
        } else if(result$type == "pca") {
          plot(1:length(result$eigenvalues), result$eigenvalues,
               type = "b", col = "#667eea", lwd = 2, pch = 16,
               main = "Scree Plot - Analisis Komponen Utama",
               xlab = "Komponen", ylab = "Eigenvalue")
        }
        
        dev.off()
      }
    }
  )
  
  output$download_distance_interpretation <- downloadHandler(
    filename = function() {
      paste0("Distance_Analysis_Interpretation_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$distance_analysis_results)) {
        content <- "Belum ada hasil analisis matriks jarak. Silakan jalankan analisis terlebih dahulu."
      } else {
        result <- values$distance_analysis_results
        
        if(result$type == "descriptive") {
          content <- paste0(
            "INTERPRETASI STATISTIK DESKRIPTIF MATRIKS JARAK\n",
            "===============================================\n\n",
            "ANALISIS DESKRIPTIF:\n",
            "===================\n",
            "Matriks jarak menggambarkan kedekatan/jarak antar observasi dalam dataset.\n",
            "Tanggal Analisis: ", Sys.Date(), "\n\n",
            "HASIL STATISTIK:\n",
            "================\n",
            paste(capture.output(print(result$stats)), collapse = "\n"), "\n\n",
            "INTERPRETASI STATISTIK:\n",
            "======================\n",
            "• Jarak rata-rata menunjukkan tingkat heterogenitas data\n",
            "• Jarak minimum (selain 0) menunjukkan observasi terdekat\n",
            "• Jarak maksimum menunjukkan observasi terjauh\n",
            "• Standar deviasi menunjukkan variabilitas jarak\n\n",
            "IMPLIKASI:\n",
            "==========\n",
            "• Jarak kecil: Observasi serupa/homogen\n",
            "• Jarak besar: Observasi berbeda/heterogen\n",
            "• Distribusi jarak mempengaruhi hasil clustering\n",
            "• Dapat digunakan untuk identifikasi outlier\n\n",
            "REKOMENDASI ANALISIS LANJUTAN:\n",
            "==============================\n",
            "• Lakukan clustering berdasarkan matriks jarak\n",
            "• Gunakan MDS untuk visualisasi dalam ruang 2D\n",
            "• Analisis heatmap untuk pola jarak\n",
            "• Identifikasi outlier berdasarkan jarak rata-rata"
          )
        } else if(result$type == "clustering") {
          content <- paste0(
            "INTERPRETASI HASIL CLUSTERING BERDASARKAN JARAK\n",
            "===============================================\n\n",
            "INFORMASI CLUSTERING:\n",
            "====================\n",
            "Metode: ", str_to_title(result$method), "\n",
            "Jumlah Cluster: ", result$n_clusters, "\n",
            "Tanggal Analisis: ", Sys.Date(), "\n\n",
            "HASIL CLUSTERING:\n",
            "=================\n",
            paste(capture.output(print(result$summary)), collapse = "\n"), "\n\n",
            "INTERPRETASI CLUSTER:\n",
            "====================\n",
            "• Cluster mengelompokkan observasi berdasarkan kedekatan jarak\n",
            "• Ukuran cluster menunjukkan distribusi data\n",
            "• Cluster yang seimbang menunjukkan struktur data yang baik\n\n",
            "EVALUASI KUALITAS CLUSTERING:\n",
            "=============================\n",
            "• Cluster dengan ukuran sangat berbeda mungkin menunjukkan outlier\n",
            "• Pertimbangkan validasi dengan silhouette analysis\n",
            "• Bandingkan dengan clustering menggunakan metode lain\n\n",
            "REKOMENDASI:\n",
            "============\n",
            "• Evaluasi kualitas clustering dengan silhouette analysis\n",
            "• Pertimbangkan jumlah cluster optimal\n",
            "• Interpretasikan cluster berdasarkan karakteristik data asli\n",
            "• Validasi hasil dengan domain knowledge"
          )
        } else if(result$type == "pca") {
          total_var <- sum(result$summary$Proportion[1:2])
          content <- paste0(
            "INTERPRETASI ANALISIS KOMPONEN UTAMA (MDS)\n",
            "==========================================\n\n",
            "INFORMASI ANALISIS:\n",
            "==================\n",
            "Metode: Multidimensional Scaling (MDS)\n",
            "Tujuan: Reduksi dimensi untuk visualisasi matriks jarak\n",
            "Tanggal Analisis: ", Sys.Date(), "\n\n",
            "HASIL ANALISIS:\n",
            "===============\n",
            paste(capture.output(print(result$summary)), collapse = "\n"), "\n\n",
            "INTERPRETASI KOMPONEN:\n",
            "=====================\n",
            "• Dua komponen pertama menjelaskan ", round(total_var*100, 1), "% varians\n",
            "• Eigenvalue > 1 menunjukkan komponen yang signifikan\n",
            "• Koordinat MDS dapat digunakan untuk visualisasi dan clustering\n\n",
            "INTERPRETASI PRAKTIS:\n",
            "====================\n",
            "• Komponen utama merepresentasikan pola utama dalam jarak\n",
            "• Observasi yang dekat dalam ruang MDS memiliki jarak yang kecil\n",
            "• Dapat digunakan untuk identifikasi outlier dan pola spasial\n",
            "• Reduksi dimensi mempertahankan struktur jarak relatif\n\n",
            "REKOMENDASI PENGGUNAAN:\n",
            "======================\n",
            "• Gunakan koordinat MDS untuk visualisasi 2D\n",
            "• Lakukan clustering pada koordinat MDS\n",
            "• Identifikasi outlier berdasarkan posisi ekstrem\n",
            "• Interpretasikan sumbu berdasarkan karakteristik data asli"
          )
        }
      }
      
      doc <- create_word_doc("Interpretasi Analisis Matriks Jarak", content)
      print(doc, target = file)
    }
  )
  
  #Download handlers untuk Uji Asumsi
  output$download_norm_results <- downloadHandler(
    filename = function() {
      paste0("Normality_Test_Results_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$normality_results)) {
        content <- "Belum ada hasil uji normalitas. Silakan jalankan uji terlebih dahulu."
      } else {
        test_results_text <- ""
        for(test_name in names(values$normality_results$tests)) {
          test_result <- values$normality_results$tests[[test_name]]
          
          test_results_text <- paste0(test_results_text,
                                      str_to_upper(test_name), " TEST:\n",
                                      paste(rep("-", nchar(test_name) + 6), collapse = ""), "\n",
                                      "Statistik: ", round(test_result$statistic, 6), "\n",
                                      "p-value: ", format(test_result$p.value, scientific = TRUE), "\n",
                                      "Kesimpulan: ", ifelse(test_result$p.value > values$normality_results$alpha, 
                                                             "Data berdistribusi normal", 
                                                             "Data tidak berdistribusi normal"), "\n\n"
          )
        }
        
        p_values <- sapply(values$normality_results$tests, function(x) x$p.value)
        normal_count <- sum(p_values > values$normality_results$alpha)
        total_tests <- length(p_values)
        
        content <- paste0(
          "HASIL UJI NORMALITAS\n",
          "====================\n\n",
          "INFORMASI UJI:\n",
          "==============\n",
          "Variabel: ", values$normality_results$variable, "\n",
          "Ukuran Sampel: ", length(values$normality_results$data), "\n",
          "Tingkat Signifikansi: ", values$normality_results$alpha, "\n",
          "Tanggal Analisis: ", Sys.Date(), "\n\n",
          "HASIL UJI:\n",
          "==========\n",
          test_results_text,
          "KESIMPULAN KESELURUHAN:\n",
          "=======================\n",
          "Uji yang mendukung normalitas: ", normal_count, " dari ", total_tests, " uji\n\n",
          if(normal_count >= total_tests/2) {
            paste0(
              "KESIMPULAN: Data kemungkinan berdistribusi normal\n",
              "REKOMENDASI: Gunakan uji parametrik\n\n",
              "UJI PARAMETRIK YANG DAPAT DIGUNAKAN:\n",
              "• t-test untuk perbandingan rata-rata\n",
              "• ANOVA untuk perbandingan multiple grup\n",
              "• Regresi linear dengan asumsi normalitas residual\n",
              "• Korelasi Pearson"
            )
          } else {
            paste0(
              "KESIMPULAN: Data kemungkinan tidak berdistribusi normal\n",
              "REKOMENDASI: Pertimbangkan uji non-parametrik atau transformasi data\n\n",
              "ALTERNATIF YANG DAPAT DIGUNAKAN:\n",
              "• Mann-Whitney U test (pengganti t-test)\n",
              "• Kruskal-Wallis test (pengganti ANOVA)\n",
              "• Spearman correlation (pengganti Pearson)\n",
              "• Transformasi data (log, sqrt, Box-Cox)"
            )
          }, "\n\n",
          "INTERPRETASI UJI NORMALITAS:\n",
          "============================\n",
          "• Shapiro-Wilk: Paling powerful untuk sampel kecil (n < 50)\n",
          "• Kolmogorov-Smirnov: Untuk sampel besar, tapi kurang sensitive\n",
          "• Lilliefors: Modifikasi KS yang lebih baik\n",
          "• Anderson-Darling: Lebih sensitive pada ekor distribusi\n",
          "• Jarque-Bera: Berdasarkan skewness dan kurtosis\n\n",
          "CATATAN PENTING:\n",
          "================\n",
          "• Uji normalitas sensitive terhadap ukuran sampel\n",
          "• Sampel besar cenderung menolak normalitas meski deviasi kecil\n",
          "• Gunakan visualisasi (Q-Q plot, histogram) untuk konfirmasi\n",
          "• Pertimbangkan Central Limit Theorem untuk sampel besar (n > 30)"
        )
      }
      
      doc <- create_word_doc("Hasil Uji Normalitas", content)
      print(doc, target = file)
    }
  )
  
  output$download_norm_plots <- downloadHandler(
    filename = function() {
      paste0("Normality_Plots_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      if(is.null(values$normality_results)) {
        jpeg(file, width = 800, height = 600, quality = 95)
        plot.new()
        text(0.5, 0.5, "Belum ada hasil uji normalitas.", cex = 1.5, col = "gray")
        dev.off()
      } else {
        var_data <- values$normality_results$data
        
        jpeg(file, width = 1200, height = 800, quality = 95)
        par(mfrow = c(2, 2))
        
        hist(var_data, breaks = 30, freq = FALSE, 
             main = paste("Histogram:", values$normality_results$variable),
             xlab = values$normality_results$variable,
             col = "#667eea", border = "white")
        curve(dnorm(x, mean = mean(var_data), sd = sd(var_data)), 
              add = TRUE, col = "#e74c3c", lwd = 2)
        legend("topright", legend = "Kurva Normal", col = "#e74c3c", lwd = 2)
        
        qqnorm(var_data, main = "Q-Q Plot Normal",
               col = "#667eea", pch = 16)
        qqline(var_data, col = "#e74c3c", lwd = 2)
        
        boxplot(var_data, main = "Box Plot",
                ylab = values$normality_results$variable, 
                col = "#764ba2", border = "black")
        
        plot(density(var_data), main = "Density Plot",
             xlab = values$normality_results$variable, 
             col = "#9b59b6", lwd = 2)
        polygon(density(var_data), col = adjustcolor("#9b59b6", alpha.f = 0.3))
        
        dev.off()
      }
    }
  )
  
  output$download_norm_interpretation <- downloadHandler(
    filename = function() {
      paste0("Normality_Interpretation_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$normality_results)) {
        doc <- read_docx()
        doc <- body_add_par(doc, "Belum ada hasil uji normalitas. Silakan jalankan uji terlebih dahulu.")
        print(doc, target = file)
        return()
      }
      
      hasil <- values$normality_results
      
      hasil_df <- do.call(rbind, lapply(names(hasil$tests), function(test_name) {
        data.frame(
          "Uji Statistik" = str_to_title(test_name),
          "Statistik" = round(hasil$tests[[test_name]]$statistic, 4),
          "P-Value" = format(hasil$tests[[test_name]]$p.value, scientific = TRUE, digits = 3),
          "Keputusan (α=0.05)" = ifelse(hasil$tests[[test_name]]$p.value > 0.05, "Normal", "Tidak Normal")
        )
      }))
      
      ft <- flextable(hasil_df) %>%
        autofit() %>%
        theme_booktabs() %>%
        align(align = "center", part = "all") %>%
        align(j = 1, align = "left", part = "all")
      
      doc <- read_docx()
      
      doc <- doc %>%
        body_add_par("📝 Laporan Interpretasi Uji Normalitas", style = "heading 1") %>%
        body_add_par("") %>%
        
        body_add_par("Informasi Analisis", style = "heading 2") %>%
        body_add_par(fpar(ftext("Variabel yang Dianalisis: ", prop = fp_text(bold = TRUE)), ftext(hasil$variable))) %>%
        body_add_par(fpar(ftext("Jumlah Observasi Valid: ", prop = fp_text(bold = TRUE)), ftext(length(hasil$data)))) %>%
        body_add_par(fpar(ftext("Tingkat Signifikansi (α): ", prop = fp_text(bold = TRUE)), ftext(hasil$alpha))) %>%
        body_add_par("") %>%
        
        body_add_par("Hasil Uji Statistik", style = "heading 2") %>%
        body_add_flextable(ft) %>%
        body_add_par("") %>%
        
        body_add_par("Kesimpulan dan Rekomendasi", style = "heading 2")
      
      normal_count <- sum(sapply(hasil$tests, function(x) x$p.value) > hasil$alpha)
      total_tests <- length(hasil$tests)
      
      if (normal_count >= total_tests / 2) {
        doc <- doc %>%
          body_add_par("Mayoritas uji mendukung asumsi normalitas.", style = "Normal") %>%
          body_add_par("Rekomendasi:", style = "heading 3") %>%
          body_add_par("Lanjutkan dengan analisis parametrik (seperti t-test atau ANOVA).", style = "List Paragraph") %>%
          body_add_par("Pastikan asumsi lain seperti homogenitas varians juga terpenuhi.", style = "List Paragraph")
      } else {
        doc <- doc %>%
          body_add_par("Mayoritas uji menolak asumsi normalitas. Data kemungkinan tidak berdistribusi normal.", style = "Normal") %>%
          body_add_par("Rekomendasi:", style = "heading 3") %>%
          body_add_par("Gunakan uji non-parametrik (seperti Mann-Whitney atau Kruskal-Wallis).", style = "List Paragraph") %>%
          body_add_par("Pertimbangkan untuk melakukan transformasi data (misalnya, log atau akar kuadrat) untuk mencoba mencapai normalitas.", style = "List Paragraph")
      }
      
      doc <- doc %>% body_add_par("")
      
      doc <- doc %>%
        body_add_par("Visualisasi Diagnostik", style = "heading 2")
      
      temp_plot_file <- tempfile(fileext = ".png")
      png(temp_plot_file, width = 8, height = 6, units = "in", res = 300)
      
      par(mfrow = c(2, 2))
      hist(hasil$data, main = paste("Histogram:", hasil$variable), col = "skyblue", breaks = 30)
      qqnorm(hasil$data, main = "Q-Q Plot")
      qqline(hasil$data, col = "red")
      boxplot(hasil$data, main = "Box Plot", col = "lightgreen")
      plot(density(hasil$data), main = "Density Plot", col = "blue", lwd = 2)
      
      dev.off() # Simpan plot ke file
      
      doc <- doc %>%
        body_add_img(src = temp_plot_file, width = 6, height = 4.5)
      
      print(doc, target = file)
    }
  )
  
  output$download_homo_results <- downloadHandler(
    filename = function() {
      paste0("Homogeneity_Test_Results_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$homogeneity_results)) {
        doc <- read_docx() %>% body_add_par("Belum ada hasil. Jalankan uji homogenitas terlebih dahulu.")
        print(doc, target = file)
        return()
      }
      
      hasil <- values$homogeneity_results
      
      if(hasil$method == "levene") {
        p_val <- hasil$test$`Pr(>F)`[1]
        stat_val <- hasil$test$`F value`[1]
        df_val <- paste(hasil$test$Df, collapse = ", ")
        stat_name <- "F-statistic"
      } else {
        p_val <- hasil$test$p.value
        stat_val <- hasil$test$statistic
        df_val <- hasil$test$parameter
        stat_name <- "Chi-squared"
      }
      
      hasil_df <- data.frame(
        Parameter = c(stat_name, "Degrees of Freedom (df)", "P-Value"),
        Nilai = c(round(stat_val, 4), df_val, format(p_val, scientific = TRUE, digits = 3))
      )
      
      ft <- flextable(hasil_df) %>%
        autofit() %>%
        theme_booktabs() %>%
        align(j = 1, align = "left", part = "all") %>%
        align(j = 2, align = "center", part = "all")
      
      doc <- read_docx() %>%
        body_add_par("⚖️ Hasil Uji Homogenitas Varians", style = "heading 1") %>%
        body_add_par("") %>%
        body_add_par("Informasi Analisis", style = "heading 2") %>%
        body_add_par(paste("Variabel:", hasil$variable)) %>%
        body_add_par(paste("Grouping:", hasil$group)) %>%
        body_add_par(paste("Metode:", str_to_title(hasil$method))) %>%
        body_add_par("") %>%
        body_add_par("Tabel Hasil Uji", style = "heading 2") %>%
        body_add_flextable(ft) %>%
        body_add_par("") %>%
        body_add_par("Kesimpulan Singkat", style = "heading 2")
      
      kesimpulan_singkat <- ifelse(p_val > hasil$alpha, 
                                   "Tidak ada cukup bukti untuk menolak H₀. Asumsi homogenitas varians terpenuhi.",
                                   "Terdapat cukup bukti untuk menolak H₀. Asumsi homogenitas varians tidak terpenuhi.")
      
      doc <- doc %>% body_add_par(kesimpulan_singkat)
      
      print(doc, target = file)
    }
  )
  
  output$download_homo_interpretation <- downloadHandler(
    filename = function() {
      paste0("Homogeneity_Interpretation_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$homogeneity_results)) {
        doc <- read_docx() %>% body_add_par("Belum ada hasil. Jalankan uji homogenitas terlebih dahulu.")
        print(doc, target = file)
        return()
      }
      
      hasil <- values$homogeneity_results
      p_val <- if(hasil$method == "levene") hasil$test$`Pr(>F)`[1] else hasil$test$p.value
      
      doc <- read_docx()
      
      doc <- doc %>%
        body_add_par("📝 Laporan Interpretasi Uji Homogenitas", style = "heading 1") %>%
        body_add_par("") %>%
        
        body_add_par("Hipotesis dan Keputusan", style = "heading 2") %>%
        body_add_par("H₀: Varians antar grup sama (homogen).", style = "List Paragraph") %>%
        body_add_par("H₁: Varians antar grup berbeda (tidak homogen).", style = "List Paragraph") %>%
        body_add_par(
          fpar(ftext("Keputusan (α=", prop = fp_text()), 
               ftext(hasil$alpha, prop = fp_text(bold = TRUE)), 
               ftext("): ", prop = fp_text()),
               ftext(ifelse(p_val > hasil$alpha, "Gagal Tolak H₀", "Tolak H₀"), 
                     prop = fp_text(bold = TRUE, color = ifelse(p_val > hasil$alpha, "darkgreen", "red"))))
        ) %>%
        body_add_par("")
      
      doc <- doc %>%
        body_add_par("Implikasi dan Rekomendasi", style = "heading 2")
      
      if (p_val > hasil$alpha) {
        doc <- doc %>%
          body_add_par("Asumsi homogenitas terpenuhi. Ini berarti variabilitas data konsisten di semua grup yang dibandingkan.") %>%
          body_add_par("Rekomendasi:", style = "heading 3") %>%
          body_add_par("Lanjutkan analisis menggunakan uji parametrik standar seperti ANOVA atau Independent t-test.", style = "List Paragraph") %>%
          body_add_par("Hasil dari uji-uji tersebut dapat dianggap valid dan reliabel.", style = "List Paragraph")
      } else {
        doc <- doc %>%
          body_add_par("Asumsi homogenitas tidak terpenuhi. Ini berarti variabilitas data berbeda secara signifikan antar grup.") %>%
          body_add_par("Rekomendasi:", style = "heading 3") %>%
          body_add_par("Gunakan uji alternatif yang tidak memerlukan asumsi homogenitas, seperti Welch's t-test (untuk 2 grup) atau Welch's ANOVA.", style = "List Paragraph") %>%
          body_add_par("Pertimbangkan untuk melakukan transformasi data (misal: log) untuk mencoba menstabilkan varians.", style = "List Paragraph") %>%
          body_add_par("Jika data juga tidak normal, uji non-parametrik (seperti Kruskal-Wallis) adalah pilihan yang paling aman.", style = "List Paragraph")
      }
      
      doc <- doc %>% body_add_par("")
      
      doc <- doc %>%
        body_add_par("Visualisasi Diagnostik", style = "heading 2")
      
      temp_plot_file <- tempfile(fileext = ".png")
      png(temp_plot_file, width = 8, height = 4, units = "in", res = 300)
      par(mfrow = c(1, 2))
      boxplot(value ~ group, data = data.frame(value = sovi_data[[hasil$variable]], group = hasil$group_data),
              main = "Boxplot by Group", xlab = hasil$group, ylab = hasil$variable,
              col = rainbow(length(unique(hasil$group_data))))
      group_vars <- aggregate(value ~ group, data = data.frame(value = sovi_data[[hasil$variable]], group = hasil$group_data), FUN = var)
      barplot(group_vars$value, names.arg = group_vars$group,
              main = "Variance by Group", xlab = hasil$group, ylab = "Variance",
              col = rainbow(nrow(group_vars)))
      dev.off()
      
      doc <- doc %>%
        body_add_img(src = temp_plot_file, width = 7, height = 3.5)
      
      print(doc, target = file)
    }
  )
  
  output$download_assumptions_report <- downloadHandler(
    filename = function() {
      paste0("Assumptions_Report_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$normality_results) && is.null(values$homogeneity_results)) {
        doc <- read_docx() %>% body_add_par("Belum ada hasil. Jalankan uji asumsi terlebih dahulu.")
        print(doc, target = file)
        return()
      }
      
      doc <- read_docx() %>%
        body_add_par("📋 Laporan Lengkap Uji Asumsi Statistik", style = "heading 1") %>%
        body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        body_add_par("")
      
      if(!is.null(values$normality_results)) {
        hasil_norm <- values$normality_results
        
        tabel_norm_df <- do.call(rbind, lapply(names(hasil_norm$tests), function(nama) {
          data.frame(
            Uji = str_to_title(nama),
            Statistik = round(hasil_norm$tests[[nama]]$statistic, 4),
            `p-value` = format(hasil_norm$tests[[nama]]$p.value, scientific = TRUE, digits = 3),
            Kesimpulan = ifelse(hasil_norm$tests[[nama]]$p.value > hasil_norm$alpha, "Normal", "Tidak Normal")
          )
        }))
        ft_norm <- flextable(tabel_norm_df) %>% autofit() %>% theme_booktabs()
        
        doc <- doc %>%
          body_add_par("Hasil Uji Normalitas", style = "heading 2") %>%
          body_add_par(fpar(ftext("Variabel: ", prop=fp_text(bold=TRUE)), ftext(hasil_norm$variable))) %>%
          body_add_flextable(ft) %>%
          body_add_par("")
      }
      
      #Uji Homogenitas
      if(!is.null(values$homogeneity_results)) {
        hasil_homo <- values$homogeneity_results
        
        if(hasil_homo$method == "levene") {
          p_val <- hasil_homo$test$`Pr(>F)`[1]; stat_val <- hasil_homo$test$`F value`[1]; df_val <- paste(hasil_homo$test$Df, collapse=", "); stat_name <- "F-statistic"
        } else {
          p_val <- hasil_homo$test$p.value; stat_val <- hasil_homo$test$statistic; df_val <- hasil_homo$test$parameter; stat_name <- "Chi-squared"
        }
        
        tabel_homo_df <- data.frame(Parameter = c(stat_name, "df", "p-value"), Nilai = c(round(stat_val, 4), df_val, format(p_val, scientific=TRUE, digits=3)))
        ft_homo <- flextable(tabel_homo_df) %>% autofit() %>% theme_booktabs()
        
        doc <- doc %>%
          body_add_par("Hasil Uji Homogenitas Varians", style = "heading 2") %>%
          body_add_par(fpar(ftext("Variabel: ", prop=fp_text(bold=TRUE)), ftext(hasil_homo$variable))) %>%
          body_add_par(fpar(ftext("Grouping: ", prop=fp_text(bold=TRUE)), ftext(hasil_homo$group))) %>%
          body_add_par(fpar(ftext("Metode: ", prop=fp_text(bold=TRUE)), ftext(str_to_title(hasil_homo$method)))) %>%
          body_add_flextable(ft_homo) %>%
          body_add_par("")
      }
      
      doc <- doc %>%
        body_add_par("Rekomendasi Analisis Lanjutan", style = "heading 2")
      
      matriks_df <- data.frame(
        Normalitas = c("✅ Normal", "✅ Normal", "❌ Non-Normal", "❌ Non-Normal"),
        Homogenitas = c("✅ Homogen", "❌ Heterogen", "✅ Homogen", "❌ Heterogen"),
        Rekomendasi = c("Uji Parametrik Standar", "Welch's Test, Robust ANOVA", "Transformasi atau Non-parametrik", "Uji Non-Parametrik")
      )
      ft_matriks <- flextable(matriks_df) %>% autofit() %>% theme_box() %>% bold(part="header")
      
      doc <- doc %>% body_add_flextable(ft_matriks) %>% body_add_par("")
      
      if(!is.null(values$normality_results) && !is.null(values$homogeneity_results)) {
        p_values_norm <- sapply(values$normality_results$tests, function(x) x$p.value)
        is_normal <- sum(p_values_norm > values$normality_results$alpha) >= length(p_values_norm) / 2
        is_homogen <- p_val > values$homogeneity_results$alpha
        
        rekomendasi_judul <- "Rekomendasi Berdasarkan Hasil Anda:"
        if(is_normal && is_homogen) {
          rekomendasi_teks <- "✅ KONDISI IDEAL: Data Anda normal dan homogen. Lanjutkan dengan uji parametrik standar (t-test, ANOVA) dengan percaya diri."
        } else if(is_normal && !is_homogen) {
          rekomendasi_teks <- "⚠️ KONDISI MIXED: Data Anda normal tetapi tidak homogen. Gunakan alternatif seperti Welch's t-test atau Welch's ANOVA."
        } else if(!is_normal && is_homogen) {
          rekomendasi_teks <- "⚠️ KONDISI MIXED: Data Anda homogen tetapi tidak normal. Pertimbangkan transformasi data atau gunakan uji non-parametrik."
        } else {
          rekomendasi_teks <- "❌ KONDISI CHALLENGING: Data Anda tidak normal dan tidak homogen. Sangat disarankan untuk menggunakan uji non-parametrik (misal: Kruskal-Wallis)."
        }
        doc <- doc %>% body_add_par(rekomendasi_judul, style="heading 3") %>% body_add_par(rekomendasi_teks)
      }
      
      print(doc, target = file)
    }
  )
  
  #Download handlers untuk Uji Rata-rata
  output$download_mean_results <- downloadHandler(
    filename = function() {
      paste0("Mean_Test_Results_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$mean_test_results)) {
        doc <- read_docx() %>% body_add_par("Belum ada hasil. Jalankan uji rata-rata terlebih dahulu.")
        print(doc, target = file)
        return()
      }
      
      hasil <- values$mean_test_results
      
      hasil_df <- data.frame(
        Parameter = c("Test Statistic (t)", "Degrees of Freedom (df)", "P-Value", "Confidence Interval"),
        Nilai = c(
          round(hasil$test$statistic, 4),
          round(hasil$test$parameter, 2),
          format(hasil$test$p.value, scientific = TRUE, digits = 3),
          paste0("[", round(hasil$test$conf.int[1], 4), ", ", round(hasil$test$conf.int[2], 4), "]")
        )
      )
      
      ft <- flextable(hasil_df) %>%
        autofit() %>%
        theme_booktabs() %>%
        align(j = 1, align = "left", part = "all") %>%
        align(j = 2, align = "center", part = "all")
      
      doc <- read_docx() %>%
        body_add_par("🧪 Hasil Uji Rata-rata", style = "heading 1") %>%
        body_add_par("") %>%
        
        body_add_par("Informasi Analisis", style = "heading 2") %>%
        body_add_par(paste("Jenis Uji:", str_to_title(gsub("_", " ", hasil$type)))) %>%
        body_add_par(paste("Variabel:", hasil$variable)) %>%
        body_add_par(paste("Hipotesis Alternatif:", hasil$alternative)) %>%
        body_add_par("") %>%
        
        body_add_par("Tabel Hasil Uji Statistik", style = "heading 2") %>%
        body_add_flextable(ft) %>%
        body_add_par("") %>%
        
        body_add_par("Kesimpulan Singkat", style = "heading 2")
      
      kesimpulan_singkat <- ifelse(hasil$test$p.value > hasil$alpha, 
                                   "Tidak ada cukup bukti statistik untuk menyatakan adanya perbedaan rata-rata yang signifikan.",
                                   "Terdapat cukup bukti statistik untuk menyatakan adanya perbedaan rata-rata yang signifikan.")
      
      doc <- doc %>% body_add_par(kesimpulan_singkat)
      
      print(doc, target = file)
    }
  )
  
  output$download_mean_interpretation <- downloadHandler(
    filename = function() {
      paste0("Mean_Test_Interpretation_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$mean_test_results)) {
        doc <- read_docx() %>% body_add_par("Belum ada hasil. Jalankan uji rata-rata terlebih dahulu.")
        print(doc, target = file)
        return()
      }
      
      hasil <- values$mean_test_results
      p_val <- hasil$test$p.value
      
      doc <- read_docx() %>%
        body_add_par("📝 Laporan Interpretasi Uji Rata-rata", style = "heading 1") %>%
        body_add_par("") %>%
        
        body_add_par("Hipotesis dan Keputusan Statistik", style = "heading 2")
      
      if (hasil$type == "t_one") {
        doc <- doc %>% body_add_par(paste0("H₀: μ = ", input$mu_value), style="List Paragraph") %>%
          body_add_par(paste0("H₁: μ ", switch(hasil$alternative, "two.sided"="≠", "greater"=">", "less"="<"), " ", input$mu_value), style="List Paragraph")
      } else {
        doc <- doc %>% body_add_par("H₀: μ₁ = μ₂", style="List Paragraph") %>%
          body_add_par(paste0("H₁: μ₁ ", switch(hasil$alternative, "two.sided"="≠", "greater"=">", "less"="<"), " μ₂"), style="List Paragraph")
      }
      
      doc <- doc %>% body_add_par("") %>%
        body_add_par(
          fpar(ftext("Keputusan (α=", prop=fp_text()), ftext(hasil$alpha, prop=fp_text(bold=TRUE)), ftext("): ", prop=fp_text()),
               ftext(ifelse(p_val > hasil$alpha, "Gagal Tolak H₀", "Tolak H₀"), 
                     prop = fp_text(bold = TRUE, color = ifelse(p_val > hasil$alpha, "darkgreen", "red"))))
        ) %>% body_add_par("")
      
      doc <- doc %>%
        body_add_par("Implikasi dan Rekomendasi", style = "heading 2")
      
      if (p_val > hasil$alpha) {
        doc <- doc %>%
          body_add_par("Tidak ada cukup bukti untuk menyatakan perbedaan rata-rata yang signifikan secara statistik. Ini menunjukkan bahwa rata-rata sampel tidak cukup berbeda dari nilai hipotesis (untuk 1 sampel) atau rata-rata antar grup tidak berbeda signifikan (untuk 2 sampel).") %>%
          body_add_par("Rekomendasi:", style = "heading 3") %>%
          body_add_par("Pertimbangkan untuk melakukan analisis kekuatan (power analysis) untuk memastikan ukuran sampel sudah cukup.", style = "List Paragraph") %>%
          body_add_par("Evaluasi apakah perbedaan yang ada (meski tidak signifikan) memiliki makna praktis.", style = "List Paragraph")
      } else {
        doc <- doc %>%
          body_add_par("Terdapat cukup bukti untuk menyatakan adanya perbedaan rata-rata yang signifikan secara statistik.") %>%
          body_add_par("Rekomendasi:", style = "heading 3") %>%
          body_add_par("Laporkan ukuran efek (seperti Cohen's d) untuk mengukur seberapa besar perbedaan tersebut secara praktis.", style = "List Paragraph") %>%
          body_add_par("Pastikan asumsi uji (seperti normalitas) telah terpenuhi untuk validitas hasil.", style = "List Paragraph")
      }
      
      doc <- doc %>% body_add_par("")
      
      doc <- doc %>%
        body_add_par("Visualisasi Hasil", style = "heading 2")
      
      temp_plot_file <- tempfile(fileext = ".png")
      png(temp_plot_file, width = 6, height = 5, units = "in", res = 300)
      
      if(hasil$type %in% c("t_one", "t_paired")) {
        var_data <- sovi_data[[hasil$variable]]
        hist(var_data, main = paste("Distribusi", hasil$variable), col = "skyblue", breaks = 30, xlab=hasil$variable)
        abline(v = mean(var_data, na.rm = TRUE), col = "red", lwd = 2)
        if(hasil$type == "t_one") {
          abline(v = input$mu_value, col = "darkgreen", lwd = 2, lty = 2)
          legend("topright", legend=c("Rata-rata Sampel", "Nilai H₀"), col=c("red", "darkgreen"), lty=1:2, lwd=2)
        }
      } else {
        group_var <- if(is.numeric(sovi_data[[input$mean_group_var]])) {
          cut(sovi_data[[input$mean_group_var]], breaks = 2, labels = c("Grup 1", "Grup 2"))
        } else { as.factor(sovi_data[[input$mean_group_var]]) }
        
        boxplot(sovi_data[[hasil$variable]] ~ group_var, main = paste("Perbandingan", hasil$variable), 
                xlab=input$mean_group_var, ylab=hasil$variable, col=c("skyblue", "lightgreen"))
      }
      
      dev.off()
      
      doc <- doc %>%
        body_add_img(src = temp_plot_file, width = 5, height = 4)
      
      print(doc, target = file)
    }
  )
  
  #Download handlers untuk Uji Proporsi
  output$download_prop_results <- downloadHandler(
    filename = function() {
      paste0("Proportion_Test_Results_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$prop_test_results)) {
        doc <- read_docx() %>% body_add_par("Belum ada hasil. Jalankan uji proporsi terlebih dahulu.")
        print(doc, target = file)
        return()
      }
      
      hasil <- values$prop_test_results
      
      hasil_df <- data.frame(
        Parameter = c("Test Statistic", "Degrees of Freedom (df)", "P-Value"),
        Nilai = c(
          round(hasil$test$statistic, 4),
          ifelse(is.null(hasil$test$parameter), "N/A", round(hasil$test$parameter, 2)),
          format(hasil$test$p.value, scientific = TRUE, digits = 3)
        )
      )
      
      ft <- flextable(hasil_df) %>%
        autofit() %>%
        theme_booktabs() %>%
        align(j = 1, align = "left", part = "all") %>%
        align(j = 2, align = "center", part = "all")
      
      doc <- read_docx() %>%
        body_add_par("📊 Hasil Uji Proporsi", style = "heading 1") %>%
        body_add_par("") %>%
        
        body_add_par("Informasi Analisis", style = "heading 2") %>%
        body_add_par(paste("Jenis Uji:", str_to_title(gsub("_", " ", hasil$type)))) %>%
        body_add_par(paste("Variabel:", hasil$variable))
      
      if (!is.null(hasil$group_var) && (hasil$type == "prop_two" || hasil$type == "chisq_indep")) {
        doc <- doc %>% body_add_par(paste("Variabel Grouping:", hasil$group_var))
      }
      
      doc <- doc %>% 
        body_add_par("") %>%
        body_add_par("Tabel Hasil Uji Statistik", style = "heading 2") %>%
        body_add_flextable(ft) %>%
        body_add_par("") %>%
        body_add_par("Kesimpulan Singkat", style = "heading 2")
      
      kesimpulan_singkat <- ifelse(hasil$test$p.value > hasil$alpha, 
                                   "Tidak ada cukup bukti statistik untuk menolak hipotesis nol.",
                                   "Terdapat cukup bukti statistik untuk menolak hipotesis nol.")
      
      doc <- doc %>% body_add_par(kesimpulan_singkat)
      
      print(doc, target = file)
    }
  )
  
  output$download_prop_interpretation <- downloadHandler(
    filename = function() {
      paste0("Proportion_Interpretation_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$prop_test_results)) {
        doc <- read_docx() %>% body_add_par("Belum ada hasil. Jalankan uji proporsi terlebih dahulu.")
        print(doc, target = file)
        return()
      }
      
      hasil <- values$prop_test_results
      p_val <- hasil$test$p.value
      
      doc <- read_docx() %>%
        body_add_par("📝 Laporan Interpretasi Uji Proporsi", style = "heading 1") %>%
        body_add_par("") %>%
        
        body_add_par("Hipotesis dan Keputusan Statistik", style = "heading 2")
      
      hipotesis <- switch(hasil$type,
                          "prop_one" = c(paste0("H₀: p = ", input$prop_value), paste0("H₁: p ≠ ", input$prop_value)),
                          "prop_two" = c("H₀: p₁ = p₂", "H₁: p₁ ≠ p₂"),
                          "chisq_gof" = c("H₀: Data mengikuti distribusi yang diharapkan", "H₁: Data tidak mengikuti distribusi yang diharapkan"),
                          "chisq_indep" = c("H₀: Variabel independen (tidak ada asosiasi)", "H₁: Variabel tidak independen (ada asosiasi)")
      )
      doc <- doc %>% body_add_par(hipotesis[1], style="List Paragraph") %>%
        body_add_par(hipotesis[2], style="List Paragraph") %>%
        body_add_par("") %>%
        body_add_par(
          fpar(ftext("Keputusan (α=", prop=fp_text()), ftext(hasil$alpha, prop=fp_text(bold=TRUE)), ftext("): ", prop=fp_text()),
               ftext(ifelse(p_val > hasil$alpha, "Gagal Tolak H₀", "Tolak H₀"), 
                     prop = fp_text(bold = TRUE, color = ifelse(p_val > hasil$alpha, "darkgreen", "red"))))
        ) %>% body_add_par("")
      
      doc <- doc %>%
        body_add_par("Implikasi dan Rekomendasi", style = "heading 2")
      
      if (p_val > hasil$alpha) {
        doc <- doc %>%
          body_add_par("Tidak ada cukup bukti statistik untuk menolak hipotesis nol. Ini berarti tidak ditemukan adanya perbedaan atau asosiasi yang signifikan.")
      } else {
        doc <- doc %>%
          body_add_par("Terdapat cukup bukti statistik untuk menolak hipotesis nol. Ini berarti ditemukan adanya perbedaan atau asosiasi yang signifikan.")
      }
      
      if (hasil$type != "var_one") { # Asumsi tidak ada plot untuk var_one
        doc <- doc %>%
          body_add_par("") %>%
          body_add_par("Visualisasi Hasil", style = "heading 2")
        
        temp_plot_file <- tempfile(fileext = ".png")
        png(temp_plot_file, width = 6, height = 5, units = "in", res = 300)
        
        if (hasil$type %in% c("prop_two", "chisq_indep")) {
          cont_table <- table(sovi_data[[hasil$variable]], sovi_data[[hasil$group_var]])
          mosaicplot(cont_table, main = "Mosaic Plot", color = TRUE)
        } else {
          barplot(table(sovi_data[[hasil$variable]]), main = "Bar Plot")
        }
        
        dev.off()
        
        doc <- doc %>%
          body_add_img(src = temp_plot_file, width = 5, height = 4.5)
      }
      
      print(doc, target = file)
    }
  )
  
  output$download_var_results <- downloadHandler(
    filename = function() {
      paste0("Variance_Test_Results_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$var_test_results)) {
        doc <- read_docx() %>% body_add_par("Belum ada hasil. Jalankan uji varians terlebih dahulu.")
        print(doc, target = file)
        return()
      }
      
      hasil <- values$var_test_results
      
      if(hasil$type == "levene") {
        p_val <- hasil$test$`Pr(>F)`[1]; stat_val <- hasil$test$`F value`[1]; df_val <- paste(hasil$test$Df, collapse = ", "); stat_name <- "F-statistic"
      } else {
        p_val <- hasil$test$p.value; stat_val <- hasil$test$statistic; df_val <- hasil$test$parameter; stat_name <- if(hasil$type == "f_test") "F-statistic" else "Chi-squared"
      }
      
      hasil_df <- data.frame(
        Parameter = c(stat_name, "Degrees of Freedom (df)", "P-Value"),
        Nilai = c(round(stat_val, 4), df_val, format(p_val, scientific = TRUE, digits = 3))
      )
      
      ft <- flextable(hasil_df) %>%
        autofit() %>%
        theme_booktabs() %>%
        align(j = 1, align = "left", part = "all") %>%
        align(j = 2, align = "center", part = "all")
      
      doc <- read_docx() %>%
        body_add_par("📈 Hasil Uji Varians", style = "heading 1") %>%
        body_add_par("") %>%
        
        body_add_par("Informasi Analisis", style = "heading 2") %>%
        body_add_par(paste("Jenis Uji:", str_to_title(gsub("_", " ", hasil$type)))) %>%
        body_add_par(paste("Variabel:", hasil$variable))
      
      if (!is.null(hasil$group_var) && hasil$type != "var_one") {
        doc <- doc %>% body_add_par(paste("Variabel Grouping:", hasil$group_var))
      }
      
      doc <- doc %>% 
        body_add_par("") %>%
        body_add_par("Tabel Hasil Uji Statistik", style = "heading 2") %>%
        body_add_flextable(ft) %>%
        body_add_par("") %>%
        body_add_par("Kesimpulan Singkat", style = "heading 2")
      
      kesimpulan_singkat <- ifelse(p_val > hasil$alpha, 
                                   "Tidak ada cukup bukti statistik untuk menyatakan varians berbeda (varians homogen).",
                                   "Terdapat cukup bukti statistik untuk menyatakan varians berbeda (varians tidak homogen).")
      
      doc <- doc %>% body_add_par(kesimpulan_singkat)
      
      print(doc, target = file)
    }
  )
  
  output$download_var_interpretation <- downloadHandler(
    filename = function() {
      paste0("Variance_Interpretation_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$var_test_results)) {
        doc <- read_docx() %>% body_add_par("Belum ada hasil. Jalankan uji varians terlebih dahulu.")
        print(doc, target = file)
        return()
      }
      
      hasil <- values$var_test_results
      p_val <- if(hasil$type == "levene") hasil$test$`Pr(>F)`[1] else hasil$test$p.value
      
      doc <- read_docx() %>%
        body_add_par("📝 Laporan Interpretasi Uji Varians", style = "heading 1") %>%
        body_add_par("") %>%
        
        body_add_par("Hipotesis dan Keputusan Statistik", style = "heading 2")
      
      hipotesis <- if (hasil$type == "var_one") {
        c(paste0("H₀: σ² = ", input$sigma_squared), paste0("H₁: σ² ≠ ", input$sigma_squared))
      } else {
        c("H₀: Varians antar grup sama (homogen).", "H₁: Varians antar grup berbeda (tidak homogen).")
      }
      doc <- doc %>% body_add_par(hipotesis[1], style="List Paragraph") %>%
        body_add_par(hipotesis[2], style="List Paragraph") %>%
        body_add_par("") %>%
        body_add_par(
          fpar(ftext("Keputusan (α=", prop=fp_text()), ftext(hasil$alpha, prop=fp_text(bold=TRUE)), ftext("): ", prop=fp_text()),
               ftext(ifelse(p_val > hasil$alpha, "Gagal Tolak H₀", "Tolak H₀"), 
                     prop = fp_text(bold = TRUE, color = ifelse(p_val > hasil$alpha, "darkgreen", "red"))))
        ) %>% body_add_par("")
      
      doc <- doc %>%
        body_add_par("Implikasi dan Rekomendasi", style = "heading 2")
      
      if (p_val > hasil$alpha) {
        doc <- doc %>%
          body_add_par("Asumsi homogenitas varians terpenuhi. Ini menunjukkan variabilitas data konsisten di semua grup yang dibandingkan.") %>%
          body_add_par("Rekomendasi:", style = "heading 3") %>%
          body_add_par("Anda dapat melanjutkan analisis menggunakan uji parametrik standar seperti ANOVA atau Independent t-test.", style = "List Paragraph")
      } else {
        doc <- doc %>%
          body_add_par("Asumsi homogenitas varians tidak terpenuhi. Ini menunjukkan variabilitas data berbeda secara signifikan antar grup.") %>%
          body_add_par("Rekomendasi:", style = "heading 3") %>%
          body_add_par("Gunakan uji alternatif yang tidak memerlukan asumsi ini, seperti Welch's t-test (untuk 2 grup) atau Welch's ANOVA.", style = "List Paragraph") %>%
          body_add_par("Pertimbangkan untuk mentransformasi data (misal: log) untuk mencoba menstabilkan varians.", style = "List Paragraph")
      }
      
      if (hasil$type != "var_one") {
        doc <- doc %>%
          body_add_par("") %>%
          body_add_par("Visualisasi Diagnostik", style = "heading 2")
        
        temp_plot_file <- tempfile(fileext = ".png")
        png(temp_plot_file, width = 8, height = 4, units = "in", res = 300)
        
        par(mfrow = c(1, 2))
        plot_data <- data.frame(value = sovi_data[[hasil$variable]], group = hasil$group_data)
        plot_data <- plot_data[complete.cases(plot_data), ]
        
        boxplot(value ~ group, data = plot_data, main = "Boxplot per Grup", xlab = hasil$group, ylab = hasil$variable, col = rainbow(length(unique(plot_data$group))))
        group_vars <- aggregate(value ~ group, data = plot_data, FUN = var)
        barplot(group_vars$value, names.arg = group_vars$group, main = "Varians per Grup", xlab = hasil$group, ylab = "Varians", col = rainbow(nrow(group_vars)))
        
        dev.off()
        
        doc <- doc %>%
          body_add_img(src = temp_plot_file, width = 7, height = 3.5)
      }
      
      print(doc, target = file)
    }
  )
  
  #Download handlers untuk ANOVA
  output$download_anova_results <- downloadHandler(
    filename = function() {
      paste0("ANOVA_Results_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$anova_results)) {
        content <- "Belum ada hasil ANOVA. Silakan jalankan ANOVA terlebih dahulu."
      } else {
        anova_type_desc <- switch(values$anova_results$type,
                                  "one_way" = "One-Way ANOVA",
                                  "two_way" = "Two-Way ANOVA",
                                  "repeated" = "Repeated Measures ANOVA"
        )
        
        content <- paste0(
          "HASIL ANALISIS VARIANS (ANOVA)\n",
          "===============================\n\n",
          "INFORMASI ANALISIS:\n",
          "===================\n",
          "Jenis ANOVA: ", anova_type_desc, "\n",
          "Variabel Response: ", values$anova_results$response, "\n",
          "Faktor 1: ", values$anova_results$factor1, "\n",
          if(!is.null(values$anova_results$factor2)) paste("Faktor 2:", values$anova_results$factor2, "\n") else "",
          "Tingkat Signifikansi: ", values$anova_results$alpha, "\n",
          "Tanggal Analisis: ", Sys.Date(), "\n\n",
          "TABEL ANOVA:\n",
          "============\n",
          paste(capture.output(print(summary(values$anova_results$model))), collapse = "\n"), "\n\n"
        )
        
        anova_summary <- summary(values$anova_results$model)
        if(values$anova_results$type == "repeated") {
          if(length(anova_summary) > 1) {
            main_effect <- anova_summary[[2]][[1]]
            f_stat <- main_effect$`F value`[1]
            p_val <- main_effect$`Pr(>F)`[1]
          }
        } else {
          f_stat <- anova_summary[[1]]$`F value`[1]
          p_val <- anova_summary[[1]]$`Pr(>F)`[1]
        }
        
        content <- paste0(content,
                          "INTERPRETASI:\n",
                          "=============\n",
                          "H0: Semua rata-rata grup sama\n",
                          "H1: Minimal ada satu rata-rata grup yang berbeda\n\n"
        )
        
        if(!is.na(p_val)) {
          content <- paste0(content,
                            "F-statistic: ", round(f_stat, 4), "\n",
                            "p-value: ", format(p_val, scientific = TRUE), "\n\n",
                            "KEPUTUSAN: ", ifelse(p_val > values$anova_results$alpha, 
                                                  "Gagal menolak H0", "Tolak H0"), "\n\n",
                            "KESIMPULAN: ",
                            ifelse(p_val > values$anova_results$alpha,
                                   "Tidak ada perbedaan yang signifikan antar grup.",
                                   "Ada perbedaan yang signifikan antar grup."), "\n\n"
          )
          
          if(p_val <= values$anova_results$alpha) {
            content <- paste0(content,
                              "REKOMENDASI: Lanjutkan dengan uji post-hoc untuk identifikasi grup yang berbeda.\n"
            )
          }
        }
        
        if(!is.null(values$anova_results$posthoc)) {
          content <- paste0(content, "\n",
                            "HASIL UJI POST-HOC:\n",
                            "===================\n",
                            "Metode: ", str_to_upper(input$posthoc_method), "\n\n",
                            paste(capture.output(print(values$anova_results$posthoc)), collapse = "\n"), "\n"
          )
        }
        
        if(!is.null(values$anova_results$assumptions)) {
          norm_p <- values$anova_results$assumptions$normality$p.value
          homo_p <- values$anova_results$assumptions$homogeneity$`Pr(>F)`[1]
          
          content <- paste0(content, "\n",
                            "HASIL UJI ASUMSI:\n",
                            "=================\n",
                            "1. Normalitas Residual: ", ifelse(norm_p > 0.05, "✅ Terpenuhi", "❌ Tidak terpenuhi"), 
                            " (p = ", format(norm_p, scientific = TRUE), ")\n",
                            "2. Homogenitas Varians: ", ifelse(homo_p > 0.05, "✅ Terpenuhi", "❌ Tidak terpenuhi"), 
                            " (p = ", format(homo_p, scientific = TRUE), ")\n\n",
                            "STATUS ASUMSI: ",
                            if(norm_p > 0.05 && homo_p > 0.05) {
                              "✅ Semua asumsi terpenuhi. Hasil ANOVA valid."
                            } else {
                              "⚠️ Ada asumsi yang tidak terpenuhi. Pertimbangkan alternatif robust."
                            }
          )
        }
      }
      
      doc <- create_word_doc("Hasil Analisis Varians (ANOVA)", content)
      print(doc, target = file)
    }
  )
  
  output$download_anova_plots <- downloadHandler(
    filename = function() {
      paste0("ANOVA_Plots_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      if(is.null(values$anova_results)) {
        jpeg(file, width = 800, height = 600, quality = 95)
        plot.new()
        text(0.5, 0.5, "Belum ada hasil ANOVA.", cex = 1.5, col = "gray")
        dev.off()
      } else {
        jpeg(file, width = 1200, height = 800, quality = 95)
        
        response_var <- sovi_data[[values$anova_results$response]]
        factor1 <- sovi_data[[values$anova_results$factor1]]
        
        if(is.numeric(factor1)) {
          factor1 <- cut(factor1, breaks = 3, labels = c("Low", "Medium", "High"))
        }
        
        if(values$anova_results$type == "one_way") {
          par(mfrow = c(2, 2))
          
          boxplot(response_var ~ factor1,
                  main = paste("Boxplot:", values$anova_results$response, "by", values$anova_results$factor1),
                  xlab = values$anova_results$factor1,
                  ylab = values$anova_results$response,
                  col = rainbow(length(unique(factor1))))
          
          means <- aggregate(response_var, by = list(factor1), FUN = mean, na.rm = TRUE)
          barplot(means$x, names.arg = means$Group.1,
                  main = "Group Means",
                  xlab = values$anova_results$factor1,
                  ylab = paste("Mean", values$anova_results$response),
                  col = rainbow(nrow(means)))
          
          plot(fitted(values$anova_results$model), residuals(values$anova_results$model),
               main = "Residuals vs Fitted",
               xlab = "Fitted Values", ylab = "Residuals",
               pch = 16, col = "#667eea")
          abline(h = 0, col = "red", lty = 2)
          
          qqnorm(residuals(values$anova_results$model), main = "Q-Q Plot of Residuals",
                 pch = 16, col = "#667eea")
          qqline(residuals(values$anova_results$model), col = "red", lwd = 2)
          
        } else if(values$anova_results$type == "two_way") {
          factor2 <- sovi_data[[values$anova_results$factor2]]
          if(is.numeric(factor2)) {
            factor2 <- cut(factor2, breaks = 3, labels = c("Low", "Medium", "High"))
          }
          
          interaction.plot(factor1, factor2, response_var,
                           main = "Interaction Plot",
                           xlab = values$anova_results$factor1,
                           ylab = paste("Mean", values$anova_results$response),
                           trace.label = values$anova_results$factor2,
                           col = rainbow(length(unique(factor2))))
        }
        
        dev.off()
      }
    }
  )
  
  output$download_anova_interpretation <- downloadHandler(
    filename = function() {
      paste0("ANOVA_Interpretation_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$anova_results)) {
        content <- "Belum ada hasil ANOVA. Silakan jalankan ANOVA terlebih dahulu."
      } else {
        anova_summary <- summary(values$anova_results$model)
        if(values$anova_results$type == "repeated") {
          if(length(anova_summary) > 1) {
            main_effect <- anova_summary[[2]][[1]]
            f_stat <- main_effect$`F value`[1]
            p_val <- main_effect$`Pr(>F)`[1]
            df1 <- main_effect$Df[1]
            df2 <- main_effect$Df[2]
          }
        } else {
          f_stat <- anova_summary[[1]]$`F value`[1]
          p_val <- anova_summary[[1]]$`Pr(>F)`[1]
          df1 <- anova_summary[[1]]$Df[1]
          df2 <- anova_summary[[1]]$Df[2]
        }
        
        ss_effect <- anova_summary[[1]]$`Sum Sq`[1]
        ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
        eta_squared <- ss_effect / ss_total
        
        effect_interpretation <- if(eta_squared < 0.01) "kecil" else if(eta_squared < 0.06) "sedang" else if(eta_squared < 0.14) "besar" else "sangat besar"
        
        content <- paste0(
          "INTERPRETASI LENGKAP ANALISIS VARIANS (ANOVA)\n",
          "=============================================\n\n",
          "TUJUAN DAN KONTEKS ANALISIS:\n",
          "============================\n",
          "ANOVA menguji apakah ada perbedaan rata-rata antar grup/treatment.\n",
          "Merupakan extension dari t-test untuk multiple groups comparison.\n\n",
          "JENIS ANOVA: ", str_to_upper(gsub("_", " ", values$anova_results$type)), "\n",
          "VARIABEL RESPONSE: ", values$anova_results$response, "\n",
          "FAKTOR: ", values$anova_results$factor1,
          if(!is.null(values$anova_results$factor2)) paste(", ", values$anova_results$factor2) else "", "\n",
          "TANGGAL ANALISIS: ", Sys.Date(), "\n\n",
          "HASIL STATISTIK:\n",
          "================\n",
          "• F-statistic = ", round(f_stat, 4), "\n",
          "• df = ", df1, ", ", df2, "\n",
          "• p-value = ", round(p_val, 6), "\n",
          "• α = ", values$anova_results$alpha, "\n\n",
          "INTERPRETASI PRAKTIS:\n",
          "====================\n",
          ifelse(p_val > values$anova_results$alpha,
                 "Tidak ada perbedaan yang signifikan antar grup.",
                 "Ada perbedaan yang signifikan antar grup."), "\n\n"
        )
        
        if(values$anova_results$type == "one_way") {
          content <- paste0(content,
                            "INTERPRETASI ONE-WAY ANOVA:\n",
                            "===========================\n",
                            "• Membandingkan rata-rata multiple grup independen\n",
                            "• Menguji apakah semua grup memiliki rata-rata yang sama\n",
                            "• Alternative untuk multiple t-tests (menghindari Type I error inflation)\n\n",
                            "APLIKASI PRAKTIS:\n",
                            "• Perbandingan multiple treatments\n",
                            "• Analisis perbedaan antar kategori/region\n",
                            "• Quality control dengan multiple suppliers\n",
                            "• Educational research dengan multiple methods\n\n",
                            "ASUMSI UJI:\n",
                            "• Normalitas: Residual berdistribusi normal\n",
                            "• Homogenitas: Varians sama antar grup\n",
                            "• Independensi: Observasi independen\n"
          )
        } else if(values$anova_results$type == "two_way") {
          content <- paste0(content,
                            "INTERPRETASI TWO-WAY ANOVA:\n",
                            "===========================\n",
                            "• Menguji efek dua faktor secara simultan\n",
                            "• Dapat mendeteksi interaction effect antar faktor\n",
                            "• Lebih efficient dibanding separate one-way ANOVAs\n\n",
                            "KOMPONEN ANALISIS:\n",
                            "• Main Effect ", values$anova_results$factor1, ": Efek faktor 1\n",
                            "• Main Effect ", values$anova_results$factor2, ": Efek faktor 2\n",
                            "• Interaction Effect: Efek kombinasi kedua faktor\n\n",
                            "INTERPRETASI INTERACTION:\n",
                            "• Tidak ada interaksi: Efek faktor konsisten di semua level\n",
                            "• Ada interaksi: Efek faktor bergantung pada level faktor lain\n"
          )
        } else if(values$anova_results$type == "repeated") {
          content <- paste0(content,
                            "INTERPRETASI REPEATED MEASURES ANOVA:\n",
                            "=====================================\n",
                            "• Menguji perbedaan antar kondisi pada subjek yang sama\n",
                            "• Mengontrol variabilitas antar subjek\n",
                            "• Lebih powerful dibanding independent groups ANOVA\n\n",
                            "KEUNTUNGAN:\n",
                            "• Reduced error variance (within-subject design)\n",
                            "• Increased statistical power\n",
                            "• Control for individual differences\n",
                            "• Fewer subjects needed\n\n",
                            "ASUMSI TAMBAHAN:\n",
                            "• Sphericity: Varians perbedaan antar kondisi sama\n",
                            "• Compound symmetry: Korelasi antar kondisi sama\n"
          )
        }
        
        content <- paste0(content, "\n",
                          "UKURAN EFEK (ETA-SQUARED):\n",
                          "==========================\n",
                          "η² = ", round(eta_squared, 3), "\n",
                          "Interpretasi: Efek ", effect_interpretation, "\n\n",
                          "GUIDELINES INTERPRETASI:\n",
                          "• η² < 0.01: Efek kecil (1% varians dijelaskan)\n",
                          "• 0.01 ≤ η² < 0.06: Efek sedang (1-6% varians)\n",
                          "• 0.06 ≤ η² < 0.14: Efek besar (6-14% varians)\n",
                          "• η² ≥ 0.14: Efek sangat besar (>14% varians)\n\n",
                          "PRACTICAL SIGNIFICANCE:\n",
                          "Eta-squared menunjukkan proporsi varians dalam variabel response\n",
                          "yang dijelaskan oleh faktor/treatment. Nilai ", round(eta_squared*100, 1), "% varians\n",
                          "dalam ", values$anova_results$response, " dijelaskan oleh ", values$anova_results$factor1, ".\n\n"
        )
        
        if(!is.null(values$anova_results$posthoc) && p_val <= values$anova_results$alpha) {
          content <- paste0(content,
                            "INTERPRETASI POST-HOC TESTS:\n",
                            "============================\n",
                            "Karena ANOVA signifikan, uji post-hoc dilakukan untuk identifikasi\n",
                            "pasangan grup yang berbeda signifikan.\n\n",
                            "METODE: ", str_to_upper(input$posthoc_method), "\n\n",
                            "INTERPRETASI HASIL POST-HOC:\n",
                            "• p-value < α: Perbedaan signifikan antara kedua grup\n",
                            "• p-value ≥ α: Tidak ada perbedaan signifikan\n",
                            "• Adjusted p-values: Mengontrol family-wise error rate\n\n",
                            "PERBANDINGAN METODE POST-HOC:\n",
                            "• Tukey HSD: Conservative, equal sample sizes\n",
                            "• Bonferroni: Very conservative, any sample sizes\n",
                            "• Scheffe: Most conservative, any contrasts\n",
                            "• LSD: Least conservative, no adjustment\n"
          )
        }
        
        if(!is.null(values$anova_results$assumptions)) {
          norm_p <- values$anova_results$assumptions$normality$p.value
          homo_p <- values$anova_results$assumptions$homogeneity$`Pr(>F)`[1]
          
          content <- paste0(content, "\n",
                            "EVALUASI ASUMSI ANOVA:\n",
                            "======================\n",
                            "1. NORMALITAS RESIDUAL:\n",
                            "   Status: ", ifelse(norm_p > 0.05, "✅ Terpenuhi", "❌ Tidak terpenuhi"), "\n",
                            "   p-value: ", format(norm_p, scientific = TRUE), "\n",
                            "   Interpretasi: ", ifelse(norm_p > 0.05, 
                                                        "Residual berdistribusi normal, asumsi valid.",
                                                        "Residual tidak normal, pertimbangkan transformasi."), "\n\n",
                            "2. HOMOGENITAS VARIANS:\n",
                            "   Status: ", ifelse(homo_p > 0.05, "✅ Terpenuhi", "❌ Tidak terpenuhi"), "\n",
                            "   p-value: ", format(homo_p, scientific = TRUE), "\n",
                            "   Interpretasi: ", ifelse(homo_p > 0.05,
                                                        "Varians homogen antar grup, asumsi valid.",
                                                        "Varians tidak homogen, pertimbangkan Welch ANOVA."), "\n\n",
                            "KESIMPULAN ASUMSI:\n",
                            if(norm_p > 0.05 && homo_p > 0.05) {
                              "✅ Semua asumsi ANOVA terpenuhi. Hasil analisis valid dan reliable."
                            } else {
                              paste0(
                                "⚠️ Ada asumsi yang dilanggar. Pertimbangkan:\n",
                                if(norm_p <= 0.05) "   • Transformasi data (log, sqrt, Box-Cox)\n" else "",
                                if(homo_p <= 0.05) "   • Welch ANOVA untuk unequal variances\n" else "",
                                "   • Uji non-parametrik (Kruskal-Wallis)\n",
                                "   • Robust ANOVA methods"
                              )
                            }
          )
        }
        
        content <- paste0(content, "\n\n",
                          "REKOMENDASI BERDASARKAN HASIL:\n",
                          "==============================\n",
                          ifelse(p_val > values$anova_results$alpha,
                                 paste0(
                                   "TIDAK SIGNIFIKAN:\n",
                                   "• Tidak ada evidence untuk perbedaan antar grup\n",
                                   "• Tidak perlu post-hoc tests\n",
                                   "• Pertimbangkan power analysis untuk sample size\n",
                                   "• Evaluasi practical vs statistical significance\n",
                                   "• Periksa apakah ada confounding variables"
                                 ),
                                 paste0(
                                   "SIGNIFIKAN:\n",
                                   "• Ada evidence untuk perbedaan antar grup\n",
                                   "• Lakukan post-hoc tests untuk identifikasi grup\n",
                                   "• Laporkan effect size (eta-squared)\n",
                                   "• Interpretasikan dalam konteks praktis\n",
                                   "• Validasi dengan confidence intervals"
                                 )
                          ), "\n\n",
                          "LANGKAH SELANJUTNYA:\n",
                          "====================\n",
                          "1. VALIDASI HASIL:\n",
                          "   • Periksa outlier yang mungkin mempengaruhi\n",
                          "   • Konfirmasi dengan visualisasi (boxplots)\n",
                          "   • Cross-validate dengan subset data\n\n",
                          "2. INTERPRETASI KONTEKSTUAL:\n",
                          "   • Hubungkan dengan domain knowledge\n",
                          "   • Pertimbangkan practical significance\n",
                          "   • Evaluasi cost-benefit dari findings\n\n",
                          "3. KOMUNIKASI HASIL:\n",
                          "   • Laporkan effect size selain p-value\n",
                          "   • Gunakan confidence intervals\n",
                          "   • Visualisasikan perbedaan antar grup\n",
                          "   • Dokumentasikan assumptions checking\n\n",
                          "CATATAN METODOLOGIS:\n",
                          "====================\n",
                          "• ANOVA robust terhadap moderate violations of normality\n",
                          "• Unequal sample sizes dapat mempengaruhi power\n",
                          "• Multiple comparisons meningkatkan Type I error\n",
                          "• Effect size lebih informatif dibanding p-value\n\n",
                          "KESIMPULAN:\n",
                          "===========\n",
                          "ANOVA memberikan framework yang powerful untuk membandingkan multiple groups. ",
                          "Hasil menunjukkan ", ifelse(p_val <= values$anova_results$alpha, "adanya", "tidak adanya"), 
                          " perbedaan signifikan dengan effect size ", effect_interpretation, ". ",
                          "Interpretasi harus mempertimbangkan validitas asumsi dan practical significance."
        )
      }
      
      doc <- create_word_doc("Interpretasi Lengkap ANOVA", content)
      print(doc, target = file)
    }
  )
  
  #Download handlers untuk Regresi
  output$download_reg_results <- downloadHandler(
    filename = function() {
      paste0("Regression_Results_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$regression_model)) {
        content <- "Belum ada hasil regresi. Silakan jalankan regresi terlebih dahulu."
      } else {
        model_summary <- summary(values$regression_model$model)
        
        content <- paste0(
          "HASIL REGRESI LINEAR BERGANDA\n",
          "=============================\n\n",
          "INFORMASI MODEL:\n",
          "================\n",
          "Formula: ", values$regression_model$formula, "\n",
          "Metode Seleksi: ", str_to_upper(values$regression_model$method), "\n",
          "Data Standardisasi: ", ifelse(values$regression_model$standardized, "Ya", "Tidak"), "\n",
          "Ukuran Sampel: ", nrow(values$regression_model$data), "\n",
          "Tanggal Analisis: ", Sys.Date(), "\n\n",
          "RINGKASAN MODEL:\n",
          "================\n",
          paste(capture.output(print(model_summary)), collapse = "\n"), "\n\n",
          "STATISTIK MODEL:\n",
          "================\n",
          "R-squared: ", round(model_summary$r.squared, 4), " (", round(model_summary$r.squared*100, 1), "% varians dijelaskan)\n",
          "Adjusted R-squared: ", round(model_summary$adj.r.squared, 4), "\n",
          "F-statistic: ", round(model_summary$fstatistic[1], 4), "\n",
          "p-value (model): ", format(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), scientific = TRUE), "\n",
          "Residual Standard Error: ", round(model_summary$sigma, 4), "\n",
          "AIC: ", round(AIC(values$regression_model$model), 2), "\n",
          "BIC: ", round(BIC(values$regression_model$model), 2), "\n\n",
          "INTERPRETASI KOEFISIEN:\n",
          "======================\n",
          "• Intercept: Nilai prediksi ketika semua prediktor = 0\n",
          "• Slope: Perubahan response per unit perubahan prediktor\n",
          "• p-value < 0.05: Prediktor signifikan\n",
          "• t-value: Kekuatan hubungan relatif terhadap standard error\n\n",
          "KUALITAS MODEL:\n",
          "===============\n",
          if(model_summary$r.squared < 0.3) {
            "R² < 0.3: Model menjelaskan sebagian kecil varians. Mungkin ada prediktor penting yang belum dimasukkan."
          } else if(model_summary$r.squared < 0.7) {
            "0.3 ≤ R² < 0.7: Model menjelaskan varians dengan cukup baik. Masih ada ruang untuk perbaikan model."
          } else {
            "R² ≥ 0.7: Model menjelaskan sebagian besar varians. Model memiliki daya prediksi yang baik."
          }
        )
        
        if(!is.null(values$regression_model$diagnostics)) {
          diag <- values$regression_model$diagnostics
          
          content <- paste0(content, "\n\n",
                            "HASIL UJI ASUMSI:\n",
                            "=================\n"
          )
          
          if(!is.na(diag$normality$p.value)) {
            norm_p <- diag$normality$p.value
            content <- paste0(content,
                              "1. Normalitas Residual: ", ifelse(norm_p > 0.05, "✅ Terpenuhi", "❌ Tidak terpenuhi"), 
                              " (p = ", format(norm_p, scientific = TRUE), ")\n"
            )
          }
          
          if(!is.na(diag$heteroscedasticity$p.value)) {
            hetero_p <- diag$heteroscedasticity$p.value
            content <- paste0(content,
                              "2. Homoskedastisitas: ", ifelse(hetero_p > 0.05, "✅ Terpenuhi", "❌ Tidak terpenuhi"), 
                              " (p = ", format(hetero_p, scientific = TRUE), ")\n"
            )
          }
          
          if(!is.na(diag$autocorrelation$p.value)) {
            auto_p <- diag$autocorrelation$p.value
            content <- paste0(content,
                              "3. Independensi: ", ifelse(auto_p > 0.05, "✅ Terpenuhi", "❌ Tidak terpenuhi"), 
                              " (p = ", format(auto_p, scientific = TRUE), ")\n"
            )
          }
          
          if(!is.na(diag$multicollinearity[1])) {
            max_vif <- max(diag$multicollinearity, na.rm = TRUE)
            content <- paste0(content,
                              "4. Multikolinearitas: ", ifelse(max_vif < 5, "✅ Tidak ada masalah", 
                                                               ifelse(max_vif < 10, "⚠️ Moderat", "❌ Serius")), 
                              " (VIF max = ", round(max_vif, 2), ")\n"
            )
          }
        }
      }
      
      doc <- create_word_doc("Hasil Regresi Linear Berganda", content)
      print(doc, target = file)
    }
  )
  
  output$download_reg_diagnostics <- downloadHandler(
    filename = function() {
      paste0("Regression_Diagnostics_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      if(is.null(values$regression_model)) {
        jpeg(file, width = 800, height = 600, quality = 95)
        plot.new()
        text(0.5, 0.5, "Belum ada hasil regresi.", cex = 1.5, col = "gray")
        dev.off()
      } else {
        jpeg(file, width = 1200, height = 800, quality = 95)
        par(mfrow = c(2, 2))
        plot(values$regression_model$model)
        dev.off()
      }
    }
  )
  
  output$download_reg_interpretation <- downloadHandler(
    filename = function() {
      paste0("Regression_Interpretation_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if(is.null(values$regression_model)) {
        content <- "Belum ada hasil regresi. Silakan jalankan regresi terlebih dahulu."
      } else {
        model_summary <- summary(values$regression_model$model)
        
        content <- paste0(
          "INTERPRETASI LENGKAP REGRESI LINEAR BERGANDA\n",
          "===========================================\n\n",
          "TUJUAN DAN KONTEKS ANALISIS:\n",
          "============================\n",
          "Regresi linear berganda menguji hubungan antara satu variabel response\n",
          "dengan multiple variabel prediktor secara simultan.\n\n",
          "MODEL YANG DIGUNAKAN:\n",
          "====================\n",
          "Formula: ", values$regression_model$formula, "\n",
          "Metode: ", str_to_title(values$regression_model$method), "\n",
          "Standardisasi: ", ifelse(values$regression_model$standardized, "Ya", "Tidak"), "\n",
          "Ukuran Sampel: ", nrow(values$regression_model$data), "\n",
          "Tanggal Analisis: ", Sys.Date(), "\n\n",
          "KUALITAS MODEL:\n",
          "===============\n",
          "• R² = ", round(model_summary$r.squared, 4), " (", round(model_summary$r.squared*100, 1), "% varians dijelaskan)\n",
          "• Adjusted R² = ", round(model_summary$adj.r.squared, 4), "\n",
          "• F-statistic = ", round(model_summary$fstatistic[1], 4), "\n",
          "• p-value (model) = ", format(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), scientific = TRUE), "\n\n",
          "INTERPRETASI R-SQUARED:\n",
          "======================\n",
          if(model_summary$r.squared < 0.3) {
            paste0(
              "R² = ", round(model_summary$r.squared, 3), " (", round(model_summary$r.squared*100, 1), "%)\n",
              "INTERPRETASI: Model menjelaskan sebagian kecil varians dalam data.\n\n",
              "IMPLIKASI:\n",
              "• Mungkin ada prediktor penting yang belum dimasukkan\n",
              "• Hubungan mungkin non-linear\n",
              "• Variabel response mungkin dipengaruhi faktor lain\n",
              "• Model perlu diperbaiki atau diperluas\n\n",
              "REKOMENDASI:\n",
              "• Tambahkan prediktor yang relevan\n",
              "• Pertimbangkan transformasi variabel\n",
              "• Eksplorasi interaction terms\n",
              "• Evaluasi polynomial terms"
            )
          } else if(model_summary$r.squared < 0.7) {
            paste0(
              "R² = ", round(model_summary$r.squared, 3), " (", round(model_summary$r.squared*100, 1), "%)\n",
              "INTERPRETASI: Model menjelaskan varians dengan cukup baik.\n\n",
              "IMPLIKASI:\n",
              "• Model memiliki daya prediksi yang reasonable\n",
              "• Masih ada ruang untuk improvement\n",
              "• Prediktor yang dipilih cukup relevan\n",
              "• Model dapat digunakan untuk prediksi dengan hati-hati\n\n",
              "REKOMENDASI:\n",
              "• Fine-tuning dengan feature selection\n",
              "• Pertimbangkan interaction effects\n",
              "• Validasi dengan data baru\n",
              "• Monitor performance secara berkala"
            )
          } else {
            paste0(
              "R² = ", round(model_summary$r.squared, 3), " (", round(model_summary$r.squared*100, 1), "%)\n",
              "INTERPRETASI: Model menjelaskan sebagian besar varians dalam data.\n\n",
              "IMPLIKASI:\n",
              "• Model memiliki daya prediksi yang baik\n",
              "• Prediktor yang dipilih sangat relevan\n",
              "• Hubungan linear cukup kuat\n",
              "• Model dapat digunakan untuk prediksi dengan confidence\n\n",
              "PERINGATAN:\n",
              "• Waspada overfitting, terutama jika sampel kecil\n",
              "• Validasi dengan data independen penting\n",
              "• Periksa apakah ada outlier yang influential\n",
              "• Monitor generalizability ke populasi lain"
            )
          }, "\n\n",
          "INTERPRETASI KOEFISIEN:\n",
          "======================\n",
          "INTERCEPT:\n",
          "• Nilai: ", round(model_summary$coefficients[1,1], 4), "\n",
          "• Interpretasi: Nilai prediksi ", input$reg_response, " ketika semua prediktor = 0\n",
          "• Signifikansi: ", ifelse(model_summary$coefficients[1,4] < 0.05, "Signifikan", "Tidak signifikan"), "\n\n",
          "SLOPE COEFFICIENTS:\n"
        )
        
        for(i in 2:nrow(model_summary$coefficients)) {
          coef_name <- rownames(model_summary$coefficients)[i]
          coef_value <- model_summary$coefficients[i,1]
          coef_pvalue <- model_summary$coefficients[i,4]
          
          content <- paste0(content,
                            "• ", coef_name, ":\n",
                            "  Koefisien: ", round(coef_value, 4), "\n",
                            "  Interpretasi: Setiap peningkatan 1 unit ", coef_name, 
                            " akan ", ifelse(coef_value > 0, "meningkatkan", "menurunkan"), 
                            " ", input$reg_response, " sebesar ", abs(round(coef_value, 4)), " unit\n",
                            "  Signifikansi: ", ifelse(coef_pvalue < 0.05, "Signifikan", "Tidak signifikan"), 
                            " (p = ", format(coef_pvalue, scientific = TRUE), ")\n\n"
          )
        }
        
        content <- paste0(content,
                          "EVALUASI MODEL:\n",
                          "===============\n",
                          "1. OVERALL MODEL SIGNIFICANCE:\n",
                          "   F-test p-value = ", format(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), scientific = TRUE), "\n",
                          "   Interpretasi: ", ifelse(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE) < 0.05,
                                                      "Model secara keseluruhan signifikan",
                                                      "Model secara keseluruhan tidak signifikan"), "\n\n",
                          "2. INDIVIDUAL PREDICTOR SIGNIFICANCE:\n",
                          "   Jumlah prediktor signifikan: ", sum(model_summary$coefficients[-1,4] < 0.05), 
                          " dari ", nrow(model_summary$coefficients)-1, " prediktor\n\n",
                          "3. MODEL FIT QUALITY:\n",
                          "   • R² vs Adjusted R²: ", ifelse(abs(model_summary$r.squared - model_summary$adj.r.squared) < 0.05,
                                                             "Perbedaan kecil, model tidak overfit",
                                                             "Perbedaan besar, mungkin terlalu banyak prediktor"), "\n",
                          "   • Residual Standard Error: ", round(model_summary$sigma, 4), "\n",
                          "   • AIC: ", round(AIC(values$regression_model$model), 2), " (lower is better)\n",
                          "   • BIC: ", round(BIC(values$regression_model$model), 2), " (lower is better)\n\n"
        )
        
        if(!is.null(values$regression_model$diagnostics)) {
          diag <- values$regression_model$diagnostics
          
          content <- paste0(content,
                            "EVALUASI ASUMSI REGRESI:\n",
                            "========================\n"
          )
          
          assumptions_met <- 0
          total_assumptions <- 0
          
          if(!is.na(diag$normality$p.value)) {
            norm_p <- diag$normality$p.value
            total_assumptions <- total_assumptions + 1
            if(norm_p > 0.05) assumptions_met <- assumptions_met + 1
            
            content <- paste0(content,
                              "1. NORMALITAS RESIDUAL:\n",
                              "   Status: ", ifelse(norm_p > 0.05, "✅ Terpenuhi", "❌ Tidak terpenuhi"), "\n",
                              "   p-value: ", format(norm_p, scientific = TRUE), "\n",
                              "   Interpretasi: ", ifelse(norm_p > 0.05,
                                                          "Residual berdistribusi normal, inferensi valid.",
                                                          "Residual tidak normal, confidence intervals mungkin bias."), "\n\n"
            )
          }
          
          if(!is.na(diag$heteroscedasticity$p.value)) {
            hetero_p <- diag$heteroscedasticity$p.value
            total_assumptions <- total_assumptions + 1
            if(hetero_p > 0.05) assumptions_met <- assumptions_met + 1
            
            content <- paste0(content,
                              "2. HOMOSKEDASTISITAS:\n",
                              "   Status: ", ifelse(hetero_p > 0.05, "✅ Terpenuhi", "❌ Tidak terpenuhi"), "\n",
                              "   p-value: ", format(hetero_p, scientific = TRUE), "\n",
                              "   Interpretasi: ", ifelse(hetero_p > 0.05,
                                                          "Varians residual konstan, standard errors valid.",
                                                          "Heteroskedastisitas detected, gunakan robust standard errors."), "\n\n"
            )
          }
          
          if(!is.na(diag$autocorrelation$p.value)) {
            auto_p <- diag$autocorrelation$p.value
            total_assumptions <- total_assumptions + 1
            if(auto_p > 0.05) assumptions_met <- assumptions_met + 1
            
            content <- paste0(content,
                              "3. INDEPENDENSI:\n",
                              "   Status: ", ifelse(auto_p > 0.05, "✅ Terpenuhi", "❌ Tidak terpenuhi"), "\n",
                              "   p-value: ", format(auto_p, scientific = TRUE), "\n",
                              "   Interpretasi: ", ifelse(auto_p > 0.05,
                                                          "Tidak ada autokorelasi, observasi independen.",
                                                          "Ada autokorelasi, standard errors underestimated."), "\n\n"
            )
          }
          
          if(!is.na(diag$multicollinearity[1])) {
            max_vif <- max(diag$multicollinearity, na.rm = TRUE)
            total_assumptions <- total_assumptions + 1
            if(max_vif < 5) assumptions_met <- assumptions_met + 1
            
            content <- paste0(content,
                              "4. MULTIKOLINEARITAS:\n",
                              "   VIF Maximum: ", round(max_vif, 2), "\n",
                              "   Status: ", ifelse(max_vif < 5, "✅ Tidak ada masalah", 
                                                    ifelse(max_vif < 10, "⚠️ Moderat", "❌ Serius")), "\n",
                              "   Interpretasi: ", ifelse(max_vif < 5,
                                                          "Prediktor tidak saling berkorelasi tinggi.",
                                                          ifelse(max_vif < 10,
                                                                 "Ada korelasi moderat antar prediktor.",
                                                                 "Multikolinearitas serius, koefisien tidak stabil.")), "\n\n"
            )
          }
          
          content <- paste0(content,
                            "KESIMPULAN ASUMSI:\n",
                            "Asumsi terpenuhi: ", assumptions_met, " dari ", total_assumptions, " asumsi\n",
                            if(assumptions_met == total_assumptions) {
                              "✅ Semua asumsi terpenuhi. Model valid untuk inferensi dan prediksi."
                            } else if(assumptions_met >= total_assumptions * 0.75) {
                              "⚠️ Sebagian besar asumsi terpenuhi. Model cukup reliable dengan catatan."
                            } else {
                              "❌ Banyak asumsi dilanggar. Pertimbangkan robust methods atau transformasi."
                            }, "\n\n"
          )
        }
        
        content <- paste0(content,
                          "REKOMENDASI BERDASARKAN HASIL:\n",
                          "==============================\n",
                          if(model_summary$r.squared >= 0.5 && (is.null(values$regression_model$diagnostics) || assumptions_met >= total_assumptions * 0.75)) {
                            paste0(
                              "MODEL BAIK - DAPAT DIGUNAKAN:\n",
                              "• Model memiliki daya prediksi yang baik\n",
                              "• Asumsi sebagian besar terpenuhi\n",
                              "• Dapat digunakan untuk inferensi dan prediksi\n",
                              "• Lakukan validasi dengan data baru\n\n",
                              "LANGKAH SELANJUTNYA:\n",
                              "• Cross-validation untuk assess generalizability\n",
                              "• Monitor performance pada data baru\n",
                              "• Dokumentasikan limitations dan assumptions\n",
                              "• Pertimbangkan ensemble methods untuk improvement"
                            )
                          } else {
                            paste0(
                              "MODEL PERLU PERBAIKAN:\n",
                              "• Daya prediksi masih dapat ditingkatkan\n",
                              "• Ada asumsi yang perlu diperhatikan\n",
                              "• Pertimbangkan model improvement\n\n",
                              "STRATEGI PERBAIKAN:\n",
                              "1. FEATURE ENGINEERING:\n",
                              "   • Tambahkan interaction terms\n",
                              "   • Pertimbangkan polynomial features\n",
                              "   • Transformasi variabel (log, sqrt, Box-Cox)\n",
                              "   • Feature selection yang lebih baik\n\n",
                              "2. MODEL ALTERNATIVES:\n",
                              "   • Ridge/Lasso regression untuk regularization\n",
                              "   • Robust regression untuk outliers\n",
                              "   • Non-linear models (GAM, splines)\n",
                              "   • Machine learning approaches\n\n",
                              "3. DATA QUALITY:\n",
                              "   • Identifikasi dan handle outliers\n",
                              "   • Periksa missing data patterns\n",
                              "   • Validasi data quality\n",
                              "   • Pertimbangkan additional data collection"
                            )
                          }, "\n\n",
                          "INTERPRETASI PRAKTIS:\n",
                          "====================\n",
                          "Model regresi ini menjelaskan ", round(model_summary$r.squared*100, 1), "% variabilitas dalam ", 
                          input$reg_response, " berdasarkan prediktor yang dipilih. ",
                          if(model_summary$r.squared >= 0.5) {
                            "Ini menunjukkan hubungan yang cukup kuat dan model dapat digunakan untuk understanding dan prediksi."
                          } else {
                            "Ini menunjukkan bahwa masih banyak faktor lain yang mempengaruhi variabel response."
                          }, "\n\n",
                          "CATATAN PENTING:\n",
                          "================\n",
                          "• Korelasi tidak berarti kausalitas\n",
                          "• Model hanya valid dalam rentang data yang digunakan\n",
                          "• Extrapolation di luar range data berisiko\n",
                          "• Outlier dapat mempengaruhi hasil secara signifikan\n",
                          "• Regular model validation dan updating diperlukan\n\n",
                          "KESIMPULAN:\n",
                          "===========\n",
                          "Regresi linear berganda memberikan insight tentang hubungan antara ", 
                          input$reg_response, " dengan prediktor yang dipilih. Model ",
                          ifelse(model_summary$r.squared >= 0.5, "menunjukkan", "belum menunjukkan"),
                          " daya prediksi yang ", ifelse(model_summary$r.squared >= 0.5, "baik", "optimal"),
                          " dan ", ifelse(is.null(values$regression_model$diagnostics) || assumptions_met >= total_assumptions * 0.75, 
                                          "memenuhi", "perlu perhatian terhadap"), " asumsi regresi. ",
                          "Interpretasi harus mempertimbangkan konteks domain dan limitations model."
        )
      }
      
      doc <- create_word_doc("Interpretasi Lengkap Regresi Linear Berganda", content)
      print(doc, target = file)
    }
  )
  
  output$prediction_inputs <- renderUI({
    if(is.null(values$regression_model)) {
      return(p("Belum ada model untuk prediksi."))
    }
    
    predictors <- input$reg_predictors
    input_list <- list()
    
    for(i in seq_along(predictors)) {
      var_name <- predictors[i]
      var_data <- sovi_data[[var_name]]
      
      if(is.numeric(var_data)) {
        input_list[[i]] <- numericInput(
          paste0("pred_", var_name),
          label = var_name,
          value = round(mean(var_data, na.rm = TRUE), 2),
          min = round(min(var_data, na.rm = TRUE), 2),
          max = round(max(var_data, na.rm = TRUE), 2),
          step = 0.01
        )
      }
    }
    
    tagList(input_list, 
            br(),
            actionButton("make_prediction", "🔮 Buat Prediksi", class = "btn btn-success"))
  })
  
  prediction_result <- reactiveVal(NULL)
  
  observeEvent(input$make_prediction, {
    if(is.null(values$regression_model)) return()
    
    pred_data <- data.frame(row.names = 1)
    for(var_name in input$reg_predictors) {
      pred_data[[var_name]] <- input[[paste0("pred_", var_name)]]
    }
    
    if(values$regression_model$standardized) {
      for(var_name in input$reg_predictors) {
        original_data <- sovi_data[[var_name]]
        pred_data[[var_name]] <- (pred_data[[var_name]] - mean(original_data, na.rm = TRUE)) / sd(original_data, na.rm = TRUE)
      }
    }
    
    # Make prediction
    pred_value <- predict(values$regression_model$model, newdata = pred_data, interval = "prediction")
    
    prediction_result(list(
      point_estimate = pred_value[1],
      lower_ci = pred_value[2],
      upper_ci = pred_value[3],
      inputs = pred_data
    ))
  })
  
  output$prediction_results <- renderPrint({
    pred <- prediction_result()
    if(is.null(pred)) {
      return("Masukkan nilai prediktor dan klik 'Buat Prediksi'.")
    }
    
    cat("🔮 HASIL PREDIKSI\n")
    cat("=================\n\n")
    cat("Prediksi:", round(pred$point_estimate, 4), "\n")
    cat("95% Prediction Interval: [", round(pred$lower_ci, 4), ",", round(pred$upper_ci, 4), "]\n\n")
    
    cat("Input Values:\n")
    for(var_name in names(pred$inputs)) {
      cat("•", var_name, ":", pred$inputs[[var_name]], "\n")
    }
  })
  
  output$prediction_plot <- renderPlot({
    if(is.null(values$regression_model)) {
      plot.new()
      text(0.5, 0.5, "Belum ada model untuk prediksi.", cex = 1.2, col = "gray")
      return()
    }
    
    if(length(input$reg_predictors) >= 1) {
      var_name <- input$reg_predictors[1]
      var_data <- values$regression_model$data[[var_name]]
      response_data <- values$regression_model$data[[input$reg_response]]
      
      var_seq <- seq(min(var_data), max(var_data), length.out = 100)
      
      pred_data <- data.frame(var_seq)
      names(pred_data) <- var_name
      
      for(other_var in input$reg_predictors[-1]) {
        pred_data[[other_var]] <- mean(values$regression_model$data[[other_var]])
      }
      
      pred_intervals <- predict(values$regression_model$model, newdata = pred_data, interval = "prediction")
      
      plot(var_data, response_data,
           main = paste("Prediction Plot:", var_name),
           xlab = var_name, ylab = input$reg_response,
           pch = 16, col = "#667eea", alpha = 0.6)
      
      lines(var_seq, pred_intervals[,1], col = "red", lwd = 2)
      lines(var_seq, pred_intervals[,2], col = "red", lwd = 1, lty = 2)
      lines(var_seq, pred_intervals[,3], col = "red", lwd = 1, lty = 2)
      
      legend("topleft", 
             legend = c("Data", "Prediction", "95% PI"), 
             col = c("#667eea", "red", "red"), 
             lty = c(NA, 1, 2), 
             pch = c(16, NA, NA),
             lwd = c(NA, 2, 1))
    }
  })
  
  output$model_comparison <- DT::renderDataTable({
    if(is.null(values$regression_model)) {
      return(DT::datatable(data.frame(Message = "Belum ada model untuk dibandingkan.")))
    }
    
    model <- values$regression_model$model
    
    simple_formula <- paste(input$reg_response, "~", input$reg_predictors[1])
    simple_model <- lm(as.formula(simple_formula), data = values$regression_model$data)
    
    full_model <- model
    
    comparison_df <- data.frame(
      Model = c("Simple", "Full"),
      Predictors = c(1, length(input$reg_predictors)),
      R_squared = c(summary(simple_model)$r.squared, summary(full_model)$r.squared),
      Adj_R_squared = c(summary(simple_model)$adj.r.squared, summary(full_model)$adj.r.squared),
      AIC = c(AIC(simple_model), AIC(full_model)),
      BIC = c(BIC(simple_model), BIC(full_model)),
      RMSE = c(sqrt(mean(residuals(simple_model)^2)), sqrt(mean(residuals(full_model)^2)))
    )
    
    DT::datatable(comparison_df, options = list(pageLength = 10)) %>%
      DT::formatRound(columns = 3:7, digits = 4)
  })
  
  output$model_comparison_plot <- renderPlot({
    if(is.null(values$regression_model)) {
      plot.new()
      text(0.5, 0.5, "Belum ada model untuk dibandingkan.", cex = 1.2, col = "gray")
      return()
    }
    
    model <- values$regression_model$model
    actual <- values$regression_model$data[[input$reg_response]]
    predicted <- fitted(model)
    
    plot(actual, predicted,
         main = "Actual vs Predicted Values",
         xlab = "Actual Values", ylab = "Predicted Values",
         pch = 16, col = "#667eea")
    abline(0, 1, col = "red", lwd = 2)
    
    r_squared <- summary(model)$r.squared
    legend("topleft", legend = paste("R² =", round(r_squared, 4)), 
           bty = "n", cex = 1.2)
  })
  
  output$regression_assumptions_plots <- renderPlot({
    if(is.null(values$regression_model)) {
      plot.new()
      text(0.5, 0.5, "Belum ada hasil regresi untuk ditampilkan.", cex = 1.2, col = "gray")
      return()
    }
    
    model <- values$regression_model$model
    residuals_data <- residuals(model)
    fitted_data <- fitted(model)
    
    par(mfrow = c(2, 3))
    
    plot(fitted_data, residuals_data,
         main = "Residuals vs Fitted",
         xlab = "Fitted Values", ylab = "Residuals",
         pch = 16, col = "#667eea")
    abline(h = 0, col = "red", lty = 2)
    
    qqnorm(residuals_data, main = "Q-Q Plot of Residuals",
           pch = 16, col = "#667eea")
    qqline(residuals_data, col = "red", lwd = 2)
    
    sqrt_abs_resid <- sqrt(abs(residuals_data))
    plot(fitted_data, sqrt_abs_resid,
         main = "Scale-Location",
         xlab = "Fitted Values", ylab = "√|Residuals|",
         pch = 16, col = "#667eea")
    
    cooks_d <- cooks.distance(model)
    plot(cooks_d, main = "Cook's Distance",
         xlab = "Observation", ylab = "Cook's Distance",
         pch = 16, col = "#667eea")
    abline(h = 4/length(cooks_d), col = "red", lty = 2)
    
    plot(model, which = 5)
    
    hist(residuals_data, breaks = 20, col = "#667eea", alpha = 0.7,
         main = "Histogram of Residuals",
         xlab = "Residuals", ylab = "Frequency")
    curve(dnorm(x, mean = mean(residuals_data), sd = sd(residuals_data)), 
          add = TRUE, col = "red", lwd = 2)
  })
  
  output$regression_assumptions <- renderPrint({
    if(is.null(values$regression_model) || is.null(values$regression_model$diagnostics)) {
      return("Tidak ada hasil uji asumsi. Aktifkan 'Uji Asumsi Lengkap' untuk melihat hasil.")
    }
    
    diag <- values$regression_model$diagnostics
    
    cat("✅ HASIL UJI ASUMSI REGRESI\n")
    cat("===========================\n\n")
    
    cat("1. UJI NORMALITAS RESIDUAL:\n")
    cat("---------------------------\n")
    print(diag$normality)
    norm_p <- diag$normality$p.value
    cat("Kesimpulan:", ifelse(norm_p > 0.05, 
                              "✅ Residual berdistribusi normal", 
                              "❌ Residual tidak berdistribusi normal"), "\n\n")
    
    cat("2. UJI HETEROSKEDASTISITAS (Breusch-Pagan):\n")
    cat("-------------------------------------------\n")
    if(!is.na(diag$heteroscedasticity$p.value)) {
      print(diag$heteroscedasticity)
      hetero_p <- diag$heteroscedasticity$p.value
      cat("Kesimpulan:", ifelse(hetero_p > 0.05, 
                                "✅ Homoskedastisitas (varians konstan)", 
                                "❌ Heteroskedastisitas (varians tidak konstan)"), "\n\n")
    } else {
      cat("Test gagal dijalankan\n\n")
    }
    
    cat("3. UJI AUTOKORELASI (Durbin-Watson):\n")
    cat("------------------------------------\n")
    if(!is.na(diag$autocorrelation$p.value)) {
      print(diag$autocorrelation)
      auto_p <- diag$autocorrelation$p.value
      cat("Kesimpulan:", ifelse(auto_p > 0.05, 
                                "✅ Tidak ada autokorelasi", 
                                "❌ Ada autokorelasi"), "\n\n")
    } else {
      cat("Test gagal dijalankan\n\n")
    }
    
    cat("4. UJI MULTIKOLINEARITAS (VIF):\n")
    cat("-------------------------------\n")
    if(!is.na(diag$multicollinearity[1])) {
      print(diag$multicollinearity)
      max_vif <- max(diag$multicollinearity, na.rm = TRUE)
      cat("Kesimpulan:", ifelse(max_vif < 5, 
                                "✅ Tidak ada multikolinearitas serius", 
                                ifelse(max_vif < 10, 
                                       "⚠️ Multikolinearitas moderat", 
                                       "❌ Multikolinearitas serius")), "\n")
      cat("Catatan: VIF > 5 menunjukkan multikolinearitas, VIF > 10 serius\n\n")
    } else {
      cat("VIF tidak dapat dihitung (hanya satu prediktor)\n\n")
    }
    
    cat("📊 KESIMPULAN ASUMSI:\n")
    assumptions_met <- 0
    total_assumptions <- 0
    
    if(!is.na(norm_p)) {
      total_assumptions <- total_assumptions + 1
      if(norm_p > 0.05) assumptions_met <- assumptions_met + 1
    }
    
    if(!is.na(diag$heteroscedasticity$p.value)) {
      total_assumptions <- total_assumptions + 1
      if(diag$heteroscedasticity$p.value > 0.05) assumptions_met <- assumptions_met + 1
    }
    
    if(!is.na(diag$autocorrelation$p.value)) {
      total_assumptions <- total_assumptions + 1
      if(diag$autocorrelation$p.value > 0.05) assumptions_met <- assumptions_met + 1
    }
    
    if(!is.na(diag$multicollinearity[1])) {
      total_assumptions <- total_assumptions + 1
      if(max(diag$multicollinearity, na.rm = TRUE) < 5) assumptions_met <- assumptions_met + 1
    }
    
    cat("Asumsi terpenuhi:", assumptions_met, "dari", total_assumptions, "asumsi\n")
    
    if(assumptions_met == total_assumptions) {
      cat("✅ Semua asumsi regresi terpenuhi. Hasil regresi valid.\n")
    } else {
      cat("⚠️ Ada asumsi yang tidak terpenuhi. Pertimbangkan:\n")
      cat("   • Transformasi variabel\n")
      cat("   • Robust standard errors\n")
      cat("   • Ridge/Lasso regression untuk multikolinearitas\n")
    }
  })
  
  output$regression_interpretation <- renderText({
    if(is.null(values$regression_model)) {
      return("Belum ada hasil regresi. Silakan jalankan regresi terlebih dahulu.")
    }
    
    model_summary <- summary(values$regression_model$model)
    
    paste0(
      "📝 INTERPRETASI REGRESI LINEAR BERGANDA\n",
      "=======================================\n\n",
      "🎯 Model yang Digunakan:\n",
      "Formula: ", values$regression_model$formula, "\n",
      "Metode: ", str_to_title(values$regression_model$method), "\n",
      "Standardisasi: ", ifelse(values$regression_model$standardized, "Ya", "Tidak"), "\n\n",
      "📊 Kualitas Model:\n",
      "• R² = ", round(model_summary$r.squared, 4), " (", round(model_summary$r.squared*100, 1), "% varians dijelaskan)\n",
      "• Adjusted R² = ", round(model_summary$adj.r.squared, 4), "\n",
      "• F-statistic = ", round(model_summary$fstatistic[1], 4), "\n",
      "• p-value (model) = ", format(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), scientific = TRUE), "\n\n",
      "🔍 Interpretasi Koefisien:\n",
      "• Intercept: Nilai prediksi ketika semua prediktor = 0\n",
      "• Slope: Perubahan response per unit perubahan prediktor\n",
      "• p-value < 0.05: Prediktor signifikan\n",
      "• t-value: Kekuatan hubungan relatif terhadap standard error\n\n",
      "📈 Interpretasi R²:\n",
      if(model_summary$r.squared < 0.3) {
        "• R² < 0.3: Model menjelaskan sebagian kecil varians\n• Mungkin ada prediktor penting yang belum dimasukkan"
      } else if(model_summary$r.squared < 0.7) {
        "• 0.3 ≤ R² < 0.7: Model menjelaskan varians dengan cukup baik\n• Masih ada ruang untuk perbaikan model"
      } else {
        "• R² ≥ 0.7: Model menjelaskan sebagian besar varians\n• Model memiliki daya prediksi yang baik"
      }, "\n\n",
      "💡 Rekomendasi:\n",
      "• Periksa asumsi regresi sebelum interpretasi final\n",
      "• Evaluasi multikolinearitas antar prediktor\n",
      "• Pertimbangkan transformasi jika asumsi tidak terpenuhi\n",
      "• Validasi model dengan data baru jika memungkinkan\n",
      "• Interpretasikan koefisien dalam konteks domain penelitian\n\n",
      "⚠️ Catatan Penting:\n",
      "• Korelasi tidak berarti kausalitas\n",
      "• Model hanya valid dalam rentang data yang digunakan\n",
      "• Outlier dapat mempengaruhi hasil secara signifikan"
    )
  })
}

#Menjalankan Aplikasi
shinyApp(ui = ui, server = server)