# List of countries
Systems <- c("AT","BE","BG","HR","CY",
             "CZ","DK","EE","FI","FR",
             "DE","EL","HU","IE","IT",
             "LV","LT","LU","MT","NL",
             "NO","PL","PT","RO","SK",
             "SI","ES","SE","CH","UK",
             "EU")

# Full names
Systems.fullname <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                      "Czech Republic","Denmark","Estonia","Finland","France",
                      "Germany","Greece","Hungary","Ireland","Italy",
                      "Latvia","Lithuania","Luxembourg","Malta","Netherlands",
                      "Norway","Poland","Portugal","Romania","Slovakia",
                      "Slovenia","Spain","Sweden","Switzerland","United Kingdom",
                      "Europe")

# Directory where your TeX files are stored / will be generated
tex_dir <- "Charts"

# Aggregated plastics ##########################################################
# Loop over countries
for (i in seq_along(Systems)) {
  c <- Systems[i]
  fullname <- Systems.fullname[i]
  
  tex_file <- paste0("Chart_Aggregated_Plastic_", c, ".tex")
  
  # TeX content with ##1 for macros
  tex_content <- sprintf('
\\input{code_header}

%% For importing data
\\readdef{flow_%s_full.txt}\\myflowdef

\\input{code_dataimportation}

%% For importing data
\\newcommand{\\tottext}[1]{\\csname#1textPlastic\\endcsname}
\\newcommand{\\totwidth}[1]{\\csname#1widthPlastic\\endcsname}

\\input{code_tikzpicture}

\\node[font=\\Huge](title) at (0,20.9){\\textbf{Plastic emissions in %s}};

\\end{tikzpicture}
\\end{document}
', c, fullname)
  
  # Write TeX file in tex_dir
  writeLines(tex_content, file.path(tex_dir, tex_file))
  
  # Save current working directory
  old_wd <- getwd()
  
  # Set working directory to tex_dir
  setwd(tex_dir)

  # Compile TeX to PDF quietly
  tryCatch(
    {
      res <- system(
        paste("pdflatex -halt-on-error -interaction=nonstopmode", shQuote(tex_file)),
        ignore.stdout = TRUE,  # suppress normal output
        ignore.stderr = TRUE,  # suppress error output
        intern = FALSE
      )
      if (res != 0) stop("PDF compilation failed for ", fullname)
      message("Finished PDF for aggregated plastics in ", fullname)
    },
    error = function(e) message("Error: ", e$message)
  )
  
  # Remove temporary files
  temp_ext <- c(".aux", ".log", ".out", ".toc")
  for (ext in temp_ext) {
    f <- sub("\\.tex$", ext, tex_file)
    if (file.exists(f)) file.remove(f)
  }
  
  # Restore working directory
  setwd(old_wd)
}

# Polymers ####################################################################
# Loop over polymers and countries
for (mat in Materials) {
  for (i in seq_along(Systems)) {
    c <- Systems[i]
    fullname <- Systems.fullname[i]
    
    # Create TeX filename
    tex_file <- paste0("Chart_", mat, "_", c, ".tex")
    
    # Create TeX content
    tex_content <- sprintf('
\\input{code_header}

%% For importing data
\\readdef{flow_%s_full.txt}\\myflowdef

\\input{code_dataimportation}

%% For importing data
\\newcommand{\\tottext}[1]{\\csname#1text%s\\endcsname}
\\newcommand{\\totwidth}[1]{\\csname#1width%s\\endcsname}

\\input{code_tikzpicture}

\\node[font=\\Huge](title) at (0,20.9){\\textbf{%s in %s}};

\\end{tikzpicture}
\\end{document}
', c, mat, mat, mat, fullname)
    
    # Write TeX file
    writeLines(tex_content, file.path(tex_dir, tex_file))
    
    # Save current working directory
    old_wd <- getwd()
    setwd(tex_dir)
    
    # Compile TeX to PDF quietly
    tryCatch(
      {
        res <- system(
          paste("pdflatex -halt-on-error -interaction=nonstopmode", shQuote(tex_file)),
          ignore.stdout = TRUE,  # suppress normal output
          ignore.stderr = TRUE,  # suppress error output
          intern = FALSE
        )
        if (res != 0) stop("PDF compilation failed for ", mat, " in ", fullname)
        message("Finished PDF for ", mat, " in ", fullname)
      },
      error = function(e) message("Error: ", e$message)
    )
    
    # Remove temporary files
    temp_ext <- c(".aux", ".log", ".out", ".toc")
    for (ext in temp_ext) {
      f <- sub("\\.tex$", ext, tex_file)
      if (file.exists(f)) file.remove(f)
    }
    
    # Restore working directory
    setwd(old_wd)
  }
}

