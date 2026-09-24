#----------------------------------------------------------#
# Reprodukovatelne vykresleni rozhodovacich diagramu -----
#----------------------------------------------------------#
# Prevadi predlohy z draw.io (Diagrams/*.drawio) na obrazky pouzitelne
# v metodice: TikZ zdroj -> PDF (vektor, pro sazbu) -> PNG (rastr, pro nahled
# v Markdownu na GitHubu).
#
# PROC TAKTO:
# Diagramy byly v puvodni metodice (.docx) vlozene jen jako rastrove PNG
# vyexportovane rucne z draw.io. Zdrojem pravdy ale jsou soubory
# Diagrams/*.drawio, ktere jsou v repozitari a jsou to citelne XML. Tento
# skript z nich obrazky generuje znovu, takze:
#   - zmena diagramu = zmena .drawio predlohy + znovuspusteni skriptu,
#   - v repozitari neni zadny "osirely" obrazek bez zdroje,
#   - vysledek nezavisi na tom, kdo ma nainstalovanou desktopovou draw.io.
#
# Prevod zamerne NEmeni vzhled - geometrie, barvy, tloustky linek, velikosti
# pisma i tvary hrotu sipek se prebiraji z predlohy tak, aby vysledek odpovidal
# obrazkum v puvodnim .docx.
#
# ZAVISLOSTI:
#   - R: xml2 (cteni predloh), tinytex (sazba), pdftools (rastrovani)
#   - LaTeX: pdflatex + balicky standalone, pgf/tikz (dopln tinytex::tlmgr_install)
#
# ROZSAH PODPORY:
# Prevodnik neni obecny konvertor mxGraph -> TikZ. Umi prave to, co pouzivaji
# rozhodovaci stromy v metodikach: zaoblene obdelniky, kosoctverce, osove
# zarovnane hrany s prazdnym hrotem a popisky hran. Na cemkoli jinem zamerne
# spadne s chybou (viz dio_stop_nepodporovano), aby misto tise zdeformovaneho
# obrazku bylo videt, ze predloha prerostla moznosti skriptu.
#----------------------------------------------------------#

#----------------------------------------------------------#
# Nacteni knihoven -----
#----------------------------------------------------------#
packages <- c("xml2", "tinytex", "pdftools")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

#----------------------------------------------------------#
# Konstanty prevodu -----
#----------------------------------------------------------#
# 1 px predlohy = 1 pt v TikZ. Prevod je zamerne 1:1, takze vsechny pomery
# (velikost pisma vs. velikost bloku) zustavaji stejne jako v draw.io; vysledny
# obrazek se pak zmensuje az pri vlozeni do dokumentu (width=\linewidth),
# stejne jako se v .docx zmensovalo vlozene PNG.
DIO_PX <- 1

# Zaobleni rohu obdelniku. draw.io pro rounded=1 pouziva relativni arcSize
# 15 % z kratsi strany, maximalne vsak DIO_ARC_MAX px.
DIO_ARC_REL <- 0.15
DIO_ARC_MAX <- 8

# Vychozi hodnoty draw.io pro vlastnosti, ktere predloha nemusi uvadet
DIO_DEFAULT_FONTSIZE <- 12
DIO_DEFAULT_FONTCOLOR <- "000000"
DIO_DEFAULT_STROKE <- "000000"
DIO_DEFAULT_ARROWSIZE <- 6

# Radkovani textu v blocich (draw.io sazi radky s ~1.2 nasobkem velikosti pisma)
DIO_LINE_SPACING <- 1.2

#----------------------------------------------------------#
# Pomocne funkce -----
#----------------------------------------------------------#

dio_stop_nepodporovano <- function(...) {
  stop(
    "Predloha obsahuje prvek, ktery prevodnik neumi: ", ...,
    "\nBud prvek v predloze zjednoduste, nebo doplnte podporu v ",
    "R/05_metodiky/51_metodika_diagramy.R.",
    call. = FALSE
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

# Rozklad draw.io stylu "klic=hodnota;klic2=hodnota2;priznak;" na seznam.
# Klice bez "=" (napr. "rhombus") se berou jako priznak s hodnotou "1".
dio_style <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(list())

  parts <- strsplit(x, ";", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return(list())

  ma <- grepl("=", parts, fixed = TRUE)
  keys <- ifelse(ma, sub("=.*$", "", parts), parts)
  vals <- ifelse(ma, sub("^[^=]*=", "", parts), "1")

  stats::setNames(as.list(trimws(vals)), trimws(keys))
}

# Barva z draw.io stylu na sestimistny hex bez "#".
# Resi i zapis light-dark(#svetla, #tmava) z novejsich verzi draw.io - bere se
# svetla varianta, protoze metodika se sazi na bily papir.
dio_color <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NA_character_)
  if (identical(tolower(x), "none")) return(NA_character_)

  m <- regmatches(x, regexec("light-dark\\(\\s*([^,)]+)", x))[[1]]
  if (length(m) == 2) x <- trimws(m[2])

  x <- sub("^#", "", trimws(x))
  if (!grepl("^[0-9A-Fa-f]{6}$", x)) {
    dio_stop_nepodporovano("barva '", x, "' (ocekavan hex #RRGGBB)")
  }
  toupper(x)
}

# Escapovani znaku, ktere maji v LaTeXu zvlastni vyznam.
# Ceska diakritika se nechava v UTF-8 (dokument se sazi s inputenc/T1).
dio_tex_escape <- function(x) {
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  for (ch in c("&", "%", "$", "#", "_", "{", "}")) {
    x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
  }
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
  x
}

# Dekodovani HTML entit, ktere draw.io pouziva v popiscich
dio_unescape_html <- function(x) {
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x
}

#----------------------------------------------------------#
# Popisky: HTML -> radky formatovanych useku -----
#----------------------------------------------------------#
# draw.io uklada popisky jako drobne HTML, napr.
#   <font style="font-weight: normal;">Je dosazen<br>cilovy stav </font>procenta
# Vysledkem prevodu je seznam radku; kazdy radek je seznam useku (runs)
# s textem a priznaky bold/italic. Tucne/netucne useky se tak v sazbe
# zachovaji presne tak, jak je ma predloha - coz je u tehle diagramu podstatne,
# protoze tucne je vzdy prave klicova cast otazky.

# Zjisteni, jak otevirajici tag meni tloustku pisma.
# Vraci TRUE (tucne), FALSE (netucne) nebo NA (dedi se z okoli).
dio_tag_bold <- function(tag) {
  name <- tolower(sub("^<\\s*([a-zA-Z]+).*$", "\\1", tag))
  if (name == "b" || name == "strong") return(TRUE)

  w <- regmatches(tag, regexec("font-weight\\s*:\\s*([a-zA-Z0-9]+)", tag))[[1]]
  if (length(w) == 2) {
    w <- tolower(w[2])
    if (w %in% c("normal", "400", "300", "200", "100")) return(FALSE)
    if (w %in% c("bold", "bolder", "500", "600", "700", "800", "900")) return(TRUE)
  }
  NA
}

dio_tag_italic <- function(tag) {
  name <- tolower(sub("^<\\s*([a-zA-Z]+).*$", "\\1", tag))
  if (name == "i" || name == "em") return(TRUE)

  w <- regmatches(tag, regexec("font-style\\s*:\\s*([a-zA-Z]+)", tag))[[1]]
  if (length(w) == 2) {
    w <- tolower(w[2])
    if (w == "italic" || w == "oblique") return(TRUE)
    if (w == "normal") return(FALSE)
  }
  NA
}

dio_label_lines <- function(html, base_bold = FALSE, base_italic = FALSE) {
  if (is.null(html) || is.na(html) || !nzchar(trimws(html))) return(list())

  # Blokove tagy a <br> ukoncuji radek; sentinel \001 oznaci zlom
  s <- html
  s <- gsub("<br\\s*/?>", "\001", s, ignore.case = TRUE)
  s <- gsub("</\\s*(div|p|li)\\s*>", "\001", s, ignore.case = TRUE)
  s <- gsub("<\\s*(div|p|ul|ol|li)[^>]*>", "", s, ignore.case = TRUE)

  lines <- list()
  cur <- list()
  stack <- list()

  # Aktualni formatovani = posledni nezdedena hodnota na zasobniku
  eff <- function(key, base) {
    val <- base
    for (fr in stack) if (!is.na(fr[[key]])) val <- fr[[key]]
    val
  }

  push_text <- function(txt) {
    if (!nzchar(txt)) return(invisible(NULL))
    cur[[length(cur) + 1]] <<- list(
      text   = txt,
      bold   = eff("bold", base_bold),
      italic = eff("italic", base_italic)
    )
  }

  end_line <- function() {
    lines[[length(lines) + 1]] <<- cur
    cur <<- list()
  }

  # Prochazeni stringu: strida se text a tagy/zlomy
  pos <- 1
  n <- nchar(s)
  while (pos <= n) {
    rest <- substr(s, pos, n)
    m <- regexpr("\001|<[^>]*>", rest)
    if (m == -1) {
      push_text(dio_unescape_html(rest))
      break
    }

    if (m > 1) push_text(dio_unescape_html(substr(rest, 1, m - 1)))

    tok <- substr(rest, m, m + attr(m, "match.length") - 1)
    pos <- pos + m + attr(m, "match.length") - 2

    if (identical(tok, "\001")) {
      end_line()
    } else if (grepl("^</", tok)) {
      if (length(stack) > 0) stack[[length(stack)]] <- NULL
    } else if (grepl("/>$", tok)) {
      # samouzavirajici tag bez obsahu - nic nemeni
    } else {
      stack[[length(stack) + 1]] <- list(
        bold   = dio_tag_bold(tok),
        italic = dio_tag_italic(tok)
      )
    }
    pos <- pos + 1
  }

  end_line()

  # Prazdne radky na zacatku a konci (dusledek <div> obalek) se zahodi
  nonempty <- vapply(lines, function(l) {
    length(l) > 0 && any(nzchar(trimws(vapply(l, `[[`, "", "text"))))
  }, logical(1))
  if (!any(nonempty)) return(list())
  lines[seq(min(which(nonempty)), max(which(nonempty)))]
}

# Radky formatovanych useku -> jeden TikZ argument s "\\" mezi radky
dio_lines_to_tex <- function(lines) {
  if (length(lines) == 0) return("")

  out <- vapply(lines, function(runs) {
    paste0(vapply(runs, function(r) {
      # Nezlomitelna mezera z &nbsp; musi v LaTeXu zustat nezlomitelna
      txt <- dio_tex_escape(r$text)
      txt <- gsub(" ", "~", txt, fixed = TRUE)
      if (isTRUE(r$bold))   txt <- paste0("\\textbf{", txt, "}")
      if (isTRUE(r$italic)) txt <- paste0("\\textit{", txt, "}")
      txt
    }, ""), collapse = "")
  }, "")

  paste(out, collapse = "\\\\")
}

#----------------------------------------------------------#
# Cteni predlohy -----
#----------------------------------------------------------#

dio_read <- function(path) {
  if (!file.exists(path)) stop("Predloha nenalezena: ", path, call. = FALSE)

  doc <- xml2::read_xml(path)

  # Komprimovane .drawio (mxfile s base64 obsahem) skript necte - v repozitari
  # jsou predlohy ulozene jako citelne XML a takove maji byt, aby se daly
  # verzovat a porovnavat.
  if (length(xml2::xml_find_all(doc, "//mxGraphModel")) == 0) {
    dio_stop_nepodporovano(
      "predloha '", basename(path), "' neobsahuje mxGraphModel ",
      "(pravdepodobne je ulozena komprimovane - v draw.io vypnete ",
      "Extras > Edit Diagram > komprimaci, resp. ulozte jako .drawio s XML)"
    )
  }

  diagrams <- xml2::xml_find_all(doc, "//diagram")
  if (length(diagrams) > 1) {
    dio_stop_nepodporovano(
      "predloha ma ", length(diagrams), " stranek; ocekava se jedna"
    )
  }

  cells <- xml2::xml_find_all(doc, "//mxGraphModel//mxCell")

  vertices <- list()
  edges <- list()

  for (cell in cells) {
    id    <- xml2::xml_attr(cell, "id")
    style <- dio_style(xml2::xml_attr(cell, "style"))
    value <- xml2::xml_attr(cell, "value")
    geom  <- xml2::xml_find_first(cell, "./mxGeometry")

    num <- function(node, attr, default = NA_real_) {
      if (inherits(node, "xml_missing")) return(default)
      v <- suppressWarnings(as.numeric(xml2::xml_attr(node, attr)))
      if (is.na(v)) default else v
    }

    if (identical(xml2::xml_attr(cell, "vertex"), "1")) {
      if (inherits(geom, "xml_missing") || is.na(num(geom, "width"))) {
        dio_stop_nepodporovano("uzel '", id, "' bez rozmeru v mxGeometry")
      }
      vertices[[id]] <- list(
        id    = id,
        value = value,
        style = style,
        x = num(geom, "x", 0), y = num(geom, "y", 0),
        w = num(geom, "width"), h = num(geom, "height")
      )

    } else if (identical(xml2::xml_attr(cell, "edge"), "1")) {
      sp <- xml2::xml_find_first(cell, "./mxGeometry/mxPoint[@as='sourcePoint']")
      tp <- xml2::xml_find_first(cell, "./mxGeometry/mxPoint[@as='targetPoint']")
      off <- xml2::xml_find_first(cell, "./mxGeometry/mxPoint[@as='offset']")
      pts <- xml2::xml_find_all(cell, "./mxGeometry/Array[@as='points']/mxPoint")

      if (length(pts) > 0) {
        dio_stop_nepodporovano(
          "hrana '", id, "' ma rucne vlozene zlomove body (Array/points)"
        )
      }

      edges[[length(edges) + 1]] <- list(
        id     = id,
        value  = value,
        style  = style,
        source = xml2::xml_attr(cell, "source"),
        target = xml2::xml_attr(cell, "target"),
        sx = num(sp, "x"), sy = num(sp, "y"),
        tx = num(tp, "x"), ty = num(tp, "y"),
        # relativni poloha popisku na hrane a jeho odsazeni od linky
        lab_pos = num(geom, "x", 0),
        lab_off = num(geom, "y", 0),
        off_x = num(off, "x", 0), off_y = num(off, "y", 0)
      )
    }
  }

  if (length(vertices) == 0) stop("Predloha neobsahuje zadne uzly: ", path, call. = FALSE)

  list(vertices = vertices, edges = edges, path = path)
}

#----------------------------------------------------------#
# Geometrie hran -----
#----------------------------------------------------------#

# Bod na obalce uzlu; draw.io kotvi exitX/entryX na bounding box, takze
# pro kosoctverec 0.5/1 odpovida spodnimu vrcholu, 0/0.5 levemu atd.
dio_anchor <- function(v, ax, ay) {
  c(v$x + ax * v$w, v$y + ay * v$h)
}

dio_edge_points <- function(edge, vertices) {
  st <- edge$style

  get_anchor <- function(id, px, py, fallback_x, fallback_y, kind) {
    if (!is.na(id) && !is.null(vertices[[id]])) {
      ax <- suppressWarnings(as.numeric(st[[px]]))
      ay <- suppressWarnings(as.numeric(st[[py]]))
      if (is.na(ax) || is.na(ay)) {
        dio_stop_nepodporovano(
          "hrana '", edge$id, "' nema urceny ", kind,
          " (chybi ", px, "/", py, "); automaticke routovani skript neumi"
        )
      }
      return(dio_anchor(vertices[[id]], ax, ay))
    }
    if (!is.na(fallback_x) && !is.na(fallback_y)) return(c(fallback_x, fallback_y))
    dio_stop_nepodporovano("hrana '", edge$id, "' nema ", kind)
  }

  p0 <- get_anchor(edge$source, "exitX", "exitY", edge$sx, edge$sy, "zacatek")
  p1 <- get_anchor(edge$target, "entryX", "entryY", edge$tx, edge$ty, "konec")

  # Skript kresli hrany jako jednu useckou; osove zarovnani je proto podminka.
  # Tolerance 0.5 px pokryva zaokrouhleni souradnic v predloze.
  if (abs(p0[1] - p1[1]) > 0.5 && abs(p0[2] - p1[2]) > 0.5) {
    dio_stop_nepodporovano(
      "hrana '", edge$id, "' neni osove zarovnana (",
      sprintf("%.1f,%.1f -> %.1f,%.1f", p0[1], p0[2], p1[1], p1[2]),
      "); lomene hrany skript nekresli"
    )
  }

  # Dokonale svisle/vodorovne srovnani, aby linky nebyly o zlomek px sikme
  if (abs(p0[1] - p1[1]) <= 0.5) p1[1] <- p0[1]
  if (abs(p0[2] - p1[2]) <= 0.5) p1[2] <- p0[2]

  list(p0 = p0, p1 = p1)
}

# Poloha popisku hrany podle mxGraph: podel hrany relativni pozice
# t = x/2 + 0.5, k tomu odsazeni po normale (dy, -dx)/len nasobene hodnotou y.
dio_edge_label_point <- function(edge, p0, p1) {
  d <- p1 - p0
  len <- sqrt(sum(d^2))
  if (len == 0) return(p0)

  t <- edge$lab_pos / 2 + 0.5
  base <- p0 + d * t
  normal <- c(d[2], -d[1]) / len

  base + normal * edge$lab_off + c(edge$off_x, edge$off_y)
}

#----------------------------------------------------------#
# Prevod na TikZ -----
#----------------------------------------------------------#

dio_font_cmd <- function(size_px) {
  sprintf(
    "\\fontsize{%.2f}{%.2f}\\selectfont",
    size_px * DIO_PX, size_px * DIO_PX * DIO_LINE_SPACING
  )
}

dio_to_tikz <- function(model) {
  vertices <- model$vertices
  edges <- model$edges

  # Definice barev - kazda pouzita barva jako pojmenovana LaTeX barva
  colors <- character(0)
  color_name <- function(hex) {
    if (is.na(hex)) return(NA_character_)
    nm <- paste0("dio", hex)
    if (!nm %in% names(colors)) colors[[nm]] <<- hex
    nm
  }

  body <- character(0)

  #--------------------------------------------------#
  ## Uzly ----
  #--------------------------------------------------#
  for (v in vertices) {
    st <- v$style
    fill <- color_name(dio_color(st$fillColor %||% "FFFFFF"))
    stroke <- color_name(dio_color(st$strokeColor %||% DIO_DEFAULT_STROKE))

    cx <- v$x + v$w / 2
    cy <- v$y + v$h / 2

    draw_opts <- c(
      if (!is.na(fill)) paste0("fill=", fill),
      if (!is.na(stroke)) paste0("draw=", stroke) else "draw=none",
      sprintf("line width=%.2fpt", as.numeric(st$strokeWidth %||% "1") * DIO_PX)
    )

    is_rhombus <- isTRUE(!is.null(st$rhombus)) ||
      identical(st$shape, "rhombus")

    if (is_rhombus) {
      # Kosoctverec se kresli jako cesta, nikoli jako TikZ node - node by se
      # pri delsim textu sam zvetsil a rozhodil geometrii predlohy.
      body <- c(body, sprintf(
        "  \\path[%s] (%.2f,%.2f) -- (%.2f,%.2f) -- (%.2f,%.2f) -- (%.2f,%.2f) -- cycle;",
        paste(draw_opts, collapse = ", "),
        cx, v$y, v$x + v$w, cy, cx, v$y + v$h, v$x, cy
      ))
    } else {
      arc <- if (!is.null(st$rounded) && identical(st$rounded, "1")) {
        min(DIO_ARC_MAX, DIO_ARC_REL * min(v$w, v$h))
      } else 0

      body <- c(body, sprintf(
        "  \\path[%s%s] (%.2f,%.2f) rectangle (%.2f,%.2f);",
        paste(draw_opts, collapse = ", "),
        if (arc > 0) sprintf(", rounded corners=%.2fpt", arc * DIO_PX) else "",
        v$x, v$y, v$x + v$w, v$y + v$h
      ))
    }

    #------------------------------------------#
    ### Text uzlu ----
    #------------------------------------------#
    lines <- dio_label_lines(
      v$value,
      base_bold = identical(st$fontStyle %||% "0", "1")
    )
    if (length(lines) == 0) next

    fsize <- as.numeric(st$fontSize %||% as.character(DIO_DEFAULT_FONTSIZE))
    fcol <- color_name(dio_color(st$fontColor %||% DIO_DEFAULT_FONTCOLOR))

    # spacingTop posouva text v bloku svisle (v predloze -4 px = o 4 px vys)
    ty <- cy + as.numeric(st$spacingTop %||% "0")

    # whiteSpace=wrap - draw.io zalamuje prilis dlouhy radek na sirku uzlu
    # zmensenou o vnitrni odsazeni (spacing, vychozi 2 px). Bez zalamovani by
    # text pretekal pres okraj tvaru, coz predloha nedela.
    wrap <- if (identical(st$whiteSpace %||% "", "wrap")) {
      sprintf(
        ", text width=%.2fpt",
        max(1, v$w - 2 * as.numeric(st$spacing %||% "2")) * DIO_PX
      )
    } else ""

    body <- c(body, sprintf(
      "  \\node[align=center, inner sep=0pt, outer sep=0pt, text=%s, font=%s%s] at (%.2f,%.2f) {%s};",
      fcol, dio_font_cmd(fsize), wrap, cx, ty, dio_lines_to_tex(lines)
    ))
  }

  #--------------------------------------------------#
  ## Hrany ----
  #--------------------------------------------------#
  for (e in edges) {
    st <- e$style

    if (!identical(st$endArrow %||% "block", "block")) {
      dio_stop_nepodporovano(
        "hrana '", e$id, "' ma hrot '", st$endArrow, "' (podporovan jen 'block')"
      )
    }

    pts <- dio_edge_points(e, vertices)
    p0 <- pts$p0
    p1 <- pts$p1

    stroke <- color_name(dio_color(st$strokeColor %||% DIO_DEFAULT_STROKE))
    lw <- as.numeric(st$strokeWidth %||% "1") * DIO_PX
    asize <- as.numeric(st$endSize %||% as.character(DIO_DEFAULT_ARROWSIZE)) * DIO_PX

    # endFill=0 = prazdny hrot. Vypln je bila (ne "none"), aby linka pod
    # hrotem neprosvitala - presne tak, jak hrot vykresluje draw.io.
    arrow_fill <- if (identical(st$endFill %||% "1", "0")) "white" else stroke

    body <- c(body, sprintf(
      paste0(
        "  \\draw[draw=%s, line width=%.2fpt, ",
        "-{Triangle[length=%.2fpt, width=%.2fpt, color=%s, fill=%s, line width=%.2fpt]}",
        "] (%.2f,%.2f) -- (%.2f,%.2f);"
      ),
      stroke, lw, asize, asize * 0.9, stroke, arrow_fill, lw,
      p0[1], p0[2], p1[1], p1[2]
    ))

    #------------------------------------------#
    ### Popisek hrany ----
    #------------------------------------------#
    lines <- dio_label_lines(
      e$value,
      base_bold = identical(st$fontStyle %||% "0", "1")
    )
    if (length(lines) == 0) next

    lp <- dio_edge_label_point(e, p0, p1)
    fsize <- as.numeric(st$fontSize %||% as.character(DIO_DEFAULT_FONTSIZE))
    fcol <- color_name(dio_color(st$fontColor %||% DIO_DEFAULT_FONTCOLOR))

    body <- c(body, sprintf(
      "  \\node[align=center, inner sep=0pt, outer sep=0pt, text=%s, font=%s] at (%.2f,%.2f) {%s};",
      fcol, dio_font_cmd(fsize), lp[1], lp[2], dio_lines_to_tex(lines)
    ))
  }

  #--------------------------------------------------#
  ## Slozeni dokumentu ----
  #--------------------------------------------------#
  color_defs <- vapply(
    names(colors),
    function(nm) sprintf("\\definecolor{%s}{HTML}{%s}", nm, colors[[nm]]),
    ""
  )

  c(
    "% Tento soubor je GENEROVANY - needitujte ho.",
    paste0("% Zdroj: ", basename(model$path)),
    "% Generuje R/05_metodiky/51_metodika_diagramy.R z predlohy v Diagrams/.",
    "\\documentclass[border=2pt]{standalone}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}",
    "\\usepackage[scaled]{helvet}",
    "\\renewcommand{\\familydefault}{\\sfdefault}",
    "\\usepackage{tikz}",
    "\\usetikzlibrary{arrows.meta}",
    "% draw.io slova nedeli - zakaz deleni drzi zlomy radku na stejnych mistech",
    "\\hyphenpenalty=10000",
    "\\exhyphenpenalty=10000",
    "\\hbadness=10000",
    unname(color_defs),
    "\\begin{document}",
    # y=-1pt prevraci osu, takze lze pouzivat souradnice z predlohy beze zmeny
    sprintf(
      "\\begin{tikzpicture}[x=%.4fpt, y=-%.4fpt, line cap=butt, line join=miter]",
      DIO_PX, DIO_PX
    ),
    body,
    "\\end{tikzpicture}",
    "\\end{document}"
  )
}

#----------------------------------------------------------#
# Vykresleni jedne predlohy -----
#----------------------------------------------------------#
# Postup: .drawio -> .tex (TikZ) -> .pdf (pdflatex) -> .png (pdftools).
# PDF se pouziva pro sazbu metodiky, PNG pro nahled Markdownu na GitHubu.

dio_render <- function(drawio_path, out_dir, dpi = 300, quiet = TRUE) {
  stopifnot(dir.exists(out_dir))

  name <- tools::file_path_sans_ext(basename(drawio_path))
  tex_path <- file.path(out_dir, paste0(name, ".tex"))
  pdf_path <- file.path(out_dir, paste0(name, ".pdf"))
  png_path <- file.path(out_dir, paste0(name, ".png"))

  message("Diagram: ", basename(drawio_path))

  model <- dio_read(drawio_path)
  tex <- dio_to_tikz(model)
  writeLines(tex, tex_path, useBytes = TRUE)

  # latexmk z tinytexu dorovnava chybejici LaTeX balicky sam
  owd <- setwd(out_dir)
  on.exit(setwd(owd), add = TRUE)
  tinytex::latexmk(basename(tex_path), engine = "pdflatex", clean = TRUE)
  setwd(owd)

  if (!file.exists(pdf_path)) stop("Sazba diagramu neprosla: ", tex_path, call. = FALSE)

  # Rastr pres pdftools (vlastni poppler), aby prevod nezavisel na externim nastroji
  pdftools::pdf_convert(
    pdf_path, format = "png", pages = 1, dpi = dpi,
    filenames = png_path, verbose = FALSE
  )

  info <- pdftools::pdf_pagesize(pdf_path)
  message(sprintf(
    "  -> %s (%.0f x %.0f pt), %s (%d dpi)",
    basename(pdf_path), info$width[1], info$height[1], basename(png_path), dpi
  ))

  list(tex = tex_path, pdf = pdf_path, png = png_path)
}

#----------------------------------------------------------#
# Spusteni -----
#----------------------------------------------------------#
# Diagramy metodiky obojzivelniku. Nazvy predloh odpovidaji obrazkum, ktere
# byly vlozene v .docx (nazev byl dohledatelny z alt textu obrazku):
#   strom_obojzivelnici_lok_long - rozhodovani na urovni dilci plochy
#   strom_obojzivelnici_long     - rozhodovani na urovni sledovaneho uzemi

metodika_diagramy <- function(
    drawio_dir = "Diagrams",
    out_dir = "Metodiky/Obojzivelnici/diagramy",
    predlohy = c("strom_obojzivelnici_lok_long", "strom_obojzivelnici_long"),
    dpi = 300
) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  invisible(lapply(predlohy, function(nm) {
    dio_render(file.path(drawio_dir, paste0(nm, ".drawio")), out_dir, dpi = dpi)
  }))
}

if (sys.nframe() == 0) metodika_diagramy()
