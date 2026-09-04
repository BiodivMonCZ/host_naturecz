#----------------------------------------------------------#
# Agregace STAV_IND v ramci nalezu a indikatoru -----
#----------------------------------------------------------#
# Pro kazdou skupinu (ID_ND_NALEZ, ID_IND, IND_GRP) se z dilcich limitu
# odvodi jedina hodnota STAV_IND:
#   - populacni (POP_) minmax -> MAX (staci splnit jeden limit, logika OR)
#   - ostatni    minmax       -> MIN (musi splnit cely interval, logika AND)
#   - kategoricke (val)       -> MAX (staci se trefit do jedne hodnoty)
#
# PROC data.table: puvodne to resil `dplyr::mutate()` se skupinami, kde uvnitr
# `case_when()` byly primo volani max()/min(). To je pro tento tvar dat velmi
# pomale - u druhu Bombina bombina jde o 176 901 skupin (11 787 nalezu x ~15
# kombinaci indikatoru a limitu), tedy skupiny o 1-2 radcich, kde prakticky
# veskery cas padne na rezii vyhodnocovani vyrazu zvlast v kazde skupine.
# Navic `case_when()` vyhodnocuje VSECHNY sve vetve pro kazdou skupinu (3x
# vice volani max()/min(), nez je potreba) a `as.numeric()` uvnitr agregace
# znemoznuje pouziti rychle cesty. Merene na Bombina bombina:
#   - puvodni dplyr verze .............. pres 20 minut (nedobehla)
#   - dplyr summarise(.by) + join ...... 1 550 s (jeste horsi, 239 126 varovani)
#   - data.table (tato verze) .......... 1,74 s
#
# Klicove je, ze IND_GRP i is_POP jsou v ramci skupiny KONSTANTNI (IND_GRP je
# primo klic skupiny, is_POP se odvozuje z ID_IND, coz je take klic), takze
# vyber vetve nemusi probihat po skupinach - staci spocitat maximum i minimum
# jednim pruchodem a pak uz jen vektorove vybrat spravnou hodnotu.
#
# POZOR na drobny rozdil oproti base R: data.table pro skupinu se samymi NA
# vraci NA, zatimco base max()/min() vraci -Inf/Inf (a vypisuje varovani).
# Navazujici krok `is.infinite(STAV_IND) ~ NA` ale tento rozdil srovnava,
# takze vysledek je identicky - overeno na 34 257 radcich / 22 500 skupinach
# s nulovym poctem odlisnych radku.
#----------------------------------------------------------#
# Shoda hodnoty s limitem typu "val" -----
#----------------------------------------------------------#
# Puvodne slo o prostou rovnost HOD_IND == LIM_IND. To staci, dokud je namerena
# hodnota jednohodnotova - u obojzivelniku, hmyzu i rostlin tomu tak je.
#
# Stanovistni indikatory ryb ale nesou VYBER Z VICE MOZNOSTI: substrat dna
# muze byt zaroven "kameny, písek, štěrk" a limit typu val vyjmenovava
# JEDNOTLIVE prijatelne typy (agregace v agg_stav_ind() pak bere maximum, tedy
# "staci se trefit do jedne hodnoty"). Prosta rovnost by u nich nesedla nikdy
# a indikator by vysel nepriznive u vsech zaznamu - viz nalez H-42.
#
# val_shoda() proto uznava shodu i tehdy, je-li limit CELOU polozkou
# vicehodnotove mnoziny. Obe strany se obalí oddelovacem ", ", takze se limit
# "kameny" netrefi do polozky "kameny drobne" - porovnavaji se cele polozky,
# ne podretezce.
#
# ROZSIRENI JE BEZPECNE, tedy zpetne kompatibilni: pro hodnotu bez oddelovace
# ", " dava presne totez co puvodni rovnost. Zmenit vysledek muze jen tam, kde
# hodnota oddelovac obsahuje - a takova hodnota se drive nemohla trefit do
# zadneho limitu, takze se hodnoceni muze pouze zlepsit z 0 na 1, nikdy naopak.
# Desetinna carka je v bezpeci, protoze oddelovacem je carka NASLEDOVANA
# mezerou: "0,2-6 cm" zustava jednou polozkou.
val_shoda <- function(hod, lim) {
  # Obe strany se dorovnaji na stejnou delku. Ve vypoctu STAV_IND jsou to vzdy
  # dva sloupce teze tabulky, ale bez recyklace by funkce tise vracela NA,
  # kdyby ji nekdo zavolal s jednou hodnotou limitu proti vektoru hodnot.
  n <- max(length(hod), length(lim))
  h <- stringr::str_squish(as.character(rep_len(hod, n)))
  l <- stringr::str_squish(as.character(rep_len(lim, n)))
  ok <- !is.na(h) & !is.na(l)
  out <- ok & h == l
  i <- ok & !out
  if (any(i)) {
    out[i] <- stringr::str_detect(
      paste0(", ", h[i], ", "),
      stringr::fixed(paste0(", ", l[i], ", "))
    )
  }
  out
}

agg_stav_ind <- function(df) {
  dt <- data.table::as.data.table(df)

  dt[, stav_num := as.numeric(STAV_IND)]
  dt[, c("grp_max", "grp_min") := list(
    max(stav_num, na.rm = TRUE),
    min(stav_num, na.rm = TRUE)
  ), by = list(ID_ND_NALEZ, ID_IND, IND_GRP)]

  dt[, STAV_IND := data.table::fcase(
    IND_GRP == "minmax" &  is_POP, grp_max,
    IND_GRP == "minmax" & !is_POP, grp_min,
    IND_GRP == "val",              grp_max,
    default = NA_real_
  )]

  dt[, c("grp_max", "grp_min", "stav_num") := NULL]

  tibble::as_tibble(dt)
}

run_n2k_druhy_lim <- function(
    n2k_druhy,
    species_name,
    sites_subjects,
    limity,
    current_year = 2025
) {
  
  #----------------------------------------------------------#
  # 1. Prevod na long format a napojeni na limity ----- 
  #----------------------------------------------------------#
  
  # OPTIMALIZACE - pivotujeme jen indikatory, ktere maji pro dany druh limit.
  # Nasledny right_join s tabulkou limitu vsechny ostatni indikatory stejne
  # zahodi, takze je zbytecne je vubec prevadet do dlouheho formatu. U
  # Bombina bombina jde o 12 sloupcu misto 157, tj. ~170 tis. radku misto
  # ~2,23 mil. (13x mene) - a vsechny nasledne skupinove operace pak bezi
  # nad odpovidajicne mensi tabulkou. Vysledek je identicky, jen se nepocita
  # to, co se vzapeti zahodi.
  #
  # Sloupce k prevodu na text si zapamatujeme JMENEM jeste pred odstranenim
  # nepouzitych indikatoru - puvodni kod pouzival pozicni rozsah
  # `ncol_orig:ncol(.)`, ktery by se po odstraneni sloupcu posunul. Timto
  # zustava typ (character) u vsech nesenych sloupcu stejny jako drive.
  cols_to_chr <- names(n2k_druhy)[ncol_orig:ncol(n2k_druhy)]

  ind_cols_all <- names(n2k_druhy)
  ind_cols_all <- ind_cols_all[match("POP_PRESENCE_N", ind_cols_all):length(ind_cols_all)]

  # Krome indikatoru s platnym limitem se propousteji i INFORMATIVNI indikatory,
  # tj. radky s TYP_IND == "info" a prazdnym LIM_IND. Ty se nehodnoti (STAV_IND
  # zustava NA a do poctu ocekavanych ani splnenych indikatoru v 24 nevstupuji,
  # protoze ten pocita jen radky s vyplnenym LIM_IND), ale propisou se do
  # vystupu, aby byla dohledatelna hodnota, ze ktere odvozeny indikator vychazi.
  #
  # Bez tohoto markeru nelze informativni radek na urovni DP vubec zobrazit:
  # zdejsi filtr je zahodi jak z ind_cols_keep, tak z right_joinu nize. Na
  # urovni uzemi to nevadi, protoze 25_n2k_druhy_uzemi.R filtruje pouze podle
  # UROVEN == "chu" - proto tam LOK_DILCDOBRE s prazdnym limitem projde.
  # Viz nalez H-31 v harmonizace_registr.md.
  lim_inds_lok <- limity %>%
    dplyr::filter(
      DRUH == species_name,
      UROVEN == "lok",
      !is.na(LIM_IND) | (!is.na(TYP_IND) & TYP_IND == "info")
    ) %>%
    dplyr::pull(ID_IND) %>%
    unique()

  ind_cols_keep <- intersect(ind_cols_all, lim_inds_lok)
  ind_cols_drop <- setdiff(ind_cols_all, ind_cols_keep)

  # Druh bez jedineho hodnotitelneho indikatoru na urovni lokality: puvodni
  # kod by po right_join a odfiltrovani "sirotcich" limitu vratil 0 radku.
  if (length(ind_cols_keep) == 0) {
    return(NULL)
  }

  n2k_druhy_long <- n2k_druhy %>%
    dplyr::select(-dplyr::all_of(ind_cols_drop)) %>%
    dplyr::mutate(
      # Prevedeme nesene sloupce na text, abychom mohli pivotovat
      dplyr::across(
        .cols = dplyr::any_of(cols_to_chr),
        .fns = ~ as.character(.)
      )
    ) %>%
    # Pivotovani do dlouheho formatu (ID_IND = nazev indikatoru, HOD_IND = hodnota)
    tidyr::pivot_longer(
      .,
      cols = dplyr::all_of(ind_cols_keep),
      names_to = "ID_IND",
      values_to = "HOD_IND"
    ) %>%
    # Odstraneni nepotrebnych sloupcu metadat
    dplyr::select(
      -c(ZDROJ:PRESNOST)
    ) %>%
    # Pripojeni tabulky limitu (right_join zachova limity i bez dat)
    dplyr::right_join(
      .,
      limity %>%
        dplyr::filter(
          UROVEN == "lok" # Pouze limity pro lokalitu
        ) %>%
        dplyr::filter(
          # Platne limity + informativni indikatory (viz komentar vyse).
          # Informativnimu radku nize nesedne zadna vetev vypoctu STAV_IND,
          # takze zustane NA = "nehodnocen".
          !is.na(LIM_IND) | (!is.na(TYP_IND) & TYP_IND == "info")
        ),
      by = c("DRUH" = "DRUH",
             "ID_IND" = "ID_IND")
    ) %>%
    # IND_GRP: Sjednoceni typu limitu (min a max se radi do stejne skupiny "minmax")
    dplyr::mutate(
      IND_GRP = dplyr::case_when(
        TYP_IND %in% c("min", "max") ~ "minmax",
        TRUE ~ TYP_IND
      )
    ) 
  
  # ------------------------------------------#
  # 2. Porovnani s limity ----- 
  # ------------------------------------------#
  
  n2k_druhy_lim_pre <- n2k_druhy_long %>%
    dplyr::mutate(
      # 1. Orizneme mezery v hodnotach a limitech
      HOD_IND_trim = stringr::str_trim(HOD_IND),
      LIM_IND_trim = stringr::str_trim(LIM_IND),
      # 2. Regex detekce: Je to cislo? (Volitelne minus, cislice, volitelne tecka a cislice)
      # Pokud data obsahuji desetinnou carku misto tecky, regex vrati FALSE a vznikne NA (coz je pozadovane)
      is_num_hod = stringr::str_detect(HOD_IND_trim, "^-?\\d+(\\.\\d+)?$"),
      is_num_lim = stringr::str_detect(LIM_IND_trim, "^-?\\d+(\\.\\d+)?$"),
      # 3. Podmineny prevod na cisla
      HOD_IND_num = dplyr::if_else(is_num_hod, as.numeric(HOD_IND_trim), NA_real_),
      LIM_IND_num = dplyr::if_else(is_num_lim, as.numeric(LIM_IND_trim), NA_real_)
    ) %>%
    # Odstraneni pomocnych sloupcu pro cistotu
    dplyr::select(-c(HOD_IND_trim, LIM_IND_trim, is_num_hod, is_num_lim)) %>%
    dplyr::mutate(
      # STAV_IND: Vyhodnoceni splneni limitu (1 = splneno, 0 = nesplneno)
      STAV_IND = dplyr::case_when(
        # Pro MIN limit: Hodnota musi byt vetsi nebo rovna
        TYP_IND == "min" & HOD_IND_num < LIM_IND_num ~ 0,
        TYP_IND == "min" & HOD_IND_num >= LIM_IND_num ~ 1,
        # Pro MAX limit: Hodnota musi byt mensi nebo rovna
        TYP_IND == "max" & HOD_IND_num > LIM_IND_num ~ 0,
        TYP_IND == "max" & HOD_IND_num <= LIM_IND_num ~ 1,
        # Pro VAL limit (text): hodnota se musi rovnat limitu, nebo - jde-li
        # o vicehodnotovou mnozinu "a, b, c" - limit musi byt jednou z jejich
        # polozek. Viz val_shoda() nahore a nalez H-42.
        #
        # Vetev s NA musi byt PRVNI: bez ni by zaverecny chytac
        # `TYP_IND == "val" ~ 0` prohlasil nezmereny indikator za nesplneny,
        # coz je presne to, co opravoval nalez H-21.
        TYP_IND == "val" & (is.na(HOD_IND) | is.na(LIM_IND)) ~ NA_real_,
        TYP_IND == "val" & val_shoda(HOD_IND, LIM_IND) ~ 1,
        TYP_IND == "val" ~ 0
      )
    ) %>%
    dplyr::select(-c(HOD_IND_num, LIM_IND_num)) %>%
    dplyr::mutate(
      # is_POP: Indikator, zda se jedna o populacni parametr (zacina POP_)
      # (nezavisi na skupine, proto se pocita mimo seskupeni)
      is_POP = stringr::str_starts(ID_IND, "POP_")
    ) %>%
    # Agregace vice hodnot pro jeden indikator (min/max logika) - viz
    # komentar u agg_stav_ind() nahore (tento krok byl hlavnim uzkym hrdlem
    # celeho vyhodnoceni, proto je resen pres data.table).
    agg_stav_ind() %>%
    dplyr::select(-is_POP) %>%
    dplyr::mutate(
      # Osetreni nekonecnych hodnot vzniklych agragaci prazdnych dat
      STAV_IND = dplyr::case_when(
        is.infinite(STAV_IND) ~ NA,
        TRUE ~ STAV_IND
      )
    ) %>%
    # Vyber nejlepsi varianty pro unikatnost v ramci nalezu (redukce radku).
    # OPTIMALIZACE: puvodne group_by() + arrange(desc) + slice(1). Pozor -
    # arrange() ve vychozim nastaveni seskupeni IGNORUJE, takze se puvodne
    # seradila cela tabulka podle desc(STAV_IND) a slice(1) pak z kazde
    # skupiny vzal prvni radek v tomto globalnim poradi, tj. radek s
    # nejvyssim STAV_IND (NA radi arrange() vzdy na konec). Presne totez
    # dela distinct(.keep_all = TRUE), ktery ponechava PRVNI vyskyt kazde
    # kombinace klice - ale bez pomaleho skupinoveho slice() nad statisici
    # skupin. Zaverecne arrange() obnovuje puvodni poradi radku (podle
    # klicu skupin), na kterem zavisi dplyr::first() v navazujicim kroku.
    dplyr::arrange(dplyr::desc(STAV_IND)) %>%
    dplyr::distinct(ID_ND_NALEZ, ID_IND, .keep_all = TRUE) %>%
    dplyr::arrange(ID_ND_NALEZ, ID_IND)
  
  # ------------------------------------------#
  # 3. Hodnoceni nalezu ----- 
  # ------------------------------------------#
  
  n2k_druhy_lim <- n2k_druhy_lim_pre %>%
    dplyr::group_by(ID_ND_NALEZ) %>%
    dplyr::mutate(
      # CELKOVE_SUM: Soucet vsech splnenych limitu pro nalez
      CELKOVE_SUM = as.character(
        sum(
          STAV_IND, 
          na.rm = TRUE)
      )
    ) %>%
    dplyr::select(-c(ID_IND:IND_GRP)) %>%
    # Pivotovani CELKOVE_SUM zpet do dlouheho formatu
    tidyr::pivot_longer(
      .,
      cols = ncol(.),
      names_to = "ID_IND",
      values_to = "HOD_IND"
    ) %>%
    dplyr::distinct() %>%
    # Spojeni s puvodnimi vysledky
    dplyr::bind_rows(
      ., 
      n2k_druhy_lim_pre
    ) %>%
    dplyr::arrange(ID_ND_NALEZ) %>%
    # Filtrace "sirotcich" limitu, ktere nemaji prirazeny nalez
    dplyr::filter(is.na(ID_ND_NALEZ) == FALSE) %>%
    dplyr::distinct()
  
  return(n2k_druhy_lim)
}