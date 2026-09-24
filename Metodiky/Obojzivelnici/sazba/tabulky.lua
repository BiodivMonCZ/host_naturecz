-- Nastaveni relativnich sirek sloupcu tabulek.
--
-- Tabulky v Markdownu (roury) nenesou informaci o sirce sloupcu. Pandoc pro
-- ne proto vysazi tabulku v "prirozene" sirce, kde se text v bunkach nezalamuje
-- - siroke tabulky metodiky (limity po druzich, seznam EVL) pak pretekaji za
-- pravy okraj stranky a konec radku se do PDF vubec nedostane.
--
-- Filtr kazdemu sloupci priradi relativni sirku podle nejdelsiho textu v nem,
-- takze pandoc vysazi sloupce typu p{...} a text se zalamuje. Sirky jsou
-- omezene zdola i shora, aby jeden dlouhy sloupec neumackal ostatni.
--
-- Pouziva se pres pandoc --lua-filter ze skriptu
-- R/05_metodiky/53_metodika_md_pdf.R.

local MIN_ZNAKU = 8    -- pod tuto sirku sloupec nespadne (kratke kody, cisla)
local MAX_ZNAKU = 40   -- nad tuto sirku uz se sloupec dal nezvysuje

local function delka_bunky(cell)
  return #pandoc.utils.stringify(cell.contents)
end

-- Nejdelsi text v kazdem sloupci; pocita se pres hlavicku i telo tabulky
local function delky_sloupcu(tbl, ncol)
  local max = {}
  for i = 1, ncol do max[i] = 0 end

  local function projdi(rows)
    for _, row in ipairs(rows) do
      local i = 1
      for _, cell in ipairs(row.cells) do
        if i <= ncol then
          local d = delka_bunky(cell)
          if d > max[i] then max[i] = d end
          i = i + (cell.col_span or 1)
        end
      end
    end
  end

  for _, h in ipairs(tbl.head.rows) do projdi({ h }) end
  for _, body in ipairs(tbl.bodies) do projdi(body.body) end

  return max
end

function Table(tbl)
  local ncol = #tbl.colspecs
  if ncol == 0 then
    return nil
  end

  -- Tabulka, ktera sirky uz ma (napr. z grid tabulky), se nechava byt
  for _, cs in ipairs(tbl.colspecs) do
    if cs[2] ~= nil then
      return nil
    end
  end

  local max = delky_sloupcu(tbl, ncol)

  local vahy = {}
  local soucet = 0
  for i = 1, ncol do
    local v = math.max(MIN_ZNAKU, math.min(max[i], MAX_ZNAKU))
    vahy[i] = v
    soucet = soucet + v
  end

  if soucet == 0 then
    return nil
  end

  local colspecs = {}
  for i = 1, ncol do
    colspecs[i] = { tbl.colspecs[i][1], vahy[i] / soucet }
  end
  tbl.colspecs = colspecs

  return tbl
end
