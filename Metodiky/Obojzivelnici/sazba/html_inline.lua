-- Prevod zbytkoveho HTML z Markdownu na LaTeX.
--
-- Metodika je ulozena jako GitHub Flavored Markdown, ktery nema vlastni
-- znaceni pro podtrzeni ani horni index - nese je jako HTML (<u>, <sup>).
-- Zapisovac LaTeXu takove useky bez tohoto filtru bez varovani zahodi,
-- takze by z metodiky zmizelo zvyrazneni klicovych formulaci (v dokumentu
-- je pres 30 podtrzenych useku).
--
-- Pouziva se pres pandoc --lua-filter ze skriptu
-- R/05_metodiky/53_metodika_md_pdf.R.

local prevod = {
  ["<u>"]     = "\\uline{",
  ["</u>"]    = "}",
  ["<sup>"]   = "\\textsuperscript{",
  ["</sup>"]  = "}",
  ["<sub>"]   = "\\textsubscript{",
  ["</sub>"]  = "}",
  ["<br>"]    = "\\newline ",
  ["<br/>"]   = "\\newline ",
  ["<br />"]  = "\\newline ",
}

-- Neprevedene HTML se vypise jako varovani. Tichy vypadek textu je horsi
-- nez hlaseni pri sazbe - je z nej videt, ze dokument pouzil znaceni,
-- na ktere filtr neni pripraveny.
function RawInline(el)
  if el.format ~= "html" then
    return nil
  end

  local klic = string.lower(el.text)
  local latex = prevod[klic]

  if latex ~= nil then
    return pandoc.RawInline("latex", latex)
  end

  io.stderr:write("html_inline.lua: neprevedene HTML '" .. el.text .. "'\n")
  return nil
end
