;;; ef-monokai-machine-theme.el --- Legible dark theme with vibrant Monokai colors -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/ef-themes
;; Keywords: faces, theme, accessibility

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The `ef-themes' are a collection of light and dark themes for GNU
;; Emacs whose goal is to provide colorful ("pretty") yet legible
;; options for users who want something with a bit more flair than the
;; `modus-themes' (also designed by me).

;;; Code:

(eval-and-compile
  (require 'ef-themes)

;;;###theme-autoload
  (deftheme ef-monokai-machine
    "Legible dark theme with vibrant Monokai colors."
    :background-mode 'dark
    :kind 'color-scheme
    :family 'ef)

  (defconst ef-monokai-machine-palette
    '(
;;; Basic values

      (bg-main     "#000000")
      (fg-main     "#f2fffc")
      (bg-dim      "#131313")
      (fg-dim      "#6b7678")
      (bg-alt      "#161b1e")
      (fg-alt      "#96cbfe")

      (bg-active   "#3a4449")
      (bg-inactive "#000000")

;;; Basic hues for foreground values

      (red             "#ff6d7e")
      (red-warmer      "#ff6d7e")
      (red-cooler      "#ff6d7e")
      (red-faint       "#c6c6c6")
      (green           "#a2e57b")
      (green-warmer    "#a2e57b")
      (green-cooler    "#a2e57b")
      (green-faint     "#8b9798")
      (yellow          "#FFFFB6")
      (yellow-warmer   "#FFD2A7")
      (yellow-cooler   "#FFFFB6")
      (yellow-faint    "#b4c1c0")
      (blue            "#96cbfe")
      (blue-warmer     "#96cbfe")
      (blue-cooler     "#96cbfe")
      (blue-faint      "#8b9798")
      (magenta         "#c6c5fe")
      (magenta-warmer  "#c6c5fe")
      (magenta-cooler  "#96cbfe")
      (magenta-faint   "#8b9798")
      (cyan            "#96cbfe")
      (cyan-warmer     "#96cbfe")
      (cyan-cooler     "#96cbfe")
      (cyan-faint      "#8b9798")

;;; Basic hues for background values

      (bg-red-intense     "#2e363b")
      (bg-green-intense   "#2e363b")
      (bg-yellow-intense  "#2e363b")
      (bg-blue-intense    "#2e363b")
      (bg-magenta-intense "#2e363b")
      (bg-cyan-intense    "#2e363b")

      (bg-red-subtle      "#161b1e")
      (bg-green-subtle    "#161b1e")
      (bg-yellow-subtle   "#161b1e")
      (bg-blue-subtle     "#161b1e")
      (bg-magenta-subtle  "#161b1e")
      (bg-cyan-subtle     "#161b1e")

;;; Diffs

      (bg-added          "#2e363b")
      (bg-added-faint    "#161b1e")
      (bg-added-refine   "#3a4449")
      (fg-added          "#a2e57b")

      (bg-changed        "#2e363b")
      (bg-changed-faint  "#161b1e")
      (bg-changed-refine "#3a4449")
      (fg-changed        "#FFFFB6")

      (bg-removed        "#2e363b")
      (bg-removed-faint  "#161b1e")
      (bg-removed-refine "#3a4449")
      (fg-removed        "#ff6d7e")

;;; Graphs

      (bg-graph-red-0     "#ff6d7e")
      (bg-graph-red-1     "#c6c6c6")
      (bg-graph-green-0   "#a2e57b")
      (bg-graph-green-1   "#8b9798")
      (bg-graph-yellow-0  "#FFFFB6")
      (bg-graph-yellow-1  "#FFD2A7")
      (bg-graph-blue-0    "#96cbfe")
      (bg-graph-blue-1    "#8b9798")
      (bg-graph-magenta-0 "#c6c5fe")
      (bg-graph-magenta-1 "#8b9798")
      (bg-graph-cyan-0    "#96cbfe")
      (bg-graph-cyan-1    "#8b9798")

;;; Special hues

      (bg-mode-line       "#161b1e")
      (fg-mode-line       "#f2fffc")
      (bg-completion      "#2e363b")
      (bg-hover           "#3a4449")
      (bg-hover-secondary "#2e363b")
      (bg-hl-line         "#161b1e")
      (bg-paren           "#3a4449")
      (bg-err             "#2e363b") ; check with err
      (bg-warning         "#2e363b") ; check with warning
      (bg-info            "#2e363b") ; check with info

      (border        "#545f62")
      (cursor        "#f2fffc")
      (fg-intense    "#ffffff")

      (modeline-err     "#ff6d7e")
      (modeline-warning "#FFD2A7")
      (modeline-info    "#a2e57b")

      (underline-err     "#ff6d7e")
      (underline-warning "#FFD2A7")
      (underline-info    "#a2e57b")

      (bg-char-0 "#96cbfe")
      (bg-char-1 "#c6c5fe")
      (bg-char-2 "#FFFFB6")

;;; Mappings

;;;; General mappings

      (bg-fringe unspecified)
      (fg-fringe unspecified)

      (err red)
      (warning yellow-warmer)
      (info green)

      (link cyan)
      (link-alt magenta)
      (name yellow-warmer)
      (keybind cyan)
      (identifier magenta)
      (prompt green)

      (bg-region "#2e363b")
      (fg-region unspecified)

;;;; Code mappings

      (builtin cyan)
      (comment magenta-faint)
      (constant magenta)
      (fnname yellow-warmer)
      (keyword cyan)
      (preprocessor cyan)
      (docstring cyan-faint)
      (string green)
      (type yellow)
      (variable magenta)
      (rx-escape green) ; compare with `string'
      (rx-construct red)

;;;; Accent mappings

      (accent-0 cyan)
      (accent-1 magenta)
      (accent-2 green)
      (accent-3 red)

;;;; Date mappings

      (date-common green)
      (date-deadline red)
      (date-deadline-subtle red-faint)
      (date-event fg-alt)
      (date-holiday magenta)
      (date-now fg-main)
      (date-range fg-alt)
      (date-scheduled yellow)
      (date-scheduled-subtle yellow-faint)
      (date-weekday cyan)
      (date-weekend red-faint)

;;;; Prose mappings

      (prose-code magenta)
      (prose-done green)
      (prose-macro green)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-table-formula err)
      (prose-tag yellow-faint)
      (prose-todo red)
      (prose-verbatim cyan)

;;;; Mail mappings

      (mail-cite-0 cyan)
      (mail-cite-1 magenta)
      (mail-cite-2 green)
      (mail-cite-3 yellow)
      (mail-part magenta-faint)
      (mail-recipient yellow-warmer)
      (mail-subject cyan)
      (mail-other magenta)

;;;; Search mappings

      (bg-search-match bg-warning)
      (bg-search-current bg-yellow-intense)
      (bg-search-lazy bg-blue-intense)
      (bg-search-replace bg-red-intense)

      (bg-search-rx-group-0 bg-magenta-intense)
      (bg-search-rx-group-1 bg-green-intense)
      (bg-search-rx-group-2 bg-red-subtle)
      (bg-search-rx-group-3 bg-cyan-subtle)

;;;; Space mappings

      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-yellow-intense)

;;;; Tab mappings

      (bg-tab-bar      bg-alt)
      (bg-tab-current  bg-main)
      (bg-tab-other    bg-active)

;;;; Terminal mappings

      (bg-term-black           "black")
      (fg-term-black           "black")
      (bg-term-black-bright    "#545f62")
      (fg-term-black-bright    "#545f62")

      (bg-term-red             red)
      (fg-term-red             red)
      (bg-term-red-bright      red-warmer)
      (fg-term-red-bright      red-warmer)

      (bg-term-green           green)
      (fg-term-green           green)
      (bg-term-green-bright    green-warmer)
      (fg-term-green-bright    green-warmer)

      (bg-term-yellow          yellow)
      (fg-term-yellow          yellow)
      (bg-term-yellow-bright   yellow-cooler)
      (fg-term-yellow-bright   yellow-cooler)

      (bg-term-blue            blue)
      (fg-term-blue            blue)
      (bg-term-blue-bright     blue-cooler)
      (fg-term-blue-bright     blue-cooler)

      (bg-term-magenta         magenta)
      (fg-term-magenta         magenta)
      (bg-term-magenta-bright  magenta-cooler)
      (fg-term-magenta-bright  magenta-cooler)

      (bg-term-cyan            cyan)
      (fg-term-cyan            cyan)
      (bg-term-cyan-bright     cyan-cooler)
      (fg-term-cyan-bright     cyan-cooler)

      (bg-term-white           "#8b9798")
      (fg-term-white           "#8b9798")
      (bg-term-white-bright    "white")
      (fg-term-white-bright    "white")

;;;; Rainbow mappings

      (rainbow-0 red)
      (rainbow-1 cyan)
      (rainbow-2 magenta)
      (rainbow-3 yellow)
      (rainbow-4 green)
      (rainbow-5 blue)
      (rainbow-6 yellow-warmer)
      (rainbow-7 green-warmer)
      (rainbow-8 cyan-warmer))
    "The `ef-monokai-machine' palette.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

  (defcustom ef-monokai-machine-palette-overrides nil
    "Overrides for `ef-monokai-machine-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Ef themes,
refer to `ef-themes-common-palette-overrides'.

To preview the palette entries, use `ef-themes-preview-colors' or
`ef-themes-preview-colors-current' (read the documentation for
further details)."
  :group 'ef-themes
  :package-version '(ef-themes . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(ef-themes) Palette overrides"))

  (ef-themes-theme ef-monokai-machine ef-monokai-machine-palette ef-monokai-machine-palette-overrides)

  (provide-theme 'ef-monokai-machine))

;;; ef-monokai-machine-theme.el ends here
