;;; dark-material-theme.el --- A dark material color theme for Emacs.

;; Copyright (C) 2011-2013

;; Author: Kelvin Smith <oneKelvinSmith@gmail.com>
;; URL: http://github.com/oneKelvinSmith/dark-material-emacs
;; Package-Version: 20150521.2257
;; Version: 0.2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of the popular Textmate theme dark-material for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The Dark Material theme requires Emacs 24 or later!"))

(deftheme dark-material "The dark-material colour theme")

(defgroup dark-material nil
  "dark-material theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom dark-material-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'dark-material)

(defcustom dark-material-use-variable-pitch t
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'dark-material)

(defcustom dark-material-high-contrast-mode-line nil
  "Make the active/inactive mode line stand out more."
  :type 'boolean
  :group 'dark-material)

(defcustom dark-material-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'dark-material)

(defcustom dark-material-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'dark-material)

(defcustom dark-material-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'dark-material)

(defcustom dark-material-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'dark-material)

(defcustom dark-material-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'dark-material)

(let* ((class                    '((class color) (min-colors 257)))
       ;; Primary colors
       (yellow                   "#E6DB74")
       (orange                   "#FFCB6B")
       (red                      "#F92672")
       (magenta                  "#FD5FF0")
       (violet                   "#AE81FF")
       (blue                     "#66D9EF")
       (cyan                     "#A1EFE4")
       (green                    "#A6E22E")
       (gray                     "#474747")
       ;; Darker and lighter accented colors
       (yellow-d                 "#968B26")
       (yellow-l                 "#F3EA98")
       (orange-d                 "#A45E0A")
       (orange-l                 "#FEB257")
       (red-d                    "#A20C41")
       (red-l                    "#FC5C94")
       (magenta-d                "#A41F99")
       (magenta-l                "#FE87F4")
       (violet-d                 "#562AA6")
       (violet-l                 "#C2A1FF")
       (blue-d                   "#21889B")
       (blue-l                   "#8DE6F7")
       (cyan-d                   "#349B8D")
       (cyan-l                   "#BBF7EF")
       (green-d                  "#67930F")
       (green-l                  "#C1F161")
       (gray-d                   "#333333")
       (gray-l                   "#6b6b6b")
       ;; Adaptive colors
       (dark-material-fg               "#F8F8F2")
       (dark-material-bg               "#212121")
       (dark-material-hl-line          "#3E3D31")
       (dark-material-hl               "#49483E")
       (dark-material-emph             "#F8F8F0")
       (dark-material-comments         "#4A4A4A")
       ;; Adaptive higher/lower contrast accented colors
       (dark-material-fg-hc            "#141414")
       (dark-material-fg-lc            "#171A0B")
       ;; High contrast colors
       (yellow-hc                yellow-l)
       (yellow-lc                yellow-d)
       (orange-hc                orange-l)
       (orange-lc                orange-d)
       (red-hc                   red-l)
       (red-lc                   red-d)
       (magenta-hc               magenta-l)
       (magenta-lc               magenta-d)
       (violet-hc                violet-l)
       (violet-lc                violet-d)
       (blue-hc                  blue-l)
       (blue-lc                  blue-d)
       (cyan-hc                  cyan-l)
       (cyan-lc                  cyan-d)
       (green-hc                 green-l)
       (green-lc                 green-d)
       ;; customize based face properties
       (s-variable-pitch         (if dark-material-use-variable-pitch
                                     'variable-pitch 'default))
       (s-fringe-bg              (if dark-material-distinct-fringe-background
                                     dark-material-hl dark-material-bg))
       (s-mode-line-fg           (if dark-material-high-contrast-mode-line
                                     dark-material-bg dark-material-fg))
       (s-mode-line-bg           (if dark-material-high-contrast-mode-line
                                     dark-material-fg dark-material-hl-line))
       (s-mode-line-buffer-id-fg (if dark-material-high-contrast-mode-line
                                     'unspecified green))
       (s-mode-line-inactive-fg  (if dark-material-high-contrast-mode-line
                                     dark-material-fg dark-material-comments))
       (s-mode-line-inactive-bg  (if dark-material-high-contrast-mode-line
                                     dark-material-hl-line dark-material-bg))
       (s-mode-line-inactive-bc  (if dark-material-high-contrast-mode-line
                                     dark-material-fg dark-material-hl-line))

       ;; Definitions for terminals that do not support 256 colors
       (terminal-class                    '((class color) (min-colors 89)))
       ;; Primary colors
       (terminal-yellow                   "#CDC673")
       (terminal-orange                   "#FF8C00")
       (terminal-red                      "#FF1493")
       (terminal-magenta                  "#D700D7")
       (terminal-violet                   "#AF87FF")
       (terminal-blue                     "#5FD7FF")
       (terminal-cyan                     "#5FFFFF")
       (terminal-green                    "#87D700")
       (terminal-gray                     "#444444")
       ;; Darker and lighter accented colors
       ;;
       ;; TODO: find terminal equivalents for all window colors (on the right)
       ;;
       (terminal-yellow-d                 "#968B26")
       (terminal-yellow-l                 "#FFF68F")
       (terminal-orange-d                 "#A0522D")
       (terminal-orange-l                 "#FFA54F")
       (terminal-red-d                    "#5F0000")
       (terminal-red-l                    "#EE6AA7")
       (terminal-magenta-d                "#A41F99")
       (terminal-magenta-l                "#FE87F4")
       (terminal-violet-d                 "#562AA6")
       (terminal-violet-l                 "#C2A1FF")
       (terminal-blue-d                   "#21889B")
       (terminal-blue-l                   "#8DE6F7")
       (terminal-cyan-d                   "#349B8D")
       (terminal-cyan-l                   "#AFEEEE")
       (terminal-green-d                  "#6B8E23")
       (terminal-green-l                  "#B3EE3A")
       (terminal-gray-d                   "#3a3a3a")
       (terminal-gray-l                   "#6c6c6c")
       ;; Adaptive colors
       (terminal-dark-material-fg               "#F5F5F5")
       (terminal-dark-material-bg               "#212121")
       (terminal-dark-material-hl-line          "#212121")
       (terminal-dark-material-hl               "#303030")
       (terminal-dark-material-emph             "#FFFAFA")
       (terminal-dark-material-comments         "#8B8878")
       ;; Adaptive higher/lower contrast accented colors
       (terminal-dark-material-fg-hc            "#171A0B")
       (terminal-dark-material-fg-lc            "#141414")
       ;; High contrast colors
       (terminal-yellow-hc                yellow-d)
       (terminal-yellow-lc                yellow-l)
       (terminal-orange-hc                orange-d)
       (terminal-orange-lc                orange-l)
       (terminal-red-hc                   red-d)
       (terminal-red-lc                   red-l)
       (terminal-magenta-hc               magenta-d)
       (terminal-magenta-lc               magenta-l)
       (terminal-violet-hc                violet-d)
       (terminal-violet-lc                violet-l)
       (terminal-blue-hc                  blue-d)
       (terminal-blue-lc                  blue-l)
       (terminal-cyan-hc                  cyan-d)
       (terminal-cyan-lc                  cyan-l)
       (terminal-green-hc                 green-d)
       (terminal-green-lc                 green-l)
       ;; customize based face properties
       (terminal-s-variable-pitch         (if dark-material-use-variable-pitch
                                              'variable-pitch 'default))
       (terminal-s-fringe-bg              (if dark-material-distinct-fringe-background
                                              terminal-dark-material-hl terminal-dark-material-bg))
       (terminal-s-mode-line-fg           (if dark-material-high-contrast-mode-line
                                              terminal-dark-material-bg terminal-dark-material-fg))
       (terminal-s-mode-line-bg           (if dark-material-high-contrast-mode-line
                                              terminal-dark-material-fg terminal-dark-material-hl-line))
       (terminal-s-mode-line-buffer-id-fg (if dark-material-high-contrast-mode-line
                                              'unspecified terminal-green))
       (terminal-s-mode-line-inactive-fg  (if dark-material-high-contrast-mode-line
                                              terminal-dark-material-fg terminal-dark-material-comments))
       (terminal-s-mode-line-inactive-bg  (if dark-material-high-contrast-mode-line
                                              terminal-dark-material-hl-line terminal-dark-material-bg))
       (terminal-s-mode-line-inactive-bc  (if dark-material-high-contrast-mode-line
                                              terminal-dark-material-fg terminal-dark-material-hl-line)))

  ;; Define faces
  (custom-theme-set-faces
   'dark-material
   '(button ((t (:underline t))))

   ;; basic coloring
   `(default
      ((,class (:foreground ,dark-material-fg
                            :background ,dark-material-bg))
       (,terminal-class (:foreground ,terminal-dark-material-fg
                                     :background ,terminal-dark-material-bg))))

   `(shadow
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(match
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-emph
                                    :weight bold))))

   `(cursor
     ((,class (:foreground ,dark-material-bg
                           :background ,dark-material-fg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-dark-material-bg
                                    :background ,terminal-dark-material-fg
                                    :inverse-video t))))

   `(mouse
     ((,class (:foreground ,dark-material-bg
                           :background ,dark-material-fg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-dark-material-bg
                                    :background ,terminal-dark-material-fg
                                    :inverse-video t))))

   `(escape-glyph-face
     ((,class (:foreground ,red)))
     ((,terminal-class (:foreground ,terminal-red))))

   `(fringe
     ((,class (:foreground ,dark-material-fg
                           :background ,s-fringe-bg))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :background ,terminal-s-fringe-bg))))

   `(highlight
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   `(link
     ((,class (:foreground ,blue
                           :underline t
                           :weight bold))
      (,terminal-class (:foreground ,terminal-blue
                                    :underline t
                                    :weight bold))))

   `(link-visited
     ((,class (:foreground ,blue
                           :underline t
                           :weight normal))
      (,terminal-class (:foreground ,terminal-blue
                                    :underline t
                                    :weight normal))))

   `(success
     ((,class (:foreground ,green ))
      (,terminal-class (:foreground ,terminal-green ))))

   `(warning
     ((,class (:foreground ,yellow ))
      (,terminal-class (:foreground ,terminal-yellow ))))

   `(error
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(lazy-highlight
     ((,class (:foreground ,dark-material-bg
                           :background ,yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-dark-material-bg
                                    :background ,terminal-yellow
                                    :weight normal))))

   `(escape-glyph
     ((,class (:foreground ,violet))
      (,terminal-class (:foreground ,terminal-violet))))

   ;; compilation
   `(compilation-column-face
     ((,class (:foreground ,cyan
                           :underline nil))
      (,terminal-class (:foreground ,terminal-cyan
                                    :underline nil))))

   `(compilation-column-number
     ((,class (:inherit font-lock-doc-face
                        :foreground ,cyan
                        :underline nil))
      (,terminal-class (:inherit font-lock-doc-face
                                 :foreground ,terminal-cyan
                                 :underline nil))))

   `(compilation-enter-directory-face
     ((,class (:foreground ,green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-green
                                    :underline nil))))

   `(compilation-error
     ((,class (:inherit error
                        :underline nil))
      (,terminal-class (:inherit error
                                 :underline nil))))

   `(compilation-error-face
     ((,class (:foreground ,red
                           :underline nil))
      (,terminal-class (:foreground ,terminal-red
                                    :underline nil))))

   `(compilation-face
     ((,class (:foreground ,dark-material-fg
                           :underline nil))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :underline nil))))

   `(compilation-info
     ((,class (:foreground ,dark-material-comments
                           :underline nil
                           :bold nil))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :underline nil
                                    :bold nil))))

   `(compilation-info-face
     ((,class (:foreground ,blue
                           :underline nil))
      (,terminal-class (:foreground ,terminal-blue
                                    :underline nil))))

   `(compilation-leave-directory-face
     ((,class (:foreground ,green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-green
                                    :underline nil))))

   `(compilation-line-face
     ((,class (:foreground ,green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-green
                                    :underline nil))))

   `(compilation-line-number
     ((,class (:foreground ,green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-green
                                    :underline nil))))

   `(compilation-warning
     ((,class (:inherit warning
                        :underline nil))
      (,terminal-class (:inherit warning
                                 :underline nil))))

   `(compilation-warning-face
     ((,class (:foreground ,yellow
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight normal
                                    :underline nil))))

   `(compilation-mode-line-exit
     ((,class (:inherit compilation-info
                        :foreground ,green
                        :weight bold))
      (,terminal-class (:inherit compilation-info
                                 :foreground ,terminal-green
                                 :weight bold))))

   `(compilation-mode-line-fail
     ((,class (:inherit compilation-error
                        :foreground ,red
                        :weight bold))
      (,terminal-class (:inherit compilation-error
                                 :foreground ,terminal-red
                                 :weight bold))))

   `(compilation-mode-line-run
     ((,class (:foreground ,orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-orange
                                    :weight bold))))

   ;; cua
   `(cua-global-mark
     ((,class (:background ,yellow
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-yellow
                                    :foreground ,terminal-dark-material-bg))))

   `(cua-rectangle
    ((,class (:inherit region
                        :background ,magenta
                        :foreground ,dark-material-bg))
      (,terminal-class (:inherit region
                                 :background ,terminal-magenta
                                 :foreground ,terminal-dark-material-bg))))

   `(cua-rectangle-noselect
     ((,class (:inherit region
                        :background ,dark-material-hl
                        :foreground ,dark-material-comments))
      (,terminal-class (:inherit region
                                 :background ,terminal-dark-material-hl
                                 :foreground ,terminal-dark-material-comments))))

   ;; diary
   `(diary
     ((,class (:foreground ,yellow))
      (,class (:foreground ,terminal-yellow))))

   ;; dired
   `(dired-directory
     ((,class (:foreground ,blue
                           :weight normal))
      (,terminal-class (:foreground ,terminal-blue
                                    :weight normal))))

   `(dired-flagged
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(dired-header
     ((,class (:foreground ,dark-material-bg
                           :background ,blue))
      (,terminal-class (:foreground ,terminal-dark-material-bg
                                    :background ,terminal-blue))))

   `(dired-ignored
     ((,class (:inherit shadow))
      (,terminal-class (:inherit shadow))))

   `(dired-mark
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   `(dired-marked
     ((,class (:foreground ,magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-magenta
                                    :weight bold))))

   `(dired-perm-write
     ((,class (:foreground ,dark-material-fg
                           :underline t))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :underline t))))

   `(dired-symlink
     ((,class (:foreground ,cyan
                           :weight normal
                           :slant italic))
      (,terminal-class (:foreground ,terminal-cyan
                                    :weight normal
                                    :slant italic))))

   `(dired-warning
     ((,class (:foreground ,orange
                           :underline t))
      (,terminal-class (:foreground ,terminal-orange
                                    :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,class (:background ,dark-material-hl
                           :foreground ,cyan))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-cyan))))

   `(dropdown-list-selection-face
     ((,class (:background ,cyan-lc
                           :foreground ,cyan-hc))
      (,terminal-class (:background ,terminal-cyan-lc
                                    :foreground ,terminal-cyan-hc))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,class (:background ,blue
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-blue
                                    :foreground ,terminal-dark-material-bg))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,class (:inherit ecb-history-bucket-node-face
                        :foreground ,yellow))
      (,terminal-class (:inherit ecb-history-bucket-node-face
                                 :foreground ,terminal-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,class (:inherit ecb-directories-general-face
                        :foreground ,dark-material-fg))
      (,terminal-class (:inherit ecb-directories-general-face
                                 :foreground ,terminal-dark-material-fg))))

   `(ecb-history-dead-buffer-face
     ((,class (:inherit ecb-history-general-face
                        :foreground ,dark-material-comments))
      (,terminal-class (:inherit ecb-history-general-face
                                 :foreground ,terminal-dark-material-comments))))

   `(ecb-directory-not-accessible-face
     ((,class (:inherit ecb-directories-general-face
                        :foreground ,dark-material-comments))
      (,terminal-class (:inherit ecb-directories-general-face
                                 :foreground ,terminal-dark-material-comments))))

   `(ecb-bucket-node-face
     ((,class (:inherit ecb-default-general-face
                        :weight normal
                        :foreground ,blue))
      (,terminal-class (:inherit ecb-default-general-face
                                 :weight normal
                                 :foreground ,terminal-blue))))

   `(ecb-tag-header-face
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   `(ecb-analyse-bucket-element-face
     ((,class (:inherit ecb-analyse-general-face
                        :foreground ,green))
      (,terminal-class (:inherit ecb-analyse-general-face
                                 :foreground ,terminal-green))))

   `(ecb-directories-general-face
     ((,class (:inherit ecb-default-general-face
                        :height 1.0))
      (,terminal-class (:inherit ecb-default-general-face
                                 :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,class (:inherit ecb-methods-general-face
                        :foreground ,cyan))
      (,terminal-class (:inherit ecb-methods-general-face
                                 :foreground ,terminal-cyan))))

   `(ecb-mode-line-prefix-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(ecb-tree-guide-line-face
     ((,class (:inherit ecb-default-general-face
                        :foreground ,dark-material-hl
                        :height 1.0))
      (,terminal-class (:inherit ecb-default-general-face
                                 :foreground ,terminal-dark-material-hl
                                 :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,class (:foreground ,dark-material-emph))
      (,terminal-class (:foreground ,terminal-dark-material-emph))))

   `(ee-category
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(ee-link
     ((,class (:inherit link))
      (,terminal-class (:inherit link))))

   `(ee-link-visited
     ((,class (:inherit link-visited))
      (,terminal-class (:inherit link-visited))))

   `(ee-marked
     ((,class (:foreground ,magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-magenta
                                    :weight bold))))

   `(ee-omitted
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(ee-shadow
     ((,class (:inherit shadow))
      (,terminal-class (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(grep-error-face
     ((,class (:foreground ,red
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-red
                                    :weight bold
                                    :underline t))))

   `(grep-hit-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(grep-match-face
     ((,class (:foreground ,orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-orange
                                    :weight bold))))

   ;; faces used by isearch
   `(isearch
     ((,class (:foreground ,dark-material-bg
                           :background ,magenta
                           :weight normal))
      (,terminal-class (:foreground ,terminal-dark-material-bg
                                    :background ,terminal-magenta
                                    :weight normal))))

   `(isearch-fail
     ((,class (:foreground ,red
                           :background ,dark-material-bg
                           :bold t))
      (,terminal-class (:foreground ,terminal-red
                                    :background ,terminal-dark-material-bg
                                    :bold t))))

   ;; man
   `(Man-overstrike
     ((,class (:foreground ,blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-blue
                                    :weight bold))))

   `(Man-reverse
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(Man-underline
     ((,class (:foreground ,green :underline t))
      (,terminal-class (:foreground ,terminal-green :underline t))))

   ;; misc faces
   `(menu
     ((,class (:foreground ,dark-material-fg
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :background ,terminal-dark-material-bg))))

   `(minibuffer-prompt
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(mode-line
     ((,class (:inverse-video unspecified
                              :underline unspecified
                              :foreground ,s-mode-line-fg
                              :background ,s-mode-line-bg
                              :box (:line-width 1
                                                :color ,s-mode-line-bg
                                                :style unspecified)))
      (,terminal-class (:inverse-video unspecified
                                       :underline unspecified
                                       :foreground ,terminal-s-mode-line-fg
                                       :background ,terminal-s-mode-line-bg
                                       :box (:line-width 1
                                                         :color ,terminal-s-mode-line-bg
                                                         :style unspecified)))))

   `(mode-line-buffer-id
     ((,class (:foreground ,s-mode-line-buffer-id-fg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-s-mode-line-buffer-id-fg
                                    :weight bold))))

   `(mode-line-inactive
     ((,class (:inverse-video unspecified
                              :underline unspecified
                              :foreground ,s-mode-line-inactive-fg
                              :background ,s-mode-line-inactive-bg
                              :box (:line-width 1
                                                :color ,s-mode-line-inactive-bc
                                                :style unspecified)))
      (,terminal-class (:inverse-video unspecified
                                       :underline unspecified
                                       :foreground ,terminal-s-mode-line-inactive-fg
                                       :background ,terminal-s-mode-line-inactive-bg
                                       :box (:line-width 1
                                                         :color ,terminal-s-mode-line-inactive-bc
                                                         :style unspecified)))))

   `(header-line
     ((,class (:inverse-video unspecified
                              :underline unspecified
                              :foreground ,dark-material-emph
                              :background ,dark-material-hl
                              :box (:line-width 1
                                                :color ,dark-material-hl
                                                :style unspecified)))
      (,terminal-class (:inverse-video unspecified
                                       :underline unspecified
                                       :foreground ,terminal-dark-material-emph
                                       :background ,terminal-dark-material-hl
                                       :box (:line-width 1
                                                         :color ,terminal-dark-material-hl
                                                         :style unspecified)))))

   `(region
     ((,class (:background ,dark-material-hl
                           :inherit t))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :inherit t))))

   `(secondary-selection
     ((,class (:background ,dark-material-hl
                           :inherit t))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :inherit t))))


   `(trailing-whitespace
     ((,class (:background ,red))
      (,terminal-class (:background ,terminal-red))))

   `(vertical-border
     ((,class (:foreground ,dark-material-hl))
      (,terminal-class (:foreground ,terminal-dark-material-hl))))

   ;; font lock
   `(font-lock-builtin-face
     ((,class (:foreground ,cyan
                           :weight normal))
      (,terminal-class (:foreground ,terminal-cyan
                                    :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(font-lock-comment-face
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(font-lock-constant-face
     ((,class (:foreground ,violet))
      (,terminal-class (:foreground ,terminal-violet))))

   `(font-lock-doc-face
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(font-lock-function-name-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(font-lock-keyword-face
     ((,class (:foreground ,violet
                           :weight normal))
      (,terminal-class (:foreground ,terminal-violet
                                    :weight normal))))

   `(font-lock-negation-char-face
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   `(font-lock-preprocessor-face
     ((,class (:foreground ,violet))
      (,terminal-class (:foreground ,terminal-violet))))

   `(font-lock-regexp-grouping-construct
     ((,class (:foreground ,yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,class (:foreground ,violet
                           :weight normal))
      (,terminal-class (:foreground ,terminal-violet
                                    :weight normal))))

   `(font-lock-string-face
     ((,class (:foreground ,green-l))
      (,terminal-class (:foreground ,terminal-green-l))))

   `(font-lock-type-face
     ((,class (:foreground ,blue
                           :italic nil))
      (,terminal-class (:foreground ,terminal-blue
                                    :italic nil))))

   `(font-lock-variable-name-face
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(font-lock-warning-face
     ((,class (:foreground ,orange
                           :weight bold
                           :italic t
                           :underline t))
      (,terminal-class (:foreground ,terminal-orange
                                    :weight bold
                                    :italic t
                                    :underline t))))

   `(c-annotation-face
     ((,class (:inherit font-lock-constant-face))
      (,terminal-class (:inherit font-lock-constant-face))))


   ;;; external

   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,class (:foreground ,dark-material-comments
                           :background ,dark-material-bg
                           :inverse-video nil))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :background ,terminal-dark-material-bg
                                    :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,class (:foreground ,yellow
                           :background ,dark-material-bg
                           :inverse-video nil
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :background ,terminal-dark-material-bg
                                    :inverse-video nil
                                    :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,class (:inherit bold
                        :foreground ,dark-material-emph))
      (,terminal-class (:inherit bold
                                 :foreground ,terminal-dark-material-emph))))

   `(font-latex-doctex-documentation-face
     ((,class (:background unspecified))
      (,terminal-class (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,class (:inherit italic :foreground ,dark-material-emph))
      (,terminal-class (:inherit italic :foreground ,terminal-dark-material-emph))))

   `(font-latex-math-face
     ((,class (:foreground ,violet))
      (,terminal-class (:foreground ,terminal-violet))))

   `(font-latex-sectioning-0-face
     ((,class (:inherit font-latex-sectioning-1-face
                        :height ,dark-material-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-1-face
                                 :height ,dark-material-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,class (:inherit font-latex-sectioning-2-face
                        :height ,dark-material-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-2-face
                                 :height ,dark-material-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,class (:inherit font-latex-sectioning-3-face
                        :height ,dark-material-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-3-face
                                 :height ,dark-material-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,class (:inherit font-latex-sectioning-4-face
                        :height ,dark-material-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-4-face
                                 :height ,dark-material-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,class (:inherit font-latex-sectioning-5-face
                        :height ,dark-material-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-5-face
                                 :height ,dark-material-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,yellow
                        :weight bold))
      (,terminal-class (:inherit ,terminal-s-variable-pitch :
                                 foreground ,terminal-yellow
                                 :weight bold))))

   `(font-latex-sedate-face
     ((,class (:foreground ,dark-material-emph))
      (,terminal-class (:foreground ,terminal-dark-material-emph))))

   `(font-latex-slide-title-face
     ((,class (:inherit (,s-variable-pitch font-lock-type-face)
                        :weight bold
                        :height ,dark-material-height-plus-3))
      (,terminal-class (:inherit (,terminal-s-variable-pitch font-lock-type-face)
                                 :weight bold
                                 :height ,dark-material-height-plus-3))))

   `(font-latex-string-face
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(font-latex-subscript-face
     ((,class (:height ,dark-material-height-minus-1))
      (,terminal-class (:height ,dark-material-height-minus-1))))

   `(font-latex-superscript-face
     ((,class (:height ,dark-material-height-minus-1))
      (,terminal-class (:height ,dark-material-height-minus-1))))

   `(font-latex-verbatim-face
     ((,class (:inherit fixed-pitch
                        :foreground ,dark-material-fg
                        :slant italic))
      (,terminal-class (:inherit fixed-pitch
                                 :foreground ,terminal-dark-material-fg
                                 :slant italic))))

   `(font-latex-warning-face
     ((,class (:inherit bold
                        :foreground ,orange))
      (,terminal-class (:inherit bold
                                 :foreground ,terminal-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,class (:background ,dark-material-hl
                           :foreground ,cyan))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-cyan))))

   `(ac-selection-face
     ((,class (:background ,cyan-lc
                           :foreground ,cyan-hc))
      (,terminal-class (:background ,terminal-cyan-lc
                                    :foreground ,terminal-cyan-hc))))

   `(ac-candidate-mouse-face
     ((,class (:background ,cyan-hc
                           :foreground ,cyan-lc))
      (,terminal-class (:background ,terminal-cyan-hc
                                    :foreground ,terminal-cyan-lc))))

   `(ac-completion-face
     ((,class (:foreground ,dark-material-emph
                           :underline t))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :underline t))))

   `(ac-gtags-candidate-face
     ((,class (:background ,dark-material-hl
                           :foreground ,blue))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-blue))))

   `(ac-gtags-selection-face
     ((,class (:background ,blue-lc
                           :foreground ,blue-hc))
      (,terminal-class (:background ,terminal-blue-lc
                                    :foreground ,terminal-blue-hc))))

   `(ac-yasnippet-candidate-face
     ((,class (:background ,dark-material-hl
                           :foreground ,yellow))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-yellow))))

   `(ac-yasnippet-selection-face
     ((,class (:background ,yellow-lc
                           :foreground ,yellow-hc))
      (,terminal-class (:background ,terminal-yellow-lc
                                    :foreground ,terminal-yellow-hc))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,class (:foreground ,magenta
                           :background unspecified
                           :slant normal))
      (,terminal-class (:foreground ,terminal-magenta
                                    :background unspecified
                                    :slant normal))))

   `(ahs-edit-mode-face
     ((,class (:foreground ,dark-material-bg
                           :background ,magenta))
      (,terminal-class (:foreground ,terminal-dark-material-bg
                                    :background ,terminal-magenta))))

   `(ahs-face
     ((,class (:foreground ,magenta
                           :background unspecified))
      (,terminal-class (:foreground ,terminal-magenta
                                    :background unspecified))))

   `(ahs-plugin-bod-face
     ((,class (:foreground ,magenta
                           :background unspecified ))
      (,terminal-class (:foreground ,terminal-magenta
                                    :background unspecified ))))

   `(ahs-plugin-defalt-face
     ((,class (:foreground ,magenta
                           :background unspecified))
      (,terminal-class (:foreground ,terminal-magenta
                                    :background unspecified))))

   `(ahs-plugin-whole-buffer-face
     ((,class (:foreground ,magenta
                           :background unspecified))
      (,terminal-class (:foreground ,terminal-magenta
                                    :background unspecified))))

   `(ahs-warning-face
     ((,class (:foreground ,red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-red
                                    :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(android-mode-error-face
     ((,class (:foreground ,orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-orange
                                    :weight bold))))

   `(android-mode-info-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(android-mode-verbose-face
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(android-mode-warning-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,class (:foreground ,magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-magenta
                                    :weight bold))))

   ;; bm
   `(bm-face
     ((,class (:background ,yellow-lc
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-yellow-lc
                                    :foreground ,terminal-dark-material-bg))))

   `(bm-fringe-face
     ((,class (:background ,yellow-lc
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-yellow-lc
                                    :foreground ,terminal-dark-material-bg))))

   `(bm-fringe-persistent-face
     ((,class (:background ,green-lc
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-green-lc
                                    :foreground ,terminal-dark-material-bg))))

   `(bm-persistent-face
     ((,class (:background ,green-lc
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-green-lc
                                    :foreground ,terminal-dark-material-bg))))

   ;; calfw
   `(cfw:face-day-title
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   `(cfw:face-annotation
     ((,class (:inherit cfw:face-day-title
                        :foreground ,yellow))
      (,terminal-class (:inherit cfw:face-day-title
                                 :foreground ,terminal-yellow))))

   `(cfw:face-default-content
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(cfw:face-default-day
     ((,class (:inherit cfw:face-day-title
                        :weight bold))
      (,terminal-class (:inherit cfw:face-day-title
                                 :weight bold))))

   `(cfw:face-disable
     ((,class (:inherit cfw:face-day-title
                        :foreground ,dark-material-comments))
      (,terminal-class (:inherit cfw:face-day-title
                                 :foreground ,terminal-dark-material-comments))))

   `(cfw:face-grid
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(cfw:face-header
     ((,class (:foreground ,blue-hc
                           :background ,blue-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-blue-hc
                                    :background ,terminal-blue-lc
                                    :weight bold))))

   `(cfw:face-holiday
     ((,class (:background nil
                           :foreground ,red
                           :weight bold))
      (,terminal-class (:background nil
                                    :foreground ,terminal-red
                                    :weight bold))))

   `(cfw:face-periods
     ((,class (:foreground ,magenta))
      (,terminal-class (:foreground ,terminal-magenta))))

   `(cfw:face-select
     ((,class (:background ,magenta-lc
                           :foreground ,magenta-hc))
      (,terminal-class (:background ,terminal-magenta-lc
                                    :foreground ,terminal-magenta-hc))))

   `(cfw:face-saturday
     ((,class (:foreground ,cyan-hc
                           :background ,cyan-lc))
      (,terminal-class (:foreground ,terminal-cyan-hc
                                    :background ,terminal-cyan-lc))))

   `(cfw:face-sunday
     ((,class (:foreground ,red-hc
                           :background ,red-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-red-hc
                                    :background ,terminal-red-lc
                                    :weight bold))))

   `(cfw:face-title
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,yellow
                        :weight bold
                        :height ,dark-material-height-plus-4))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-yellow
                                 :weight bold
                                 :height ,dark-material-height-plus-4))))

   `(cfw:face-today
     ((,class (:weight bold
                       :background ,dark-material-hl
                       :foreground nil))
      (,terminal-class (:weight bold
                                :background ,terminal-dark-material-hl
                                :foreground nil))))

   `(cfw:face-today-title
     ((,class (:background ,yellow-lc
                           :foreground ,yellow-hc
                           :weight bold))
      (,terminal-class (:background ,terminal-yellow-lc
                                    :foreground ,terminal-yellow-hc
                                    :weight bold))))

   `(cfw:face-toolbar
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-fg))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-fg))))

   `(cfw:face-toolbar-button-off
     ((,class (:background ,yellow-lc
                           :foreground ,yellow-hc
                           :weight bold))
      (,terminal-class (:background ,terminal-yellow-lc
                                    :foreground ,terminal-yellow-hc
                                    :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,class (:background ,yellow-hc
                           :foreground ,yellow-lc
                           :weight bold))
      (,terminal-class (:background ,terminal-yellow-hc
                                    :foreground ,terminal-yellow-lc
                                    :weight bold))))


   `(clojure-keyword-faceF78C6C
     ((,class (:foreground "#F78C6C"
                           :weight bold
                           ))
      (,terminal-class (:foreground ,terminal-orange
                                    :weight bold
                                    ))))

   ;; clojure-test-mode
   `(clojure-test-failure-face
     ((,class (:foreground ,orange
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-orange
                                    :weight bold
                                    :underline t))))

   `(clojure-test-error-face
     ((,class (:foreground ,red
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-red
                                    :weight bold
                                    :underline t))))

   `(clojure-test-success-face
     ((,class (:foreground ,green
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-green
                                    :weight bold
                                    :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,class (:background ,dark-material-hl
                           :foreground ,cyan))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-cyan))))

   `(company-tooltip-selection
     ((,class (:background ,cyan-lc
                           :foreground ,cyan-hc))
      (,terminal-class (:background ,terminal-cyan-lc
                                    :foreground ,terminal-cyan-hc))))

   `(company-tooltip-mouse
     ((,class (:background ,cyan-hc
                           :foreground ,cyan-lc))
      (,terminal-class (:background ,terminal-cyan-hc
                                    :foreground ,terminal-cyan-lc))))

   `(company-tooltip-common
     ((,class (:foreground ,dark-material-emph
                           :underline t))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :underline t))))

   `(company-tooltip-common-selection
     ((,class (:foreground ,dark-material-emph
                           :underline t))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :underline t))))

   `(company-preview
     ((,class (:background ,dark-material-hl
                           :foreground ,cyan))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-cyan))))

   `(company-preview-common
     ((,class (:foreground ,dark-material-emph
                           :underline t))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :underline t))))

   `(company-scrollbar-bg
     ((,class (:background ,gray-l))
      (,terminal-class (:background ,terminal-gray-l))))

   `(company-scrollbar-fg
     ((,class (:background ,yellow-d))
      (,terminal-class (:background ,terminal-yellow-d))))

   ;; cscope
   `(cscope-file-face
     ((,class (:foreground ,green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-green
                                    :weight bold))))

   `(cscope-function-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(cscope-line-number-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(cscope-line-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(cscope-mouse-face
     ((,class (:background ,blue
                           :foreground ,dark-material-fg))
      (,terminal-class (:background ,terminal-blue
                                    :foreground ,terminal-dark-material-fg))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-emph
                           :underline ,dark-material-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-emph
                                    :underline ,terminal-dark-material-emph
                                    :weight bold))))

   `(ctbl:face-continue-bar
     ((,class (:background ,dark-material-hl
                           :foreground ,yellow))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-yellow))))

   `(ctbl:face-row-select
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-fg
                           :underline t))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-fg
                                    :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   `(coffee-mode-function-param
     ((,class (:foreground ,violet
                           :slant italic))
      (,terminal-class (:foreground ,terminal-violet
                                    :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,class (:inherit ,s-variable-pitch
                        :height ,dark-material-height-plus-3
                        :foreground ,violet
                        :weight bold))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,dark-material-height-plus-3
                                 :foreground ,terminal-violet
                                 :weight bold))))

   `(custom-variable-tag
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,cyan
                        :height ,dark-material-height-plus-3))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-cyan
                                 :height ,dark-material-height-plus-3))))

   `(custom-comment-tag
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(custom-group-tag
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,blue
                        :height ,dark-material-height-plus-3))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-blue
                                 :height ,dark-material-height-plus-3))))

   `(custom-group-tag-1
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,red
                        :height ,dark-material-height-plus-3))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-red
                                 :height ,dark-material-height-plus-3))))

   `(custom-state
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   ;; diff
   `(diff-added
     ((,class (:foreground ,green
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-green
                                    :background ,terminal-dark-material-bg))))

   `(magit-diff-added-highlight
     ((,class (:background ,green-l
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-green-l
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))
   `(magit-diff-removed-highlight
     ((,class (:background ,red-l
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-red-l
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))

   `(diff-changed
     ((,class (:foreground ,blue
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-blue
                                    :background ,terminal-dark-material-bg))))

   `(diff-removed
     ((,class (:foreground ,red
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-red
                                    :background ,terminal-dark-material-bg))))

   `(diff-header
     ((,class (:background ,dark-material-bg))
      (,terminal-class (:background ,terminal-dark-material-bg))))

   `(diff-file-header
     ((,class (:background ,dark-material-bg
                           :foreground ,dark-material-fg
                           :weight bold))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-dark-material-fg
                                    :weight bold))))

   `(diff-refine-added
     ((,class :foreground ,dark-material-bg
              :background ,green)
      (,terminal-class :foreground ,terminal-dark-material-bg
                       :background ,terminal-green)))

   `(diff-refine-change
     ((,class :foreground ,dark-material-bg
              :background ,blue)
      (,terminal-class :foreground ,terminal-dark-material-bg
                       :background ,terminal-blue)))

   `(diff-refine-removed
     ((,class (:foreground ,dark-material-bg
                           :background ,red))
      (,terminal-class (:foreground ,terminal-dark-material-bg
                                    :background ,terminal-red))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,class (:background ,orange-lc))
      (,terminal-class (:background ,terminal-orange-lc))))

   `(ediff-fine-diff-B
     ((,class (:background ,green-lc))
      (,terminal-class (:background ,terminal-green-lc))))

   `(ediff-fine-diff-C
     ((,class (:background ,yellow-lc))
      (,terminal-class (:background ,terminal-yellow-lc))))

   `(ediff-current-diff-C
     ((,class (:background ,blue-lc))
      (,terminal-class (:background ,terminal-blue-lc))))

   `(ediff-even-diff-A
     ((,class (:background ,dark-material-comments
                           :foreground ,dark-material-fg-lc ))
      (,terminal-class (:background ,terminal-dark-material-comments
                                    :foreground ,terminal-dark-material-fg-lc ))))

   `(ediff-odd-diff-A
     ((,class (:background ,dark-material-comments
                           :foreground ,dark-material-fg-hc ))
      (,terminal-class (:background ,terminal-dark-material-comments
                                    :foreground ,terminal-dark-material-fg-hc ))))

   `(ediff-even-diff-B
     ((,class (:background ,dark-material-comments
                           :foreground ,dark-material-fg-hc ))
      (,terminal-class (:background ,terminal-dark-material-comments
                                    :foreground ,terminal-dark-material-fg-hc ))))

   `(ediff-odd-diff-B
     ((,class (:background ,dark-material-comments
                           :foreground ,dark-material-fg-lc ))
      (,terminal-class (:background ,terminal-dark-material-comments
                                    :foreground ,terminal-dark-material-fg-lc ))))

   `(ediff-even-diff-C
     ((,class (:background ,dark-material-comments
                           :foreground ,dark-material-fg ))
      (,terminal-class (:background ,terminal-dark-material-comments
                                    :foreground ,terminal-dark-material-fg ))))

   `(ediff-odd-diff-C
     ((,class (:background ,dark-material-comments
                           :foreground ,dark-material-bg ))
      (,terminal-class (:background ,terminal-dark-material-comments
                                    :foreground ,terminal-dark-material-bg ))))

   ;; diff-hl
   `(diff-hl-change
     ((,class (:background ,blue-lc
                           :foreground ,blue-hc))
      (,terminal-class (:background ,terminal-blue-lc
                                    :foreground ,terminal-blue-hc))))

   `(diff-hl-delete
     ((,class (:background ,red-lc
                           :foreground ,red-hc))
      (,terminal-class (:background ,terminal-red-lc
                                    :foreground ,terminal-red-hc))))

   `(diff-hl-insert
     ((,class (:background ,green-lc
                           :foreground ,green-hc))
      (,terminal-class (:background ,terminal-green-lc
                                    :foreground ,terminal-green-hc))))

   `(diff-hl-unknown
     ((,class (:background ,cyan-lc
                           :foreground ,cyan-hc))
      (,terminal-class (:background ,terminal-cyan-lc
                                    :foreground ,terminal-cyan-hc))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,red)
                   :inherit unspecified))
      (,class (:foreground ,red-hc
                           :background ,red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style line))) terminal-class)
       (:underline (:style line :color ,terminal-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-red-hc
                                    :background ,terminal-red-lc
                                    :weight bold
                                    :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,yellow)
                   :inherit unspecified))
      (,class (:foreground ,yellow-hc
                           :background ,yellow-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style line))) terminal-class)
       (:underline (:style line :color ,terminal-yellow)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-yellow-hc
                                    :background ,terminal-yellow-lc
                                    :weight bold
                                    :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,class (:foreground ,red
                           :background unspecified
                           :weight bold))
      (,terminal-class (:foreground ,terminal-red
                                    :background unspecified
                                    :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,class (:foreground ,yellow
                           :background unspecified
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :background unspecified
                                    :weight bold))))

   `(edts-face-error-mode-line
     ((,class (:background ,red
                           :foreground unspecified))
      (,terminal-class (:background ,terminal-red
                                    :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,class (:background ,yellow
                           :foreground unspecified))
      (,terminal-class (:background ,terminal-yellow
                                    :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(elfeed-search-feed-face
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(elfeed-search-tag-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(elfeed-search-title-face
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,class (:inherit font-lock-keyword-face))
      (,terminal-class (:inherit font-lock-keyword-face))))

   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,red)
                   :inherit unspecified))
      (,class (:foreground ,red-hc
                           :background ,red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-red-hc
                                    :background ,terminal-red-lc
                                    :weight bold
                                    :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,orange)
                   :inherit unspecified))
      (,class (:foreground ,orange-hc
                           :background ,orange-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-orange)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-orange-hc
                                    :background ,terminal-orange-lc
                                    :weight bold
                                    :underline t))))

   ;; epc
   `(epc:face-title
     ((,class (:foreground ,blue
                           :background ,dark-material-bg
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-blue
                                    :background ,terminal-dark-material-bg
                                    :weight normal
                                    :underline nil))))

   ;; erc
   `(erc-action-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-bold-face
     ((,class (:weight bold))
      (,terminal-class (:weight bold))))

   `(erc-current-nick-face
     ((,class (:foreground ,blue :weight bold))
      (,terminal-class (:foreground ,terminal-blue
                                    :weight bold))))

   `(erc-dangerous-host-face
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(erc-highlight-face
     ((,class (:inherit erc-default-face
                        :background ,dark-material-hl))
      (,terminal-class (:inherit erc-default-face
                                 :background ,terminal-dark-material-hl))))

   `(erc-direct-msg-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-error-face
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-input-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(erc-keyword-face
     ((,class (:foreground ,blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-blue
                                    :weight bold))))

   `(erc-nick-default-face
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   `(erc-my-nick-face
     ((,class (:foreground ,red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-red
                                    :weight bold))))

   `(erc-nick-msg-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-notice-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(erc-pal-face
     ((,class (:foreground ,orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-orange
                                    :weight bold))))

   `(erc-prompt-face
     ((,class (:foreground ,orange
                           :background ,dark-material-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-orange
                                    :background ,terminal-dark-material-bg
                                    :weight bold))))

   `(erc-timestamp-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   `(eshell-ls-archive
     ((,class (:foreground ,red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-red
                                    :weight bold))))

   `(eshell-ls-backup
     ((,class (:inherit font-lock-comment-face))
      (,terminal-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,class (:inherit font-lock-comment-face))
      (,terminal-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,class (:foreground ,blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-blue
                                    :weight bold))))

   `(eshell-ls-executable
     ((,class (:foreground ,red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-red
                                    :weight bold))))

   `(eshell-ls-unreadable
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(eshell-ls-missing
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,class (:inherit font-lock-doc-face))
      (,terminal-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   `(eshell-ls-symlink
     ((,class (:foreground ,cyan
                           :weight bold))
      (,terminal-class (:foreground ,terminal-cyan
                                    :weight bold))))

   ;; fic
   `(fic-author-face
     ((,class (:background ,dark-material-bg
                           :foreground ,orange
                           :underline t
                           :slant italic))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-orange
                                    :underline t
                                    :slant italic))))

   `(fic-face
     ((,class (:background ,dark-material-bg
                           :foreground ,orange
                           :weight normal
                           :slant italic))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-orange
                                    :weight normal
                                    :slant italic))))

   `(font-lock-fic-face
     ((,class (:background ,dark-material-bg
                           :foreground ,orange
                           :weight normal
                           :slant italic))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-orange
                                    :weight normal
                                    :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,class (:foreground ,blue
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-blue
                                    :weight normal
                                    :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,class (:foreground ,red-hc
                           :background ,red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,terminal-class (:foreground ,terminal-red-hc
                                    :background ,terminal-red-lc
                                    :weight bold
                                    :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,class (:foreground ,green-hc
                           :background ,green-lc))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,terminal-class (:foreground ,terminal-green-hc
                                    :background ,terminal-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,class (:foreground ,yellow-hc
                           :background ,yellow-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,terminal-class (:foreground ,terminal-yellow-hc
                                    :background ,terminal-yellow-lc
                                    :weight bold
                                    :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,red)
                   :inherit unspecified))
      (,class (:foreground ,red-hc
                           :background ,red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-red-hc
                                    :background ,terminal-red-lc
                                    :weight bold
                                    :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,yellow)
                   :inherit unspecified))
      (,class (:foreground ,yellow-hc
                           :background ,yellow-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-yellow)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-yellow-hc
                                    :background ,terminal-yellow-lc
                                    :weight bold
                                    :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,blue)
                   :inherit unspecified))
      (,class (:foreground ,blue-hc
                           :background ,blue-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-blue)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-blue-hc
                                    :background ,terminal-blue-lc
                                    :weight bold
                                    :underline t))))

   `(flycheck-fringe-error
     ((,class (:foreground ,red-hc
                           :background ,red-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-red-hc
                                    :background ,terminal-red-lc
                                    :weight bold))))

   `(flycheck-fringe-warning
     ((,class (:foreground ,yellow-hc
                           :background ,yellow-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow-hc
                                    :background ,terminal-yellow-lc
                                    :weight bold))))

   `(flycheck-fringe-info
     ((,class (:foreground ,blue-hc
                           :background ,blue-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-blue-hc
                                    :background ,terminal-blue-lc
                                    :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,yellow)
                   :inherit unspecified))
      (,class (:foreground ,yellow
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-yellow)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold
                                    :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,red)
                   :inherit unspecified))
      (,class (:foreground ,red
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-red
                                    :weight bold
                                    :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,class (:background ,green
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-green
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))

   `(git-gutter:deleted
     ((,class (:background ,red
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-red
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))

   `(git-gutter:modified
     ((,class (:background ,blue
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-blue
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))

   `(git-gutter:unchanged
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,class (:foreground ,green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-green
                                    :weight bold))))

   `(git-gutter-fr:deleted
     ((,class (:foreground ,red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-red
                                    :weight bold))))

   `(git-gutter-fr:modified
     ((,class (:foreground ,blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-blue
                                    :weight bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,class (:background ,green
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-green
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))

   `(git-gutter+-deleted
     ((,class (:background ,red
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-red
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))

   `(git-gutter+-modified
     ((,class (:background ,blue
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-blue
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))

   `(git-gutter+-unchanged
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))

   `(git-gutter-fr+-added
     ((,class (:foreground ,green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-green
                                    :weight bold))))

   `(git-gutter-fr+-deleted
     ((,class (:foreground ,red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-red
                                    :weight bold))))

   `(git-gutter-fr+-modified
     ((,class (:foreground ,blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-blue
                                    :weight bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(guide-key/key-face
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(guide-key/prefix-command-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   ;; gnus
   `(gnus-group-mail-1
     ((,class (:weight bold
                       :inherit gnus-group-mail-1-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,class (:inherit gnus-group-news-1-empty))
      (,terminal-class (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,class (:weight bold
                       :inherit gnus-group-mail-2-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,class (:inherit gnus-group-news-2-empty))
      (,terminal-class (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,class (:weight bold
                       :inherit gnus-group-mail-3-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,class (:inherit gnus-group-news-3-empty))
      (,terminal-class (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,class (:weight bold
                       :inherit gnus-group-mail-low-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,class (:inherit gnus-group-news-low-empty))
      (,terminal-class (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,class (:weight bold
                       :inherit gnus-group-news-1-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,class (:weight bold
                       :inherit gnus-group-news-2-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,class (:weight bold
                       :inherit gnus-group-news-3-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,class (:weight bold
                       :inherit gnus-group-news-4-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,class (:weight bold
                       :inherit gnus-group-news-5-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,class (:weight bold
                       :inherit gnus-group-news-6-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,class (:weight bold
                       :inherit gnus-group-news-low-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,class (:inherit message-header-other))
      (,terminal-class (:inherit message-header-other))))

   `(gnus-header-from
     ((,class (:inherit message-header-other))
      (,terminal-class (:inherit message-header-other))))

   `(gnus-header-name
     ((,class (:inherit message-header-name))
      (,terminal-class (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,class (:inherit message-header-other))
      (,terminal-class (:inherit message-header-other))))

   `(gnus-header-subject
     ((,class (:inherit message-header-subject))
      (,terminal-class (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(gnus-summary-high-ancient
     ((,class (:foreground ,blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-blue
                                    :weight bold))))

   `(gnus-summary-high-read
     ((,class (:foreground ,green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-green
                                    :weight bold))))

   `(gnus-summary-high-ticked
     ((,class (:foreground ,orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-orange
                                    :weight bold))))

   `(gnus-summary-high-unread
     ((,class (:foreground ,dark-material-fg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :weight bold))))

   `(gnus-summary-low-ancient
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(gnus-summary-low-read
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(gnus-summary-low-ticked
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(gnus-summary-low-unread
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(gnus-summary-normal-ancient
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(gnus-summary-normal-read
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(gnus-summary-normal-ticked
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(gnus-summary-normal-unread
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(gnus-summary-selected
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   `(gnus-cite-1
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(gnus-cite-2
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(gnus-cite-3
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(gnus-cite-4
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(gnus-cite-5
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(gnus-cite-6
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(gnus-cite-7
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(gnus-cite-8
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(gnus-cite-9
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(gnus-cite-10
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(gnus-cite-11
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(gnus-group-news-1-empty
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(gnus-group-news-2-empty
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(gnus-group-news-3-empty
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(gnus-group-news-4-empty
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(gnus-group-news-5-empty
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(gnus-group-news-6-empty
     ((,class (:foreground ,blue-lc))
      (,terminal-class (:foreground ,terminal-blue-lc))))

   `(gnus-group-news-low-empty
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(gnus-signature
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(gnus-x-face
     ((,class (:background ,dark-material-fg
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-dark-material-fg
                                    :foreground ,terminal-dark-material-bg))))


   ;; helm (these probably need tweaking)
   `(helm-apt-deinstalled
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(helm-apt-installed
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(helm-bookmark-directory
     ((,class (:inherit helm-ff-directory))
      (,terminal-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(helm-bookmark-gnus
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(helm-bookmark-info
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(helm-bookmark-man
     ((,class (:foreground ,violet))
      (,terminal-class (:foreground ,terminal-violet))))

   `(helm-bookmark-w3m
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(helm-bookmarks-su
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(helm-buffer-not-saved
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(helm-buffer-saved-out
     ((,class (:foreground ,red
                           :background ,dark-material-bg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-red
                                    :background ,terminal-dark-material-bg
                                    :inverse-video t))))

   `(helm-buffer-size
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(helm-candidate-number
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-emph
                           :bold t))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-emph
                                    :bold t))))

   `(helm-ff-directory
     ((,class (:background ,dark-material-bg
                           :foreground ,blue))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-blue))))

   `(helm-ff-executable
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(helm-ff-file
     ((,class (:background ,dark-material-bg
                           :foreground ,dark-material-fg))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-dark-material-fg))))

   `(helm-ff-invalid-symlink
     ((,class (:background ,dark-material-bg
                           :foreground ,orange
                           :slant italic))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-orange
                                    :slant italic))))

   `(helm-ff-prefix
     ((,class (:background ,yellow
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-yellow
                                    :foreground ,terminal-dark-material-bg))))

   `(helm-ff-symlink
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(helm-grep-file
     ((,class (:foreground ,cyan
                           :underline t))
      (,terminal-class (:foreground ,terminal-cyan
                                    :underline t))))

   `(helm-grep-finish
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(helm-grep-lineno
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(helm-grep-match
     ((,class (:inherit match)))
     ((,terminal-class (:inherit match))))

   `(helm-grep-running
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(helm-header
     ((,class (:inherit header-line))
      (,terminal-class (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(helm-lisp-show-completion
     ((,class (:foreground ,yellow
                           :background ,dark-material-hl
                           :bold t))
      (,terminal-class (:foreground ,terminal-yellow
                                    :background ,terminal-dark-material-hl
                                    :bold t))))

   `(helm-M-x-key
     ((,class (:foreground ,orange
                           :underline t))
      (,terminal-class (:foreground ,terminal-orange
                                    :underline t))))

   `(helm-moccur-buffer
     ((,class (:foreground ,cyan
                           :underline t))
      (,terminal-class (:foreground ,terminal-cyan
                                    :underline t))))

   `(helm-match
     ((,class (:background ,violet-d
                           :foreground ,dark-material-bg
                           :underline nil))
      (,terminal-class (:background ,terminal-violet-d
                                    :foreground ,terminal-dark-material-bg
                                    :underline nil))))

   `(helm-selection
     ((,class (:background ,red
                           :underline nil))
      (,terminal-class (:background ,terminal-red
                                    :foreground ,terminal-dark-material-bg
                                    :underline nil))))

   `(helm-selection-line
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-emph
                           :underline nil))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-emph
                                    :underline nil))))

   `(helm-separator
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(helm-source-header
     ((,class (:background ,blue-lc
                           :foreground ,dark-material-bg
                           :underline nil))
      (,terminal-class (:background ,terminal-blue-lc
                                    :foreground ,terminal-dark-material-bg
                                    :underline nil))))

   `(helm-swoop-target-line-face
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   `(helm-swoop-target-line-block-face
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   `(helm-swoop-target-word-face
     ((,class (:foreground ,magenta
                           :background unspecified))
      (,terminal-class (:foreground ,terminal-magenta
                                    :background unspecified))))

   `(helm-time-zone-current
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(helm-time-zone-home
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(helm-visible-mark
     ((,class (:background ,dark-material-bg
                           :foreground ,magenta :bold t))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,class :foreground ,blue)
      (,terminal-class :foreground ,terminal-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,class :foreground ,blue-l)
      (,terminal-class :foreground ,terminal-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,class :foreground ,blue-l)
      (,terminal-class :foreground ,terminal-blue-l)))

   `(helm-ls-git-untracked-face
     ((,class :foreground ,orange)
      (,terminal-class :foreground ,terminal-orange)))

   `(helm-ls-git-added-copied-face
     ((,class :foreground ,green)
      (,terminal-class :foreground ,terminal-green)))

   `(helm-ls-git-added-modified-face
     ((,class :foreground ,green-l)
      (,terminal-class :foreground ,terminal-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,class :foreground ,red)
      (,terminal-class :foreground ,terminal-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,class :foreground ,red-l)
      (,terminal-class :foreground ,terminal-red-l)))

   `(helm-ls-git-conflict-face
     ((,class :foreground ,yellow)
      (,terminal-class :foreground ,terminal-yellow)))

   ;; hi-lock-mode
   `(hi-yellown
     ((,class (:foreground ,yellow-lc
                           :background ,yellow-hc))
      (,terminal-class (:foreground ,terminal-yellow-lc
                                    :background ,terminal-yellow-hc))))

   `(hi-pink
     ((,class (:foreground ,magenta-lc
                           :background ,magenta-hc))
      (,terminal-class (:foreground ,terminal-magenta-lc
                                    :background ,terminal-magenta-hc))))

   `(hi-green
     ((,class (:foreground ,green-lc
                           :background ,green-hc))
      (,terminal-class (:foreground ,terminal-green-lc
                                    :background ,terminal-green-hc))))

   `(hi-blue
     ((,class (:foreground ,blue-lc
                           :background ,blue-hc))
      (,terminal-class (:foreground ,terminal-blue-lc
                                    :background ,terminal-blue-hc))))

   `(hi-black-b
     ((,class (:foreground ,dark-material-emph
                           :background ,dark-material-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :background ,terminal-dark-material-bg
                                    :weight bold))))

   `(hi-blue-b
     ((,class (:foreground ,blue-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-blue-lc
                                    :weight bold))))

   `(hi-green-b
     ((,class (:foreground ,green-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-green-lc
                                    :weight bold))))

   `(hi-red-b
     ((,class (:foreground ,red
                           :weight bold))))

   `(hi-black-hb
     ((,class (:foreground ,dark-material-emph
                           :background ,dark-material-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :background ,terminal-dark-material-bg
                                    :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(highlight-changes-delete
     ((,class (:foreground ,red
                           :underline t))
      (,terminal-class (:foreground ,terminal-red
                                    :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   `(highlight-indentation-current-column-face
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   ;; hl-line-mode
   `(hl-line
     ((,class (:background ,dark-material-hl-line
                           :inherit t))
      (,terminal-class (:background ,terminal-dark-material-hl-line
                                    :inherit t))))

   `(hl-line-face
     ((,class (:background ,dark-material-hl-line
                           :inherit t))
      (,terminal-class (:background ,terminal-dark-material-hl-line
                                    :inherit t))))

   ;; ido-mode
   `(ido-first-match
     ((,class (:foreground ,yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight normal))))

   `(ido-only-match
     ((,class (:foreground ,dark-material-bg
                           :background ,yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-dark-material-bg
                                    :background ,terminal-yellow
                                    :weight normal))))

   `(ido-subdir
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(ido-incomplete-regexp
     ((,class (:foreground ,red
                           :weight bold ))
      (,terminal-class (:foreground ,terminal-red
                                    :weight bold ))))

   `(ido-indicator
     ((,class (:background ,red
                           :foreground ,dark-material-bg
                           :width condensed))
      (,terminal-class (:background ,terminal-red
                                    :foreground ,terminal-dark-material-bg
                                    :width condensed))))

   `(ido-virtual
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(jabber-activity-face
     ((,class (:weight bold
                       :foreground ,red))
      (,terminal-class (:weight bold
                                :foreground ,terminal-red))))

   `(jabber-activity-personal-face
     ((,class (:weight bold
                       :foreground ,blue))
      (,terminal-class (:weight bold
                                :foreground ,terminal-blue))))

   `(jabber-chat-error
     ((,class (:weight bold
                       :foreground ,red))
      (,terminal-class (:weight bold
                                :foreground ,terminal-red))))

   `(jabber-chat-prompt-foreign
     ((,class (:weight bold
                       :foreground ,red))
      (,terminal-class (:weight bold
                                :foreground ,terminal-red))))

   `(jabber-chat-prompt-local
     ((,class (:weight bold
                       :foreground ,blue))
      (,terminal-class (:weight bold
                                :foreground ,terminal-blue))))

   `(jabber-chat-prompt-system
     ((,class (:weight bold
                       :foreground ,green))
      (,terminal-class (:weight bold
                                :foreground ,terminal-green))))

   `(jabber-chat-text-foreign
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(jabber-chat-text-local
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(jabber-chat-rare-time-face
     ((,class (:underline t
                          :foreground ,green))
      (,terminal-class (:underline t
                                   :foreground ,terminal-green))))

   `(jabber-roster-user-away
     ((,class (:slant italic
                      :foreground ,green))
      (,terminal-class (:slant italic
                               :foreground ,terminal-green))))

   `(jabber-roster-user-chatty
     ((,class (:weight bold
                       :foreground ,orange))
      (,terminal-class (:weight bold
                                :foreground ,terminal-orange))))

   `(jabber-roster-user-dnd
     ((,class (:slant italic
                      :foreground ,red))
      (,terminal-class (:slant italic
                               :foreground ,terminal-red))))

   `(jabber-roster-user-error
     ((,class (:weight light
                       :slant italic
                       :foreground ,red))
      (,terminal-class (:weight light
                                :slant italic
                                :foreground ,terminal-red))))

   `(jabber-roster-user-offline
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(jabber-roster-user-online
     ((,class (:weight bold
                       :foreground ,blue))
      (,terminal-class (:weight bold
                                :foreground ,terminal-blue))))

   `(jabber-roster-user-xa
     ((,class (:slant italic
                      :foreground ,magenta))
      (,terminal-class (:slant italic
                               :foreground ,terminal-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(js2-external-variable
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(js2-function-param
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(js2-instance-member
     ((,class (:foreground ,magenta)))
     ((,terminal-class (:foreground ,terminal-magenta))))

   `(js2-jsdoc-html-tag-delimiter
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(js2-jsdoc-html-tag-name
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(js2-jsdoc-tag
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(js2-jsdoc-type
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(js2-jsdoc-value
     ((,class (:foreground ,violet))
      (,terminal-class (:foreground ,terminal-violet))))

   `(js2-magic-paren
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(js2-private-function-call
     ((,class (:foreground ,yellow)))
     ((,terminal-class (:foreground ,terminal-yellow))))

   `(js2-private-member
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(js2-warning
     ((,class (:underline ,orange))
      (,terminal-class (:underline ,terminal-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,class (:inherit bold))
      (,terminal-class (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,class (:foreground ,dark-material-comments
                           :background ,s-fringe-bg))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :background ,terminal-s-fringe-bg))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,class (:inherit dired-directory))
      (,terminal-class (:inherit dired-directory))))

   `(lusty-file-face
     ((,class nil)
      (,terminal-class nil)))

   `(lusty-match-face
     ((,class (:inherit ido-first-match))
      (,terminal-class (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,class (:foreground ,cyan
                           :weight bold))
      (,terminal-class (:foreground ,terminal-cyan
                                    :weight bold))))

   ;; magit
   `(magit-section-heading
     ((,class (:foreground ,cyan
                           :weight bold))
      (,terminal-class (:foreground ,terminal-cyan
                                    :weight bold))))

   `(magit-branch-local
     ((,class (:foreground ,orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-orange
                                    :weight bold))))

   `(magit-branch-remote
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   `(magit-item-highlight
     ((,class (:background ,dark-material-hl
                           :weight unspecified))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :weight unspecified))))
   `(magit-log-author
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(magit-log-graph
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(magit-log-head-label-bisect-bad
     ((,class (:background ,red-hc
                           :foreground ,red-lc
                           :box 1))
      (,terminal-class (:background ,terminal-red-hc
                                    :foreground ,terminal-red-lc
                                    :box 1))))

   `(magit-log-head-label-bisect-good
     ((,class (:background ,green-hc
                           :foreground ,green-lc
                           :box 1))
      (,terminal-class (:background ,terminal-green-hc
                                    :foreground ,terminal-green-lc
                                    :box 1))))

   `(magit-log-head-label-default
     ((,class (:background ,dark-material-hl
                           :box 1))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :box 1))))

   `(magit-log-head-label-local
     ((,class (:background ,blue-lc
                           :foreground ,blue-hc
                           :box 1))
      (,terminal-class (:background ,terminal-blue-lc
                                    :foreground ,terminal-blue-hc
                                    :box 1))))

   `(magit-log-head-label-patches
     ((,class (:background ,red-lc
                           :foreground ,red-hc
                           :box 1))
      (,terminal-class (:background ,terminal-red-lc
                                    :foreground ,terminal-red-hc
                                    :box 1))))

   `(magit-log-head-label-remote
     ((,class (:background ,green-lc
                           :foreground ,green-hc
                           :box 1))
      (,terminal-class (:background ,terminal-green-lc
                                    :foreground ,terminal-green-hc
                                    :box 1))))

   `(magit-log-head-label-tags
     ((,class (:background ,yellow-lc
                           :foreground ,yellow-hc
                           :box 1))
      (,terminal-class (:background ,terminal-yellow-lc
                                    :foreground ,terminal-yellow-hc
                                    :box 1))))

   `(magit-log-sha1
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   ;; monky
   `(monky-section-title
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   `(monky-diff-add
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(monky-diff-del
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(markdown-header-face-1
     ((,class (:inherit markdown-header-face
                        :height ,dark-material-height-plus-4))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,dark-material-height-plus-4))))

   `(markdown-header-face-2
     ((,class (:inherit markdown-header-face
                        :height ,dark-material-height-plus-3))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,dark-material-height-plus-3))))

   `(markdown-header-face-3
     ((,class (:inherit markdown-header-face
                        :height ,dark-material-height-plus-2))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,dark-material-height-plus-2))))

   `(markdown-header-face-4
     ((,class (:inherit markdown-header-face
                        :height ,dark-material-height-plus-1))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,dark-material-height-plus-1))))

   `(markdown-header-face-5
     ((,class (:inherit markdown-header-face))
      (,terminal-class (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,class (:inherit markdown-header-face))
      (,terminal-class (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(message-header-name
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(message-header-other
     ((,class (:foreground ,dark-material-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :weight normal))))

   `(message-header-to
     ((,class (:foreground ,dark-material-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :weight normal))))

   `(message-header-cc
     ((,class (:foreground ,dark-material-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :weight normal))))

   `(message-header-newsgroups
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   `(message-header-subject
     ((,class (:foreground ,cyan
                           :weight normal))
      (,terminal-class (:foreground ,terminal-cyan
                                    :weight normal))))

   `(message-header-xheader
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(message-mml
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   `(message-separator
     ((,class (:foreground ,dark-material-comments
                           :slant italic))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(mew-face-header-from
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(mew-face-header-date
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(mew-face-header-to
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(mew-face-header-key
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(mew-face-header-private
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(mew-face-header-important
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(mew-face-header-marginal
     ((,class (:foreground ,dark-material-fg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :weight bold))))

   `(mew-face-header-warning
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(mew-face-header-xmew
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(mew-face-header-xmew-bad
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(mew-face-body-url
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(mew-face-body-comment
     ((,class (:foreground ,dark-material-fg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :slant italic))))

   `(mew-face-body-cite1
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(mew-face-body-cite2
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(mew-face-body-cite3
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(mew-face-body-cite4
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(mew-face-body-cite5
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(mew-face-mark-review
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(mew-face-mark-escape
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(mew-face-mark-delete
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(mew-face-mark-unlink
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(mew-face-mark-refile
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(mew-face-mark-unread
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(mew-face-eof-message
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(mew-face-eof-part
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(mingus-pausing-face
     ((,class (:foreground ,magenta))
      (,terminal-class (:foreground ,terminal-magenta))))

   `(mingus-playing-face
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(mingus-playlist-face
     ((,class (:foreground ,cyan ))
      (,terminal-class (:foreground ,terminal-cyan ))))

   `(mingus-song-file-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(mingus-stopped-face
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   ;; moccur
   `(moccur-current-line-face
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(moccur-edit-done-face
     ((,class (:foreground ,dark-material-comments
                           :background ,dark-material-bg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :background ,terminal-dark-material-bg
                                    :slant italic))))

   `(moccur-edit-face
     ((,class (:background ,yellow
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-yellow
                                    :foreground ,terminal-dark-material-bg))))

   `(moccur-edit-file-face
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   `(moccur-edit-reject-face
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(moccur-face
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-emph
                                    :weight bold))))

   `(search-buffers-face
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-emph
                                    :weight bold))))

   `(search-buffers-header-face
     ((,class (:background ,dark-material-hl
                           :foreground ,yellow
                           :weight bold))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-yellow
                                    :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,class (:foreground ,green
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-green
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-2-face
     ((,class (:foreground ,blue
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-blue
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-3-face
     ((,class (:foreground ,orange
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-orange
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-4-face
     ((,class (:foreground ,yellow
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-yellow
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-5-face
     ((,class (:foreground ,cyan
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-cyan
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-6-face
     ((,class (:foreground ,green
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-green
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-7-face
     ((,class (:foreground ,blue
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-blue
                                    :slant italic
                                    :weight normal))))

   `(mu4e-flagged-face
     ((,class (:foreground ,magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-magenta
                                    :weight bold))))

   `(mu4e-view-url-number-face
     ((,class (:foreground ,yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight normal))))

   `(mu4e-warning-face
     ((,class (:foreground ,red
                           :slant normal
                           :weight bold))
      (,terminal-class (:foreground ,terminal-red
                                    :slant normal
                                    :weight bold))))

   `(mu4e-header-highlight-face
     ((,class (:inherit unspecified
                        :foreground unspecified
                        :background ,dark-material-hl
                        :underline ,dark-material-emph
                        :weight normal))
      (,terminal-class (:inherit unspecified
                                 :foreground unspecified
                                 :background ,terminal-dark-material-hl
                                 :underline ,terminal-dark-material-emph
                                 :weight normal))))


   `(mu4e-draft-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,class (:inherit font-lock-comment-face))
      (,terminal-class (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,class (:inherit font-lock-builtin-face
                        :weight normal))
      (,terminal-class (:inherit font-lock-builtin-face
                                 :weight normal))))

   `(mu4e-header-face
     ((,class (:inherit default))
      (,terminal-class (:inherit default))))

   `(mu4e-header-marks-face
     ((,class (:inherit font-lock-preprocessor-face))
      (,terminal-class (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,class (:inherit font-lock-type-face))
      (,terminal-class (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,class (:inherit font-lock-pseudo-keyword-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-pseudo-keyword-face
                                 :weight bold))))

   `(mu4e-moved-face
     ((,class (:inherit font-lock-comment-face
                        :slant italic))
      (,terminal-class (:inherit font-lock-comment-face
                                 :slant italic))))

   `(mu4e-ok-face
     ((,class (:inherit font-lock-comment-face
                        :slant normal
                        :weight bold))
      (,terminal-class (:inherit font-lock-comment-face
                                 :slant normal
                                 :weight bold))))

   `(mu4e-replied-face
     ((,class (:inherit font-lock-builtin-face
                        :weight normal))
      (,terminal-class (:inherit font-lock-builtin-face
                                 :weight normal))))

   `(mu4e-system-face
     ((,class (:inherit font-lock-comment-face
                        :slant italic))
      (,terminal-class (:inherit font-lock-comment-face
                                 :slant italic))))

   `(mu4e-title-face
     ((,class (:inherit font-lock-type-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-type-face
                                 :weight bold))))

   `(mu4e-trashed-face
     ((,class (:inherit font-lock-comment-face
                        :strike-through t))
      (,terminal-class (:inherit font-lock-comment-face
                                 :strike-through t))))

   `(mu4e-unread-face
     ((,class (:inherit font-lock-keyword-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-keyword-face
                                 :weight bold))))

   `(mu4e-view-attach-number-face
     ((,class (:inherit font-lock-variable-name-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-variable-name-face
                                 :weight bold))))

   `(mu4e-view-contact-face
     ((,class (:foreground ,dark-material-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :weight normal))))

   `(mu4e-view-header-key-face
     ((,class (:inherit message-header-name
                        :weight normal))
      (,terminal-class (:inherit message-header-name
                                 :weight normal))))

   `(mu4e-view-header-value-face
     ((,class (:foreground ,cyan
                           :weight normal
                           :slant normal))
      (,terminal-class (:foreground ,terminal-cyan
                                    :weight normal
                                    :slant normal))))

   `(mu4e-view-link-face
     ((,class (:inherit link))
      (,terminal-class (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,class (:foreground ,blue
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-blue
                                    :weight normal
                                    :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   ;; nav
   `(nav-face-heading
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(nav-face-button-num
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(nav-face-dir
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(nav-face-hdir
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(nav-face-file
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(nav-face-hfile
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   ;; org-mode
   `(org-agenda-structure
     ((,class (:foreground ,dark-material-emph
                           :background ,dark-material-hl
                           :weight bold
                           :slant normal
                           :inverse-video nil
                           :height ,dark-material-height-plus-1
                           :underline nil
                           :box (:line-width 2 :color ,dark-material-bg)))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :background ,terminal-dark-material-hl
                                    :weight bold
                                    :slant normal
                                    :inverse-video nil
                                    :height ,dark-material-height-plus-1
                                    :underline nil
                                    :box (:line-width 2 :color ,terminal-dark-material-bg)))))

   `(org-agenda-calendar-event
     ((,class (:foreground ,dark-material-emph))
      (,terminal-class (:foreground ,terminal-dark-material-emph))))

   `(org-agenda-calendar-sexp
     ((,class (:foreground ,dark-material-fg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :slant italic))))

   `(org-agenda-date
     ((,class (:foreground ,dark-material-comments
                           :background ,dark-material-bg
                           :weight normal
                           :inverse-video nil
                           :overline nil
                           :slant normal
                           :height 1.0
                           :box (:line-width 2 :color ,dark-material-bg)))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :background ,terminal-dark-material-bg
                                    :weight normal
                                    :inverse-video nil
                                    :overline nil
                                    :slant normal
                                    :height 1.0
                                    :box (:line-width 2 :color ,terminal-dark-material-bg)))) t)

   `(org-agenda-date-weekend
     ((,class (:inherit org-agenda-date
                        :inverse-video nil
                        :background unspecified
                        :foreground ,dark-material-comments
                        :weight unspecified
                        :underline t
                        :overline nil
                        :box unspecified))
      (,terminal-class (:inherit org-agenda-date
                                 :inverse-video nil
                                 :background unspecified
                                 :foreground ,terminal-dark-material-comments
                                 :weight unspecified
                                 :underline t
                                 :overline nil
                                 :box unspecified))) t)

   `(org-agenda-date-today
     ((,class (:inherit org-agenda-date
                        :inverse-video t
                        :weight bold
                        :underline unspecified
                        :overline nil
                        :box unspecified
                        :foreground ,blue
                        :background ,dark-material-bg))
      (,terminal-class (:inherit org-agenda-date
                                 :inverse-video t
                                 :weight bold
                                 :underline unspecified
                                 :overline nil
                                 :box unspecified
                                 :foreground ,terminal-blue
                                 :background ,terminal-dark-material-bg))) t)

   `(org-agenda-done
     ((,class (:foreground ,dark-material-comments
                           :slant italic))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :slant italic))) t)

   `(org-archived
     ((,class (:foreground ,dark-material-comments
                           :weight normal))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :weight normal))))

   `(org-block
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(org-block-begin-line
     ((,class (:foreground ,dark-material-comments
                           :slant italic))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :slant italic))))

   `(org-checkbox
     ((,class (:background ,dark-material-bg
                           :foreground ,dark-material-fg
                           :box (:line-width 1 :style released-button)))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-dark-material-fg
                                    :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(org-date
     ((,class (:foreground ,blue
                           :underline t))
      (,terminal-class (:foreground ,terminal-blue
                                    :underline t))))

   `(org-done
     ((,class (:weight bold
                       :foreground ,green))
      (,terminal-class (:weight bold
                                :foreground ,terminal-green))))

   `(org-ellipsis
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(org-formula
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(org-headline-done
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(org-hide
     ((,class (:foreground ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-dark-material-bg))))

   `(org-level-1
     ((,class (:inherit ,s-variable-pitch
                        :height ,dark-material-height-plus-4
                        :foreground ,orange))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,dark-material-height-plus-4
                                 :foreground ,terminal-orange))))

   `(org-level-2
     ((,class (:inherit ,s-variable-pitch
                        :height ,dark-material-height-plus-3
                        :foreground ,green))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,dark-material-height-plus-3
                                 :foreground ,terminal-green))))

   `(org-level-3
     ((,class (:inherit ,s-variable-pitch
                        :height ,dark-material-height-plus-2
                        :foreground ,blue))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,dark-material-height-plus-2
                                 :foreground ,terminal-blue))))

   `(org-level-4
     ((,class (:inherit ,s-variable-pitch
                        :height ,dark-material-height-plus-1
                        :foreground ,yellow))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,dark-material-height-plus-1
                                 :foreground ,terminal-yellow))))

   `(org-level-5
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,cyan))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-cyan))))

   `(org-level-6
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,green))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-green))))

   `(org-level-7
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,red))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-red))))

   `(org-level-8
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,blue))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-blue))))

   `(org-link
     ((,class (:foreground ,yellow
                           :underline t))
      (,terminal-class (:foreground ,terminal-yellow
                                    :underline t))))

   `(org-sexp-date
     ((,class (:foreground ,violet))
      (,terminal-class (:foreground ,terminal-violet))))

   `(org-scheduled
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(org-scheduled-previously
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(org-scheduled-today
     ((,class (:foreground ,blue
                           :weight normal))
      (,terminal-class (:foreground ,terminal-blue
                                    :weight normal))))

   `(org-special-keyword
     ((,class (:foreground ,dark-material-comments
                           :weight bold))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :weight bold))))

   `(org-table
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(org-tag
     ((,class (:weight bold))
      (,terminal-class (:weight bold))))

   `(org-time-grid
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(org-todo
     ((,class (:foreground ,red
                           :weight bold)))
     ((,terminal-class (:foreground ,terminal-red
                                    :weight bold))))

   `(org-upcoming-deadline
     ((,class (:foreground ,yellow
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight normal
                                    :underline nil))))

   `(org-warning
     ((,class (:foreground ,orange
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-orange
                                    :weight normal
                                    :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,class (:background ,blue-lc
                           :foreground ,blue-hc))
      (,terminal-class (:background ,terminal-blue-lc
                                    :foreground ,terminal-blue-hc))))

   `(org-habit-clear-future-face
     ((,class (:background ,blue-lc))
      (,terminal-class (:background ,terminal-blue-lc))))

   `(org-habit-ready-face
     ((,class (:background ,green-lc
                           :foreground ,green))
      (,terminal-class (:background ,terminal-green-lc
                                    :foreground ,terminal-green))))

   `(org-habit-ready-future-face
     ((,class (:background ,green-lc))
      (,terminal-class (:background ,terminal-green-lc))))

   `(org-habit-alert-face
     ((,class (:background ,yellow
                           :foreground ,yellow-lc))
      (,terminal-class (:background ,terminal-yellow
                                    :foreground ,terminal-yellow-lc))))

   `(org-habit-alert-future-face
     ((,class (:background ,yellow-lc))
      (,terminal-class (:background ,terminal-yellow-lc))))

   `(org-habit-overdue-face
     ((,class (:background ,red
                           :foreground ,red-lc))
      (,terminal-class (:background ,terminal-red
                                    :foreground ,terminal-red-lc))))

   `(org-habit-overdue-future-face
     ((,class (:background ,red-lc))
      (,terminal-class (:background ,terminal-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(org-agenda-restriction-lock
     ((,class (:background ,yellow))
      (,terminal-class (:background ,terminal-yellow))))

   `(org-clock-overlay
     ((,class (:background ,yellow))
      (,terminal-class (:background ,terminal-yellow))))

   `(org-column
     ((,class (:background ,dark-material-hl
                           :strike-through nil
                           :underline nil
                           :slant normal
                           :weight normal
                           :inherit default))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :strike-through nil
                                    :underline nil
                                    :slant normal
                                    :weight normal
                                    :inherit default))))

   `(org-column-title
     ((,class (:background ,dark-material-hl
                           :underline t
                           :weight bold))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :underline t
                                    :weight bold))))

   `(org-date-selected
     ((,class (:foreground ,red
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-red
                                    :inverse-video t))))

   `(org-document-info
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(org-document-title
     ((,class (:foreground ,dark-material-emph
                           :weight bold
                           :height ,dark-material-height-plus-4))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :weight bold
                                    :height ,dark-material-height-plus-4))))

   `(org-drawer
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(org-footnote
     ((,class (:foreground ,magenta
                           :underline t))
      (,terminal-class (:foreground ,terminal-magenta
                                    :underline t))))

   `(org-latex-and-export-specials
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(org-mode-line-clock-overrun
     ((,class (:inherit mode-line
                        :background ,red))
      (,terminal-class (:inherit mode-line
                                 :background ,terminal-red))))

   ;; outline
   `(outline-1
     ((,class (:inherit org-level-1))
      (,terminal-class (:inherit org-level-1))))

   `(outline-2
     ((,class (:inherit org-level-2))
      (,terminal-class (:inherit org-level-2))))

   `(outline-3
     ((,class (:inherit org-level-3))
      (,terminal-class (:inherit org-level-3))))

   `(outline-4
     ((,class (:inherit org-level-4))
      (,terminal-class (:inherit org-level-4))))

   `(outline-5
     ((,class (:inherit org-level-5))
      (,terminal-class (:inherit org-level-5))))

   `(outline-6
     ((,class (:inherit org-level-6))
      (,terminal-class (:inherit org-level-6))))

   `(outline-7
     ((,class (:inherit org-level-7))
      (,terminal-class (:inherit org-level-7))))

   `(outline-8
     ((,class (:inherit org-level-8))
      (,terminal-class (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,terminal-class (:foreground ,dark-material-comments))))

   ;; perspective
   `(persp-selected-face
     ((,class (:foreground ,blue
                           :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,class (:foreground ,yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight normal))))

   ;; popup
   `(popup-face
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-fg))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-fg))))

   `(popup-isearch-match
     ((,class (:background ,yellow
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-yellow
                                    :foreground ,terminal-dark-material-bg))))

   `(popup-menu-face
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-fg))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-fg))))

   `(popup-menu-mouse-face
     ((,class (:background ,blue
                           :foreground ,dark-material-fg))
      (,terminal-class (:background ,terminal-blue
                                    :foreground ,terminal-dark-material-fg))))

   `(popup-menu-selection-face
     ((,class (:background ,magenta
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-magenta
                                    :foreground ,terminal-dark-material-bg))))

   `(popup-scroll-bar-background-face
     ((,class (:background ,dark-material-comments))
      (,terminal-class (:background ,terminal-dark-material-comments))))

   `(popup-scroll-bar-foreground-face
     ((,class (:background ,dark-material-emph))
      (,terminal-class (:background ,terminal-dark-material-emph))))

   `(popup-tip-face
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-fg))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-fg))))

   ;; powerline
   `(powerline-active1
     ((,class (:background ,gray))
      (,terminal-class (:background ,terminal-gray))))

   `(powerline-active2
     ((,class (:background ,gray-l))
      (,terminal-class (:background ,terminal-gray-l))))

   `(powerline-inactive1
     ((,class (:background ,gray-d))
      (,terminal-class (:background ,terminal-gray-d))))

   `(powerline-inactive2
     ((,class (:background ,gray))
      (,terminal-class (:background ,terminal-gray))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(rainbow-delimiters-depth-2-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(rainbow-delimiters-depth-3-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(rainbow-delimiters-depth-4-face
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(rainbow-delimiters-depth-5-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(rainbow-delimiters-depth-6-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(rainbow-delimiters-depth-7-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(rainbow-delimiters-depth-8-face
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(rainbow-delimiters-depth-9-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(rainbow-delimiters-depth-10-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(rainbow-delimiters-depth-12-face
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(rainbow-delimiters-unmatched-face
     ((,class (:foreground ,dark-material-fg
                           :background ,dark-material-bg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :background ,terminal-dark-material-bg
                                    :inverse-video t))))

   ;; rhtm-mode
   `(erb-face
     ((,class (:foreground ,dark-material-emph
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :background ,terminal-dark-material-bg))))

   `(erb-delim-face
     ((,class (:foreground ,cyan
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-cyan
                                    :background ,terminal-dark-material-bg))))

   `(erb-exec-face
     ((,class (:foreground ,dark-material-emph
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :background ,terminal-dark-material-bg))))

   `(erb-exec-delim-face
     ((,class (:foreground ,cyan
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-cyan
                                    :background ,terminal-dark-material-bg))))

   `(erb-out-face
     ((,class (:foreground ,dark-material-emph
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :background ,terminal-dark-material-bg))))

   `(erb-out-delim-face
     ((,class (:foreground ,cyan
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-cyan
                                    :background ,terminal-dark-material-bg))))

   `(erb-comment-face
     ((,class (:foreground ,dark-material-emph
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :background ,terminal-dark-material-bg))))

   `(erb-comment-delim-face
     ((,class (:foreground ,cyan
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-cyan
                                    :background ,terminal-dark-material-bg))))

   ;; rst-mode
   `(rst-level-1-face
     ((,class (:background ,yellow
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-yellow
                                    :foreground ,terminal-dark-material-bg))))

   `(rst-level-2-face
     ((,class (:background ,cyan
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-cyan
                                    :foreground ,terminal-dark-material-bg))))

   `(rst-level-3-face
     ((,class (:background ,blue
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-blue
                                    :foreground ,terminal-dark-material-bg))))

   `(rst-level-4-face
     ((,class (:background ,violet
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-violet
                                    :foreground ,terminal-dark-material-bg))))

   `(rst-level-5-face
     ((,class (:background ,magenta
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-magenta
                                    :foreground ,terminal-dark-material-bg))))

   `(rst-level-6-face
     ((,class (:background ,red
                           :foreground ,dark-material-bg))
      (,terminal-class (:background ,terminal-red
                                    :foreground ,terminal-dark-material-bg))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(rpm-spec-doc-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(rpm-spec-ghost-face
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(rpm-spec-macro-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(rpm-spec-package-face
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(rpm-spec-section-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(rpm-spec-tag-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(rpm-spec-var-face
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,class (:foreground ,violet
                           :weight bold))
      (,terminal-class (:foreground ,terminal-violet
                                    :weight bold))))

   `(sh-escaped-newline
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   `(sh-heredoc
     ((,class (:foreground ,yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight bold))))

   ;; erlang
   `(erlang-font-lock-exported-function-name-face
     ((,class (:foreground "DeepSkyBlue"))
      (,terminal-class (:foreground "DeepSkyBlue"))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   `(sp-wrap-overlay-face
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   `(sp-wrap-tag-overlay-face
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   `(sp-show-pair-enclosing
     ((,class (:inherit highlight))
      (,terminal-class (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,class (:foreground ,cyan
                           :background ,dark-material-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-cyan
                                    :background ,terminal-dark-material-bg
                                    :weight normal
                                    :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,class (:foreground ,red
                           :background ,dark-material-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-red
                                    :background ,terminal-dark-material-bg
                                    :weight normal
                                    :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,class (:foreground ,cyan
                           :background ,dark-material-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-cyan
                                    :background ,terminal-dark-material-bg
                                    :weight normal
                                    :inverse-video t))))

   `(show-paren-mismatch
     ((,class (:foreground ,red
                           :background ,dark-material-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-red
                                    :background ,terminal-dark-material-bg
                                    :weight normal
                                    :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,class (:foreground ,cyan
                           :background ,dark-material-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-cyan
                                    :background ,terminal-dark-material-bg
                                    :weight normal
                                    :inverse-video t))))

   `(paren-face-mismatch
     ((,class (:foreground ,red
                           :background ,dark-material-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-red
                                    :background ,terminal-dark-material-bg
                                    :weight normal
                                    :inverse-video t))))

   `(paren-face-no-match
     ((,class (:foreground ,red
                           :background ,dark-material-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-red
                                    :background ,terminal-dark-material-bg
                                    :weight normal
                                    :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   ;; speedbar
   `(speedbar-button-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,dark-material-comments))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-dark-material-comments))))

   `(speedbar-directory-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,blue))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-blue))))

   `(speedbar-file-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,dark-material-fg))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-dark-material-fg))))

   `(speedbar-highlight-face
     ((,class (:inherit ,s-variable-pitch
                        :background ,dark-material-hl))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :background ,terminal-dark-material-hl))))

   `(speedbar-selected-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,yellow
                        :underline t))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-yellow
                                 :underline t))))

   `(speedbar-separator-face
     ((,class (:inherit ,s-variable-pitch
                        :background ,blue
                        :foreground ,dark-material-bg
                        :overline ,cyan-lc))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :background ,terminal-blue
                                 :foreground ,terminal-dark-material-bg
                                 :overline ,terminal-cyan-lc))))

   `(speedbar-tag-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,green))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,class (:background ,blue
                           :foreground ,dark-material-bg
                           :height ,dark-material-height-plus-1
                           :weight bold))
      (,terminal-class (:background ,terminal-blue
                                    :foreground ,terminal-dark-material-bg
                                    :height ,dark-material-height-plus-1
                                    :weight bold))))

   `(sr-editing-path-face
     ((,class (:background ,yellow
                           :foreground ,dark-material-bg
                           :weight bold
                           :height ,dark-material-height-plus-1))
      (,terminal-class (:background ,terminal-yellow
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold
                                    :height ,dark-material-height-plus-1))))

   `(sr-highlight-path-face
     ((,class (:background ,green
                           :foreground ,dark-material-bg
                           :weight bold
                           :height ,dark-material-height-plus-1))
      (,terminal-class (:background ,terminal-green
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold
                                    :height ,dark-material-height-plus-1))))

   `(sr-passive-path-face
     ((,class (:background ,dark-material-comments
                           :foreground ,dark-material-bg
                           :weight bold
                           :height ,dark-material-height-plus-1))
      (,terminal-class (:background ,terminal-dark-material-comments
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold
                                    :height ,dark-material-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,class (:inherit dired-marked))
      (,terminal-class (:inherit dired-marked))))

   `(sr-marked-file-face
     ((,class (:inherit dired-marked))
      (,terminal-class (:inherit dired-marked))))

   `(sr-alt-marked-dir-face
     ((,class (:background ,magenta
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-magenta
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))

   `(sr-alt-marked-file-face
     ((,class (:background ,magenta
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-magenta
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,class (:inherit dired-directory
                        :weight normal))
      (,terminal-class (:inherit dired-directory
                                 :weight normal))))

   `(sr-symlink-directory-face
     ((,class (:inherit dired-directory
                        :slant italic
                        :weight normal))
      (,terminal-class (:inherit dired-directory
                                 :slant italic
                                 :weight normal))))

   `(sr-symlink-face
     ((,class (:inherit dired-symlink
                        :slant italic
                        :weight normal))
      (,terminal-class (:inherit dired-symlink
                                 :slant italic
                                 :weight normal))))

   `(sr-broken-link-face
     ((,class (:inherit dired-warning
                        :slant italic
                        :weight normal))
      (,terminal-class (:inherit dired-warning
                                 :slant italic
                                 :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(sr-encrypted-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(sr-log-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(sr-packaged-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(sr-html-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(sr-xml-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,class (:background ,red
                           :foreground ,dark-material-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-red
                                    :foreground ,terminal-dark-material-bg
                                    :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,class (:background unspecified
                           :foreground ,yellow))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-yellow))))

   `(syslog-hour-face
     ((,class (:background unspecified
                           :foreground ,green))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-green))))

   `(syslog-error-face
     ((,class (:background unspecified
                           :foreground ,red
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-red
                                    :weight bold))))

   `(syslog-warn-face
     ((,class (:background unspecified
                           :foreground ,orange
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-orange
                                    :weight bold))))

   `(syslog-info-face
     ((,class (:background unspecified
                           :foreground ,blue
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-blue
                                    :weight bold))))

   `(syslog-debug-face
     ((,class (:background unspecified
                           :foreground ,cyan
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-cyan
                                    :weight bold))))

   `(syslog-su-face
     ((,class (:background unspecified
                           :foreground ,magenta))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-magenta))))

   ;; table
   `(table-cell
     ((,class (:foreground ,dark-material-fg
                           :background ,dark-material-hl))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :background ,terminal-dark-material-hl))))

   ;; term
   `(term-color-black
     ((,class (:foreground ,dark-material-bg
                           :background ,dark-material-hl))
      (,terminal-class (:foreground ,terminal-dark-material-bg
                                    :background ,terminal-dark-material-hl))))

   `(term-color-red
     ((,class (:foreground ,red
                           :background ,red-d))
      (,terminal-class (:foreground ,terminal-red
                                    :background ,terminal-red-d))))

   `(term-color-green
     ((,class (:foreground ,green
                           :background ,green-d))
      (,terminal-class (:foreground ,terminal-green
                                    :background ,terminal-green-d))))

   `(term-color-yellow
     ((,class (:foreground ,yellow
                           :background ,yellow-d))
      (,terminal-class (:foreground ,terminal-yellow
                                    :background ,terminal-yellow-d))))

   `(term-color-blue
     ((,class (:foreground ,blue
                           :background ,blue-d))
      (,terminal-class (:foreground ,terminal-blue
                                    :background ,terminal-blue-d))))

   `(term-color-magenta
     ((,class (:foreground ,magenta
                           :background ,magenta-d))
      (,terminal-class (:foreground ,terminal-magenta
                                    :background ,terminal-magenta-d))))

   `(term-color-cyan
     ((,class (:foreground ,cyan
                           :background ,cyan-d))
      (,terminal-class (:foreground ,terminal-cyan
                                    :background ,terminal-cyan-d))))

   `(term-color-white
     ((,class (:foreground ,dark-material-emph
                           :background ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :background ,terminal-dark-material-fg))))

   `(term-default-fg-color
     ((,class (:inherit term-color-white))
      (,terminal-class (:inherit term-color-white))))

   `(term-default-bg-color
     ((,class (:inherit term-color-black))
      (,terminal-class (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,class (:background ,yellow-lc
                           :foreground ,yellow-hc
                           :inherit ,s-variable-pitch))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,class (:foreground ,magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-magenta
                                    :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,class (:foreground ,blue
                           :background ,dark-material-hl
                           :weight bold))
      (,terminal-class (:foreground ,terminal-blue
                                    :background ,terminal-dark-material-hl
                                    :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,class (:foreground ,dark-material-emph))
      (,terminal-class (:foreground ,terminal-dark-material-emph))))

   `(tuareg-font-lock-error-face
     ((,class (:foreground ,yellow
                           :background ,red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-yellow
                                    :background ,terminal-red
                                    :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,class (:foreground ,dark-material-comments
                           :background ,dark-material-bg))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :background ,terminal-dark-material-bg))))

   `(undo-tree-visualizer-unmodified-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(undo-tree-visualizer-current-face
     ((,class (:foreground ,blue
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-blue
                                    :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,class (:foreground ,dark-material-emph
                           :background ,dark-material-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :background ,terminal-dark-material-bg
                                    :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   ;; volatile highlights
   `(vhl/default-face
     ((,class (:background ,green-lc
                           :foreground ,green-hc))
      (,terminal-class (:background ,terminal-green-lc
                                    :foreground ,terminal-green-hc))))

   ;; w3m
   `(w3m-anchor
     ((,class (:inherit link))
      (,terminal-class (:inherit link))))

   `(w3m-arrived-anchor
     ((,class (:inherit link-visited))
      (,terminal-class (:inherit link-visited))))

   `(w3m-form
     ((,class (:background ,dark-material-bg
                           :foreground ,dark-material-fg))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-dark-material-fg))))

   `(w3m-header-line-location-title
     ((,class (:background ,dark-material-hl
                           :foreground ,yellow))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-yellow))))

   `(w3m-header-line-location-content

     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-fg))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-fg))))

   `(w3m-bold
     ((,class (:foreground ,dark-material-emph
                           :weight bold))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :weight bold))))

   `(w3m-image-anchor
     ((,class (:background ,dark-material-bg
                           :foreground ,cyan
                           :inherit link))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-cyan
                                    :inherit link))))

   `(w3m-image
     ((,class (:background ,dark-material-bg
                           :foreground ,cyan))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,class (:foreground ,dark-material-emph))
      (,terminal-class (:foreground ,terminal-dark-material-emph))))

   `(w3m-lnum-match
     ((,class (:background ,dark-material-hl))
      (,terminal-class (:background ,terminal-dark-material-hl))))

   `(w3m-lnum
     ((,class (:underline nil
                          :bold nil
                          :foreground ,red))
      (,terminal-class (:underline nil
                                   :bold nil
                                   :foreground ,terminal-red))))

   `(w3m-session-select
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(w3m-session-selected
     ((,class (:foreground ,dark-material-emph
                           :bold t
                           :underline t))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :bold t
                                    :underline t))))

   `(w3m-tab-background
     ((,class (:background ,dark-material-bg
                           :foreground ,dark-material-fg))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-dark-material-fg))))

   `(w3m-tab-selected-background
     ((,class (:background ,dark-material-bg
                           :foreground ,dark-material-fg))
      (,terminal-class (:background ,terminal-dark-material-bg
                                    :foreground ,terminal-dark-material-fg))))

   `(w3m-tab-mouse
     ((,class (:background ,dark-material-hl
                           :foreground ,yellow))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-yellow))))

   `(w3m-tab-selected
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-emph
                           :bold t))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-emph
                                    :bold t))))

   `(w3m-tab-unselected
     ((,class (:background ,dark-material-hl
                           :foreground ,dark-material-fg))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-dark-material-fg))))

   `(w3m-tab-selected-retrieving
     ((,class (:background ,dark-material-hl
                           :foreground ,red))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-red))))

   `(w3m-tab-unselected-retrieving
     ((,class (:background ,dark-material-hl
                           :foreground ,orange))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-orange))))

   `(w3m-tab-unselected-unseen
     ((,class (:background ,dark-material-hl
                           :foreground ,violet))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :foreground ,terminal-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(web-mode-comment-face
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   `(web-mode-constant-face
     ((,class (:foreground ,blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-blue
                                    :weight bold))))

   `(web-mode-current-element-highlight-face
     ((,class (:underline unspecified
                          :weight unspecified
                          :background ,dark-material-hl))
      (,terminal-class (:underline unspecified
                                   :weight unspecified
                                   :background ,terminal-dark-material-hl))))

   `(web-mode-css-at-rule-face
     ((,class (:foreground ,violet
                           :slant italic))
      (,terminal-class (:foreground ,terminal-violet
                                    :slant italic))))

   `(web-mode-css-pseudo-class-face
     ((,class (:foreground ,green
                           :slant italic))
      (,terminal-class (:foreground ,terminal-green
                                    :slant italic))))

   `(web-mode-doctype-face
     ((,class (:foreground ,dark-material-comments
                           :slant italic
                           :weight bold))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :slant italic
                                    :weight bold))))

   `(web-mode-folded-face
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(web-mode-function-name-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(web-mode-html-attr-name-face
     ((,class (:foreground ,blue
                           :slant normal))
      (,terminal-class (:foreground ,terminal-blue
                                    :slant normal))))

   `(web-mode-html-attr-value-face
     ((,class (:foreground ,cyan
                           :slant italic))
      (,terminal-class (:foreground ,terminal-cyan
                                    :slant italic))))

   `(web-mode-html-tag-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(web-mode-keyword-face
     ((,class (:foreground ,yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-yellow
                                    :weight normal))))

   `(web-mode-preprocessor-face
     ((,class (:foreground ,yellow
                           :slant normal
                           :weight unspecified))
      (,terminal-class (:foreground ,terminal-yellow
                                    :slant normal
                                    :weight unspecified))))

   `(web-mode-string-face
     ((,class (:foreground ,cyan))
      (,terminal-class (:foreground ,terminal-cyan))))

   `(web-mode-type-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(web-mode-variable-name-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(web-mode-warning-face
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(web-mode-block-attr-name-face
     ((,class (:inherit web-mode-html-attr-name-face))
      (,terminal-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-block-attr-value-face
     ((,class (:inherit web-mode-html-attr-value-face))
      (,terminal-class (:inherit web-mode-html-attr-value-face))))

   `(web-mode-block-comment-face
     ((,class (:inherit web-mode-comment-face))
      (,terminal-class (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,class (:inherit font-lock-preprocessor-face))
      (,terminal-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-face
     ((,class (:background unspecified))
      (,terminal-class (:background unspecified))))

   `(web-mode-block-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,class (:box 1
                    :weight bold))
      (,terminal-class (:box 1
                             :weight bold))))

   `(web-mode-css-color-face
     ((,class (:inherit font-lock-builtin-face))
      (,terminal-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-face
     ((,class (:inherit font-lock-builtin-face))
      (,terminal-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-priority-face
     ((,class (:inherit font-lock-builtin-face))
      (,terminal-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,class (:inherit font-lock-variable-name-face))
      (,terminal-class (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,class (:inherit font-lock-keyword-face))
      (,terminal-class (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-json-context-face
     ((,class (:foreground ,violet))
      (,terminal-class (:foreground ,terminal-violet))))

   `(web-mode-json-key-face
     ((,class (:foreground ,violet))
      (,terminal-class (:foreground ,terminal-violet))))

   `(web-mode-json-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(web-mode-part-comment-face
     ((,class (:inherit web-mode-comment-face))
      (,terminal-class (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,class (:inherit web-mode-block-face))
      (,terminal-class (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(web-mode-whitespace-face
     ((,class (:background ,red))
      (,terminal-class (:background ,terminal-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,class (:background unspecified
                           :foreground ,dark-material-comments
                          :inverse-video unspecified
                           :slant italic))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-dark-material-comments
                                    :inverse-video unspecified
                                    :slant italic))))

   `(whitespace-hspace
     ((,class (:background unspecified
                           :foreground ,dark-material-emph
                           :inverse-video unspecified))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-dark-material-emph
                                    :inverse-video unspecified))))

   `(whitespace-tab
     ((,class (:background unspecified
                           :foreground ,red
                           :inverse-video unspecified
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-red
                                    :inverse-video unspecified
                                    :weight bold))))

   `(whitespace-newline
     ((,class(:background unspecified
                          :foreground ,dark-material-comments
                          :inverse-video unspecified))
      (,terminal-class(:background unspecified
                                   :foreground ,terminal-dark-material-comments
                                   :inverse-video unspecified))))

   `(whitespace-trailing
     ((,class (:background unspecified
                           :foreground ,orange-lc
                           :inverse-video t))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-orange-lc
                                    :inverse-video t))))

   `(whitespace-line
     ((,class (:background unspecified
                           :foreground ,magenta
                           :inverse-video unspecified))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-magenta
                                    :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,class (:background ,red-lc
                           :foreground unspecified
                           :inverse-video unspecified))
      (,terminal-class (:background ,terminal-red-lc
                                    :foreground unspecified
                                    :inverse-video unspecified))))

   `(whitespace-indentation
     ((,class (:background unspecified
                           :foreground ,yellow
                           :inverse-video unspecified
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-yellow
                                    :inverse-video unspecified
                                    :weight bold))))

   `(whitespace-empty
     ((,class (:background unspecified
                           :foreground ,red-lc
                           :inverse-video t))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-red-lc
                                    :inverse-video t))))

   `(whitespace-space-after-tab
     ((,class (:background unspecified
                           :foreground ,orange
                           :inverse-video t
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-orange
                                    :inverse-video t
                                    :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(wl-highlight-folder-many-face
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(wl-highlight-folder-path-face
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange))))

   `(wl-highlight-folder-unread-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(wl-highlight-folder-zero-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(wl-highlight-folder-unknown-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(wl-highlight-message-citation-header
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(wl-highlight-message-cited-text-1
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(wl-highlight-message-cited-text-2
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(wl-highlight-message-cited-text-3
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(wl-highlight-message-cited-text-4
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(wl-highlight-message-header-contents-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(wl-highlight-message-headers-face
     ((,class (:foreground ,red))
      (,terminal-class (:foreground ,terminal-red))))

   `(wl-highlight-message-important-header-contents
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(wl-highlight-message-header-contents
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(wl-highlight-message-important-header-contents2
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(wl-highlight-message-signature
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(wl-highlight-summary-answered-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(wl-highlight-summary-disposed-face
     ((,class (:foreground ,dark-material-fg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-dark-material-fg
                                    :slant italic))))

   `(wl-highlight-summary-new-face
     ((,class (:foreground ,blue))
      (,terminal-class (:foreground ,terminal-blue))))

   `(wl-highlight-summary-normal-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(wl-highlight-summary-thread-top-face
     ((,class (:foreground ,yellow))
      (,terminal-class (:foreground ,terminal-yellow))))

   `(wl-highlight-thread-indent-face
     ((,class (:foreground ,magenta))
      (,terminal-class (:foreground ,terminal-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,class (:foreground ,dark-material-fg))
      (,terminal-class (:foreground ,terminal-dark-material-fg))))

   `(wl-highlight-summary-displaying-face
     ((,class (:underline t
                          :weight bold))
      (,terminal-class (:underline t
                                   :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,class (:inherit error))
      (,terminal-class (:inherit error))))

   `(weechat-highlight-face
     ((,class (:foreground ,dark-material-emph
                           :weight bold))
      (,terminal-class (:foreground ,terminal-dark-material-emph
                                    :weight bold))))

   `(weechat-nick-self-face
     ((,class (:foreground ,green
                           :weight unspecified
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-green
                                    :weight unspecified
                                    :inverse-video t))))

   `(weechat-prompt-face
     ((,class (:inherit minibuffer-prompt))
      (,terminal-class (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,class (:foreground ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments))))

   ;; which-func-mode
   `(which-func
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   ;; window-number-mode
   `(window-number-face
     ((,class (:foreground ,green))
      (,terminal-class (:foreground ,terminal-green))))

   `(elixir-attribute-face
     ((,class (:foreground ,magenta))
      (,terminal-class (:foreground ,terminal-magenta))))

   `(elixir-atom-face
     ((,class (:foreground ,orange))
      (,terminal-class (:foreground ,terminal-orange-lc))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,class (:foreground ,dark-material-comments
                           :background ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :background ,terminal-dark-material-comments))))

   `(yascroll:thumb-fringe
     ((,class (:foreground ,dark-material-comments
                           :background ,dark-material-comments))
      (,terminal-class (:foreground ,terminal-dark-material-comments
                                    :background ,terminal-dark-material-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,class (:background ,dark-material-hl
                           :box ,dark-material-emph))
      (,terminal-class (:background ,terminal-dark-material-hl
                                    :box ,terminal-dark-material-emph)))))

  (custom-theme-set-variables
   'dark-material
   `(ansi-color-names-vector [,dark-material-bg ,red ,green ,yellow
                                          ,blue ,magenta ,cyan ,dark-material-fg])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,dark-material-hl)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors (,magenta ,violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,dark-material-hl . 0)(,green-lc . 20)(,cyan-lc . 30)(,blue-lc . 50)
       (,yellow-lc . 60)(,orange-lc . 70)(,magenta-lc . 85)(,dark-material-hl . 100)))

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     (unspecified ,dark-material-bg ,dark-material-hl
                  ,red-d ,red
                  ,green-d ,green
                  ,yellow-d ,yellow
                  ,blue-d ,blue
                  ,magenta-d ,magenta
                  ,cyan-d ,cyan
                  ,dark-material-fg ,dark-material-emph))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dark-material)

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; eval: (when (fboundp 'aggressive-indent-mode) (aggressive-indent-mode -1))
;; fill-column: 95
;; End:

;;; dark-material-theme.el ends here
