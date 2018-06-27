(defvar tabbar-packages
  '(
    tabbar
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(face-attribute 'default :background)

(defvar tabbar-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function tabbar/init-<package-tabbar>
;;
;; (defun tabbar/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
(defun tabbar/init-tabbar ()
  "Initialize tabbar"
  (use-package tabbar
    :init

    (tabbar-mode 1)

    (set-face-attribute
     'tabbar-default nil
     :background (face-attribute 'fringe :background)
     :foreground (face-attribute 'default :background)
     :box `(:line-width 1 :color ,(face-attribute 'fringe :background)))

    (set-face-attribute
     'tabbar-unselected nil
     :background (face-attribute 'line-number :background)
     :foreground (face-attribute 'default :background)
     :slant 'normal
     :weight 'medium
     :box `(:line-width 1 :color ,(face-attribute 'line-number :background)))


    (set-face-attribute
     'tabbar-modified nil
     :background (face-attribute 'fringe :background)
     :foreground (face-attribute 'fringe :foreground)
     :slant 'normal
     :weight 'medium
     :box `(:line-width 5 :color ,(face-attribute 'fringe :background)))

    (set-face-attribute
     'tabbar-selected-modified nil
     :background (face-attribute 'fringe :foreground)
     :foreground (face-attribute 'fringe :background)
     :slant 'normal
     :weight 'bold
     :box `(:line-width 5 :color ,(face-attribute 'fringe :foreground)))

    (set-face-attribute
     'tabbar-selected nil
     :background (face-attribute 'default :foreground)
     :foreground (face-attribute 'default :background)
     :box `(:line-width 5 :color ,(face-attribute 'default :foreground)))

    (set-face-attribute
     'tabbar-highlight nil
     :background (face-attribute 'fringe :foreground)
     :foreground (face-attribute 'fringe :background)
     :box `(:line-width 5 :color ,(face-attribute 'fringe :foreground)))

    (set-face-attribute
     'tabbar-button nil
     :box `(:line-width 1 :color ,(face-attribute 'fringe :background)))

    (set-face-attribute
     'tabbar-separator nil
     :background (face-attribute 'fringe :background)
     :height 0.2)

    ;; Change padding of the tabs
    ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
    (custom-set-variables
     '(tabbar-separator (quote (0.5))))

    ;; adding spaces
    (defun tabbar-buffer-tab-label (tab)
      "Return a label for TAB.
That is, a string used to represent it on the tab bar."
      (let ((label  (if tabbar--buffer-show-groups
                        (format "[%s]  " (tabbar-tab-tabset tab))
                      (format "%s  " (tabbar-tab-value tab)))))
        ;; Unless the tab bar auto scrolls to keep the selected tab
        ;; visible, shorten the tab label to keep as many tabs as possible
        ;; in the visible area of the tab bar.
        (if tabbar-auto-scroll-flag
            label
          (tabbar-shorten
           label (max 1 (/ (window-width)
                           (length (tabbar-view
                                    (tabbar-current-tabset)))))))))

    )
  )
