;; -*- lexical-binding: t -*-

(def-project-mode! +MM-Web-mode
  :match "\\/Code\\/Meisterlabs\\/"
  :add-hooks (+MM/setup-keybindings))

(defun +MM|alternate-file ()
  "Find the alternate file for the current buffer."
  (interactive)
  (let* ((file-path (file-name-nondirectory buffer-file-name))
         (dir-path (file-name-directory buffer-file-name))
         (alternate-file (if (string= file-path "component.js") "controller.js" "component.js")))
    (find-file (f-join dir-path alternate-file))))

(fset '+MM|turn-style-object-into-function
      (lambda (&optional arg)
        "Convert a style object into a Style function, needs to be focused on the starting {"
        (interactive "p")
        (kmacro-exec-ring-item (quote ([?y ?s ?a ?B ?b ?i ?S ?t ?y ?l ?e escape ?l ?a ?f ?u ?n ?c ?t ?i ?o ?n ?  S-backspace ?  ?\( ?o ?p ?t ?i ?o ?n ?s ?, ?  ?R ?u ?l ?e ?s escape ?l ?l ?y ?s ?a ?B ?B ?i ?  escape ?l ?a return ?r ?e ?t ?u ?r ?n ?  escape ?l ?j ?> ?i ?\{ ?k ?$ ?% ?a return escape ?k ?a ?\; escape ?= ?= ?j ?b ?l ?%] 0 "%d")) arg)))

(defun +MM|convert-to-new-redux-style ()
  "Converts the current buffer to the new redux style."
  (interactive)
  (shell-command (template "jscodeshift --dry --print --silent --transform ~/Code/Meisterlabs/jscodeshift/redux/v5.8.0/actions-controllers.js <<(buffer-file-name)>>") (current-buffer)))

(defun +MM|create-action ()
  "Create an action file from a controller file"
  (interactive)
  (let* ((action-name (read-string "Action Name: "))
         (root-dir (file-name-directory (buffer-file-name)))
         (actions-dir (concat root-dir "actions/"))
         (actions-index-file (concat actions-dir "index.js"))
         (action-file (concat actions-dir (concat action-name ".js"))))
    (unless (file-directory-p actions-dir) (mkdir actions-dir))
    (copy-file
     (concat (projectile-project-root) "snippets/Core/Action.js")
     action-file)
    (with-temp-file actions-index-file
      (+js|generate-index actions-dir))
    (find-file action-file)))

(defun +MM|canvas-create-action ()
  "Create an action file from a controller file"
  (interactive)
  (let* ((action-name (read-string "Action Name: "))
         (actions-dir (f-join (projectile-project-root) "src/containers/MapEditor/actions"))
         (actions-index-file (f-join actions-dir "index.js"))
         (action-file (f-join actions-dir (concat action-name ".js"))))
    (copy-file
     (f-join (projectile-project-root) "snippets/Core/Action.js")
     action-file)
    (with-temp-file actions-index-file
      (+js|generate-index actions-dir))
    (find-file action-file)))

(defun +MM|canvas-files ()
  "Project files but only for canvas"
  (interactive)
  (ivy-read "Find File: "
            (--filter (s-contains? "MapEditor" it) (projectile-current-project-files))
            :action #'projectile-find-file))

(defun +MM|new-worktree ()
  "New worktree with the devpanel files and an npm install"
  (interactive)
  (let* ((origin-path (projectile-project-root))
         (path (call-interactively 'magit-worktree-branch-project-worktree))
         (local-devutil-file (f-join origin-path "src/apps/Main/DevPlugin.local.js"))
         (worktree-devutil-file (f-join path "src/apps/Main/DevPlugin.local.js")))
    (when (file-exists-p local-devutil-file)
      (copy-file local-devutil-file worktree-devutil-file))
    (npm-mode)
    (npm-mode-npm-ci)))

(after! magit
  (transient-append-suffix 'magit-worktree "y" '("m" "+MM|Worktree" +MM|new-worktree)))

(setq +MM-comment-headers '("EXTERNALS" "LOCALS" "HELPERS" "MAIN"))

(defun +MM:get-remaining-headers-list (header)
  "Returns a reversed list of headers to search through"
  (->> +MM-comment-headers
       (-split-on header)
       (-first-item)))

(defun +MM:goto-or-add-header (header)
  "Goes to a comment header or if it doesnt exist creates one"
  (goto-char (point-min))
  (if (search-forward header nil t)
      (progn
        (search-forward-regexp "^/\\*\\*" nil t)
        (previous-line 2))
    ;; Create header when none was found
    (progn
      (let* ((headers (reverse (+MM:get-remaining-headers-list header))))
        (--first (search-forward it nil t) headers))
      (search-forward-regexp "^/\\*\\*" nil t)
      (evil-insert-newline-above)
      (insert (template  "/** <<header>> **/\n\n\n\n"))
      (previous-line 1))))

(defun +MM:add-import-to-file (file)
  (goto-char (point-min))
  (let* ((is-local (s-contains? "./" file))
         (comment-header-title (if is-local "LOCALS" "EXTERNALS")))
    (+MM:goto-or-add-header comment-header-title)
    (evil-insert-newline-below)
    (previous-line 1)
    (+js/import-file file)))

(defun +MM|import-file ()
  (interactive)
  (+js|ivy-import-file '+MM:add-import-to-file))

(require 'ht)

(defvar +MM-Rules-percent)
(defvar +MM-Rules-pixel)
(defvar +MM-Rules-number)
(defvar +MM-Rules-deg)

(defvar +MM-Rules-flex-align
  '("center", "flex-start", "flex-end", "space-between", "space-around", "stretch"))

(defvar +MM-Rules-colors
  '("colors.red"
    "colors.grey900",
    "colors.grey700",
    "colors.grey500",
    "colors.grey300",
    "colors.grey200",
    "colors.grey100",
    "colors.white",
    "colors.blue",
    "colors.sky",
    "colors.navy",
    "colors.turquoise",
    "colors.green",
    "colors.grass",
    "colors.yellow",
    "colors.banana",
    "colors.orange",
    "colors.red",
    "colors.pink",
    "colors.purple"))

(setq +MM-Web-Rules
      (ht ("flexDirection"            '("column" "row" "row-reverse" "column-reverse"))
          ("alignContent"             '+MM-Rules-flex-align)
          ("alignItems"               '+MM-Rules-flex-align)
          ("alignSelf"                '+MM-Rules-flex-align)
          ("flexGrow"                 '+MM-Rules-number)
          ("flexShrink"               '+MM-Rules-number)
          ("flexWrap"                 '+MM-Rules-number)

          ("display"                  '("flex" "block" "inline-flex" "inline-block"))

          ("height"                   '+MM-Rules-number)
          ("maxHeight"                '+MM-Rules-number)
          ("maxSize"                  '+MM-Rules-number)
          ("maxWidth"                 '+MM-Rules-number)
          ("minHeight"                '+MM-Rules-number)
          ("minSize"                  '+MM-Rules-number)
          ("minWidth"                 '+MM-Rules-number)
          ("size"                     '+MM-Rules-number)
          ("width"                    '+MM-Rules-number)

          ("listStyleType"            '+MM-Rules-number)

          ("padding"                  '+MM-Rules-pixel)
          ("paddingHorizontal"        '+MM-Rules-pixel)
          ("paddingVertical"          '+MM-Rules-pixel)
          ("paddingTop"               '+MM-Rules-pixel)
          ("paddingBottom"            '+MM-Rules-pixel)
          ("paddingLeft"              '+MM-Rules-pixel)
          ("paddingRight"             '+MM-Rules-pixel)

          ("margin"                   '+MM-Rules-pixel)
          ("marginTop"                '+MM-Rules-pixel)
          ("marginBottom"             '+MM-Rules-pixel)
          ("marginLeft"               '+MM-Rules-pixel)
          ("marginRight"              '+MM-Rules-pixel)

          ("position"                 '("absolute" "relative" "fixed"))
          ("absoluteHorizontalCenter" 'noop)
          ("absoluteHorizontalCenter" 'noop)
          ("absoluteCenter"           'noop)
          ("absoluteHorizontalCenter" 'noop)
          ("absoluteVerticalCenter"   'noop)
          ("top"                      '+MM-Rules-number)
          ("left"                     '+MM-Rules-number)
          ("right"                    '+MM-Rules-number)
          ("bottom"                   '+MM-Rules-number)

          ("translateY"               '+MM-Rules-pixel)
          ("translateX"               '+MM-Rules-pixel)
          ("scale"                    '+MM-Rules-number)
          ("scaleX"                   '+MM-Rules-number)
          ("scaleY"                   '+MM-Rules-number)
          ("rotate"                   '+MM-Rules-deg)

          ("color"                    '+MM-Rules-colors)
          ("fontWeight"               '("heavy" "bold" "black" "light" "normal" "regular"))
          ("fontFamily"               '+MM-Rules-number)
          ("fontSize"                 '+MM-Rules-number)
          ("fontStyle"                '+MM-Rules-number)
          ("lineHeight"               '+MM-Rules-number)
          ("textDecoration"           '+MM-Rules-number)
          ("textShadow"               '+MM-Rules-number)
          ("textTransform"            '+MM-Rules-number)
          ("truncate"                 'noop)
          ("wordBreak"                '+MM-Rules-number)
          ("wordWrap"                 '+MM-Rules-number)

          ("boxSizing"                '+MM-Rules-number)

          ("cursor"                   '("pointer"))

          ("backfaceVisibility" "hidden")
          ("backgroundColor"          '+MM-Rules-colors)
          ("backgroundImage" "")
          ("backgroundPosition"       '("center"))
          ("backgroundRepeat"         '("repeat" "no-repeat" "repeat-x" "repeat-y"))
          ;; ("backgroundSize"        '+MM-Rules-number))

          ("borderBottomLeftRadius"   '+MM-Rules-number)
          ("borderBottomRightRadius"  '+MM-Rules-number)
          ("borderBottomWidth"        '+MM-Rules-number)
          ("borderColor"              '+MM-Rules-colors)
          ("borderHorizontalWidth"    '+MM-Rules-number)
          ("borderLeftWidth"          '+MM-Rules-number)
          ("borderRadius"             '+MM-Rules-number)
          ("borderRightWidth"         '+MM-Rules-number)
          ("borderStyle"              '("solid" "dashed"))
          ("borderTopLeftRadius"      '+MM-Rules-number)
          ("borderTopRightRadius"     '+MM-Rules-number)
          ("borderTopWidth"           '+MM-Rules-number)
          ("borderVerticalWidth"      '+MM-Rules-number)
          ("borderWidth"              '+MM-Rules-number)

          ("boxShadow"                '+MM-Rules-number)

          ("overflow"                 '+MM-Rules-number)
          ("overflowX"                '+MM-Rules-number)
          ("overflowY"                '+MM-Rules-number)

          ("fill"                     '+MM-Rules-colors)

          ("opacity"                  '+MM-Rules-number)
          ("visibility"               '("visible" "hidden"))

          ("pointerEvents"                 "none")
          ("userSelect"                    "none")

          ("zIndex"                   '+MM-Rules-number)))

(defvar +MM|add-prop-for-rule-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") (lambda (rule prop)
                                         (ivy-quit-and-run
                                           (progn
                                            (+MM|write-rule rule prop)
                                            (+MM|add-rule)))))
    map))


(defun +MM|write-rule (rule &optional prop extension)
  (let* ((is-zero-p (string= prop "0"))
         (prop (cond (is-zero-p prop)
                     (extension (template "'<<prop>>"))
                     (prop prop)
                     (t "")))
         (extension (cond (is-zero-p "")
                          (extension (template "<<extension>>'"))
                          (t ""))))
    (insert (template ".<<rule>>(<<prop>><<extension>>)\n"))))

(defun +MM|write-rule-and-restart (rule &optional prop extension)
  (+MM|write-rule rule prop extension)
  (+MM|add-rule))

(defun +MM|rule-numbers ()
  (-map #'number-to-string (number-sequence 0 100 10)))

(defun +MM|add-prop-for-rule (rule)
  (let* ((prop-value (ht-get +MM-Web-Rules rule))
         (prop (cond
                ((eq prop-value #'noop) nil)
                ((-contains? (list '+MM-Rules-number '+MM-Rules-pixel '+MM-Rules-percent '+MM-Rules-deg) prop-value)
                 (+MM|rule-numbers))
                (t prop-value))))
    (if prop
        (ivy-read "Add prop: " prop
                  :action (lambda (x)
                            (let ((extension (cond
                                              ((eq prop-value '+MM-Rules-pixel) "px")
                                              ((eq prop-value '+MM-Rules-percent) "%")
                                              (t nil))))
                              (+MM|write-rule-and-restart rule x extension))))
      (+MM|write-rule-and-restart rule))))

(defun +MM|add-rule ()
  (interactive)
  (ivy-read "Add Rule: " (ht-keys +MM-Web-Rules)
            :action '+MM|add-prop-for-rule))

(defun +MM/setup-keybindings ()
  "Setup Keybindings"
  (map! :map js2-mode-map
        :n "[1" #'+MM|alternate-file
        :n "]1" #'+MM|alternate-file

        :localleader
        :desc  "Import File"          "i"  #'+MM|import-file
        :desc  "Search Canvas Files"  "/"  #'+MM|canvas-files

        (:prefix ("r" . "Refactor")
          :desc  "Style function into object"    "rs"  #'+MM|turn-style-object-into-function
          :desc  "Convert into new Redux Style"  "X"   #'+MM|turn-style-object-into-function)

        (:prefix ("c" . "Create")
          :desc  "action"  "r"  #'+MM|add-rule
          :desc  "action"  "a"  #'+MM|create-action
          :desc  "action"  "A"  #'+MM|canvas-create-action)))
