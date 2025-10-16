;;; cargo-features.el -*- lexical-binding: t; -*-

(require 'toml)

(defun cargo-features-find-closest-toml (&optional dir)
  "Find the closest Cargo.toml file starting from DIR or current directory."
  (let ((start-dir (or dir default-directory)))
    (locate-dominating-file start-dir "Cargo.toml")))

(defun cargo-features-parse-available (cargo-toml-path)
  "Parse available features from Cargo.toml at CARGO-TOML-PATH.
Returns a list of (feature-name type dependency-features always-enabled-crates)."
  (when (and cargo-toml-path (file-exists-p cargo-toml-path))
    (let* ((parsed-toml (toml:read-from-file cargo-toml-path))
           (features-section (assoc "features" parsed-toml))
           (dependencies (assoc "dependencies" parsed-toml))
           (dev-dependencies (assoc "dev-dependencies" parsed-toml))
           (build-dependencies (assoc "build-dependencies" parsed-toml))
           (all-features '())
           (default-enabled-crates '()))
      
      ;; First pass: find crates enabled by "default" feature
      (when features-section
        (let ((default-feature (assoc "default" (cdr features-section))))
          (when default-feature
            (let ((default-def (cdr default-feature)))
              (cond
               ((null default-def) nil)
               ((and (consp default-def) (stringp (car default-def)))
                (dolist (item default-def)
                  (when (and (stringp item) (not (string-match-p "/" item)))
                    (push item default-enabled-crates))))
               ((vectorp default-def)
                (dotimes (i (length default-def))
                  (let ((item (aref default-def i)))
                    (when (and (stringp item) (not (string-match-p "/" item)))
                      (push item default-enabled-crates)))))
               ((and (stringp default-def) (not (string-match-p "/" default-def)))
                (push default-def default-enabled-crates)))))))
      
      ;; Second pass: add crate features with dependency feature analysis
      (when features-section
        (dolist (feature (cdr features-section))
          (when (consp feature)
            (let* ((feature-name (car feature))
                   (feature-def (cdr feature))
                   (dep-features '())
                   (always-enabled (member feature-name default-enabled-crates)))
              (cond
               ((null feature-def)
                nil)
               ((and (consp feature-def) (stringp (car feature-def)))
                (dolist (item feature-def)
                  (when (stringp item)
                    (push item dep-features))))
               ((vectorp feature-def)
                (dotimes (i (length feature-def))
                  (let ((item (aref feature-def i)))
                    (when (stringp item)
                      (push item dep-features)))))
               ((stringp feature-def)
                (push feature-def dep-features)))
              (push (cons feature-name (list "crate" (nreverse dep-features) always-enabled)) all-features)))))
      
      (nreverse all-features))))

(defun cargo-features-find-dir-locals (&optional dir)
  "Find .dir-locals.el file in DIR or closest Cargo.toml directory."
  (let* ((cargo-dir (or dir (cargo-features-find-closest-toml)))
         (dir-locals-file (when cargo-dir (expand-file-name ".dir-locals.el" cargo-dir))))
    dir-locals-file))

(defun cargo-features-get-current-from-file (dir-locals-file)
  "Get current LSP rust features and no-default-features setting from DIR-LOCALS-FILE.
Returns (features . no-default-features)."
  (when (and dir-locals-file (file-exists-p dir-locals-file))
    (with-temp-buffer
      (insert-file-contents dir-locals-file)
      (goto-char (point-min))
      (condition-case nil
          (let* ((content (read (buffer-string)))
                 (rustic-config (assoc 'rustic-mode content))
                 (lsp-features-config (when rustic-config 
                                        (assoc 'lsp-rust-features (cdr rustic-config))))
                 (lsp-no-default-config (when rustic-config
                                          (assoc 'lsp-rust-no-default-features (cdr rustic-config))))
                 (features (when lsp-features-config (cdr lsp-features-config)))
                 (no-default (when lsp-no-default-config (cdr lsp-no-default-config))))
            (cons (when features
                    (if (vectorp features)
                        (append features '())
                      features))
                  no-default))
        (error nil)))))

(defun cargo-features-get-current (&optional dir)
  "Get current LSP rust features and no-default-features from .dir-locals.el in DIR or current directory.
Returns (features . no-default-features)."
  (let ((dir-locals-file (cargo-features-find-dir-locals dir)))
    (if dir-locals-file
        (cargo-features-get-current-from-file dir-locals-file)
      (cons nil nil))))

(defun cargo-features-update-dir-locals (features no-default-features &optional dir)
  "Update .dir-locals.el with FEATURES list and NO-DEFAULT-FEATURES in DIR or current directory."
  (let* ((cargo-dir (or dir (cargo-features-find-closest-toml)))
         (dir-locals-file (expand-file-name ".dir-locals.el" cargo-dir))
         (settings `((lsp-rust-features . ,(vconcat features))
                     (lsp-rust-no-default-features . ,no-default-features)))
         (new-config `((rustic-mode . ,settings))))
    (with-temp-file dir-locals-file
      (prin1 new-config (current-buffer))
      (insert "\n"))))



(defun cargo-features-toggle-menu ()
  "Interactive menu to toggle Cargo features."
  (interactive)
  (let* ((cargo-dir (cargo-features-find-closest-toml))
         (cargo-toml (when cargo-dir (expand-file-name "Cargo.toml" cargo-dir))))
    
    (unless cargo-toml
      (user-error "No Cargo.toml found in current or parent directories"))
    
    (unless (file-exists-p cargo-toml)
      (user-error "Cargo.toml file does not exist: %s" cargo-toml))
    
    (let* ((available-features (cargo-features-parse-available cargo-toml))
           (current-state (cargo-features-get-current cargo-dir))
           (current-features (or (car current-state) '()))
           (no-default-features (cdr current-state))
           (feature-names (mapcar (lambda (f) (car f)) available-features))
           (prompt "Toggle Cargo features (marked with * are enabled):")
           (candidates (mapcar (lambda (feature)
                                 (let* ((name (car feature))
                                        (feature-info (cdr feature))
                                        (type (car feature-info))
                                        (dep-features (cadr feature-info))
                                        (always-enabled (caddr feature-info))
                                        (is-default (string= name "default"))
                                        (default-enabled (and is-default (not no-default-features)))
                                        (enabled-str (cond
                                                      ((and always-enabled (not no-default-features)) "[DEFAULT ENABLED]")
                                                      (default-enabled "[ENABLED]")
                                                      ((member name current-features) "[ENABLED]")
                                                      (t "")))
                                        (dep-str (if dep-features 
                                                     (format "dep: [ %s ]" (mapconcat 'identity dep-features " "))
                                                   "")))
                                   (format "%-30s %-18s %s" name enabled-str dep-str)))
                               available-features))
           (selection (completing-read prompt candidates nil t)))
      
      (when selection
        (let* ((selected-name (car (split-string selection " " t)))
               (selected-feature (assoc selected-name available-features))
               (is-default (string= selected-name "default")))
          (when selected-feature
            (if is-default
                (let ((new-no-default (not no-default-features)))
                  (cargo-features-update-dir-locals current-features new-no-default cargo-dir)
                  (message "Updated LSP rust no-default-features: %s" new-no-default))
              (let ((new-features 
                     (if (member selected-name current-features)
                         (remove selected-name current-features)
                       (sort (append current-features (list selected-name)) 'string<))))
                (cargo-features-update-dir-locals new-features no-default-features cargo-dir)
                (message "Updated LSP rust features: %s" 
                         (mapconcat 'identity new-features " "))))))))))

(provide 'cargo-features)
;;; cargo-features.el ends here
