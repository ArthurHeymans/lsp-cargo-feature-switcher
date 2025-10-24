;;; cargo-features.el --- Toggle Cargo features via LSP configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; URL: https://github.com/ArthurHeymans/lsp-cargo-feature-switcher
;; Keywords: rust, cargo, lsp
;; Package: lsp-cargo-feature-switcher
;; Version: 0.1.0
;; Package-Requires: ((toml "0.0.1"))

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This package provides an interactive interface for toggling Cargo features
;; in Rust projects by updating LSP configuration in .dir-locals.el files.
;;
;; Usage:
;;   M-x cargo-features-toggle-menu
;;
;; Requires: https://github.com/ArthurHeymans/emacs-toml

;;; Code:

(require 'toml)
(require 'files-x)

(defcustom cargo-features-lsp-mode 'eglot
  "LSP client mode to use for Cargo feature configuration.
Can be either 'lsp-mode or 'eglot."
  :type '(choice (const :tag "lsp-mode" lsp-mode)
                 (const :tag "eglot" eglot))
  :group 'cargo-features)

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

(defun cargo-features-find-project-root (&optional dir)
  "Find project root directory starting from DIR or current directory.
For eglot mode, looks for .git directory first, falls back to Cargo.toml directory.
For lsp-mode, uses Cargo.toml directory."
  (let ((start-dir (or dir default-directory)))
    (if (eq cargo-features-lsp-mode 'eglot)
        (or (locate-dominating-file start-dir ".git")
            (cargo-features-find-closest-toml start-dir))
      (cargo-features-find-closest-toml start-dir))))

(defun cargo-features-find-dir-locals (&optional dir)
  "Find .dir-locals.el file in DIR or closest project root."
  (let* ((project-root (cargo-features-find-project-root dir))
         (dir-locals-file (when project-root (expand-file-name ".dir-locals.el" project-root))))
    dir-locals-file))

(defun cargo-features-get-current-from-file (dir-locals-file)
  "Get current LSP/eglot rust features and no-default-features setting from DIR-LOCALS-FILE.
Returns (features . no-default-features)."
  (when (and dir-locals-file (file-exists-p dir-locals-file))
    (condition-case nil
        (let* ((dir (file-name-directory dir-locals-file))
               (class (dir-locals-read-from-dir dir))
               (variables (when class (dir-locals-get-class-variables class)))
               (rustic-config (assoc 'rustic-mode variables))
               (lsp-features-config (when rustic-config 
                                      (assoc 'lsp-rust-features (cdr rustic-config))))
               (lsp-no-default-config (when rustic-config
                                        (assoc 'lsp-rust-no-default-features (cdr rustic-config))))
               (eglot-config (when rustic-config
                               (assoc 'eglot-workspace-configuration (cdr rustic-config))))
               (eglot-data (when eglot-config
                            (let* ((config-plist (cdr eglot-config))
                                   (rust-analyzer (plist-get config-plist :rust-analyzer))
                                   (cargo (plist-get rust-analyzer :cargo)))
                              (cons (plist-get cargo :features)
                                    (plist-get cargo :noDefaultFeatures)))))
               (features (or (when lsp-features-config (cdr lsp-features-config))
                            (when eglot-data
                              (let ((cargo-features (car eglot-data)))
                                (when (vectorp cargo-features)
                                  (append cargo-features '()))))))
               (no-default (or (when lsp-no-default-config (cdr lsp-no-default-config))
                              (when eglot-data (cdr eglot-data)))))
          (cons (when features
                  (if (vectorp features)
                      (append features '())
                    features))
                no-default))
      (error nil))))

(defun cargo-features-get-current (&optional dir)
  "Get current LSP rust features and no-default-features from .dir-locals.el in DIR or current directory.
Returns (features . no-default-features)."
  (let ((dir-locals-file (cargo-features-find-dir-locals dir)))
    (if dir-locals-file
        (cargo-features-get-current-from-file dir-locals-file)
      (cons nil nil))))

(defun cargo-features-update-dir-locals-lsp-mode (features no-default-features project-root)
  "Update .dir-locals.el with lsp-mode configuration.
FEATURES is the list of features, NO-DEFAULT-FEATURES is boolean, PROJECT-ROOT is the directory."
  (let* ((dir-locals-file (expand-file-name ".dir-locals.el" project-root))
         (default-directory project-root))
    (save-window-excursion
      (modify-dir-local-variable 'rustic-mode 'lsp-rust-features (vconcat features) 'add-or-replace dir-locals-file)
      (modify-dir-local-variable 'rustic-mode 'lsp-rust-no-default-features no-default-features 'add-or-replace dir-locals-file)
      (save-buffer))
    (let ((dir-locals-buffer (find-buffer-visiting dir-locals-file)))
      (when dir-locals-buffer
        (with-current-buffer dir-locals-buffer
          (revert-buffer t t t))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'rustic-mode)
                   (string-prefix-p project-root default-directory))
          (kill-local-variable 'lsp-rust-features)
          (kill-local-variable 'lsp-rust-no-default-features)
          (hack-local-variables)
          (when (fboundp 'lsp-workspace-restart)
            (call-interactively 'lsp-workspace-restart)))))))

(defun cargo-features-update-dir-locals-eglot (features no-default-features project-root)
  "Update .dir-locals.el with eglot configuration.
FEATURES is the list of features, NO-DEFAULT-FEATURES is boolean, PROJECT-ROOT is the directory."
  (let* ((dir-locals-file (expand-file-name ".dir-locals.el" project-root))
         (default-directory project-root)
         (cargo-config (list :features (vconcat features)))
         (cargo-config (if no-default-features
                          (plist-put cargo-config :noDefaultFeatures t)
                        cargo-config))
         (eglot-config (list :rust-analyzer
                            (list :cargo cargo-config))))
    (save-window-excursion
      (modify-dir-local-variable 'rustic-mode 'eglot-workspace-configuration eglot-config 'add-or-replace dir-locals-file)
      (save-buffer))
    (let ((dir-locals-buffer (find-buffer-visiting dir-locals-file)))
      (when dir-locals-buffer
        (with-current-buffer dir-locals-buffer
          (revert-buffer t t t))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'rustic-mode)
                   (string-prefix-p project-root default-directory))
          (kill-local-variable 'eglot-workspace-configuration)
          (hack-local-variables)
          (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
            (call-interactively 'eglot-reconnect)))))))

(defun cargo-features-update-dir-locals (features no-default-features &optional dir)
  "Update .dir-locals.el with FEATURES list and NO-DEFAULT-FEATURES in DIR or current directory.
Uses either lsp-mode or eglot configuration based on `cargo-features-lsp-mode'."
  (let ((project-root (or dir (cargo-features-find-project-root))))
    (if (eq cargo-features-lsp-mode 'eglot)
        (cargo-features-update-dir-locals-eglot features no-default-features project-root)
      (cargo-features-update-dir-locals-lsp-mode features no-default-features project-root))))

;;;###autoload
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
           (current-state (cargo-features-get-current))
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
                  (cargo-features-update-dir-locals current-features new-no-default)
                  (message "Updated LSP rust no-default-features: %s" new-no-default))
              (let ((new-features 
                     (if (member selected-name current-features)
                         (remove selected-name current-features)
                       (sort (append current-features (list selected-name)) 'string<))))
                (cargo-features-update-dir-locals new-features no-default-features)
                (message "Updated LSP rust features: %s" 
                         (mapconcat 'identity new-features " "))))))))))

(provide 'lsp-cargo-feature-switcher)
;;; cargo-features.el ends here
