;;; agent-shell-workspace.el --- Dedicated workspace for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Gabor Veres

;; Author: Gabor Veres <gabor.veres@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.24.2"))
;; Keywords: convenience, tools
;; URL: https://github.com/gveres/agent-shell-workspace

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides a dedicated tab-bar workspace for agent-shell buffers.
;; Toggle with `agent-shell-workspace-toggle' to switch between your
;; regular work and an "Agents" tab with a managed layout.
;;
;; Features:
;; - Dedicated tab-bar tab with buffer isolation
;; - Compact sidebar showing agent status with icons
;; - Tiling support for viewing 2-4 agents side-by-side
;; - Agent management (kill, restart, rename, mode-set)
;; - Non-agent buffers auto-redirect to your editing tab
;;
;; Usage:
;;   (require 'agent-shell-workspace)
;;   (define-key agent-shell-command-map (kbd "w") 'agent-shell-workspace-toggle)
;;
;; Sidebar keybindings:
;;   RET   - Focus agent in main area
;;   a     - Add agent to tiled view
;;   x     - Remove agent from tiled view
;;   t     - Un-tile back to single focus
;;   R     - Rename agent buffer
;;   c     - Create new agent
;;   k     - Kill agent at point
;;   r     - Restart agent at point
;;   d     - Delete all killed buffers
;;   m     - Set session mode
;;   M     - Cycle session mode
;;   C-c C-c - Interrupt agent
;;   q     - Close sidebar

;; Acknowledgements:
;; Status detection logic adapted from agent-shell-manager.el by Jethro Kuan.

;;; Code:

(require 'agent-shell)
(require 'tab-bar)
(require 'map)
(require 'seq)

(defgroup agent-shell-workspace nil
  "Dedicated tab-bar workspace for agent-shell."
  :group 'agent-shell)

(defvar agent-shell-workspace--tab-name "Agents"
  "Name of the dedicated Agents tab.")

(defvar agent-shell-workspace--previous-tab nil
  "Name of the tab we came from, used for toggling back.")

;;; Agent buffer status tracking

(defvar agent-shell-workspace--previous-status (make-hash-table :test 'eq)
  "Hash table mapping buffers to their previous status string.")

(defvar agent-shell-workspace--finished-buffers (make-hash-table :test 'eq)
  "Hash table of buffers that just finished (transitioned from working to ready).")

(defun agent-shell-workspace--track-status (buffer raw-status)
  "Track status transitions for BUFFER given RAW-STATUS.
Returns \"finished\" if the buffer just transitioned from working to ready
and hasn't been acknowledged yet.  Otherwise returns RAW-STATUS."
  (let ((prev (gethash buffer agent-shell-workspace--previous-status)))
    ;; Detect working → ready transition
    (when (and (member prev '("working" "waiting"))
               (string= raw-status "ready"))
      (puthash buffer t agent-shell-workspace--finished-buffers))
    ;; Clear finished when buffer starts working again
    (when (member raw-status '("working" "waiting"))
      (remhash buffer agent-shell-workspace--finished-buffers))
    ;; Update previous status
    (puthash buffer raw-status agent-shell-workspace--previous-status)
    ;; Return effective status
    (if (gethash buffer agent-shell-workspace--finished-buffers)
        "finished"
      raw-status)))

(defun agent-shell-workspace--clear-finished (buffer)
  "Clear the finished state for BUFFER (user has seen it)."
  (remhash buffer agent-shell-workspace--finished-buffers))

;;; Agent buffer introspection
;;
;; These functions query agent-shell buffer state to determine status,
;; agent type, and configuration.  Status detection logic adapted from
;; agent-shell-manager.el by Jethro Kuan.

(defun agent-shell-workspace--buffer-status (buffer)
  "Return the status of agent-shell BUFFER as a string.
Possible values: \"ready\", \"working\", \"waiting\",
\"finished\", \"initializing\", \"killed\", \"unknown\"."
  (with-current-buffer buffer
    (if (not (boundp 'agent-shell--state))
        "unknown"
      (let* ((state agent-shell--state)
             (acp-proc (map-nested-elt state '(:client :process)))
             (acp-alive (and acp-proc
                             (processp acp-proc)
                             (process-live-p acp-proc)
                             (memq (process-status acp-proc)
                                   '(run open listen connect stop))))
             (comint-proc (get-buffer-process (current-buffer)))
             (comint-alive (and comint-proc
                                (processp comint-proc)
                                (process-live-p comint-proc)
                                (memq (process-status comint-proc)
                                      '(run open listen connect stop))))
             (alive (and acp-alive comint-alive)))
        (cond
         ((or (not comint-proc)
              (and (processp comint-proc) (not comint-alive)))
          "killed")
         ((and (map-elt state :client)
               (or (not acp-proc)
                   (and (processp acp-proc) (not acp-alive))))
          "killed")
         ((and alive
               (map-elt state :tool-calls)
               (> (length (map-elt state :tool-calls)) 0))
          (if (seq-find (lambda (tc)
                          (map-elt (cdr tc) :permission-request-id))
                        (map-elt state :tool-calls))
              "waiting"
            "working"))
         ((and alive (fboundp 'shell-maker-busy) (shell-maker-busy))
          "working")
         ((and alive (map-nested-elt state '(:session :id)))
          "ready")
         ((not (map-elt state :initialized))
          "initializing")
         (t "unknown"))))))

(defun agent-shell-workspace--status-face (status)
  "Return face for STATUS string."
  (pcase status
    ("ready" 'success)
    ("finished" 'agent-shell-workspace-finished)
    ("working" 'warning)
    ("waiting" 'agent-shell-workspace-waiting)
    ("initializing" 'font-lock-comment-face)
    ("killed" 'error)
    (_ 'default)))

(defun agent-shell-workspace--agent-kind (buffer)
  "Return the agent kind string for BUFFER.
Parses the prefix before \" Agent @ \" in the buffer name."
  (let ((name (buffer-name buffer)))
    (if (string-match "^\\(.*?\\) Agent @ " name)
        (match-string 1 name)
      "-")))

(defun agent-shell-workspace--buffer-config (buffer)
  "Return the agent-shell config used for BUFFER, or nil."
  (with-current-buffer buffer
    (when (derived-mode-p 'agent-shell-mode)
      (let ((prefix (replace-regexp-in-string " Agent @ .*$" "" (buffer-name))))
        (seq-find (lambda (config)
                    (string= prefix (map-elt config :buffer-name)))
                  agent-shell-agent-configs)))))

;;; Tab helpers

(defun agent-shell-workspace--tab-exists-p ()
  "Return non-nil if the Agents tab exists."
  (seq-find (lambda (tab)
              (string= (alist-get 'name tab)
                       agent-shell-workspace--tab-name))
            (tab-bar-tabs)))

(defun agent-shell-workspace--in-agents-tab-p ()
  "Return non-nil if the current tab is the Agents tab."
  (string= (alist-get 'name (tab-bar--current-tab))
           agent-shell-workspace--tab-name))

;;; Sidebar

(defvar agent-shell-workspace-sidebar-buffer-name "*Agent Sidebar*"
  "Name of the sidebar buffer.")

(defvar agent-shell-workspace-sidebar-width 24
  "Width of the sidebar window in columns.")

(defvar-local agent-shell-workspace-sidebar--refresh-timer nil
  "Timer for auto-refreshing the sidebar.")

(defvar-local agent-shell-workspace-sidebar--selected-buffer nil
  "Currently selected agent buffer in the sidebar.")

(defvar-local agent-shell-workspace--tiled nil
  "Non-nil when the main area is in tiled mode.")

(defvar-local agent-shell-workspace-sidebar--quick-switch nil
  "Non-nil when quick-switch mode is active.
Moving up/down in the sidebar automatically shows the agent at
point in the main area without stealing focus.")

;;;; Display helpers

(defun agent-shell-workspace--agent-icon (buffer)
  "Return an icon string for the agent type of BUFFER.
Uses `agent-shell--config-icon' when available to show the same
icons as agent-shell's own UI.  Falls back to a single character."
  (let ((config (agent-shell-workspace--buffer-config buffer)))
    (if (and config (fboundp 'agent-shell--config-icon))
        (let ((icon (agent-shell--config-icon :config config)))
          (if (and icon (not (string-empty-p icon)))
              icon
            (agent-shell-workspace--agent-type-fallback buffer)))
      (agent-shell-workspace--agent-type-fallback buffer))))

(defun agent-shell-workspace--agent-type-fallback (buffer)
  "Return a single character representing the agent type of BUFFER."
  (let ((kind (agent-shell-workspace--agent-kind buffer)))
    (cond
     ((string-prefix-p "Claude" kind) "C")
     ((string-prefix-p "OpenCode" kind) "O")
     ((string= kind "-") "?")
     (t (substring kind 0 1)))))

(defun agent-shell-workspace--short-name (buffer)
  "Extract the short name from BUFFER's name.
Returns the portion after \" @ \" if present, otherwise the full name."
  (let ((name (buffer-name buffer)))
    (if (string-match " @ \\(.+\\)$" name)
        (match-string 1 name)
      name)))

(defun agent-shell-workspace--status-icon (status)
  "Return a status icon string for STATUS."
  (pcase status
    ("ready" "●")
    ("finished" "✔")
    ("working" "◐")
    ("waiting" "◉")
    ("initializing" "○")
    ("killed" "✕")
    (_ "?")))

;;;; Face

(defface agent-shell-workspace-selected
  '((t :inherit highlight :extend t))
  "Face for the currently selected agent in the sidebar."
  :group 'agent-shell-workspace)

(defface agent-shell-workspace-finished
  '((t :foreground "cyan" :weight bold))
  "Face for agents that finished their turn."
  :group 'agent-shell-workspace)

(defface agent-shell-workspace-waiting
  '((t :foreground "red" :weight bold))
  "Face for agents waiting for user input."
  :group 'agent-shell-workspace)

;;;; Keymap

(defvar agent-shell-workspace-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-shell-workspace-sidebar-goto)
    (define-key map [mouse-1] #'agent-shell-workspace-sidebar-click)
    (define-key map (kbd "g") #'agent-shell-workspace-sidebar-refresh)
    (define-key map (kbd "c") #'agent-shell-workspace-sidebar-new)
    (define-key map (kbd "k") #'agent-shell-workspace-sidebar-kill)
    (define-key map (kbd "r") #'agent-shell-workspace-sidebar-restart)
    (define-key map (kbd "d") #'agent-shell-workspace-sidebar-delete-killed)
    (define-key map (kbd "m") #'agent-shell-workspace-sidebar-set-mode)
    (define-key map (kbd "M") #'agent-shell-workspace-sidebar-cycle-mode)
    (define-key map (kbd "C-c C-c") #'agent-shell-workspace-sidebar-interrupt)
    (define-key map (kbd "t") #'agent-shell-workspace-tile-toggle)
    (define-key map (kbd "a") #'agent-shell-workspace-tile-add)
    (define-key map (kbd "x") #'agent-shell-workspace-tile-remove)
    (define-key map (kbd "R") #'agent-shell-workspace-sidebar-rename)
    (define-key map (kbd "s") #'agent-shell-workspace-sidebar-toggle-quick-switch)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `agent-shell-workspace-sidebar-mode'.")

;;;; Major mode

(define-derived-mode agent-shell-workspace-sidebar-mode special-mode "AgentNav"
  "Major mode for the agent workspace sidebar.
Displays a compact list of agent-shell buffers with status icons.

\\{agent-shell-workspace-sidebar-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq cursor-type nil)
  ;; Start auto-refresh timer
  (setq agent-shell-workspace-sidebar--refresh-timer
        (run-with-timer 2 2 #'agent-shell-workspace-sidebar-refresh))
  ;; Cancel timer when buffer is killed
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when agent-shell-workspace-sidebar--refresh-timer
                (cancel-timer agent-shell-workspace-sidebar--refresh-timer)
                (setq agent-shell-workspace-sidebar--refresh-timer nil)))
            nil t))

;;;; Rendering

(defun agent-shell-workspace-sidebar--render ()
  "Render the sidebar contents."
  (let* ((buffers (sort (copy-sequence (seq-filter #'buffer-live-p (agent-shell-buffers)))
                        (lambda (a b)
                          (string< (agent-shell-workspace--short-name a)
                                   (agent-shell-workspace--short-name b)))))
         (selected agent-shell-workspace-sidebar--selected-buffer)
         (tiled agent-shell-workspace--tiled-buffers)
         (inhibit-read-only t)
         (target-line nil))
    (erase-buffer)
    (if (null buffers)
        (insert (propertize " No agent buffers" 'face 'font-lock-comment-face))
      (let ((line-num 1))
        (dolist (buf buffers)
          (let* ((agent-icon (agent-shell-workspace--agent-icon buf))
                 (status (agent-shell-workspace--track-status
                          buf (agent-shell-workspace--buffer-status buf)))
                 (icon (agent-shell-workspace--status-icon status))
                 (status-face (agent-shell-workspace--status-face status))
                 (short-name (agent-shell-workspace--short-name buf))
                 (icon-propertized (propertize icon 'face status-face))
                 (tile-indicator (if (memq buf tiled) "▫" " "))
                 (line (concat " " agent-icon " " icon-propertized " " short-name " " tile-indicator)))
            ;; Apply attention faces to entire line for visibility
            (when (string= status "waiting")
              (setq line (propertize line 'face 'agent-shell-workspace-waiting)))
            (when (string= status "finished")
              (setq line (propertize line 'face 'agent-shell-workspace-finished)))
            ;; Apply selected face if this is the selected buffer
            (when (eq buf selected)
              (setq line (propertize line 'face 'agent-shell-workspace-selected))
              (setq target-line line-num))
            ;; Add text properties for interaction
            (setq line (propertize line
                                   'agent-shell-workspace-buffer buf
                                   'mouse-face 'highlight))
            (insert line "\n")
            (setq line-num (1+ line-num))))))
    ;; Restore cursor to selected line
    (goto-char (point-min))
    (when target-line
      (forward-line (1- target-line)))))

(defun agent-shell-workspace-sidebar-refresh ()
  "Refresh the sidebar if it exists."
  (interactive)
  (when-let ((buf (get-buffer agent-shell-workspace-sidebar-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((saved-point (point)))
          (agent-shell-workspace-sidebar--render)
          (goto-char (min saved-point (point-max))))))))

(defun agent-shell-workspace-sidebar--buffer-at-point ()
  "Return the agent buffer associated with the line at point."
  (get-text-property (line-beginning-position) 'agent-shell-workspace-buffer))

;;;; Interaction commands

(defun agent-shell-workspace-sidebar-goto ()
  "Focus the agent buffer at point in the main area."
  (interactive)
  (let ((buf (agent-shell-workspace-sidebar--buffer-at-point)))
    (unless (and buf (buffer-live-p buf))
      (user-error "No live agent buffer at point"))
    (setq agent-shell-workspace-sidebar--selected-buffer buf)
    (agent-shell-workspace--clear-finished buf)
    ;; Find a non-sidebar window and display the buffer there
    (let ((target-window nil))
      (walk-windows
       (lambda (win)
         (when (and (not target-window)
                    (not (window-parameter win 'window-side))
                    (not (string= (buffer-name (window-buffer win))
                                  agent-shell-workspace-sidebar-buffer-name)))
           (setq target-window win)))
       nil nil)
      (when target-window
        (set-window-buffer target-window buf)
        (select-window target-window)))
    (agent-shell-workspace-sidebar-refresh)))

(defun agent-shell-workspace-sidebar-click (event)
  "Handle mouse click EVENT in the sidebar."
  (interactive "e")
  (mouse-set-point event)
  (agent-shell-workspace-sidebar-goto))

;;;; Quick switch

(defun agent-shell-workspace-sidebar-toggle-quick-switch ()
  "Toggle quick-switch mode in the sidebar.
When active, moving up/down shows the agent at point in the main
area without moving focus from the sidebar."
  (interactive)
  (setq agent-shell-workspace-sidebar--quick-switch
        (not agent-shell-workspace-sidebar--quick-switch))
  (if agent-shell-workspace-sidebar--quick-switch
      (progn
        (add-hook 'post-command-hook
                  #'agent-shell-workspace-sidebar--quick-switch-hook nil t)
        (message "Quick switch ON"))
    (remove-hook 'post-command-hook
                 #'agent-shell-workspace-sidebar--quick-switch-hook t)
    (message "Quick switch OFF")))

(defun agent-shell-workspace-sidebar--quick-switch-hook ()
  "Post-command hook that peeks at the agent under cursor.
Shows the buffer in the main area without moving focus."
  (when agent-shell-workspace-sidebar--quick-switch
    (let ((buf (agent-shell-workspace-sidebar--buffer-at-point)))
      (when (and buf (buffer-live-p buf)
                 (not (eq buf agent-shell-workspace-sidebar--selected-buffer)))
        (setq agent-shell-workspace-sidebar--selected-buffer buf)
        (agent-shell-workspace--clear-finished buf)
        ;; Show in main area without moving focus
        (let ((target-window nil))
          (walk-windows
           (lambda (win)
             (when (and (not target-window)
                        (not (window-parameter win 'window-side))
                        (not (string= (buffer-name (window-buffer win))
                                      agent-shell-workspace-sidebar-buffer-name)))
               (setq target-window win)))
           nil nil)
          (when target-window
            (set-window-buffer target-window buf)))
        (agent-shell-workspace-sidebar--render)))))

;;;; Sidebar open

(defun agent-shell-workspace-sidebar-open ()
  "Open the agent workspace sidebar."
  (let* ((buf (get-buffer-create agent-shell-workspace-sidebar-buffer-name))
         (window (get-buffer-window buf)))
    (unless (and window (window-live-p window))
      (setq window
            (display-buffer-in-side-window
             buf
             `((side . left)
               (slot . 0)
               (window-width . ,agent-shell-workspace-sidebar-width)
               (preserve-size . (t . nil))
               (window-parameters
                . ((no-delete-other-windows . t)
                   (no-other-window . nil))))))
      (set-window-dedicated-p window t))
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-shell-workspace-sidebar-mode)
        (agent-shell-workspace-sidebar-mode))
      (unless agent-shell-workspace-sidebar--selected-buffer
        (let ((live-buffers (seq-filter #'buffer-live-p (agent-shell-buffers))))
          (when live-buffers
            (setq agent-shell-workspace-sidebar--selected-buffer (car live-buffers)))))
      (agent-shell-workspace-sidebar--render))))

;;;; Agent management commands

(defun agent-shell-workspace-sidebar-new ()
  "Create a new agent-shell from the sidebar."
  (interactive)
  (agent-shell t)
  (agent-shell-workspace-sidebar-refresh))

(defun agent-shell-workspace-sidebar-kill ()
  "Kill the agent-shell process at point."
  (interactive)
  (let ((buffer (agent-shell-workspace-sidebar--buffer-at-point)))
    (unless (and buffer (buffer-live-p buffer))
      (user-error "No live agent buffer at point"))
    (when (yes-or-no-p (format "Kill agent-shell process in %s? " (buffer-name buffer)))
      (with-current-buffer buffer
        (let ((proc (get-buffer-process buffer)))
          (when (and proc (process-live-p proc))
            (comint-send-eof)
            (message "Sent EOF to agent-shell process in %s" (buffer-name buffer)))))
      (run-with-timer 0.1 nil #'agent-shell-workspace-sidebar-refresh))))

(defun agent-shell-workspace-sidebar-restart ()
  "Restart the agent-shell at point.
Kills the current process and starts a new one with the same config if possible."
  (interactive)
  (let ((buffer (agent-shell-workspace-sidebar--buffer-at-point)))
    (unless (and buffer (buffer-live-p buffer))
      (user-error "No live agent buffer at point"))
    (let ((config (agent-shell-workspace--buffer-config buffer))
          (buf-name (buffer-name buffer)))
      (when (yes-or-no-p (format "Restart agent-shell %s? " buf-name))
        (let ((proc (get-buffer-process buffer)))
          (when (and proc (process-live-p proc))
            (kill-process proc)))
        (kill-buffer buffer)
        (if config
            (agent-shell-start :config config)
          (agent-shell t))
        (agent-shell-workspace-sidebar-refresh)
        (message "Restarted %s" buf-name)))))

(defun agent-shell-workspace-sidebar-delete-killed ()
  "Delete all killed agent-shell buffers."
  (interactive)
  (let ((killed-buffers (seq-filter
                         (lambda (buf)
                           (and (buffer-live-p buf)
                                (string= (agent-shell-workspace--buffer-status buf) "killed")))
                         (agent-shell-buffers))))
    (if (null killed-buffers)
        (message "No killed buffers to delete")
      (when (yes-or-no-p (format "Delete %d killed buffer%s? "
                                 (length killed-buffers)
                                 (if (= (length killed-buffers) 1) "" "s")))
        (dolist (buf killed-buffers)
          (kill-buffer buf))
        (agent-shell-workspace-sidebar-refresh)
        (message "Deleted %d killed buffer%s"
                 (length killed-buffers)
                 (if (= (length killed-buffers) 1) "" "s"))))))

(defun agent-shell-workspace-sidebar-set-mode ()
  "Set session mode for the agent at point."
  (interactive)
  (let ((buffer (agent-shell-workspace-sidebar--buffer-at-point)))
    (unless (and buffer (buffer-live-p buffer))
      (user-error "No live agent buffer at point"))
    (with-current-buffer buffer
      (agent-shell-set-session-mode))
    (agent-shell-workspace-sidebar-refresh)))

(defun agent-shell-workspace-sidebar-cycle-mode ()
  "Cycle session mode for the agent at point."
  (interactive)
  (let ((buffer (agent-shell-workspace-sidebar--buffer-at-point)))
    (unless (and buffer (buffer-live-p buffer))
      (user-error "No live agent buffer at point"))
    (with-current-buffer buffer
      (agent-shell-cycle-session-mode))
    (agent-shell-workspace-sidebar-refresh)))

(defun agent-shell-workspace-sidebar-rename ()
  "Rename the agent buffer at point.
Only changes the post-@ portion, preserving the agent type prefix."
  (interactive)
  (let ((buffer (agent-shell-workspace-sidebar--buffer-at-point)))
    (unless (and buffer (buffer-live-p buffer))
      (user-error "No live agent buffer at point"))
    (let* ((old-name (buffer-name buffer))
           (prefix (if (string-match "\\(.*@ \\)" old-name)
                       (match-string 1 old-name)
                     ""))
           (old-short (agent-shell-workspace--short-name buffer))
           (new-short (read-string (format "Rename '%s' to: " old-short) old-short)))
      (when (string-empty-p new-short)
        (user-error "Name cannot be empty"))
      (with-current-buffer buffer
        (rename-buffer (concat prefix new-short) t))
      (agent-shell-workspace-sidebar-refresh))))

(defun agent-shell-workspace-sidebar-interrupt ()
  "Interrupt the agent at point."
  (interactive)
  (let ((buffer (agent-shell-workspace-sidebar--buffer-at-point)))
    (unless (and buffer (buffer-live-p buffer))
      (user-error "No live agent buffer at point"))
    (with-current-buffer buffer
      (agent-shell-interrupt))
    (agent-shell-workspace-sidebar-refresh)))

;;; Tiling

(defvar-local agent-shell-workspace--tiled-buffers nil
  "List of buffers currently shown in the tiled layout.")

(defun agent-shell-workspace-tile-toggle ()
  "Un-tile back to single focus on the current agent."
  (interactive)
  (let ((sidebar-buf (get-buffer agent-shell-workspace-sidebar-buffer-name)))
    (when (and sidebar-buf
               (buffer-local-value 'agent-shell-workspace--tiled sidebar-buf))
      (agent-shell-workspace--untile))))

(defun agent-shell-workspace-tile-add ()
  "Add the agent at point to the tiled view.
Press on each agent you want tiled.  Tiling begins once 2 agents
are marked.  Maximum 8."
  (interactive)
  (let* ((buf (agent-shell-workspace-sidebar--buffer-at-point))
         (sidebar-buf (get-buffer agent-shell-workspace-sidebar-buffer-name)))
    (unless (and buf (buffer-live-p buf))
      (user-error "No live agent buffer at point"))
    (let ((current-tiled (or (and sidebar-buf
                                  (buffer-local-value 'agent-shell-workspace--tiled-buffers
                                                      sidebar-buf))
                             '())))
      (when (memq buf current-tiled)
        (user-error "Already in tiled view"))
      (when (>= (length current-tiled) 8)
        (user-error "Maximum 8 agents for tiling"))
      (let ((new-tiled (append current-tiled (list buf))))
        (if (>= (length new-tiled) 2)
            ;; Enough to tile — do it
            (save-selected-window
              (agent-shell-workspace--tile new-tiled))
          ;; Just mark it, show indicator, wait for more
          (with-current-buffer sidebar-buf
            (setq agent-shell-workspace--tiled-buffers new-tiled))
          (agent-shell-workspace-sidebar-refresh)
          (message "Marked for tiling. Press 'a' on another agent."))))))

(defun agent-shell-workspace-tile-remove ()
  "Remove the agent at point from the tiled view.
If only one remains, un-tiles entirely."
  (interactive)
  (let* ((buf (agent-shell-workspace-sidebar--buffer-at-point))
         (sidebar-buf (get-buffer agent-shell-workspace-sidebar-buffer-name)))
    (unless (and buf (buffer-live-p buf))
      (user-error "No live agent buffer at point"))
    (let ((current-tiled (and sidebar-buf
                              (buffer-local-value 'agent-shell-workspace--tiled-buffers
                                                  sidebar-buf))))
      (unless current-tiled
        (user-error "Not in tiled mode"))
      (unless (memq buf current-tiled)
        (user-error "Buffer not in tiled view"))
      (let ((remaining (remq buf current-tiled)))
        (save-selected-window
          (if (<= (length remaining) 1)
              (agent-shell-workspace--untile)
            (agent-shell-workspace--tile remaining)))))))

(defun agent-shell-workspace--tile (buffers)
  "Arrange BUFFERS in a tiled grid in the main area.
BUFFERS should be a list of 2 to 8 buffer objects.
Layout uses a grid with COLS columns and ROWS rows,
filling left-to-right, top-to-bottom."
  (let* ((count (length buffers))
         (cols (cond ((<= count 2) 2)
                     ((<= count 4) 2)
                     ((<= count 6) 3)
                     (t 4)))
         (rows (ceiling count cols))
         (non-side-windows (seq-filter
                            (lambda (win)
                              (not (window-parameter win 'window-side)))
                            (window-list))))
    ;; Collapse to one window
    (let ((keep (car non-side-windows)))
      (dolist (win (cdr non-side-windows))
        (delete-window win))
      ;; Split into rows first, then columns within each row
      (let ((row-windows (list keep))
            (idx 0))
        ;; Create row splits
        (dotimes (_ (1- rows))
          (let ((new-row (split-window (car (last row-windows)) nil 'below)))
            (setq row-windows (append row-windows (list new-row)))))
        ;; For each row, create column splits and assign buffers
        (dolist (row-win row-windows)
          (let ((cols-this-row (min cols (- count idx))))
            (when (> cols-this-row 0)
              (set-window-buffer row-win (nth idx buffers))
              (cl-incf idx)
              (let ((prev-win row-win))
                (dotimes (_ (1- cols-this-row))
                  (let ((col-win (split-window prev-win nil 'right)))
                    (set-window-buffer col-win (nth idx buffers))
                    (cl-incf idx)
                    (setq prev-win col-win)))))))
        (select-window keep)))
    ;; Mark as tiled in sidebar buffer
    (when-let ((sidebar-buf (get-buffer agent-shell-workspace-sidebar-buffer-name)))
      (with-current-buffer sidebar-buf
        (setq agent-shell-workspace--tiled t)
        (setq agent-shell-workspace--tiled-buffers buffers)))))

(defun agent-shell-workspace--untile ()
  "Return from tiled mode to single-buffer focus.
Keeps the window showing the current buffer and deletes the rest."
  (let ((current-buf (current-buffer))
        (non-side-windows (seq-filter
                           (lambda (win)
                             (not (window-parameter win 'window-side)))
                           (window-list))))
    ;; Prefer the window showing current buffer
    (let ((keep (or (seq-find (lambda (win)
                                (eq (window-buffer win) current-buf))
                              non-side-windows)
                    (car non-side-windows))))
      (dolist (win non-side-windows)
        (unless (eq win keep)
          (delete-window win)))
      (when keep
        (select-window keep))))
  ;; Clear tiled state in sidebar buffer
  (when-let ((sidebar-buf (get-buffer agent-shell-workspace-sidebar-buffer-name)))
    (with-current-buffer sidebar-buf
      (setq agent-shell-workspace--tiled nil)
      (setq agent-shell-workspace--tiled-buffers nil)))
  (agent-shell-workspace-sidebar-refresh))

;;; Layout

(defun agent-shell-workspace--setup-layout ()
  "Set up the Agents tab layout.
Deletes other windows and shows the first agent buffer, or creates one."
  (delete-other-windows)
  (let ((agent-buffers (seq-filter #'buffer-live-p (agent-shell-buffers))))
    (if agent-buffers
        (switch-to-buffer (car agent-buffers))
      (agent-shell)))
  (agent-shell-workspace-sidebar-open))

;;; Toggle command

;;;###autoload
(defun agent-shell-workspace-toggle ()
  "Toggle the Agents workspace tab.
If already in the Agents tab, switch back to the previous tab.
If the Agents tab exists, switch to it.
Otherwise, create a new Agents tab with the standard layout."
  (interactive)
  ;; Ensure the tab-select hook is always registered
  (add-hook 'tab-bar-tab-post-select-functions
            #'agent-shell-workspace--on-tab-selected)
  (cond
   ;; Already in Agents tab -- go back
   ((agent-shell-workspace--in-agents-tab-p)
    (if agent-shell-workspace--previous-tab
        (tab-bar-switch-to-tab agent-shell-workspace--previous-tab)
      (tab-bar-switch-to-prev-tab)))

   ;; Agents tab exists but is not current -- switch to it
   ((agent-shell-workspace--tab-exists-p)
    (setq agent-shell-workspace--previous-tab
          (alist-get 'name (tab-bar--current-tab)))
    (tab-bar-switch-to-tab agent-shell-workspace--tab-name)
    (agent-shell-workspace--activate-isolation))

   ;; No Agents tab -- create one
   (t
    (setq agent-shell-workspace--previous-tab
          (alist-get 'name (tab-bar--current-tab)))
    (tab-bar-new-tab)
    (tab-bar-rename-tab agent-shell-workspace--tab-name)
    (agent-shell-workspace--setup-layout)
    (agent-shell-workspace--activate-isolation))))

;;; Buffer isolation

(defvar agent-shell-workspace--isolation-active nil
  "Non-nil when buffer isolation rules are active.")

(defun agent-shell-workspace--agent-buffer-p (buffer)
  "Return non-nil if BUFFER belongs in the Agents workspace."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (or (derived-mode-p 'agent-shell-mode)
          (derived-mode-p 'agent-shell-workspace-sidebar-mode)
          (derived-mode-p 'agent-shell-manager-mode)
          (and (derived-mode-p 'diff-mode)
               (local-variable-p 'agent-shell-on-exit))
          (string-match-p "\\*agent-shell.*\\(traffic\\|log\\)"
                          (buffer-name buffer))))))

(defun agent-shell-workspace--redirect-display-condition (buffer-name _action)
  "Return non-nil when BUFFER-NAME should be redirected out of Agents tab."
  (and agent-shell-workspace--isolation-active
       (agent-shell-workspace--in-agents-tab-p)
       (not (agent-shell-workspace--agent-buffer-p
             (get-buffer buffer-name)))))

(defun agent-shell-workspace--redirect-display (buffer alist)
  "Redirect BUFFER out of the Agents tab.
Switches to the previous tab so normal `display-buffer' rules handle
BUFFER there.  ALIST is ignored.  Returns nil so display continues."
  (ignore buffer alist)
  (if agent-shell-workspace--previous-tab
      (tab-bar-switch-to-tab agent-shell-workspace--previous-tab)
    (tab-bar-switch-to-prev-tab))
  nil)

(defun agent-shell-workspace--switch-buffer-advice (orig-fn buffer &rest args)
  "Around advice for `switch-to-buffer' to enforce Agents tab isolation.
ORIG-FN is the original function.  BUFFER and ARGS are its arguments."
  (let ((buf (if (stringp buffer) (get-buffer buffer) buffer)))
    (if (and agent-shell-workspace--isolation-active
             (agent-shell-workspace--in-agents-tab-p)
             (not (agent-shell-workspace--agent-buffer-p buf)))
        (progn
          (if agent-shell-workspace--previous-tab
              (tab-bar-switch-to-tab agent-shell-workspace--previous-tab)
            (tab-bar-switch-to-prev-tab))
          (apply orig-fn buffer args))
      (apply orig-fn buffer args))))

(defun agent-shell-workspace--activate-isolation ()
  "Activate buffer isolation rules for the Agents tab."
  (setq agent-shell-workspace--isolation-active t)
  (add-to-list 'display-buffer-alist
               '(agent-shell-workspace--redirect-display-condition
                 (agent-shell-workspace--redirect-display)))
  (advice-add 'switch-to-buffer :around
              #'agent-shell-workspace--switch-buffer-advice))

(defun agent-shell-workspace--deactivate-isolation ()
  "Deactivate buffer isolation rules for the Agents tab."
  (setq agent-shell-workspace--isolation-active nil)
  (setq display-buffer-alist
        (seq-remove (lambda (entry)
                      (eq (car entry)
                          'agent-shell-workspace--redirect-display-condition))
                    display-buffer-alist))
  (advice-remove 'switch-to-buffer
                 #'agent-shell-workspace--switch-buffer-advice))

(defun agent-shell-workspace--on-tab-selected (&rest _args)
  "Hook function for `tab-bar-tab-post-select-functions'.
Activates isolation when entering the Agents tab, deactivates when leaving."
  (if (agent-shell-workspace--in-agents-tab-p)
      (unless agent-shell-workspace--isolation-active
        (agent-shell-workspace--activate-isolation))
    (when agent-shell-workspace--isolation-active
      (agent-shell-workspace--deactivate-isolation))))

(provide 'agent-shell-workspace)

;;; agent-shell-workspace.el ends here
