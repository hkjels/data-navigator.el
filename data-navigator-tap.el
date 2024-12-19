;;; data-navigator-tap.el --- Data navigator tap WebSocket server -*- lexical-binding: t; -*-
;;
;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; URL: https://github.com/hkjels/data-navigator.el
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (data-navigator "0.1") (websocket "1.15"))
;; Keywords: data, navigation, edn, json

;;; License:
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; use M-x data-navigator-tap-mode to turn on the server
;;

;;; Code:

(require 'data-navigator)
(require 'websocket)

(defgroup data-navigator-tap nil
  "Data Navigator tap WebSocket server."
  :group 'applications
  :prefix "data-navigator-tap")

(defcustom data-navigator-tap-server-port
  nil
  "The port number that the tap WebSocket server should listen on."
  :type 'number
  :group 'data-navigator-tap)

(defun data-navigator-tap--handle-incoming-data (_websocket frame)
  "Handle incoming FRAME."
  (let ((data (websocket-frame-text frame)))
    (data-navigator-from-input data)))

(defvar data-navigator-tap--server nil
  "The WebSocket server instance.")

;;;###autoload
(defun data-navigator-tap-start (port)
  "Start a WebSocket server that listens for `tap>' messages on PORT."
  (interactive "nPort: ")
  (unless (process-live-p data-navigator-tap--server)
    (let ((port (or port 8081)))
      (setq data-navigator-tap--server
            (websocket-server
             port
             :on-message 'data-navigator-tap--handle-incoming-data
             :on-error (lambda (_websocket type err)
                         (message "WebSocket error on %s: %s" type err))
             :on-close (lambda (_websocket)
                         (message "WebSocket closed"))))
      (message "WebSocket server started on port %d" port))))

;;;###autoload
(defun data-navigator-tap-stop ()
  "Stop the running WebSocket server."
  (interactive)
  (when (process-live-p data-navigator-tap--server)
    (delete-process data-navigator-tap--server)
    (setq data-navigator-tap--server nil)
    (message "WebSocket server stopped")))

;;;###autoload
(define-minor-mode data-navigator-tap-mode
  "Toggle `data-navigator-tap-mode'."
  :init-value nil
  :global t
  (if data-navigator-tap-mode
      (data-navigator-tap-start data-navigator-tap-server-port)
    (data-navigator-tap-stop)))

(provide 'data-navigator-tap)
;;; data-navigator-tap.el ends here
