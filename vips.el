;;; vips.el --- An Emacs front end for OpenAI's GPT API and DeepL's translation API  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Marc Klemp
;; Keywords: GPT, DeepL, TTS

;; POST-related code adapted and extended from `aide.el` by Junji Zhi.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

(require 'request)

;; General settings

(defgroup vips nil
  "vips.el custom settings"
  :group 'external
  :prefix "vips-")

(defconst vips-openai-api-url "https://api.openai.com/v1/chat/completions")
(defconst vips-deepl-api-url "https://api.deepl.com/v2/translate")
(defconst vips-lmstudio-api-url "http://localhost:1234/v1/chat/completions")

(defconst vips-recenter-position -2
  "Position to recenter the view after inserting the result.")

;; OpenAI-related settings

(defcustom vips-max-tokens 4096
  "The max-tokens paramater that vips.el sends to OpenAI API."
  :type 'integer
  :group 'vips)

(defcustom vips-temperature 0.7
  "The temperature paramater that vips.el sends to OpenAI API."
  :type 'float
  :group 'vips)

(defcustom vips-top-p 0.1
  "The top-p paramater that vips.el sends to OpenAI API."
  :type 'float
  :group 'vips)

(defcustom vips-frequency-penalty 0.1
  "The frequency_penalty paramater that vips.el sends to OpenAI API."
  :type 'float
  :group 'vips)

(defcustom vips-presence-penalty 0.1
  "The presence_penalty paramater that vips.el sends to OpenAI API."
  :type 'float
  :group 'vips)

(defcustom vips-selected-voice "alloy"
  "Selected voice for text-to-speech."
  :type '(choice (const "alloy")
                 (const "echo")
                 (const "fable")
                 (const "onyx")
                 (const "nova")
                 (const "shimmer"))
  :group 'vips)

(defun vips-select-voice ()
  "Prompt user to select a voice and set the global variable 'vips-selected-voice' to the selected voice."
  (interactive)
  (setq vips-selected-voice (completing-read "Select voice: " '("alloy" "echo" "fable" "onyx" "nova" "shimmer"))))

;; DeepL-related code

(defcustom vips-languages '("DA" "EN")
  ;; Add more languages here or, better yet, in your config.el
  "List of languages available for selection."
  :type '(repeat string)
  :group 'vips)

(defvar-local vips-selected-language "DA"
  "Selected target language for translation.")

(defun vips-deepl-translate (api-key prompt)
  "Return translated text from Deepl API. \n\n API-KEY is Deepl API key. \n\n PROMPT is text string sent to the API."
  (vips-select-language)
  (let ((result nil)
        (auth-value (format "DeepL-Auth-Key %s" api-key)))
    (request
      vips-deepl-api-url
      :type "POST"
      :data `(("text" . ,prompt) ("target_lang" . ,vips-selected-language))
      :parser 'json-read
      :headers `(("Authorization" . ,auth-value))
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq result (concat "\n" (alist-get 'text (elt (alist-get 'translations data) 0)))))))
   result))

(defun vips-select-language ()
  "Prompt user to select a language and set the global variable 'vips-selected-language' to the selected language code."
  (interactive)
  (setq vips-selected-language (completing-read "Select target language: " vips-languages)))

(defun vips--deepl-translate-string (string)
  (vips-deepl-translate deepl-api-key string))

(defun vips-translate (start end)
  "Send region or paragraph to Deepl and insert result to end of region. \n\n START and END are selected region boundaries."
  (interactive "r")
  (vips-process-region start end 'vips--deepl-translate-string))

; OpenAI-related code
; GPT

(defcustom vips-main-system-messages '("You are a helpful assistant."
                                       "You are a great teacher.")
  "List of system messages available for selection."
  :type '(repeat string)
  :group 'vips)

(defcustom vips-addon-messages '("Use Markdown markup."
                                 "Use LaTeX markup."
                                 "Use Org Mode markup.")
  "List of add-on messages available for selection."
  :type '(repeat string)
  :group 'vips)

(defvar-local vips-selected-main-system-message "You are a helpful assistant."
  "Selected main system message.")

(defvar-local vips-selected-addon-message ""
  "Selected add-on message.")

(defvar-local vips-selected-system-message "You are a helpful assistant."
  "Selected system message, including the add-on message.")

(defun vips-clear-main-system-message ()
  "Clear the selected main system message."
  (interactive)
  (setq vips-selected-main-system-message "")
  (vips-combine-messages))

(defun vips-clear-addon-message ()
  "Clear the selected add-on message."
  (interactive)
  (setq vips-selected-addon-message "")
  (vips-combine-messages))

(defun vips-clear-both-messages ()
  "Clear both the selected main system message and the selected add-on message."
  (interactive)
  (vips-clear-main-system-message)
  (vips-clear-addon-message))

(defun vips-clear-selected-message ()
  "Prompt user to select which message to clear: the main system message, the add-on message, or both."
  (interactive)
  (let ((message-to-clear (completing-read "Clear which message? (main/addon/both): " '("main" "addon" "both"))))
    (cond
     ((string-equal message-to-clear "main") (vips-clear-main-system-message))
     ((string-equal message-to-clear "addon") (vips-clear-addon-message))
     ((string-equal message-to-clear "both") (vips-clear-both-messages))
     (t (message "Invalid option")))))

(defun vips-select-main-system-message ()
  "Prompt user to select a main system message and set the global variable 'vips-selected-main-system-message' to the selected message."
  (interactive)
  (setq vips-selected-main-system-message (completing-read "Select main system message: " vips-main-system-messages))
  (vips-combine-messages))

(defun vips-select-addon-message ()
  "Prompt user to select an add-on message and set the global variable 'vips-selected-addon-message' to the selected message."
  (interactive)
  (setq vips-selected-addon-message (completing-read "Select add-on message: " vips-addon-messages))
  (vips-combine-messages))

(defun vips-combine-messages ()
  "Combine the selected main system message with the selected add-on message and store it in 'vips-selected-system-message'."
  (interactive)
  (setq vips-selected-system-message (concat vips-selected-main-system-message "\n\n" vips-selected-addon-message)))

(defun vips-update-system-message-in-all-buffers ()
  "Update the system message in all buffers."
  (interactive)
  ;; Capture the current values of the selected messages.
  (let ((current-main-message vips-selected-main-system-message)
        (current-addon-message vips-selected-addon-message)
        (current-combined-message vips-selected-system-message))
    ;; Iterate over all buffers and set the selected messages.
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (setq vips-selected-main-system-message current-main-message
              vips-selected-addon-message current-addon-message
              vips-selected-system-message current-combined-message)))))

(defun vips-display-selected-messages ()
  "Display the currently selected messages to the user in a temporary buffer and frame."
  (interactive)
  ;; Capture the current values of the selected messages.
  (let ((current-main-message vips-selected-main-system-message)
        (current-addon-message vips-selected-addon-message)
        (current-combined-message vips-selected-system-message)
        (buffer-name "*VIPS Messages*"))
    ;; Kill the buffer if it exists.
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    ;; Create a new buffer and insert the captured messages.
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "Selected main system message:\n" current-main-message "\n\n")
        (insert "Selected add-on message:\n" current-addon-message "\n\n")
        (insert "Combined system message:\n" current-combined-message "\n")
        (view-mode 1)
        ;; Set up a custom quit function to delete the frame when 'q' is pressed.
        (local-set-key (kbd "q") (lambda ()
                                    (interactive)
                                    (delete-frame))))
      ;; Display the buffer in a new frame.
      (let ((display-buffer-alist
             `((,(regexp-quote buffer-name)
                (display-buffer-pop-up-frame)
                (inhibit-same-window . t)
                (reusable-frames . nil)))))
        (pop-to-buffer buffer)))))

(defun vips--openai-api-request (api-key model prompt &optional api-url)
  "Return prompt answer from OpenAI API or a local inference server.
API-KEY is OpenAI API key or an empty string for local server.
MODEL is the model string, e.g., \"gpt-4\".
PROMPT is prompt string sent to the API.
Optional API-URL is the URL of the API; defaults to OpenAI's URL."
  (let* ((api-url (or api-url vips-openai-api-url)) ; Use provided api-url or default if nil
         (auth-value (if (string-empty-p api-key) "" (format "Bearer %s" api-key)))
         (insertion-marker (make-marker))
         (prompt-end (+ (region-beginning) (length prompt)))
         (current-buffer (current-buffer)))
    (set-marker insertion-marker prompt-end current-buffer)
    (message "Request sent, waiting for response.")
    (request
      api-url
      :type "POST"
      :data (json-encode `(("model" . ,model)
                           ("messages" . ((("role" . "system") ("content" . ,vips-selected-system-message))
                                         (("role" . "user") ("content" . ,prompt))))
                           ("max_tokens" . ,vips-max-tokens)
                           ("temperature" . ,vips-temperature)
                           ("frequency_penalty" . ,vips-frequency-penalty)
                           ("presence_penalty" . ,vips-presence-penalty)
                           ("top_p" . ,vips-top-p)))
      :headers `(("Authorization" . ,auth-value) ("Content-Type" . "application/json"))
      :sync nil
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (with-current-buffer current-buffer
                    (goto-char insertion-marker)
                    (insert (concat "\n" (alist-get 'content (alist-get 'message (elt (alist-get 'choices data) 0))))))
                  (message "Response inserted.")))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error: %S" error-thrown))))
    nil))

(defun vips--openai-chat (model prompt &optional api-url)
  (vips--openai-api-request openai-api-key model prompt api-url))

(defun vips-get-region-or-paragraph ()
  "Return the start and end of the selected region or the current paragraph."
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list (save-excursion
            (backward-paragraph)
            (point))
          (save-excursion
            (forward-paragraph)
            (point)))))

(defun vips-chat-region (start end model &optional api-url)
  "Send region or paragraph to OpenAI and insert result to end of region. \n\n START and END are selected region boundaries."
  (interactive "r")
  (vips-process-region start end (lambda (region) (vips--openai-chat model region api-url))))

(defun vips-chat-region-gpt-4 ()
  "Call `vips-chat-region' with the gpt-4 model argument passed in."
  (interactive)
  (let* ((region (vips-get-region-or-paragraph))
         (start (car region))
         (end (cadr region)))
    (vips-chat-region start end "gpt-4")))

(defun vips-chat-region-lmstudio-local-model ()
  "Call `vips-chat-region' using the vips-lmstudio-api-url."
  (interactive)
  (let* ((region (vips-get-region-or-paragraph))
         (start (car region))
         (end (cadr region)))
    (vips-chat-region start end "local-model" vips-lmstudio-api-url))) ; Note: As of 30 November, 2023, LM Studio does not utilize the "model" argument and the "local-model" might as well be blank at this time. Please refer to the latest documentation for updates.

(defun vips-chat-region-gpt-3.5-turbo ()
  "Call `vips-chat-region' with the gpt-3.5-turbo model argument passed in."
  (interactive)
  (let* ((region (vips-get-region-or-paragraph))
         (start (car region))
         (end (cadr region)))
    (vips-chat-region start end "gpt-3.5-turbo")))

(defun vips-chat-region-gpt-4-turbo ()
  "Call `vips-chat-region' with the gpt-3.5-turbo model argument passed in."
  (interactive)
  (let* ((region (vips-get-region-or-paragraph))
         (start (car region))
         (end (cadr region)))
    (vips-chat-region start end "gpt-4-1106-preview")))

(defun vips-chat-region-select-model ()
  "Call `vips-chat-region' with a user-selected OpenAI model."
  (interactive)
  (let* ((region (vips-get-region-or-paragraph))
         (start (car region))
         (end (cadr region))
         (models '("gpt-4-1106-preview" "gpt-4" "gpt-3.5-turbo" "gpt-3.5-turbo-16k"))
         (selected-model (completing-read "Select a model: " models nil t)))
    (vips-chat-region start end selected-model)))

(defun vips-process-region (start end process-fn)
  "Process the region or paragraph using PROCESS-FN."
  (let* ((region (buffer-substring-no-properties start end))
         (result (funcall process-fn region)))
    (if result
        (progn
          (goto-char (max end (point))) ;; go to the end of the processed region
          (insert "" result "\n")
          (push-mark start 'no-message)
          (setq deactivate-mark nil)
          (unless (pos-visible-in-window-p (point))
            (recenter vips-recenter-position)))
      (message "Waiting for response..."))))

(defun mark-and-run-vips-chat-region-gpt-4-to-current ()
  "Mark the text from the start of the buffer to the current position and run 'vips-chat-region-gpt-4'."
  (interactive)
  (let* ((start (point-min))
         (end (if (region-active-p)
                  (region-end)
                (point))))
    (push-mark start)
    (goto-char end)
    (activate-mark)
    (vips-process-region start end (lambda (region) (vips--openai-chat "gpt-4" region)))))

(defun mark-and-run-vips-chat-region-gpt-4 ()
  "Run 'vips-chat-region-gpt-4' on the entire buffer. If no mark has been set, set and deactivate one as a workaround."
  (interactive)
  (when (not (mark))
    ;; If no mark has been set, set one at the current point as a workaround (otherwise Emacs will complain if no mark was ever set in the buffer).
    (push-mark (point))
    ;; Deactivate the mark immediately so it doesn't affect subsequent commands.
    (deactivate-mark))
  (let ((start (point-min))
        (end (point-max)))
    (vips-process-region start end (lambda (region) (vips--openai-chat "gpt-4" region)))))

(defun mark-and-run-vips-chat-region-gpt-4-turbo-to-current ()
  "Mark the text from the start of the buffer to the current position and run 'vips-chat-region-gpt-4-turbo'."
  (interactive)
  (let* ((start (point-min))
         (end (if (region-active-p)
                  (region-end)
                (point))))
    (push-mark start)
    (goto-char end)
    (activate-mark)
    (vips-process-region start end (lambda (region) (vips--openai-chat "gpt-4-1106-preview" region)))))

(defun mark-and-run-vips-chat-region-gpt-4-turbo ()
  "Run 'vips-chat-region-gpt-4-turbo' on the entire buffer. If no mark has been set, set and deactivate one as a workaround."
  (interactive)
  (when (not (mark))
    ;; If no mark has been set, set one at the current point as a workaround (otherwise Emacs will complain if no mark was ever set in the buffer).
    (push-mark (point))
    ;; Deactivate the mark immediately so it doesn't affect subsequent commands.
    (deactivate-mark))
  (let ((start (point-min))
        (end (point-max)))
    (vips-process-region start end (lambda (region) (vips--openai-chat "gpt-4-1106-preview" region)))))

(defun mark-and-run-vips-chat-region-lmstudio-local-model-to-current ()
  "Mark the text from the start of the buffer to the current position and run 'vips-chat-region-lmstudio-local-model'."
  (interactive)
  (let* ((start (point-min))
         (end (if (region-active-p)
                  (region-end)
                (point))))
    (push-mark start)
    (goto-char end)
    (activate-mark)
    (vips-process-region start end (lambda (region) (vips--openai-chat "local-model" region vips-lmstudio-api-url)))))

(defun mark-and-run-vips-chat-region-lmstudio-local-model ()
  "Run 'vips-chat-region-lmstudio-local-model' on the entire buffer. If no mark has been set, set and deactivate one as a workaround."
  (interactive)
  (when (not (mark))
    ;; If no mark has been set, set one at the current point as a workaround (otherwise Emacs will complain if no mark was ever set in the buffer).
    (push-mark (point))
    ;; Deactivate the mark immediately so it doesn't affect subsequent commands.
    (deactivate-mark))
  (let ((start (point-min))
        (end (point-max)))
    (vips-process-region start end (lambda (region) (vips--openai-chat "local-model" region vips-lmstudio-api-url)))))

; TTS

(defun vips--openai-speech-request (api-key model input voice &optional response-format speed output-file action)
  "Return audio file content from OpenAI API and save it to OUTPUT-FILE or play it.
API-KEY is OpenAI API key.
MODEL is the TTS model string, e.g., \"tts-1\".
INPUT is the text string sent to the API.
VOICE is the voice used for generating the audio.
RESPONSE-FORMAT is the format of the audio file, defaults to 'mp3'.
SPEED is the speed of the generated audio.
ACTION is either 'save or 'play, determining what to do with the response."
  (let* ((auth-value (format "Bearer %s" api-key))
         (url "https://api.openai.com/v1/audio/speech")
         (data (json-encode `(("model" . ,model)
                              ("input" . ,input)
                              ("voice" . ,voice)
                              ,@(when response-format `(("response_format" . ,response-format)))
                              ,@(when speed `(("speed" . ,speed))))))
         (response-buffer (generate-new-buffer "*vips-speech-response*"))
         (response (request
                     url
                     :type "POST"
                     :data data
                     :headers `(("Authorization" . ,auth-value) ("Content-Type" . "application/json"))
                     :parser 'buffer-string
                     :success (cl-function
                               (lambda (&key data &allow-other-keys)
                                 (with-current-buffer response-buffer
                                   (insert data))
                                 (when (eq action 'save)
                                   (let ((coding-system-for-write 'binary)) ;; Use binary encoding for writing
                                     (with-temp-file output-file
                                       (insert-buffer-substring response-buffer))
                                     (message "Speech saved to %s" output-file)))
                                 ;; Kill the response buffer after saving or playing
                                 (kill-buffer response-buffer)))
                     :error (cl-function
                             (lambda (&key error-thrown &allow-other-keys)
                               (message "Error: %S" error-thrown))))))
    response))

(defun vips-create-and-play-speech (start end)
  "Generate speech from selected text and play it.
START and END define the region of text to be synthesized."
  (interactive "r")
  (unless (use-region-p)
    (error "No region selected"))
  (let* ((model "tts-1") ;; You can prompt for this or set a default.
         (input (buffer-substring-no-properties start end))
         (voice vips-selected-voice)
         (output-file (make-temp-file "emacs_speech_" nil ".mp3"))
         (response (vips--openai-speech-request openai-api-key model input voice nil nil output-file 'save)))
    ;; Wait for the request to finish
    (while (not (request-response-done-p response)) (sleep-for 0.1))
    ;; Play the sound file
    (play-sound-file output-file)))

(defun vips-create-and-save-speech (start end)
  "Generate speech from selected text and save it to a specified location.
START and END define the region of text to be synthesized."
  (interactive "r")
  (unless (use-region-p)
    (error "No region selected"))
  (let* ((model "tts-1") ;; You can prompt for this or set a default.
         (input (buffer-substring-no-properties start end))
         (voice vips-selected-voice)
         (output-file (read-file-name "Save speech as: " nil nil nil "speech.mp3"))
         (response (vips--openai-speech-request openai-api-key model input voice nil nil output-file 'save)))
    ;; Wait for the request to finish
    (while (not (request-response-done-p response)) (sleep-for 0.1))
    ;; Notify user that the file has been saved
    (message "Speech saved to %s" output-file)))

(defun vips-create-and-play-speech-with-os (start end)
  "Generate speech from selected text, save it to a temporary file, and play it using the OS's default media player.
START and END define the region of text to be synthesized."
  (interactive "r")
  (unless (use-region-p)
    (error "No region selected"))
  (let* ((model "tts-1") ;; You can prompt for this or set a default.
         (input (buffer-substring-no-properties start end))
         (voice vips-selected-voice)
         (output-file (make-temp-file "emacs_speech_" nil ".mp3"))
         (response (vips--openai-speech-request openai-api-key model input voice nil nil output-file 'save)))
    ;; Wait for the request to finish
    (while (not (request-response-done-p response)) (sleep-for 0.1))
    ;; Play the sound file using the OS's default media player
    (cond
     ((eq system-type 'darwin) ;; macOS
      (start-process "vips-mac-play" nil "sh" "-c" (concat "open " output-file " && sleep 1 && open -a Emacs")))
     ((eq system-type 'gnu/linux) ;; Linux
      (start-process "vips-linux-play" nil "xdg-open" output-file))
     ((eq system-type 'windows-nt) ;; Windows
      (start-process "vips-windows-play" nil "cmd.exe" "/c" "start" "" output-file))
     (t
      (message "OS not supported for audio playback")))))

(define-minor-mode vips-mode
  "Minor mode to use vips functions."
  :lighter " Vips"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c <left>") 'vips-chat-region-gpt-4)
            (define-key map (kbd "C-c <right>") 'vips-chat-region-gpt-4-turbo)
            (define-key map (kbd "C-c <down>") 'vips-chat-region-select-model)
            (define-key map (kbd "C-c C-a C-c") 'mark-and-run-vips-chat-region-gpt-4-turbo-to-current)
            (define-key map (kbd "C-c C-a C-v") 'mark-and-run-vips-chat-region-gpt-4-turbo)
            (define-key map (kbd "C-c l") 'vips-chat-region-lmstudio-local-model)
            (define-key map (kbd "C-c C-l C-c") 'mark-and-run-vips-chat-region-lmstudio-local-model-to-current)
            (define-key map (kbd "C-c C-l C-v") 'mark-and-run-vips-chat-region-lmstudio-local-model)
            (define-key map (kbd "C-c SPC") 'vips-translate)
            (define-key map (kbd "C-c C-d C-s") 'vips-display-selected-messages)
            (define-key map (kbd "C-c .") 'vips-create-and-play-speech-with-os)
            map))

(provide 'vips)
