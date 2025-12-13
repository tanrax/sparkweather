;;; quick-weather.el --- Quick weather forecasts with sparklines -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andros Fenollosa. All rights reserved.

;; Author: Andros Fenollosa <hi@andros.dev>
;; Maintainer: Andros Fenollosa <hi@andros.dev>
;; Keywords: convenience, weather
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/tanrax/quick-weather.el

;; This package is a fork of sparkweather by Robin Stephenson <robin@aglet.net>
;; Original: https://github.com/aglet/sparkweather

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:

;; This package provides a quick weather forecast display using sparklines
;; with a minimal brackets UI design.
;;
;; Commands:
;; - `quick-weather-day': Show full day forecast with sparklines
;; - `quick-weather': Alias for `quick-weather-day'
;;
;; Configuration:
;; The package uses `calendar-latitude' and `calendar-longitude' for
;; the forecast location.
;;
;; Time markers can be configured via `quick-weather-time-windows'.
;; You can define zero or more time markers to highlight in the
;; sparkline display.
;;
;; Weather icons use Unicode weather glyphs from the Miscellaneous
;; Symbols block for broad compatibility.
;;
;; Weather data is provided by Open-Meteo https://open-meteo.com,
;; which is free for non-commercial use with up to 10000 requests per day.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'iso8601)
(require 'solar)

(defgroup quick-weather nil
  "Quick weather forecasts with sparklines."
  :group 'calendar
  :prefix "quick-weather-")

(defun quick-weather--validate-hour (symbol value)
  "Validate that VALUE is a valid hour (0-23) for SYMBOL."
  (unless (and (integerp value) (>= value 0) (<= value 23))
    (user-error "Hour must be 0-23, got %s" value))
  (set-default symbol value))

(defcustom quick-weather-lunch-start-hour 12
  "Start hour for lunch time window (24-hour format, 0-23).

DEPRECATED: Use `quick-weather-time-windows' instead."
  :type 'integer
  :set #'quick-weather--validate-hour
  :group 'quick-weather)

(defcustom quick-weather-lunch-end-hour 14
  "End hour for lunch time window (24-hour format, 0-23).

DEPRECATED: Use `quick-weather-time-windows' instead."
  :type 'integer
  :set #'quick-weather--validate-hour
  :group 'quick-weather)

(defcustom quick-weather-commute-start-hour 17
  "Start hour for commute time window (24-hour format, 0-23).

DEPRECATED: Use `quick-weather-time-windows' instead."
  :type 'integer
  :set #'quick-weather--validate-hour
  :group 'quick-weather)

(defcustom quick-weather-commute-end-hour 19
  "End hour for commute time window (24-hour format, 0-23).

DEPRECATED: Use `quick-weather-time-windows' instead."
  :type 'integer
  :set #'quick-weather--validate-hour
  :group 'quick-weather)

(defcustom quick-weather-time-windows
  '(("LUNCH"   13 success)
    ("COMMUTE" 17 warning))
  "List of time markers to highlight in weather forecast.
Each element is a list of (LABEL HOUR FACE).
LABEL is a string to display (e.g., \"LUNCH\", \"COMMUTE\").
HOUR is an integer (0-23) representing the specific hour to mark.
FACE is a face symbol for highlighting (e.g., success, warning, error).

Examples:
  ;; No time markers
  (setq quick-weather-time-windows \\='())

  ;; One time marker
  (setq quick-weather-time-windows
    \\='((\"LUNCH\" 13 success)))

  ;; Multiple time markers
  (setq quick-weather-time-windows
    \\='((\"BREAKFAST\"  7 success)
      (\"LUNCH\"      13 success)
      (\"DINNER\"     20 warning)
      (\"BEDTIME\"    23 error)))

  ;; With emojis
  (setq quick-weather-time-windows
    \\='((\"🍽️\" 13 success)
      (\"🚌\" 17 warning)))"
  :type '(repeat (list (string :tag "Label")
                      (integer :tag "Hour (0-23)")
                      (symbol :tag "Face")))
  :group 'quick-weather)

(defun quick-weather--get-time-windows ()
  "Get time windows to display from `quick-weather-time-windows'."
  quick-weather-time-windows)

(defconst quick-weather--buffer-name "*Quick-Weather*"
  "Name of buffer used to display weather forecasts.")

(defconst quick-weather--wmo-codes-unicode
  '((0 . ("☀" "Clear sky"))
    (1 . ("☀" "Mainly clear"))
    (2 . ("⛅" "Partly cloudy"))
    (3 . ("☁" "Overcast"))
    (45 . ("☁" "Fog"))
    (48 . ("☁" "Rime fog"))
    (51 . ("⛆" "Light drizzle"))
    (53 . ("⛆" "Moderate drizzle"))
    (55 . ("⛆" "Dense drizzle"))
    (56 . ("⛆" "Freezing drizzle"))
    (57 . ("⛆" "Freezing drizzle"))
    (61 . ("⛆" "Slight rain"))
    (63 . ("⛆" "Rain"))
    (65 . ("⛆" "Heavy rain"))
    (66 . ("⛆" "Freezing rain"))
    (67 . ("⛆" "Heavy freezing rain"))
    (71 . ("⛇" "Slight snow"))
    (73 . ("⛇" "Snow"))
    (75 . ("⛇" "Heavy snow"))
    (77 . ("⛇" "Snow grains"))
    (80 . ("⛆" "Rain showers"))
    (81 . ("⛆" "Rain showers"))
    (82 . ("⛆" "Violent rain showers"))
    (85 . ("⛇" "Snow showers"))
    (86 . ("⛇" "Heavy snow showers"))
    (95 . ("⛈" "Thunderstorm"))
    (96 . ("⛈" "Thunderstorm with hail"))
    (99 . ("⛈" "Thunderstorm with heavy hail")))
  "WMO weather code to (glyph description) mapping from Miscellaneous Symbols.")

(defun quick-weather--wmo-code-info (code)
  "Get (icon description) for WMO CODE using Unicode weather glyphs."
  (let ((info (or (alist-get code quick-weather--wmo-codes-unicode)
                  '("?" "Unknown"))))
    (list (car info) (downcase (cadr info)))))

(defun quick-weather--process-day-response (status callback)
  "Process weather API response for full day and call CALLBACK with results.
STATUS is the `url-retrieve` status parameter."
  (condition-case err
      (progn
        (when (plist-get status :error)
          (error "Network error: %s" (plist-get status :error)))

        (goto-char (point-min))
        (unless (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
          (error "Invalid HTTP response"))

        (let ((status-code (string-to-number (match-string 1))))
          (unless (= status-code 200)
            (error "HTTP error %d" status-code)))

        (unless (re-search-forward "\r?\n\r?\n" nil t)
          (error "Missing HTTP headers"))

        (let* ((json (condition-case parse-err
                         (json-parse-buffer :object-type 'alist)
                       (error (error "Failed to parse JSON: %s" parse-err))))
               (current (or (alist-get 'current json)
                           (error "Missing 'current' data in response")))
               (current-temp (or (alist-get 'temperature_2m current)
                                (error "Missing current 'temperature_2m' data in response")))
               (current-weather-code (or (alist-get 'weather_code current)
                                        (error "Missing current 'weather_code' data in response")))
               (hourly (or (alist-get 'hourly json)
                          (error "Missing 'hourly' data in response")))
               (times (or (alist-get 'time hourly)
                         (error "Missing 'time' data in response")))
               (temps (or (alist-get 'temperature_2m hourly)
                         (error "Missing 'temperature_2m' data in response")))
               (precipitation-probability (or (alist-get 'precipitation_probability hourly)
                                              (error "Missing 'precipitation_probability' data in response")))
               (precipitation (or (alist-get 'precipitation hourly)
                                  (error "Missing 'precipitation' data in response")))
               (weather-codes (or (alist-get 'weather_code hourly)
                                 (error "Missing 'weather_code' data in response")))
               (hourly-results (cl-loop for i from 0 below (length times)
                               for time-string = (elt times i)
                               for decoded-time = (iso8601-parse time-string)
                               for hour = (decoded-time-hour decoded-time)
                               collect (list hour
                                            (elt temps i)
                                            (elt precipitation-probability i)
                                            (elt precipitation i)
                                            (elt weather-codes i))))
               (results (list current-temp current-weather-code hourly-results)))
          (kill-buffer)
          (funcall callback results)))
    (error
     (kill-buffer)
     (message "Weather fetch failed: %s" (error-message-string err)))))

(defun quick-weather--fetch-day (callback)
  "Fetch full day weather for calendar location and call CALLBACK with results."
  (unless (numberp calendar-latitude)
    (error "Calendar location not set.  Set `calendar-latitude' and `calendar-longitude'"))
  (unless (and (>= calendar-latitude -90) (<= calendar-latitude 90))
    (error "Invalid latitude %s.  Must be between -90 and 90" calendar-latitude))
  (unless (numberp calendar-longitude)
    (error "Calendar location not set.  Set `calendar-latitude' and `calendar-longitude'"))
  (unless (and (>= calendar-longitude -180) (<= calendar-longitude 180))
    (error "Invalid longitude %s.  Must be between -180 and 180" calendar-longitude))
  (let ((url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current=temperature_2m,weather_code&hourly=temperature_2m,precipitation_probability,precipitation,weather_code&timezone=auto&forecast_days=1"
                    calendar-latitude calendar-longitude)))
    (url-retrieve url
                  #'quick-weather--process-day-response
                  (list callback)
                  t)))

(defun quick-weather--time-window-data (data hour)
  "Extract weather data from DATA for a specific HOUR.
Returns (index weather-code) where index is the position in the data
and weather-code is the WMO code for that hour."
  (let ((index nil)
        (weather-code nil))
    (cl-loop for row in data
            for i from 0
            when (= (car row) hour)
            do (setq index i
                     weather-code (nth 4 row))
            and return t)
    (list index weather-code)))

(defun quick-weather--sparkline (values &optional highlights current-hour)
  "Generate sparkline from VALUES.
HIGHLIGHTS is an alist of (index . face) to highlight specific positions.
CURRENT-HOUR, if provided, inserts a narrow no-break space before that hour."
  (let* ((min-val (apply #'min values))
         (max-val (apply #'max values))
         (range (- max-val min-val))
         (chars "▁▂▃▄▅▆▇█")
         (thin-space "\u202F"))
    (apply #'concat
           (cl-loop for val in values
                    for i from 0
                    for normalized = (if (zerop range)
                                         ;; Use floor for all-zero data (eg 0%
                                         ;; precipitation), mid-height otherwise
                                         ;; (eg constant temperature)
                                         (if (zerop max-val) 0 4)
                                       (min 7 (floor (* 8 (/ (- val min-val) range)))))
                    for char = (substring chars normalized (1+ normalized))
                    for face = (alist-get i highlights)
                    collect (concat
                             (when (and current-hour (= i current-hour)) thin-space)
                             (if face
                                 (propertize char 'face face)
                               char))))))

(defvar-keymap quick-weather-mode-map
  :doc "Keymap for `quick-weather-mode'."
  :parent tabulated-list-mode-map
  "q" #'quit-window
  "g" #'quick-weather-day)

(easy-menu-define quick-weather-mode-menu quick-weather-mode-map
  "Menu for Quick-Weather mode."
  '("Quick-Weather"
    ["Update" quick-weather-day]
    ["Quit" quit-window]))

(define-derived-mode quick-weather-mode tabulated-list-mode "Quick-Weather"
  "Major mode for displaying weather forecasts with sparklines."
  (setq tabulated-list-format [("" 0 nil) ("" 0 nil)]
        tabulated-list-padding 0
        cursor-type nil)
  (setq-local show-help-function nil))

;; Configure display to use minimal window height
(add-to-list 'display-buffer-alist
             `(,(regexp-quote quick-weather--buffer-name)
               (display-buffer-reuse-window display-buffer-below-selected)
               (window-height . ,(lambda (window)
                                   (fit-window-to-buffer window)
                                   (window-resize window 1)))
               (body-function . ,#'select-window)))

(defun quick-weather--display-day (data)
  "Display full day weather DATA with sparklines.
DATA is a list of (current-temp current-weather-code hourly-data).
Highlights configured time markers."
  (let* ((current-temp (car data))
         (current-weather-code (cadr data))
         (hourly-data (caddr data))
         (current-weather-info (quick-weather--wmo-code-info current-weather-code))
         (current-hour (decoded-time-hour (decode-time)))
         (temps (mapcar #'cadr hourly-data))
         (precipitation-probabilities (mapcar #'caddr hourly-data))
         (temp-min (apply #'min temps))
         (temp-max (apply #'max temps))
         (precipitation-max (apply #'max precipitation-probabilities))
         (time-windows (quick-weather--get-time-windows))
         ;; Process all time markers
         (windows-data (cl-loop for window in time-windows
                               for label = (nth 0 window)
                               for hour = (nth 1 window)
                               for face = (nth 2 window)
                               for window-data = (quick-weather--time-window-data hourly-data hour)
                               for index = (car window-data)
                               for code = (cadr window-data)
                               for info = (when code (quick-weather--wmo-code-info code))
                               collect (list label hour face index info)))
         ;; Collect all highlights for sparklines
         (highlights (cl-loop for w in windows-data
                             for index = (nth 3 w)
                             for face = (nth 2 w)
                             when index
                             collect (cons index face)))
         (temp-sparkline (quick-weather--sparkline temps highlights current-hour))
         (precipitation-sparkline (quick-weather--sparkline precipitation-probabilities highlights current-hour))
         (rainy-weather-codes (cl-loop for row in hourly-data
                                       when (> (nth 2 row) 0)
                                       collect (nth 4 row)))
         (worst-weather-code (when rainy-weather-codes (apply #'max rainy-weather-codes)))
         (worst-weather-info (when worst-weather-code (quick-weather--wmo-code-info worst-weather-code)))
         (entries nil)
         (separator (propertize "───────────────────────────────────────────────────────" 'face 'shadow)))
    ;; Top padding
    (push (list 'top-space (vector "" ""))
          entries)
    ;; Current weather
    (push (list 'current (vector ""
                                 (concat (propertize "[ NOW ]" 'face 'font-lock-keyword-face)
                                         " "
                                         (propertize (format "%d°C" (round current-temp)) 'face 'bold)
                                         " - "
                                         (car current-weather-info)
                                         " "
                                         (cadr current-weather-info))))
          entries)
    ;; Blank line after NOW
    (push (list 'blank1 (vector "" ""))
          entries)
    ;; Separator
    (push (list 'sep1 (vector "" separator))
          entries)
    ;; Forecast header
    (push (list 'forecast-header (vector "" (concat (make-string 47 ?\s)
                                                    (propertize "FORECAST" 'face 'font-lock-keyword-face))))
          entries)
    ;; Temperature range
    (push (list 'temp (vector ""
                              (concat (propertize "[" 'face 'font-lock-keyword-face)
                                      " "
                                      (propertize (format "%d—%d°C" (round temp-min) (round temp-max))
                                                  'face 'bold)
                                      " "
                                      (propertize "]" 'face 'font-lock-keyword-face)
                                      " "
                                      temp-sparkline)))
          entries)
    ;; Precipitation
    (when worst-weather-info
      (push (list 'precip (vector ""
                                  (concat (propertize "[" 'face 'font-lock-keyword-face)
                                          " "
                                          (propertize (format "%3d%% %s" (round precipitation-max) (car worst-weather-info))
                                                      'face 'bold)
                                          " "
                                          (propertize "]" 'face 'font-lock-keyword-face)
                                          " "
                                          precipitation-sparkline)))
            entries))
    ;; Blank line after precipitation
    (push (list 'blank2 (vector "" ""))
          entries)
    ;; Separator before time markers
    (when (cl-some (lambda (w) (nth 4 w)) windows-data)
      (push (list 'sep2 (vector "" separator))
            entries)
      ;; Markers header
      (push (list 'markers-header (vector "" (concat (make-string 48 ?\s)
                                                      (propertize "MARKERS" 'face 'font-lock-keyword-face))))
            entries))
    ;; Time markers (lunch, commute, etc.)
    (cl-loop for window in windows-data
            for label = (nth 0 window)
            for hour = (nth 1 window)
            for face = (nth 2 window)
            for info = (nth 4 window)
            when info
            do (push (list (intern (downcase label))
                          (vector ""
                                  (concat (propertize "[ " 'face 'font-lock-keyword-face)
                                          (propertize (format "%02d %s"
                                                             hour
                                                             label)
                                                     'face face)
                                          (propertize " ]" 'face 'font-lock-keyword-face)
                                          " "
                                          (format "%s %s" (car info) (cadr info)))))
                    entries))
    ;; Bottom padding
    (push (list 'bottom-space (vector "" ""))
          entries)
    (with-current-buffer (get-buffer-create quick-weather--buffer-name)
      (quick-weather-mode)
      (setq tabulated-list-entries (nreverse entries)
            tabulated-list-use-header-line nil)
      (tabulated-list-print t)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun quick-weather-day ()
  "Show full day weather with sparklines."
  (interactive)
  (quick-weather--fetch-day
   (lambda (data)
     (quick-weather--display-day data))))

;;;###autoload
(defalias 'quick-weather #'quick-weather-day)

(provide 'quick-weather)

;;; quick-weather.el ends here
