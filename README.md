# `quick-weather`

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

> **Note:** This is a fork of [aglet/sparkweather](https://github.com/aglet/sparkweather) with a different UI design featuring a minimal brackets style and current weather display.

Quick weather forecasts with sparklines for Emacs, using data from [Open-Meteo](https://open-meteo.com).

## Installation

```elisp
(use-package quick-weather
  :vc (:url "https://github.com/tanrax/quick-weather.el")
  :after calendar
  :config
  (setq calendar-latitude 39.46975   ; Valencia, Spain
        calendar-longitude -0.37739))
```

## Configuration

Quick-weather uses the standard Emacs calendar location variables. If you haven't set these already:

```elisp
(setq calendar-latitude -41.3   ; Wellington, New Zealand
      calendar-longitude 174.8)
```

### Customize Time Windows

You can customize the time windows for lunch and commute to match your schedule:

```elisp
(use-package quick-weather
  :vc (:url "https://github.com/tanrax/quick-weather.el")
  :after calendar
  :config
  (setq calendar-latitude 39.46975
        calendar-longitude -0.37739)

  ;; Customize lunch hours (default: 12-14)
  (setq quick-weather-lunch-start-hour 13    ; Start at 1 PM
        quick-weather-lunch-end-hour 15)     ; End at 3 PM

  ;; Customize commute hours (default: 17-19)
  (setq quick-weather-commute-start-hour 18  ; Start at 6 PM
        quick-weather-commute-end-hour 20))  ; End at 8 PM
```

You can also use **emoji labels** for a more visual experience:

```elisp
(use-package quick-weather
  :vc (:url "https://github.com/tanrax/quick-weather.el")
  :after calendar
  :config
  (setq calendar-latitude 39.46975
        calendar-longitude -0.37739)
  ;; Use emojis for labels!
  (setq quick-weather-time-windows
    '(("🍽️" 13 14 success)      ; Lunch
      ("🚌" 17 18 warning))))    ; Commute
```

### Custom Time Windows

You can define custom time windows using an alist:

```elisp
;; Basic configuration
(setq quick-weather-time-windows
  '(("LUNCH"    12 14 success)   ; Label, start-hour, end-hour, face-color
    ("COMMUTE"  17 19 warning)))

;; Customize labels and times
(setq quick-weather-time-windows
  '(("BREAK"    13 15 success)   ; Remote workers
    ("HOME"     18 20 warning)))

;; Use your language
(setq quick-weather-time-windows
  '(("COMIDA"   14 16 success)   ; Spanish lunch
    ("VUELTA"   19 21 warning))) ; Spanish commute

;; Add multiple time windows
(setq quick-weather-time-windows
  '(("BREAKFAST"  7  9 success)
    ("LUNCH"     12 14 success)
    ("DINNER"    20 22 warning)
    ("BEDTIME"   23 24 error)))
```

**Use cases for custom time windows:**
- **Remote workers**: "BREAK" / "WALK" / "EXERCISE"
- **Students**: "CLASS" / "STUDY" / "LIBRARY"
- **Shift workers**: "START" / "BREAK" / "END"
- **Different schedules**: "BREAKFAST" / "LUNCH" / "DINNER"
- **Multiple languages**: COMIDA/VUELTA (ES), DÉJEUNER/RETOUR (FR), MITTAGESSEN/PENDELN (DE)
- **Multiple windows**: Add as many as you need!

For other configuration, `M-x customize-group` / `quick-weather`.

## Usage

`M-x quick-weather` displays today's forecast with current weather, temperature sparkline, and precipitation info. A small space in the sparklines indicates the time of the forecast, and coloured blocks mark lunch/evening commute windows, with summaries.

Press `g` to refresh, `q` to close.

## Features

- **Current weather**: Temperature and weather condition at the top
- **Minimal brackets UI**: Clean, modern design with colored brackets
- **Temperature range**: Daily min-max with sparkline visualization
- **Precipitation**: Probability percentage with sparkline
- **Time windows**: Lunch and commute hour highlights with weather summaries

## License

[GPL-3.0](LICENSE)
