# `quick-weather`

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

> **Note:** This is a fork of [aglet/sparkweather](https://github.com/aglet/sparkweather) with a different UI design featuring a minimal brackets style and current weather display.

Quick weather forecasts with sparklines for Emacs, using data from [Open-Meteo](https://open-meteo.com).

```
[ NOW ] 18°C - ☀ mainly clear

───────────────────────────────────────────────────────
                                               FORECAST
[ 14—22°C ] ▂▂▃▃▄▅▅▆▇ ▇▇██▇▆▅▄▃▂▂▂▁▁▁
[  40% ⛆ ] ▁▁▁▁▁▁▁▁▁ ▁▁▂▃▄▅▅▅▃▂▁▁▁▁

───────────────────────────────────────────────────────
                                                MARKERS
[ 13 🍽️ ] ☀ clear sky
[ 17 🚌 ] ⛆ slight rain
```

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

Set your location using standard Emacs calendar variables:

```elisp
(setq calendar-latitude -41.3   ; Wellington, New Zealand
      calendar-longitude 174.8)
```

### Time Markers

Customize time markers to highlight specific hours in your day:

```elisp
;; Default configuration (lunch at 1pm, commute at 5pm)
(setq quick-weather-time-windows
  '(("LUNCH"   13 success)
    ("COMMUTE" 17 warning)))

;; With emojis
(setq quick-weather-time-windows
  '(("🍽️" 13 success)
    ("🚌" 17 warning)))

;; Multiple markers
(setq quick-weather-time-windows
  '(("BREAKFAST"  7 success)
    ("LUNCH"     13 success)
    ("DINNER"    20 warning)))

;; No markers
(setq quick-weather-time-windows '())
```

Each marker is a list of: `(LABEL HOUR FACE)` where HOUR is 0-23 and FACE can be `success`, `warning`, or `error`.

## Usage

`M-x quick-weather` displays today's forecast with current weather, temperature sparkline, and precipitation info. A small space in the sparklines indicates the current time, and colored blocks mark configured time markers with weather summaries.

Press `g` to refresh, `q` to close.

## Features

- **Current weather**: Temperature and weather condition
- **Minimal brackets UI**: Clean, modern design
- **Temperature range**: Daily min-max with sparkline visualization
- **Precipitation**: Probability percentage with sparkline
- **Time markers**: Customizable hour highlights with weather summaries

## License

[GPL-3.0](LICENSE)
