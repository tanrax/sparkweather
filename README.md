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
