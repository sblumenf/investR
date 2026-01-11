# Configuration System

## Tier 1: golem-config.yml (Single Source of Truth)
Location: inst/golem-config.yml
Contains: All numeric thresholds, worker counts, strategy defaults

## Tier 2: Strategy Config Objects
Pattern: utils_*_config.R → STRATEGY_CONFIG list
Example: ARISTOCRATS_CONFIG$strike_threshold_pct

## Tier 3: Generic Getter
```r
get_golem_config_value(section, key, fallback)
```

## Usage Rules
- NEVER hard-code magic numbers
- Always use config objects or get_golem_config_value()
- Fallbacks prevent crashes if config missing
