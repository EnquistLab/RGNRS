# Sources of the shipped history tables

These tables are the *curated inputs* of the time-versioned division reference, written
by `data-raw/history_crosswalk.R`. They are combined at build time with the user's own
copy of CShapes 2.0 by `gnrs_history_assemble()`.

**Nothing derived from CShapes 2.0 ships with this package.** CShapes is licensed
CC BY-NC-SA 4.0 and this package is MIT, so every CShapes-derived part of the history
component (periods of uncurated state codes, synthetic `CSH<gwcode>` entities and their
names, the coverage check, and geometry-derived lineage) is computed in the user's cache
from CShapes obtained under its own licence. `CSH<gwcode>` keys that appear below are
identifiers only, for entities whose names and dates were curated by hand.

| File | Content | Source and licence |
|---|---|---|
| `history_current_entities.csv` | snapshot of the service's current countries | GNRS country table; names and ids from GeoNames (CC BY 4.0) |
| `history_curated_entities.csv` | historical entities and their dates | curated for GNRS (MIT) |
| `history_curated_periods.csv` | assignments of CShapes state codes to entities over periods | curated for GNRS (MIT) |
| `history_curated_lineage.csv` | predecessor -> successor links | curated for GNRS (MIT); successor lists from Unicode CLDR `territoryAlias` (Unicode License v3) |
| `history_curated_names.csv` | curated names and former names | curated for GNRS (MIT) |
| `history_geonames_names.csv` | names of historical political entities (feature code PCLH) | GeoNames, https://www.geonames.org/ (CC BY 4.0) |
| `history_codes.csv` | former country codes and the entity each denotes | ISO 3166-3 via Debian iso-codes (LGPL-2.1+); mapping to entities curated for GNRS |

When the history component is built, `GNRS_local_citations()` also cites CShapes 2.0:
Schvitz G., Girardin L., Ruegger S., Weidmann N. B., Cederman L.-E. & Gleditsch K. S.
(2022). Mapping the International System, 1886-2019: The CShapes 2.0 Dataset. *Journal of
Conflict Resolution* 66(1): 144-161. https://doi.org/10.1177/00220027211013563
