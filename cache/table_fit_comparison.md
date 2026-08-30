| Model | Specification / Architecture | WAIC | ΔWAIC |
|:---:|:---|:---:|:---:|
| **M1** | **Base Strict RI:** `(1|ID) + (1|Cuisine)` | 55,310.6 | **Ref** |
| **M2** | **Base Relaxed CS:** `(1|ID) + (1|Cuisine)` | 55,177.5 | **-133.1** |
| **M3** | **Base Strict RS:** `(1|ID) + (1+Base|Cuisine)` | 55,171.4 | **-139.2** |
| **M4** | **Base Relaxed RS:** `(1|ID) + (1+Base|Cuisine)` | 54,961.0 | **-348.6** |
| **M5** | **Practices Strict RI:** `(1|ID) + (1|Cuisine)` | 55,312.3 | **+1.7** |
| **M6** | **Practices Relaxed CS:** `(1|ID) + (1|Cuisine)` | 54,818.5 | **-492.1** |
| **M7** | **Practices Strict RS:** `(1|ID) + (1+Pract|Cuisine)` | 55,176.8 | **-133.8** |
| **M8** | **Practices Relaxed RS:** `(1|ID) + (1+Pract|Cuisine)` | 54,447.3 | **-863.3** |
| **M9** | **Dispositions Strict RI:** `(1|ID) + (1|Cuisine)` | 55,311.4 | **+0.8** |
| **M10** | **Dispositions Relaxed CS:** `(1|ID) + (1|Cuisine)` | 54,586.2 | **-724.4** |
| **M11** | **Dispositions Strict RS:** `(1|ID) + (1+Disp|Cuisine)` | 55,248.6 | **-62.0** |
| **M12** | **Dispositions Relaxed RS:** `(1|ID) + (1+Disp|Cuisine)` | 54,348.3 | **-962.3** |
| **M13** | **Cosmopolitan Strict RI:** `(1|ID) + (1|Cuisine)` | 55,309.8 | **-0.8** |
| **M14** | **Cosmopolitan Relaxed CS:** `(1|ID) + (1|Cuisine)` | 55,069.7 | **-240.9** |
| **M15** | **Cosmopolitan Strict RS:** `(1|ID) + (1+Cosmo|Cuisine)` | 55,296.9 | **-13.7** |
| **M16** | **Cosmopolitan Relaxed RS:** `(1|ID) + (1+Cosmo|Cuisine)` | 54,805.0 | **-505.6** |
| **M17** | **Omnibus Meta Relaxed RI:** `(1|ID) + (1|Cuisine)` | 54,311.4 | **-999.2** |
| **M18** | **Omnibus Meta Relaxed RS:** `(1|ID) + (1+All|Cuisine)` | **53,934.6** | **-1,376.0** |

