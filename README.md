[![status: archive](https://github.com/GIScience/badges/raw/master/status/archive.svg)](https://github.com/GIScience/badges#archive)

**2021-06-16 -- Package simfinR is discontinued to consolidate all efforts in [simfinapi](https://github.com/matthiasgomolka/simfinapi), which provides a more up to date interface. `simfinR` will be removed from CRAN soon. **

---

SimfinR is an R package for downloading financial data from the 'SimFin' (SIMmplifying FINnance) repository using its official api at <https://simfin.com/data/access/api>.  

It includes financial statements -- balance sheet, cash flow and income statement -- and adjusted daily price of stocks. The available data is comprehensive, going back to 2005 and available for quarters (Q1, Q2, Q3, Q4) and years (FY).

# Installation

```
# CRAN
install.packages('simfinR)

# github
devtools::install_github('msperlin/simfinR)
```

# Example

See [blog post](https://www.msperlin.com/blog/post/2019-11-01-new-package-simfinr/) for more details.
