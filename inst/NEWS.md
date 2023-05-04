# ODA 2.0.0 (2023-05-03)
* Adding CTA.exe functions CTArun() and CTAparse()
* Updated batch file method for file handling and executing runs for ODArun() and CTArun()
* Updating ODAtree() ODAclean() and ODAparse() to be compatible with new workflow
* New dependancies including dplyr and tidyr

# ODA 1.4.0 (2023-03-10)

* Bug fixes with ODAclean fixed (ipsative)
* Adding CTA.exe file to installation. The software is now freely available.
* Found parsing error with ODAparse() found with multicategoral models. NOT fixed as of this update.

# ODA 1.3.0 (2022-04-06)

* Bug fixes applied for ODAparse with multiple class evaluations.
* Bug fixes applied for ODAsummary facilitating write to csv file.
* Update NOVOboot to allow selection of direction or non-directional Fisher's Exact hypothesis testing.

# ODA 1.2.1 (2022-04-05)

* Updated vignettes to reflect current functionality. 
* Modified Rscript skeleton to be compatible with current functionality.
* Updated documentation files for ODArun and ODAparse.

# ODA 1.2.0 (2022-04-01)

* Improved functionality and compatibility of ODAparse() for multiple model types.
* Added ODAsummary function to facilitate merger of reports from ODAparse().

# ODA 1.1.2 (2020-12-06)

* Added MegaODA.exe to be included in package. The software is now freely available.

# ODA 1.1.1 (2020-10-09)

* Warnings and checks added to address unsupported processors. Only Intel processors supported by MegaODA.exe currently.
* Consolidated warning messages on startup.
* Updated vignettes to render under version 1.1.0


# ODA 1.1.0 (2020-10-05)

* Initial Release to GitHub
* Prior to this it was a private package
