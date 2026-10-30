# deeptox 1.1.0
* remove unnecessary "deeptox:::"
* Escape uploaded filenames before rendering validation errors to prevent code injection
* switch to json for input and output; reconstruction of ggplots on load of json necessary

# deeptox 1.0.1
Add info text for units box and parameter fields

* add explanatory text for the units info modal
* include unit help content for length, time, and concentration
* update package version/news metadata
* minor cleanup in documentation formatting

# deeptox 1.0.0
* cleanup for v1

# deeptox 0.0.5
* replace invalid characters of the profile names that cause a crash during rendering of the markdown document
* increase allowed file size to 70 MB so that large TOXSWA out files can be uploaded
* included a vignette that describes how to create default profile files from TOXSWA out-files

# deeptox 0.0.4
* Support for TOXSWA 4 exposure profiles import included
* All substances of a TOXSWA profile are imported as seperate profiles

# deeptox 0.0.3
* Improved help and interactivity with user

# deeptox 0.0.2
* bugfixes
  * prevent crashes when ode solver cannot find a solution with excessive damage

# deeptox 0.0.1
* fully working version deployed for testing

# deeptox 0.0.0.9000
* Added a `NEWS.md` file to track changes to the package.
