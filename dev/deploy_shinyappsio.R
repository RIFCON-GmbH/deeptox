rsconnect::deployApp('.',
                     appName = "DeEP",
                     appTitle = "DeEP",
                     account = "rifcon",
                     appFiles = grep(".*\\.so$|.*\\.o$|.*\\.dll$|dev/",
                                   rsconnect::listBundleFiles(".")$contents,
                                   value=T,
                                   invert=T),
                     forceUpdate = TRUE,
                     launch.browser=FALSE
)
