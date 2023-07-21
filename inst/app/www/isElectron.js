function isElectron() {
  
  //let namesspace_str = ns;
  //let inputname_str = "isElectron";
  //let name_in_namespace = namesspace_str.concat("-", inputname_str);
  
    //alert("Checking if it is electron");
    // Renderer process
    if (typeof window !== 'undefined' && typeof window.process === 'object' && window.process.type === 'renderer') {
        let out = true;
        Shiny.setInputValue("isElectron", out);
        return true;
    }

    // Main process
    if (typeof process !== 'undefined' && typeof process.versions === 'object' && !!process.versions.electron) {
        let out = true;
        Shiny.setInputValue("isElectron", out);
        return true;
    }

    // Detect the user agent when the `nodeIntegration` option is set to false
    if (typeof navigator === 'object' && typeof navigator.userAgent === 'string' && navigator.userAgent.indexOf('Electron') >= 0) {
        let out = true;
        Shiny.setInputValue("isElectron", out);
        return true;
    }

    let out = false;
    Shiny.setInputValue("isElectron", out);    
    return false;
}

