convertDate = function(s) {
    result = as.POSIXct(s, format = "%d %B %Y at %H:%M:%S")
    return(result)
}