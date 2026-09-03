checkSignalStatus :: [Bool] -> String
checkSignalStatus (True:False:_) = "Warnung"
checkSignalStatus (True:True:_)  = "Stabil"
checkSignalStatus _              = "Unbekannt"
