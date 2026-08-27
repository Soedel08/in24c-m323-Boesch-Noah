checkAccess :: Int -> Bool -> Bool -> Bool
checkAccess age hasTicket isVIP =
    let allowed = (age >= 18 || isVIP) && hasTicket
    in allowed