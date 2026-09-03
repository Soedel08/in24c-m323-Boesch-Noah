firstUser :: [(Int, String, String, String)] -> (Int, String)
firstUser ((userId, _, _, username):_) = (userId, username)
firstUser []                           = error "User table is empty"
