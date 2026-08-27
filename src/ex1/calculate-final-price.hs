-- Berechnet den Endpreis aus Basispreis und Mitgliedschaftsstatus
-- Pure Funktion: es gibt keine Mutation, jede Zwischenstufe ist ein neuer, unveränderlicher Wert
calculateFinalPrice :: Double -> Bool -> Double
calculateFinalPrice basePrice isMember =
    let priceAfterDiscount = if isMember then basePrice * 0.90 else basePrice
        priceAfterFee      = priceAfterDiscount + 5.0
        priceAfterTax      = priceAfterFee * 1.08
    in priceAfterTax
