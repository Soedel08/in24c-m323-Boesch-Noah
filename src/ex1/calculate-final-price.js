//
// Mutating state with if statements - not pure
function calculateFinalPrice(basePrice, isMember) {
    let price = basePrice; // Mutable state
    
    // Apply 10% member discount if applicable
    if (isMember) {
        price = price * 0.90; // Variable reassignment
    }
    
    // Apply flat $5 processing fee
    price = price + 5.0; // Variable reassignment
    
    // Apply 8% sales tax
    price = price * 1.08; // Variable reassignment
    
    return price;
}