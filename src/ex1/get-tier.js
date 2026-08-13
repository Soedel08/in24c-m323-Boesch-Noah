// Get tier (= Einstufung) from score
// Mutating state (tier) with if statements - not pure
function getTier(score) {
    let tier = 'C'; // Default mutable char state
    
    if (score >= 90) {
        tier = 'A'; // Variable reassignment
    } else if (score >= 75) {
        tier = 'B'; // Variable reassignment
    }
    
    return tier;
}