// Access is granted if the prerson has a ticket AND is at least 18 years old or is a VIP.
// Mutating state with if statements - not pure
// This function is not pure because it mutates the state of the allowed variable.
function checkAccess(age, hasTicket, isVIP) {
    let allowed = false; // Mutable state
    
    if (age >= 18) {
        allowed = true; // Variable reassignment
    }
    
    if (isVIP) {
        allowed = true; // Variable reassignment overrides previous logic
    }
    
    if (!hasTicket) {
        allowed = false; // Ultimate variable reassignment override
    }
    
    return allowed;
}