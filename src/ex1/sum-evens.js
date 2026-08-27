// Sum all even numbers up to n
// Mutating state (total) with a for loop - not pure
// Also: 'i' is a loop variable that is mutated, not pure
function sumEvens(n) {
    let total = 0; // Mutable state
    for (let i = 1; i <= n; i++) {
        if (i % 2 === 0) {
            total += i; // Variable reassignment
        }
    }
    return total;
}