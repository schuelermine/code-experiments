// If A and B are even, return A % B.
// Else, return A + B.
function boxingSum(a, b) {
    if (a % 2 === 0 && b % 2 === 0) {
        return a % b;
    } else {
        return a + b;
    }
}

// Return boxing sum of 17 and 5 if input is truthy.
// Else, return -1.
function truthyBoxingSum(input) {
    if (input) {
        return boxingSum(17, 5);
    } else {
        return -1;
    }
}

// Compute the sum of boxing sum and remainder
function sumBoxing(a, b) {
    return boxingSum(a, b) + (a % b);
}

// Is the sum of boxing sum and remainder even?
function sumBoxingEven(a, b) {
    return sumBoxing(a, b) % 2 === 0;
}

// Chess sum is the sum of the squares of the boxing sums of a and b and b and a.
function chessSum(a, b) {
    return boxingSum(a, b) ** 2 + boxingSum(b, a) ** 2;
}

// Is the chess sum even?
// If yes, output the boxing sum.
// Else, output -1.
function isEvenChess(a, b) {
    if (chessSum(a, b) % 2 === 0) {
        return boxingSum(a, b);
    } else {
        return -1;
    }
}

// The square root of the boxing sum plus the square root of the chess sum.
function sumSquareRoot(a, b) {
    return Math.sqrt(boxingSum(a, b) + Math.sqrt(chessSum(a, b)));
}

// The sum of the boxing sum and the sumSquareRoot is the notational root.
function notationalRoot(a, b) {
    return sumBoxing(a, b) + sumSquareRoot(a, b);
}

// Compute the boxing sums of all pairs of integers in the range [a, b].
function computeBoxingSums(a, b) {
    var result = [];
    for (var i = a; i <= b; i++) {
        for (var j = a; j <= b; j++) {
            result.push(boxingSum(i, j));
        }
    }
    return result;
}

// Differences between fibonacci numbers and boxing sums of fibonacci numbers.
function fibonacciDifferences(a, b) {
    var fibonacciNumbers = computeFibonacci(a, b);
    var boxingSums = computeBoxingSums(a, b);
    var differences = [];
    for (var i = 0; i < fibonacciNumbers.length; i++) {
        differences.push(fibonacciNumbers[i] - boxingSums[i]);
    }
    return differences;
}

// Comput fibonacci numbers from a to b.
function computeFibonaccisFromTo(a, b) {
    var fibonacciNumbers = [];
    for (var i = a; i <= b; i++) {
        if (i === 1 || i === 2) {
            fibonacciNumbers.push(1);
        } else {
            fibonacciNumbers.push(fibonacciNumbers[i - 1] + fibonacciNumbers[i - 2]);
        }
    }
    return fibonacciNumbers;
}

// Get the nth fibonacci number.
function computeFibonacci(n) {
    let fibonnaccis = computeFibonaccisFromTo(1, n);
    return fibonnaccis[n - 1];
}

// Sanity check.
function test() {
    var a = 17, b = 5;
    var expected = -1;
    var actual = isEvenChess(a, b);
    assert.equal(actual, expected);
}
