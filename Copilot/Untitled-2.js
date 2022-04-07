

    // Calculate the nth fibonacci number.
    function computeFibonacci(a, b) {
        var fibonacciNumbers = [];
        for (var i = a; i <= b; i++) {
            fibonacciNumbers.push(fibonacci(i));
        }
        return fibonacciNumbers;
    }


    