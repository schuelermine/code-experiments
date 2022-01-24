function addOperatorsNaïve(num: string, target: number)/*: string[]*/ {
    const ops: [string, (n: number, m: number) => number][] = [
        ["+", (n, m) => n + m],
        ["-", (n, m) => n - m],
        ["*", (n, m) => n * m]
    ]
    const digits = num.split("").map(d => Number.parseInt(d))
    var expressions: {value: number, expression: string}[] = [{value: digits[0], expression: digits[0].toString()}]
    for (var i = 1; i < digits.length; i++) {
        expressions = expressions.flatMap(d => {
            var result: (typeof d)[] = []
            for (var [s, f] of ops) {
                result.push({
                    value: f(d.value, digits[i]),
                    expression: d.expression + s + digits[i].toString()
                })
            }
            return result
        })
    }
    return expressions/*.filter(({value}) => value === target).map(({expression}) => expression)*/
}