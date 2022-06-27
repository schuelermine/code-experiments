function maxScore(cardPoints: number[], k: number): number {
    const add = (n: number, m: number) => n + m
    let max = 0
    let current = cardPoints.slice(-k).reduce(add, 0)
    max = current
    for (var i = 0; i <= k - 1; i++) {
        current = current + cardPoints.at(i) - cardPoints.at(-k + i)
        max = Math.max(max, current)
    }
    return max
}
