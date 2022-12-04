function maxScore(cardPoints: number[], k: number): number {
    const add = (n: number, m: number) => n + m
    let max = 0
    for (let i = 0; i <= k; i++) {
        const j = k - i
        const score = cardPoints.slice(0, i).reduce(add, 0) + cardPoints.slice(-j).reduce(add, 0)
        max = Math.max(max, score)
    }
    return max
}
