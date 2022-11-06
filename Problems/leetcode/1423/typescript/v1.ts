enum StepDir {
    Left,
    Right
}
const stepDirs: StepDir[] = [StepDir.Left, StepDir.Right]

function maxScore(cardPoints: number[], k: number): number {
    if (k <= 0 || cardPoints.length == 0) {
        return 0
    }
    const results: number[] = []
    for (const dir of stepDirs) {
        const {score, rest} = step(cardPoints, dir)
        results.push(score + maxScore(rest, k - 1))
    }
    return Math.max(...results)
}

function step(cardPoints: number[], dir: StepDir): {score: number, rest: number[]} {
    switch (dir) {
        case StepDir.Left:
            return {
                score: cardPoints.at(0),
                rest: cardPoints.slice(1)
            }
        case StepDir.Right:
            return {
                score: cardPoints.at(-1),
                rest: cardPoints.slice(0,-1)
            }
    }
}
