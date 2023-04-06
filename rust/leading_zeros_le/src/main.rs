use core::ops::ControlFlow;

fn get<T>(control_flow: ControlFlow<T, T>) -> T {
    match control_flow {
        ControlFlow::Continue(x) => x,
        ControlFlow::Break(x) => x,
    }
}

fn continue_if<T>(cond: bool, value: T) -> ControlFlow<T, T> {
    match cond {
        true => ControlFlow::Continue(value),
        false => ControlFlow::Break(value),
    }
}

pub fn leading_zeros_le<const N: usize>(bytes: &[u8; N]) -> usize {
    get(bytes.iter().try_rfold(0, |count: usize, byte| {
        let all_zeros = *byte == 0;
        let byte_leading_zeros = match all_zeros {
            true => u8::BITS,
            false => byte.leading_zeros(),
        } as usize;
        continue_if(all_zeros, count + byte_leading_zeros)
    }))
}

fn main() {
    for (i, j, k) in (0..=u8::MAX)
        .flat_map(|i| (0..=u8::MAX).rev().map(move |j| (i, j)))
        .flat_map(|(i, j)| (0..=u8::MAX).map(move |k| (i, j, k)))
        .step_by(45)
    {
        println!(
            "Leading zeros of 0x{j:0>2x}{i:0>2x}{k:0>2x} = 0b{j:0>8b}{i:0>8b}{k:0>8b} = {}",
            leading_zeros_le(&[k, i, j])
        );
    }
}
