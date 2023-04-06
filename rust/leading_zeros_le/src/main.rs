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

fn leading_zeros_le<const N: usize>(bytes: &[u8; N]) -> usize {
    get(bytes.iter().try_rfold(0, |count: usize, byte| {
        let all_zeros = *byte == 0;
        let byte_leading_zeros = match all_zeros {
            true => u8::BITS,
            false => byte.leading_zeros(),
        } as usize;
        continue_if(all_zeros, count + byte_leading_zeros)
    }))
}

pub fn main() {
    for i in 0..u8::MAX {
        for j in 0..u8::MAX {
            for k in 0..u8::MAX {
                println!(
                    "Leading zeros of 0x{i:0>2x}{j:0>2x}{k:0>2x} = 0b{i:0>8b}{j:0>8b}{k:0>8b} = {}",
                    leading_zeros_le(&[k, j, i])
                );
            }
        }
    }
}
