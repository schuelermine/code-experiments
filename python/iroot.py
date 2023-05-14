def iroot(radicand: int, degree: int = 2, *, bits: int = 1) -> int:
    if bits < 1:
        raise ValueError(f"bits parameter of iroot() must be positive, got {bits}")

    if radicand < 0:
        raise ValueError(
            f"radicand parameter of iroot() cannot be negative, got {radicand}"
        )

    base = 1 << bits
    shamt = degree * bits
    mask = (1 << shamt) - 1
    extracted = remainder = 0
    block_count = (radicand.bit_length() + shamt - 1) // shamt
    blocks = ((radicand >> (shamt * i)) & mask for i in range(block_count - 1, -1, -1))
    for alpha in blocks:
        extracted_power_shifted = (extracted**base) << shamt
        extracted_shifted = extracted << bits  # B_y
        remainder_shifted_plus_alpha = (remainder << shamt) + alpha
        min_beta = 0
        max_beta = base
        while min_beta <= max_beta:
            beta_candidate = (min_beta + max_beta) >> 1
            next_extracted_candidate = extracted_shifted + beta_candidate
            next_extracted_candidate_power_minus_extracted_power_shifted = (
                next_extracted_candidate**base - extracted_power_shifted
            )
            if (
                next_extracted_candidate_power_minus_extracted_power_shifted
                <= remainder_shifted_plus_alpha
            ):
                min_beta = beta_candidate + 1
                next_extracted = next_extracted_candidate
                next_remainder = (
                    remainder_shifted_plus_alpha
                    - next_extracted_candidate_power_minus_extracted_power_shifted
                )
            else:
                max_beta = beta_candidate - 1

        beta = max_beta
        extracted = next_extracted
        remainder = next_remainder

    return extracted
