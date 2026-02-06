from __future__ import annotations

from typing import Literal, Union
import numpy as np


def detect_asvotes(
    signal,
    lowwl: int = 5,
    highwl: Union[str, int] = "auto",
    mad_k: float = 3.0,
    direction: Literal["positive", "both", "negative"] = "positive",
):
    """
    Adaptive slope-vote detector (block-based, offline).
    """
    y = np.asarray(signal, dtype=float)
    n = y.size
    if n < 3:
        return np.zeros(n, dtype=float)

    if direction not in ("positive", "both", "negative"):
        raise ValueError("direction must be 'positive', 'both', or 'negative'")

    if highwl == "auto":
        highwl_val = max(5, n // 3)
    else:
        highwl_val = int(highwl)

    lowwl_val = int(lowwl)
    if lowwl_val < 2:
        lowwl_val = 2
    if highwl_val < lowwl_val:
        highwl_val = lowwl_val

    wls = np.arange(lowwl_val, highwl_val + 1, dtype=int)
    votes = np.zeros(n, dtype=float)
    n_wls = wls.size

    def robust_z(x):
        med = np.nanmedian(x)
        mad = np.nanmedian(np.abs(x - med))
        if not np.isfinite(mad) or mad == 0:
            mad = 1e-8
        return (x - med) / mad

    for w in wls:
        rem = n % w
        if rem == 0:
            start = 0
            end = n
        else:
            pad = rem // 2
            start = pad
            end = n - (rem - pad)

        if end - start < w:
            continue

        idx = np.arange(start, end, dtype=int)
        B = idx.size // w
        if B == 0:
            continue

        y_block = y[idx][: w * B].reshape((w, B), order="F")

        t = np.arange(1, w + 1, dtype=float)
        t_centered = t - (w + 1) / 2.0
        denom = np.sum(t_centered * t_centered)

        has_na = np.any(~np.isfinite(y_block), axis=0)
        num = t_centered @ y_block
        slopes = num / denom
        slopes[has_na] = np.nan

        z = robust_z(slopes)
        if np.all(~np.isfinite(z)):
            continue

        vote_sign = np.zeros(B, dtype=int)
        if direction == "positive":
            vote_sign[z > mad_k] = +1
        elif direction == "negative":
            vote_sign[z < -mad_k] = +1
        else:
            vote_sign[z > mad_k] = +1
            vote_sign[z < -mad_k] = -1

        nz_blocks = np.nonzero(vote_sign != 0)[0]
        if nz_blocks.size:
            for b in nz_blocks:
                block_slice = slice(b * w, (b + 1) * w)
                votes[idx[block_slice]] += vote_sign[b]

    if n_wls > 0:
        votes = votes / n_wls
    return votes
