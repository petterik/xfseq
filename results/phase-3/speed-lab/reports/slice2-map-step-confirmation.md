Source: results/phase-3/speed-lab/bench/decision-781553e0e0ca4c35e1bb28e244ebfc49b19cb5d3-slice2-20260919-confirm.json
SHA256: 58ed3f32c491df2f744cd17e05277c7ab2ae5a1773786d391dc3156bc6c938d0
Intervals are conservative ratios of JMH reported confidence endpoints; they are not a separately estimated ratio confidence interval.
| Cell | Core ops/s [CI] | Candidate ops/s [CI] | Candidate/core % [bounds] | Prior-candidate % [bounds] | Core drift % |
|---|---:|---:|---:|---:|---:|
| first / map / list / 8 / identity | 28137252.018 [25223696.398, 31050807.637] | 14517244.529 [11811626.379, 17222862.678] | -48.41 / -61.96 / -31.72 | -38.08/-50.03/-25.93 | -0.83 |
| first / map / vector / 32 / identity | 13483711.750 [11660197.424, 15307226.075] | 6349105.884 [6105235.871, 6592975.897] | -52.91 / -60.12 / -43.46 | +4.23/-0.65/+9.20 | -14.45 |
| traverse / map / list / 1000 / identity | 34388.660 [32085.690, 36691.630] | 30825.752 [25574.152, 36077.352] | -10.36 / -30.30 / +12.44 | -11.89/-27.29/+3.66 | -6.62 |
| traverse / map / vector / 1000 / identity | 225561.288 [223437.574, 227685.003] | 139536.522 [112170.189, 166902.855] | -38.14 / -50.73 / -25.30 |  |  |
| reduceUnretained / map / vector / 1000 / identity | 197939.490 [151318.364, 244560.616] | 127374.260 [96647.323, 158101.198] | -35.65 / -60.48 / +4.48 | +59.68/+17.67/+104.26 | -12.75 |
| reduceRetained / map / vector / 1000 / identity | 215400.776 [184936.637, 245864.915] | 114063.226 [84721.163, 143405.288] | -47.05 / -65.54 / -22.46 |  |  |
| traverse / map / vector / 33 / arithmetic | 4174706.035 [3719033.645, 4630378.424] | 3518926.155 [3476469.661, 3561382.649] | -15.71 / -24.92 / -4.24 | -4.02/-10.60/+3.41 | +0.11 |

Retained Java references (same confirmation profile; internal loop baselines):

| Sink | Source | Implementation | ops/s [CI] |
|---|---|---|---:|
| traverse | list | java-dechunked-object-reduced-aware-v2 | 40633.695 [40070.078, 41197.312] |
| traverse | list | java-dechunked-object-nonreducing-v2 | 35096.329 [33804.138, 36388.520] |
| reduceUnretained | list | java-dechunked-object-reduced-aware-v2 | 36904.120 [32574.279, 41233.961] |
| reduceUnretained | list | java-dechunked-object-nonreducing-v2 | 33697.403 [33284.417, 34110.389] |
| reduceRetained | list | java-dechunked-object-reduced-aware-v2 | 31961.638 [31867.064, 32056.211] |
| reduceRetained | list | java-dechunked-object-nonreducing-v2 | 27978.256 [27700.634, 28255.877] |
| traverse | vector | java-chunked-object-reduced-aware-v2 | 195957.110 [194436.391, 197477.829] |
| traverse | vector | java-chunked-object-nonreducing-v2 | 204096.206 [202547.812, 205644.600] |
| reduceUnretained | vector | java-chunked-object-reduced-aware-v2 | 86408.010 [80441.064, 92374.957] |
| reduceUnretained | vector | java-chunked-object-nonreducing-v2 | 91163.427 [90629.905, 91696.948] |
| reduceRetained | vector | java-chunked-object-reduced-aware-v2 | 82345.044 [77896.113, 86793.974] |
| reduceRetained | vector | java-chunked-object-nonreducing-v2 | 84093.579 [79575.535, 88611.623] |
