# crew.cluster: crew launcher plugins for traditional high-performance computing clusters

In computationally demanding analysis projects, statisticians and data
scientists asynchronously deploy long-running tasks to distributed
systems, ranging from traditional clusters to cloud services. The
`crew.cluster` package extends the
[`mirai`](https://github.com/r-lib/mirai)-powered
[`crew`](https://wlandau.github.io) package with worker launcher plugins
for traditional high-performance computing systems. Inspiration also
comes from packages [`mirai`](https://github.com/r-lib/mirai),
[`future`](https://future.futureverse.org/),
[`rrq`](https://mrc-ide.github.io/rrq/),
[`clustermq`](https://mschubert.github.io/clustermq/), and
[`batchtools`](https://batchtools.mlr-org.com).

## Attribution

The template files at
<https://github.com/mschubert/clustermq/tree/master/inst> informed the
development of the `crew` launcher plugins in `crew.cluster`, and we
would like to thank Michael Schubert for developing `clustermq` and
releasing it under the permissive Apache License 2.0. See the `NOTICE`
and `README.md` files in the `crew.cluster` source code for additional
attribution.
