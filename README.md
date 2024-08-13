# iowa <img src='man/figures/logo.png' align="right" height="138.5" />

The **iowa** package implements modular
reinforcement learning models of the Iowa gambling task. Model components are
implemented in Stan, and are compiled into cmdstan models which are then made
available internally to other packages.

The package implements the simulation and fitting of models constructed by
mixing and matching various utility, updating, and temperature functions; and is
designed to be relatively extensible by allowing users to implement custom
model components.

In addition to simulating the performance of custom models, **iowa** also allows
model fitting either by maximum likelihood / maximum a posteriori estimation,
or by full posterior sampling. Currently, only single subject fitting is supported, 
but support for full hierarchical Bayesian fitting is a strong priority.

### Installation

Models in **iowa** are pre-compiled using cmdstanr, which must be installed
alongside cmdstan. Both can be installed within R using

```
remotes::install_github("stan-dev/cmdstanr")
cmdstanr::install_cmdstan()
```

**iowa** can then be installed directly from its repository:

```
devtools::install_github('areshenk-rpackages/iowa', type = 'source')
```
