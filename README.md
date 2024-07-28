# iowa <img src='man/figures/logo.png' align="right" height="138.5" />

The **iowa** implements modular reinforcement learning models of the Iowa
gambling task, using RStan as a backend.

The package allows both the simulation and fitting of models constructed by
mixing and matching various utility, updating, and temperature functions; and is
designed to be relatively extensible by allowing users to implement custom
model components and deck structures, which may then be used relatively seamlessly 
with the functions already implemented in the package.

In addition to simulating the performance of custom models, **iowa** also allows
model fitting either by maximum likelihood / maximum a posteriori estimation,
or by full posterior sampling. Currently, only single subject fitting is
supported, but implementing support for full hierarchical Bayesian fitting
is a strong priority.
