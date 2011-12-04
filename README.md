# AFRP using Coroutines

`coroutine-frp` is an experimental package that builds an AFRP interface on top of coroutines.

This package deviates a lot from "classical" FRP in that it is meant exclusively for fixed time-step simulations and as such there is no concept of continous time. Whereas classical FRP abstracts behaviours as functions of `time -> value` and events as a stream of `(time, event)` values, `coroutine-frp` models values that change over time as coroutines which are called once per each time-step. Events are modeled as coroutines which return a list of events that occur during the current time-step.

This package is currently in a very early design phase and all interfaces are in flux.
