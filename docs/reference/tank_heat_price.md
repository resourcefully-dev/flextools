# Price of a kWh of heat in a window's objective

The highest marginal value a kW of electric load can take in the
window's objective, divided by the lowest positive COP (when below 1):
no kWh of compressor heat is worth more than it. For `"cost"` the
marginal value is the highest absolute import or (clipped) export price;
for `"grid"` the derivative of the squared net flow at its largest,
`2 * (max|L - G| + Pc + Pb)`; a weight `w` mixes both as the objective
does, and a ramping penalty adds its largest derivative
`4 * lambda * Pc`.

## Usage

``` r
tank_heat_price(L, G, cop, Pc, Pb, objective, lambda, price, scale)
```

## Arguments

- L:

  numeric vector, static load (kW)

- G:

  numeric vector, production (kW)

- cop:

  numeric vector, coefficient of performance

- Pc:

  numeric, compressor cap (kW)

- Pb:

  numeric, backup heater cap (kW); 0 means no backup heater

- objective:

  `"grid"`, `"cost"` or a number strictly between 0 and 1

- lambda:

  numeric, ramping penalty weight

- price:

  numeric, highest absolute electricity price of the window

- scale:

  numeric, the weighted objective's price scale

## Value

positive number, in the objective's units per kW of heat and slot
