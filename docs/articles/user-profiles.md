# User profiles in smart charging

This article is based on the [EV user profile
concept](https://resourcefully-dev.github.io/evprof/articles/evprof.html)
from package `evprof`, and how the extra-knowledge that a classification
of EV users into user profiles could help in the smart charging
ecosystem.

A **user profile** is defined by a pattern in the connection times of EV
users, so in most of cases, there are EV user profiles with more
flexibility potential than others. A Charging Point Operator (CPO) can
take advantage of this knowledge and use this classification to set
priorities when scheduling EV charging sessions.

Shifting only the sessions of specific EV user profiles can increase the
efficiency of the system since the forecast of the flexibility potential
form EV users is more reliable. The lower number of EV sessions that
have to be re-scheduled, the better the solution, for both the CPO and
the final EV user.

## EV charging sessions simulation

First of all, we can simulate EV sessions using the `evsim` package,
which provides the function
[`evsim::get_custom_ev_model`](https://resourcefully-dev.github.io/evsim/reference/get_custom_ev_model.html)
to create a custom EV model to later simulate EV sessions with
[`evsim::simulate_sessions()`](https://resourcefully-dev.github.io/evsim/reference/simulate_sessions.html)
function. We can create an EV model with custom time-cycles and user
profiles. In this case, we will consider two different EV user profiles:

- **Worktime**: present during working days. Low-variability in
  connection times. High flexibility potential.
- **Visit**: present the whole week but with different behaviour on
  weekends. High variability in connection times. Low flexibility
  potential.

``` r

# For workdays time cycle
workdays_parameters <- dplyr::tibble(
  profile = c("Worktime", "Visit"),
  ratio = c(50, 50),
  start_mean = c(9, 11),
  start_sd = c(1, 4),
  duration_mean = c(8, 4),
  duration_sd = c(0.5, 2),
  energy_mean = c(15, 6),
  energy_sd = c(4, 3)
)

# For weekends time cycle
weekends_parameters <- dplyr::tibble(
  profile = "Visit",
  ratio = 100,
  start_mean = 12,
  start_sd = 4,
  duration_mean = 3,
  duration_sd = 2,
  energy_mean = 4,
  energy_sd = 4
)

parameters_lst <- list(workdays_parameters, weekends_parameters)

# Get the whole model
ev_model <- get_custom_ev_model(
  names = c("Workdays", "Weekends"),
  months_lst = list(1:12, 1:12),
  wdays_lst = list(1:5, 6:7),
  parameters_lst = parameters_lst,
  connection_log = FALSE,
  energy_log = FALSE,
  data_tz = "Europe/Amsterdam"
)
```

Once we have our own model, we can simulate EV sessions for three
different days as example:

``` r

set.seed(1234)
ev_sessions <- simulate_sessions(
  evmodel = ev_model, 
  sessions_day = tibble(time_cycle = c("Workdays", "Weekends"), n_sessions = c(10, 10)),
  user_profiles = NULL,
  charging_powers = tibble(power = 3.7, ratio = 1), 
  dates = seq.Date(from = dmy("05-08-2024"), to = dmy("05-08-2024")+days(2), by = "day"), 
  resolution = 15
)
```

    ## # A tibble: 21 × 11
    ##    Session Timecycle Profile  ConnectionStartDateTime ConnectionEndDateTime
    ##    <chr>   <chr>     <chr>    <dttm>                  <dttm>               
    ##  1 S1      Workdays  Worktime 2024-08-05 08:45:00     2024-08-05 16:58:00  
    ##  2 S2      Workdays  Worktime 2024-08-05 09:15:00     2024-08-05 17:04:00  
    ##  3 S3      Workdays  Worktime 2024-08-05 09:30:00     2024-08-05 17:37:00  
    ##  4 S4      Workdays  Visit    2024-08-05 09:45:00     2024-08-05 12:29:00  
    ##  5 S5      Workdays  Worktime 2024-08-05 09:45:00     2024-08-05 17:22:00  
    ##  6 S6      Workdays  Visit    2024-08-05 10:00:00     2024-08-05 14:29:00  
    ##  7 S7      Workdays  Visit    2024-08-05 10:45:00     2024-08-05 17:46:00  
    ##  8 S8      Workdays  Visit    2024-08-05 12:45:00     2024-08-05 18:29:00  
    ##  9 S9      Workdays  Visit    2024-08-05 14:45:00     2024-08-05 18:59:00  
    ## 10 S10     Workdays  Visit    2024-08-05 20:00:00     2024-08-05 23:02:00  
    ## # ℹ 11 more rows
    ## # ℹ 6 more variables: ChargingStartDateTime <dttm>, ChargingEndDateTime <dttm>,
    ## #   Power <dbl>, Energy <dbl>, ConnectionHours <dbl>, ChargingHours <dbl>

Finally we can calculate the time-series power demand from each EV with
function
[`evsim::get_demand()`](https://resourcefully-dev.github.io/evsim/reference/get_demand.html),
using parameter `by="Sessions"`:

``` r

ev_demand <- ev_sessions %>% 
  get_demand(by = "Profile") 
```

## Smart charging to solve grid congestion

Imagine that we have to charge these EVs in an installation that has a
**maximum grid connection of 12 kW**:

``` r

grid_capacity <- 12
```

Since our peak goes above 12kW, we need to use smart charging to allow
the EV users to charge under the grid capacity. In our case, we decide
to only use the flexibility from the **Worktime user profile**, since it
has a clear flexibility potential compared to the Visit user profile.

In order to define a **grid capacity** in the `smart_charing()`
function, a column with the same name than the EV user profile in the
`sessions` parameter (in this example “Worktime”) must be found in the
`opt_data` parameter. Therefore, a `"Worktime"` column is added to the
`opt_data` object as a setpoint for the EV user profile “Worktime”, and
it is defined as the **difference between the grid capacity and the rest
of power demand** (in this case the “Visit” demand):

``` r

opt_data <- tibble(
  datetime = ev_demand$datetime, # Same date-time sequence than the demand
  Worktime = grid_capacity - ev_demand$Visit
)
```

``` r

opt_data %>% 
  timefully::plot_ts(ylab = "Power demand (kW)", stackedGraph = T, fillGraph = T, legend_width = 200) %>% 
  dySeries("Worktime", "Worktime free capacity", color = "navy", strokeWidth = 2, strokePattern = "dashed") %>% 
  dyLimit(grid_capacity, "Grid capacity", color = "red")
```

Considering optimization windows of 24 hours from 6:00AM to 6:00AM, and
`"curtail"` as a smart charging method:

``` r

sc_results <- ev_sessions %>% 
  smart_charging(
    opt_data = opt_data, 
    opt_objective = "none",
    method = "curtail",
    window_days = 1, 
    window_start_hour = 6,
    responsive = list(Workdays = list(Worktime = 1))
  )
```

``` r

ev_demand_opt <- sc_results$sessions %>% 
  get_demand(by = "Profile") 
```

We can see that the Worktime power demand has been adapted to the
non-flexbile users, i.e. Visit user profile, in order to not surpass the
capacity limit of 12 kW.

Moreover, we can define which percentage of every user profile is
responsive to the smart charging signals with the parameter `responsive`
from
[`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md)
function. For example, we can set that, during working days, **60% of
the Worktime users** and **20% of the Visit users** accept to
participate to the demand-response program.

To do that, since now we consider different user profiles for an
aggregated grid capacity, the grid capacity limit has to be configured
in the `opt_data` as `"grid_capacity"` name:

``` r

opt_data <- tibble(
  datetime = ev_demand$datetime, # Same date-time sequence than the demand
  grid_capacity
)
```

We configure the **responsiveness** of every user profile with the
`responsive` parameter. **It’s always advised that the responsiveness
values are sorted from highest to lowest**:

``` r

sc_results <- ev_sessions %>% 
  smart_charging(
    opt_data = opt_data, 
    opt_objective = "none",
    method = "curtail",
    window_days = 1, 
    window_start_hour = 6,
    responsive = list(
      Workdays = list(Worktime = 0.6, Visit = 0.2)
    )
  )
```

``` r

plot(sc_results, ev_sessions)
```

From the plot we see that the grid capacity limit of 12 kW can’t be
always respected. Also, the setpoint in this plot doesn’t make a lot of
sense because represents the power limitation of the EV demand, but our
setpoint is avoiding a demand higher than 12 kW. The setpoint line in
the plot can be removed using the parameter `show_setpoint = FALSE`.

Is it for the lack of flexibility of the responsive sessions? Or maybe
there aren’t enough responsive sessions? These questions can be answered
by plotting the smart charging results **by flexbility type**:

``` r

plot(sc_results, ev_sessions, by = "FlexType", show_setpoint = FALSE) %>% 
  dyLimit(grid_capacity, "Grid capacity", color = "red")
```

The results show that during the hours where the grid capacity is
surpassed the demand corresponds to **Exploited** and **Not responsive**
sessions. Therefore, **more responsive sessions are needed.**

We can also plot the smart charging results **by user profile** to see
that the demand causing the peaks over the grid capacity comes from
**non-responsive Visit sessions** from 10:00 to 11:00:

``` r

plot(sc_results, ev_sessions, by = "Profile", show_setpoint = FALSE) %>% 
  dyLimit(grid_capacity, "Grid capacity", color = "red")
```

For a deeper analysis we can use function
[`summarise_smart_charging_sessions()`](https://resourcefully-dev.github.io/flextools/reference/summarise_smart_charging_sessions.md),
which shows the percentage of Considered, Responsive, Flexible and
Exploited sessions:

``` r

summarise_smart_charging_sessions(sc_results) %>% 
  knitr::kable()
```

| timecycle | profile  | group      | subgroup       | n_sessions | pct |
|:----------|:---------|:-----------|:---------------|-----------:|----:|
| Workdays  | Visit    | Total      | Considered     |         11 |  92 |
| Workdays  | Visit    | Total      | Not considered |          1 |   8 |
| Workdays  | Visit    | Considered | Responsive     |          2 |  18 |
| Workdays  | Visit    | Considered | Non responsive |          9 |  82 |
| Workdays  | Visit    | Responsive | Flexible       |          2 | 100 |
| Workdays  | Visit    | Flexible   | Exploited      |          1 |  50 |
| Workdays  | Visit    | Flexible   | Not exploited  |          1 |  50 |
| Workdays  | Worktime | Total      | Considered     |          9 | 100 |
| Workdays  | Worktime | Considered | Responsive     |          5 |  56 |
| Workdays  | Worktime | Considered | Non responsive |          4 |  44 |
| Workdays  | Worktime | Responsive | Flexible       |          5 | 100 |
| Workdays  | Worktime | Flexible   | Exploited      |          5 | 100 |

We can extract the following conclusions from this table:

- The Responsive percentages correspond to the configured 60% for
  Worktime and 20% for Visit in the `responsive` parameter (the
  difference is due to the small amount of sessions in the data)
- None of the Flexible Visit sessions are Exploited, so they charge in
  times where flexibility is not needed
- All Worktime Flexible sessions are Exploited, trying to match the
  setpoint and adapt to the Visit demand

If we increase the participation of Worktime users up to 70% then we
accomplish with our grid capacity again:

``` r

sc_results <- ev_sessions %>% 
  smart_charging(
    opt_data = opt_data, 
    opt_objective = "none",
    method = "curtail",
    window_days = 1, 
    window_start_hour = 6,
    responsive = list(
      Workdays = list(Worktime = 0.7, Visit = 0.2)
    )
  )
```

``` r

plot(sc_results, ev_sessions, show_setpoint = FALSE) %>% 
  dyLimit(grid_capacity, "Grid capacity", color = "red")
```

Then we can say that **we need at least 70% of the Worktime users to
participate** in the smart charging program if we want to respect our
grid constraints.
