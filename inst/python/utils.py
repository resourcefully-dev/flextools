# Dependencies
# import matplotlib.pyplot as plt
import numpy as np
from pandas import DataFrame, Series, to_datetime, merge
from dateutil.parser import parse
from datetime import datetime, timedelta
from pytz import timezone
import copy
from cvxopt import solvers as cvx
from cvxopt import matrix as mat

# Options configuration
# pd.options.mode.chained_assignment = 'warn'
cvx.options['show_progress'] = False
cvx.options['maxiters'] = 100 # Normally we have around 20 iterations
cvx.options['abstol'] = 1e-7
cvx.options['reltol'] = 1e-6
cvx.options['feastol'] = 1e-7
cvx.options['refinement'] = 1 # To avoid ValueError: domain error

# Sessions 
## Obtain schedule
def get_schedule(sessions_norm, window):
  sessions_norm = sessions_norm.set_index('Session')
  schedule = DataFrame(
    np.zeros((window[1] - window[0], len(sessions_norm.index))), 
    index = range(window[0], window[1]), 
    columns = sessions_norm.index
  )
  for si in sessions_norm.index:
    s = sessions_norm.loc[si]
    schedule.loc[schedule.index >= s['chs'], si] = s['p']
    schedule.loc[schedule.index >= s['che'], si] = 0
  return schedule
  

## Obtain aggregated demand
def get_demand(sessions_norm, window, aggregated = False, stacked = False):

  profiles_names = sessions_norm.prof.unique()
  demand_df = DataFrame(
    np.zeros((window[1] - window[0], len(profiles_names))), 
    index = range(window[0], window[1]), 
    columns = profiles_names
  )
  
  for profile in profiles_names:
    profile_sessions = sessions_norm[sessions_norm.prof == profile]
    for s_id in profile_sessions.index:
      s = profile_sessions.loc[s_id]
      demand_df.loc[(demand_df.index >= s['chs']) & (demand_df.index < s['che']), [profile]] += s['p']
      
  if aggregated:
    return DataFrame(data = demand_df.sum(axis=1), columns = ["demand"])
  else:
    return demand_df



################################## CVX OPT ########################################

def minimize_grid_flow_window(w, G, LF, LS = None, direction = 'forward', time_horizon = None, up_to_G = True, grid_cap = None):
  # Parameters check
  if (w > 1):
    print("Error: Objectives weights must sum 1.")
    return None
    
  if LS is None: LS = np.array([0.0]*len(LF))
  
  if time_horizon is None: time_horizon = len(LF)

  if not (isinstance(G, np.ndarray)): G = np.array(G)
  if not (isinstance(LS, np.ndarray)): LS = np.array(LS)
  if not (isinstance(LF, np.ndarray)): LF = np.array(LF)

  if not (len(G) == len(LS) == len(LF)):
    print("Error: S, L and V must have same length.")
    return None
    
  if ((direction != 'forward') and (direction != 'backward')):
    print("Error: Direction must be 'forward' or 'backward'")
    return None
    
  # If no optimization applied (w=0) and there's no grid constraint:
  if ((w == 0) & (grid_cap is not None)):
    if (grid_cap >= max(abs(G-LF-LS))): return LF
    
  # LF must remain static when there is G
  LF_static = np.zeros(len(LF)) # LF static vector
  static_idx = (G >= (LS + LF))
  semi_static_idx = (G < (LS + LF)) & (LF > 0) & (LS < G) # LF static and shiftable for the same timeslot
  # If all demand is lower than solar then skip optimization
  if all(static_idx): 
    return LF
  if any(static_idx): 
    LF_static[static_idx] = LF[static_idx]
  if any(semi_static_idx):
    LF_rest = G[semi_static_idx] - LS[semi_static_idx]
    LF_static[semi_static_idx] = LF_rest
  # Update LF and LS
  LS += LF_static
  LF -= LF_static

  # Optimization parameters
  time_slots = len(LF)
  E = sum(LF)
  identityMat = np.identity(time_slots)
  cumsumMat_forewards = np.tril(np.full((time_slots, time_slots), 1)) # Lower triangle matrix
  cumsumMat_backwards = np.triu(np.full((time_slots, time_slots), 1)) # Upper triangle matrix
  if (direction == 'forward'):
    cumsumMat = cumsumMat_forewards
    horizonMat = cumsumMat*np.triu(np.full((time_slots, time_slots), 1), - time_horizon)#(time_horizon - 1))
  else:
    cumsumMat = cumsumMat_backwards
    horizonMat = cumsumMat*np.tril(np.full((time_slots, time_slots), 1), time_horizon)#(time_horizon - 1))
  
  
  # Upper limit for optimal LF
  if grid_cap is None: grid_cap = max(abs(G-LF-LS))*10 # 10 times BAU maximum grid flow
  grid_limit = np.array([grid_cap]*time_slots)
  if up_to_G: grid_limit[G > (LS + LF)] = 0
  LFmax = grid_limit + G - LS
  

  # Objective function terms
  P = mat(2*identityMat, tc = 'd')
  q = mat(2*(w*LS - w*G - (1-w)*LF), tc = 'd')

  # Constraints
      # Ax = b
          # Total sum of O = E
  A = mat([[1] for i in range(time_slots)], tc='d')
  b = mat(E, tc='d')
      # Gx <= h
          # All O_t => 0  --->  -O_t <= 0
  G_o_positive = identityMat*-1
  h_o_positive = np.array([[0] for i in range(time_slots)])
        # Cumsum O <= Cumsum V
  G_cumsum = cumsumMat
  h_cumsum = mat(cumsumMat.dot(LF), tc='d')
        # O <= Cumsum V (time horizon)
  G_horizon = identityMat
  h_horizon = mat(horizonMat.dot(LF), tc='d')
        # O <= LFmax
  G_lmax = identityMat
  h_lmax = mat(LFmax, tc='d')
        # Join constraints
  Gmat = mat(np.concatenate((G_o_positive, G_cumsum, G_horizon, G_lmax), axis=0), tc='d')
  h = mat(np.concatenate((h_o_positive, h_cumsum, h_horizon, h_lmax), axis=0), tc='d')
  
  # Solve
  solver = cvx.qp(P, q, Gmat, h, A, b)
  O = np.array(solver['x']).sum(axis=1)
  LFO = LF_static + abs(O.round(2))
  return LFO


def minimize_grid_flow_time_series(w, G, LF, LS = None, direction = 'forward', time_horizon = None, up_to_G = True, grid_cap = None, window_length = None):
  
  if window_length is None: window_length = len(G)
  O_total = []
  
  for i in range(0, len(G), int(window_length)):
    G_sub = G[i:int(i+window_length)]
    LF_sub = LF[i:int(i+window_length)]
    if LS is not None:
      LS_sub = LS[i:int(i+window_length)]
    else:
      LS_sub = LS
      
    O_sub = minimize_grid_flow_window(w, G_sub, LF_sub, LS_sub, direction, time_horizon, up_to_G, grid_cap)
    
    O_total = np.append(O_total, O_sub)
  
  return O_total


# def optimization_cvx(S, L, V, w1, w2, interval_minutes):
#   # Parameters check
#   if ((w1 + w2) != 1):
#     print("Error: Objectives weights must sum 1.")
#     return None
# 
#   if not (isinstance(S, np.ndarray) and isinstance(L, np.ndarray) and isinstance(V, np.ndarray)):
#     print("Error: S, L and V must be 'numpy.ndarray' class.")
#     return None
# 
#   if not (len(S) == len(L) == len(V)):
#     print("Error: S, L and V must have same length.")
#     return None
# 
#   # Optimization parameters
#   time_slots = len(V)
#   delta_t = interval_minutes/60
#   E = V.sum()*delta_t
#   cumsumMat = np.tril(np.full((time_slots, time_slots), 1))
#   identityMat = np.identity(time_slots)
# 
#   # Objective function terms
#   # P = mat(2*identityMat, tc = 'd')
#   # q = mat(2*(L-w_gd*S), tc = 'd')
#   # P = mat(4*identityMat, tc = 'd')
#   # q = mat(2*(L - S - V), tc = 'd')
#   P = mat(2*(w1+w2)*identityMat, tc = 'd')
#   q = mat(2*(w1*L - w1*S - w2*V), tc = 'd')
# 
#   # Constraints
#       # Ax = b
#           # Total sum of O = E
#   A = mat([[delta_t] for i in range(time_slots)], tc='d')
#   b = mat(E, tc='d')
#       # Gx <= h
#           # All O_t => 0  --->  -O_t <= 0
#   G_o_positive = identityMat*-1
#   h_o_positive = np.array([[0] for i in range(time_slots)])
#           # Cumsum O <= Cumsum V
#   G_cumsum = cumsumMat*delta_t
#   h_cumsum = mat(np.cumsum(V)*delta_t, tc='d')
#       # Join constraints
#   G = mat(np.concatenate((G_o_positive, G_cumsum), axis=0), tc='d')
#   h = mat(np.concatenate((h_o_positive, h_cumsum), axis=0), tc='d')
#   
#   # Solve
#   solver = cvx.qp(P, q, G, h, A, b)
#   O = np.array(solver['x']).sum(axis=1)
#   return abs(O.round(2))



################################## Flexibility management ########################################

def postpone_sessions(sessions_prof, demand, setpoint, power_th, responsive, sort_by_flex, include_msg):
  
  # Get sessions from responsive users during this time-window
  sessions_prof = sessions_prof.sample(frac=responsive, random_state = 1234).copy()
  
  # Message list
  if include_msg: msg = list()

  while True:
    
    # demand = get_demand(sessions_prof, [setpoint.index[1], setpoint.index[-1]], aggregated = True)
    flex_demand = DataFrame({'power': demand.demand - setpoint.setpoint}).query('power >'+str(power_th))
    
    if flex_demand.empty: 
      if include_msg: msg.append("No more flexibility required")
      break
    
    flex_sessions = sessions_prof.loc[(sessions_prof['f'] > 0) & sessions_prof['chs'].isin(flex_demand.index), 
      :].sort_values(by='chs', ascending=True)

    if flex_sessions.empty: 
      if include_msg: msg.append("No more flexibility available")
      break

    flex_timeslot = flex_sessions['chs'].iloc[0]
    flex_demand_timeslot = flex_demand.at[flex_timeslot, 'power']
    flex_sessions_timeslot = flex_sessions[flex_sessions['chs'] == flex_timeslot]
    if sort_by_flex:
      flex_sessions_timeslot = flex_sessions_timeslot.sort_values(by='f', ascending=False)
    
    # DEBUG = "For timeslot " + str(int(flex_timeslot)) + " we need " + str(round(flex_demand_timeslot, 2))+ " and we have " + str(round(flex_sessions_timeslot['p'].sum(), 2))
    # if verbose: print(DEBUG)

    if flex_timeslot == flex_demand.index[-1]: 
      if include_msg: msg.append("Can't shift outside the optimization window")
      break

    for s in range(flex_sessions_timeslot.shape[0]):
      
      # Session ID
      session_id = flex_sessions_timeslot.index[s]
      
      # Update timeseries demand
      session_power = sessions_prof.at[session_id, 'p'] # Shifting session's power
      demand.loc[sessions_prof.at[session_id, 'chs']] -= session_power # Session's power shifted from this timeslot
      demand.loc[sessions_prof.at[session_id, 'che']] += session_power # to the next one
      flex_demand_timeslot -= session_power # Flexibility requirement for this timeslot

      # Update session features
      sessions_prof.at[session_id, 'chs'] += 1
      sessions_prof.at[session_id, 'che'] += 1
      sessions_prof.at[session_id, 'f'] -= 1
      sessions_prof.at[session_id, 'shifted'] += 1
      
      if flex_demand_timeslot <= power_th: 
        if include_msg: msg.append("For timeslot " + str(int(flex_timeslot)) + " setpoint achieved!")
        break
        
  if include_msg:
    return sessions_prof, msg
  else:
    return sessions_prof



def smart_charging(sessions_norm, profiles_demand, fitting_data, window_length, opt_weights, responsive, power_th=0, up_to_G = True, grid_cap = None, sort_by_flex = True, include_msg=False):
  
  # opt_weights = opt_weights.set_index('profile')
  opt_profiles = Series(opt_weights.keys(), dtype='string')
  sessions_norm = sessions_norm.set_index('Session')
  sessions_norm['f'] = (sessions_norm['coe'] - sessions_norm['chs']) - (sessions_norm['che'] - sessions_norm['chs'])
  sessions_norm['shifted'] = 0
  setpoints = profiles_demand.copy()
  
  # Initiallize output sessions
  sessions_opt = DataFrame()

  # Parameters check
  if (len(profiles_demand.index)%window_length != 0):
    message("Demand length must be multiple of optimization windows length.")
    return(None)
  
  # Get the fixed demand
  if 'fixed' in fitting_data:
    L_fixed = fitting_data['fixed'].values
  else:
    L_fixed = np.zeros(len(fitting_data))
  
  # OPTIMIZATION
  # For each profile
  for profile in opt_profiles[opt_profiles.isin(set(sessions_norm["prof"]))]:
    
    other_profiles = np.setdiff1d(list(set(sessions_norm['prof'])), profile)
    if len(other_profiles) == 0:
      load_other_profiles = np.zeros(len(fitting_data))
    else:
      load_other_profiles = setpoints[other_profiles].sum(axis=1).values
    
    O = minimize_grid_flow_time_series(
      w = opt_weights[profile], 
      G = fitting_data['solar'].values, 
      LF = setpoints[profile].values, 
      LS = load_other_profiles + L_fixed,
      direction = 'forward', 
      time_horizon = None,
      up_to_G = up_to_G,
      grid_cap = grid_cap,
      window_length = window_length
    )
    
    setpoints[profile] = O
  
  
  # FLEXIBILITY MANAGEMENT
  if include_msg: msg = dict()
  
  # For each profile
  for profile in opt_profiles[opt_profiles.isin(set(sessions_norm["prof"]))]:
    sessions_prof = sessions_norm.copy().loc[sessions_norm['prof'] == profile]
    if include_msg: msg_prof = list()
    
    # For each optimization window
    for i in range(0, len(profiles_demand.index), window_length):
      window = [i, i+window_length]
      demand_prof = DataFrame({'demand': profiles_demand[profile].values[window[0]:window[1]]}, index = range(window[0], window[1]))
      setpoint_prof = DataFrame({'setpoint': setpoints[profile].values[window[0]:window[1]]}, index = range(window[0], window[1]))

      # Filter only sessions that start and finish charging within the time window
      sessions_prof_window = sessions_prof.copy().loc[(sessions_prof['chs'] >= window[0]) & (sessions_prof['che'] < window[1])]

      # Limit sessions End Time to the window size
      sessions_prof_window.loc[sessions_prof_window['coe'] > window[1], 'coe'] = window[1]
      sessions_prof_window['f'] = (sessions_prof_window['coe'] - sessions_prof_window['chs']) - (sessions_prof_window['che'] - sessions_prof_window['chs'])
      
      # Shift sessions
      if include_msg:
        sessions_prof_opt, msg_prof_window = postpone_sessions(sessions_prof_window, demand_prof, setpoint_prof, power_th, responsive[profile], sort_by_flex, include_msg)
        msg_prof += msg_prof_window
      else:
        sessions_prof_opt = postpone_sessions(sessions_prof_window, demand_prof, setpoint_prof, power_th, responsive[profile], sort_by_flex, include_msg)

      # Update original sessions with new schedule
      sessions_norm.loc[sessions_prof_opt.index, ['chs', 'che', 'shifted']] = sessions_prof_opt[['chs', 'che', 'shifted']]
    
    if include_msg: msg[profile] = msg_prof
      
  if include_msg:
    return setpoints, sessions_norm.reset_index(), msg
  else:
    return setpoints, sessions_norm.reset_index()




################################## BATTERY ########################################

def add_battery_window(w, G, L, Bcap, Bc, Bd, SOCmin = 0, SOCmax = 100, SOCini = None, SOCend = None, up_to_G = True):
  # Parameters check
  if (w > 1):
    print("Error: Objectives weights must sum 1.")
    return None
    
  if L is None: L = np.array([0.0]*len(G))

  if not (isinstance(G, np.ndarray)): G = np.array(G)
  if not (isinstance(L, np.ndarray)): L = np.array(L)

  if not (len(G) == len(L)):
    print("Error: S, L and V must have same length.")
    return None
    
  if SOCini is None: SOCini = 0
  if SOCend is None: SOCend = 0
  # SOCend must be between SOCmin and SOCmax
  if SOCend < SOCmin: SOCend = SOCmin
  if SOCend > SOCmax: SOCend = SOCmax

  # Optimization parameters
  time_slots = len(G)
  identityMat = np.identity(time_slots)
  cumsumMat = np.tril(np.full((time_slots, time_slots), 1)) # Lower triangle matrix
  
  # Upper limit for up to solar rule
  Bmax = np.array([Bc]*time_slots)
  if up_to_G:
    Bmax[G > L] = G[G > L] - L[G > L]
    Bmax[Bmax > Bc] = Bc

  # Objective function terms
  P = mat(2*identityMat, tc = 'd')
  q = mat(2*w*(L - G), tc = 'd')

  # Constraints
        # Ax = b
          # Final SOC = SOCend
  A = mat([[1] for i in range(time_slots)], tc='d')
  b = mat((SOCend - SOCini)/100*Bcap, tc='d')
  
      # Gx <= h
        # B charging/discharging limits
  G_B_max = identityMat
  h_B_max = np.array([[Bmax[i]] for i in range(time_slots)])
  G_B_min = identityMat*-1
  h_B_min = np.array([[Bd] for i in range(time_slots)]) # Negative*Negative=Positive (+Bd)
  
        # SOC limits
  G_SOC_max = cumsumMat
  h_SOC_max = np.array([[(SOCmax - SOCini)/100*Bcap] for i in range(time_slots)])
  G_SOC_min = cumsumMat*-1
  h_SOC_min = np.array([[(SOCini - SOCmin)/100*Bcap] for i in range(time_slots)])
        
        # Join constraints
  Gmat = mat(np.concatenate((G_B_max, G_B_min, G_SOC_max, G_SOC_min), axis=0), tc='d')
  h = mat(np.concatenate((h_B_max, h_B_min, h_SOC_max, h_SOC_min), axis=0), tc='d')
  
  # Solve
  solver = cvx.qp(P, q, Gmat, h, A, b)
  BO = np.array(solver['x']).sum(axis=1)
  return BO.round(2)


def add_battery_time_series(w, G, L, Bcap, Bc, Bd, SOCmin = 0, SOCmax = 100, SOCini = None, SOCend = None, up_to_G = True, window_length = None):
  
  if window_length is None: window_length = len(G)
  B_total = []
  
  for i in range(0, len(G), int(window_length)):
    G_sub = G[i:int(i+window_length)]
    L_sub = L[i:int(i+window_length)]

    B_sub = add_battery_window(w, G_sub, L_sub, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, SOCend, up_to_G)
    
    B_total = np.append(B_total, B_sub)
  
  return B_total






# ################################## PYOMO ########################################

# def do_pyomo_optimization():
#   """Sets, parameters and static timeseries:"""
#   T = timeseries_data.index.values
#   delta_t = time_interval/60
#   w1 = 0.5
#   w2 = 0.5
#   S = (timeseries_data.Solar/2).to_dict()
#   #L = timeseries_data[np.setdiff1d(["Visit", "Shortstay", "Dinner", "Worktime", "Home", "Pillow"], flex_profiles)].sum(axis=1).to_dict()
#   L = (timeseries_data.Solar*0).to_dict()
#   E = demand.demand.sum()
#   V = demand.demand.to_dict()

#   """Modelling functions:"""
#   model = ConcreteModel(name = "EV")
#   model.T = Set(initialize = T, ordered = True)
#   model.L = Param(model.T, initialize = L)
#   model.S = Param(model.T, initialize = S)
#   model.O = Var(model.T, within = NonNegativeReals)
#   model.V = Param(model.T, initialize = V)
#   model.E = Param(initialize = E)

#   # Objectives
#   model.objective = Objective(
#       expr = sum(w1*(model.S[t] - model.L[t] - model.O[t])**2 + w2*(model.L[t] +  model.O[t])**2 for t in model.T),
#       sense = minimize
#   )

#   # Constraints
#   @model.Constraint()
#   def energy_constraint(model):
#     return sum(model.O[t] for t in model.T) == E

#   @model.Constraint(model.T)
#   def postpone_constraint(model, t):
#     return (0, sum(model.O[ti] for ti in range(t+1)), sum(model.V[ti] for ti in range(t+1)))

#   # Model definition
#   model.pprint()

#   # For a local server:
#   # wget -N -q "https://ampl.com/dl/open/ipopt/ipopt-linux64.zip"
#   # unzip -o -q ipopt-linux64

#   solver = SolverFactory('ipopt', executable = 'ipopt')
#   # solver.options()
#   solver.solve(model, tee=True)
#   # SolverFactory('couenne', executable='couenne').solve(model, tee=True).write()

#   # display solution
#   print('\nProfit = ', model.objective())

#   O_dict = model.O.extract_values()

#   Op = []
#   for i in range(len(O_dict)):
#       Op.append([O_dict[i]])

#   demand_opt = pd.DataFrame(data = Op, columns = ["demand_opt"])
#   return demand_opt

