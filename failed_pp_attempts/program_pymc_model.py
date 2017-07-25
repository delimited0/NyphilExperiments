import numpy as np
import pymc3 as pm
from pymc3 import Model, Dirichlet, Multinomial

with pm.Model() as biterm_model:
  
  
