import numpy as np
import matplotlib.pyplot as plt
import h5py
import time
import array
import math
import pathlib
import subprocess
import matplotlib.colors as colors
import matplotlib.cm as cmx

from dedalus import public as de
from dedalus.extras import flow_tools
from dedalus.extras.plot_tools import quad_mesh, pad_limits
from dedalus.tools import post

import logging
logger = logging.getLogger(__name__)

#from dedalus.tools import post
#post.merge_process_files("profs", cleanup=True)
#set_paths = list(pathlib.Path("profs").glob("profs_s*.h5"))
#post.merge_sets("profs/profs.h5", set_paths, cleanup=True)
# Parameters
# We want the simulation to start "fresh", without assuming any saved values

global restart
restart=False

def smooth(y, box_pts):
    box = np.ones(box_pts)/box_pts
    y_smooth = np.convolve(y, box, mode="same")
    return y_smooth

Brms_list = {}
urms_list = {}
growth_list = {}
t_list = {}
# Files need to be analysed
files = ["profs/profs.h5"]
file_arr = np.array(files)
# Do the necessary calculations for each file and store corresponding values into a list
for file in files:
    ind = np.where(file==file_arr)[0][0]
    print(ind)
    f = h5py.File(file,'r')
    # Load datasets
    urms = f['tasks']['urms']
    Brms = f['tasks']['Brms']
    t = urms.dims[0]['sim_time'] #tilde time
    urms_arr = urms[:,0,0,0]
    Brms_arr = Brms[:,0,0,0]
    #dt = np.diff(t)
    #logB = np.log(Brms_arr)
    #growth = np.diff(logB)/dt
    #growth_smooth = smooth(growth,100)
    #growth_smooth = np.append(growth_smooth,growth_smooth[-1])
    #growth_list[ind] = growth_smooth
    urms_list[ind] = urms_arr
    Brms_list[ind] = Brms_arr
    t_list[ind] = t


plt.plot(t_list[0][:],Brms_list[0],'-b',label="rms_magnetic field")
plt.yscale("log")
plt.legend()
plt.grid()
plt.xlabel("simulation time")
plt.savefig('field.png')
plt.close()
plt.clf()

plt.plot(t_list[0][:],urms_list[0],'-b',label="rms_velocity")
#plt.yscale("log")
plt.legend()
plt.grid()
plt.xlabel("simulation time")
plt.savefig('velocity.png')
plt.close()
plt.clf()
"""
plt.plot(t_list[0][:],growth_list[0],'-b',label="growth rate")
plt.legend()
plt.grid()
plt.xlabel("simulation time")
plt.savefig('growth_rate.png')
plt.close()
plt.clf()
"""
