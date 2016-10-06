##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2016                                               #
#    CEA (Commissariat a l'energie atomique et aux energies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file LICENCE).                      #
#                                                                        #
##########################################################################

from sage.all import *

# This is an heuristic over the possible error of the minimize/maximize 
# functions
error_allowed = 0.05

def print_debug(level,s) :
    if debug >= level : 
        print s

def minimize(f,constr,k,guess): 
    min = minimize_constrained(f,constr,guess)
    min2 = minimize_constrained(f,constr,min)
    while (abs(f(min2) - f(min)) > error_allowed):
        min = min2 
        minimize_constrained(f,constr,min2)
    return min2
    
def new_window(ev, f, mf, poly, k, low_k, up_k, non_det_c,guess) : 
    
    print_debug(1,"k tested : " + str(k))
    polypk = lambda x : poly(x) + k
    constr = list(non_det_c)
    constr.append(polypk)
    min = minimize(f,constr,k,guess)
    max = minimize(mf, constr,k,guess)
    print_debug (2,"min = ")
    print_debug (2,min)
    print_debug (3,"poly(min) = ")
    print_debug (3,poly(min))
    print_debug (3,"f(min) = ")
    print_debug (3,f(min))
    print_debug (2,"max = ")
    print_debug (2,max)
    print_debug (3,"poly(max) = ")
    print_debug (3,poly(max))
    print_debug (3,"f(max) = ")
    print_debug (3,f(max))
    print_debug (1,"Does " + str(f(min)) + " > " + str(k*(-1-ev)) + " and " 
           + str(f(max)) + " < " + str(k*(1-ev)))
    
    if (f(min) - error_allowed > (ev - 1) * k) and (f(max) + error_allowed < (1-ev)*k):
        print_debug (1,"Yes !")
        up_k = k
        
    else : 
        print_debug (1,"No !")
        low_k = k
        print_debug (1,"New window : " + str(low_k) + " to " + str(up_k))
    
    return low_k, up_k
        
def find_k(ev, f, poly,non_det_c,max_k,N,guess) :
    low_k = 0.
    up_k = max_k
    mf = lambda x : -1 * f(x)
    # We first check if max_k is a good upper bound :
    low_k,up_k = new_window(ev, f, mf, poly, up_k, low_k, up_k, non_det_c,guess)
    while (low_k != 0): # then for k = max_k, the condition is not respected.
        low_k = 0.
        up_k = 2.*up_k #up_k didn't change
        new_window(ev, f, mf, poly, up_k, low_k, up_k, non_det_c,guess)
    k = max_k/2
    i = 0
    while(i < N):
        i = i+1
        low_k,up_k=new_window(ev, f, mf, poly, k, low_k, up_k, non_det_c,guess)
        k = (low_k + up_k)/2
    return up_k

import sys


# Exemple of use : 
# sage optimizer.py 0 0.9248 3 23 10 "-2.72*x[2]*(x[0]-x[1]) - 2*x[2]*x[2]" "- x[0]*x[0] - x[1]*x[1]" "x[2]+0.1" "- x[2] + 0.1"

# debug is the level of verbosity of the script
debug = eval(sys.argv[1])

# ev is the eigenvalue of the invariant
ev = eval(sys.argv[2])

# v is the number of variables
v = eval(sys.argv[3])

# max_k is a guess of the maximal value considered for k
max_k = eval(sys.argv[4])

# N is the number of iterations of the refinement loop
N = eval(sys.argv[5])

# objective is the objective function to optimize
objective = eval("lambda x :" + sys.argv[6])

# poly is the polynomial constraint
poly = eval("lambda x :" + sys.argv[7])

# non_det_c is the list of constraints over the non deterministic assignments
non_det_c = []

i = 8

while(i<len(sys.argv)):
    non_det_c.append(eval("lambda x :" + sys.argv[i]))
    i = i+1

i = 0
minimize_guess = []

while (i < v):
    i = i+1
    minimize_guess.append(max_k)

k=find_k(ev,objective,poly,non_det_c,max_k,N, minimize_guess)

print k
