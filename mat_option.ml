include Plugin.Register
  (struct 
    let name = "Pilat"
    let shortname = "pilat"
    let help = "Frama-C Polynomial invariant generator"
   end)

module Enabled = False
  (struct 
    let option_name = "-pilat"
    let help = "when on, generates polynomial invariants for each solvable loop of the program" 
   end)
      
module Degree = Int
  (struct 
    let option_name = "-pilat-degree"
    let help = "sets the degree of the invariants seeked"
    let arg_name = "n"
    let default = 2
   end)

