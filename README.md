# sapo-simulation

This git contain various simulation os dynamic system for SEAIR and SIDARTHE model, evolution of SIR, for my thesis.
They producet using Sapo and it's "sapo input language" (sil): "https://github.com/dreossi/sapo"

Each folder contain the .sil of the simulation, but some simulation required to change parameters through the days of simulation, in this scenario [period]mod.sil file  are used to describe the change, and can so be used to produce a computable .sil that contain as starting value the result of the previous part of the simulation.
the [name]result file contain istead the complete result of my simulation.

The R file present instead an example of the code used to produce the plots presented in the thesis. (since the non_saw has 1 less step then saw to confront the two the first step of non_saw shoul be removed. non_saw=/=sawless)

The directory that contain the "saw" in the name refer to the use of an algoritm that remove usless value from the variable interval of value at each step.
In particular removing value that cuold not sum to 1 with any combination of value of the other variable.
