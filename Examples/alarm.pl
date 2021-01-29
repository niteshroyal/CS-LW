%%% -*- Mode: Prolog; -*-
:- use_module('../Inference/propositionalProbLogic_BayesBall.pl').
:- initialization(init).
:- discontiguous (::)/2, (~)/2, (:=)/2.
    

history ~ discrete([0.01:true,0.99:false]) := lvfailure~=false.
history ~ discrete([0.9:true,0.1:false]) := lvfailure~=true.

cvp ~ discrete([0.01:low,0.29:normal,0.7:high]) := lvedvolume~=high.
cvp ~ discrete([0.95:low,0.04:normal,0.01:high]) := lvedvolume~=low.
cvp ~ discrete([0.04:low,0.95:normal,0.01:high]) := lvedvolume~=normal.

pcwp ~ discrete([0.01:low,0.04:normal,0.95:high]) := lvedvolume~=high.
pcwp ~ discrete([0.95:low,0.04:normal,0.01:high]) := lvedvolume~=low.
pcwp ~ discrete([0.04:low,0.95:normal,0.01:high]) := lvedvolume~=normal.

hypovolemia ~ discrete([0.2:true,0.8:false]).

lvedvolume ~ discrete([0.05:low,0.9:normal,0.05:high]) := hypovolemia~=false, lvfailure~=false.
lvedvolume ~ discrete([0.98:low,0.01:normal,0.01:high]) := hypovolemia~=false, lvfailure~=true.
lvedvolume ~ discrete([0.01:low,0.09:normal,0.9:high]) := hypovolemia~=true, lvfailure~=false.
lvedvolume ~ discrete([0.95:low,0.04:normal,0.01:high]) := hypovolemia~=true, lvfailure~=true.

lvfailure ~ discrete([0.05:true,0.95:false]).

strokevolume ~ discrete([0.05:low,0.9:normal,0.05:high]) := hypovolemia~=false, lvfailure~=false.
strokevolume ~ discrete([0.95:low,0.04:normal,0.01:high]) := hypovolemia~=false, lvfailure~=true.
strokevolume ~ discrete([0.5:low,0.49:normal,0.01:high]) := hypovolemia~=true, lvfailure~=false.
strokevolume ~ discrete([0.98:low,0.01:normal,0.01:high]) := hypovolemia~=true, lvfailure~=true.

errlowoutput ~ discrete([0.05:true,0.95:false]).

hrbp ~ discrete([0.01:low,0.01:normal,0.98:high]) := hr~=high, errlowoutput~=false.
hrbp ~ discrete([0.01:low,0.98:normal,0.01:high]) := hr~=high, errlowoutput~=true.
hrbp ~ discrete([0.4:low,0.59:normal,0.01:high]) := hr~=low, errlowoutput~=false.
hrbp ~ discrete([0.98:low,0.01:normal,0.01:high]) := hr~=low, errlowoutput~=true.
hrbp ~ discrete([0.98:low,0.01:normal,0.01:high]) := hr~=normal, errlowoutput~=false.
hrbp ~ discrete([0.3:low,0.4:normal,0.3:high]) := hr~=normal, errlowoutput~=true.

hrekg ~ discrete([0.01:low,0.01:normal,0.98:high]) := hr~=high, errcauter~=false.
hrekg ~ discrete([0.01:low,0.98:normal,0.01:high]) := hr~=high, errcauter~=true.
hrekg ~ discrete([0.3333333:low,0.3333333:normal,0.3333333:high]) := hr~=low.
hrekg ~ discrete([0.98:low,0.01:normal,0.01:high]) := hr~=normal, errcauter~=false.
hrekg ~ discrete([0.3333333:low,0.3333333:normal,0.3333333:high]) := hr~=normal, errcauter~=true.

errcauter ~ discrete([0.1:true,0.9:false]).

hrsat ~ discrete([0.01:low,0.01:normal,0.98:high]) := hr~=high, errcauter~=false.
hrsat ~ discrete([0.01:low,0.98:normal,0.01:high]) := hr~=high, errcauter~=true.
hrsat ~ discrete([0.3333333:low,0.3333333:normal,0.3333333:high]) := hr~=low.
hrsat ~ discrete([0.98:low,0.01:normal,0.01:high]) := hr~=normal, errcauter~=false.
hrsat ~ discrete([0.3333333:low,0.3333333:normal,0.3333333:high]) := hr~=normal, errcauter~=true.

insuffanesth ~ discrete([0.1:true,0.9:false]).

anaphylaxis ~ discrete([0.01:true,0.99:false]).

tpr ~ discrete([0.3:low,0.4:normal,0.3:high]) := anaphylaxis~=false.
tpr ~ discrete([0.98:low,0.01:normal,0.01:high]) := anaphylaxis~=true.

expco2 ~ discrete([0.01:zero,0.97:low,0.01:normal,0.01:high]) := ventlung~=zero, artco2~=high.
expco2 ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := ventlung~=zero, artco2~=low.
expco2 ~ discrete([0.01:zero,0.97:low,0.01:normal,0.01:high]) := ventlung~=zero, artco2~=normal.
expco2 ~ discrete([0.01:zero,0.01:low,0.01:normal,0.97:high]) := ventlung~=high.
expco2 ~ discrete([0.01:zero,0.01:low,0.97:normal,0.01:high]) := ventlung~=low, artco2~=high.
expco2 ~ discrete([0.01:zero,0.97:low,0.01:normal,0.01:high]) := ventlung~=low, artco2~=low.
expco2 ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := ventlung~=low, artco2~=normal.
expco2 ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := ventlung~=normal, artco2~=high.
expco2 ~ discrete([0.01:zero,0.01:low,0.97:normal,0.01:high]) := ventlung~=normal, artco2~=low.
expco2 ~ discrete([0.01:zero,0.01:low,0.97:normal,0.01:high]) := ventlung~=normal, artco2~=normal.

kinkedtube ~ discrete([0.04:true,0.96:false]).

minvol ~ discrete([0.01:zero,0.97:low,0.01:normal,0.01:high]) := ventlung~=zero, intubation~=esophageal.
minvol ~ discrete([0.01:zero,0.01:low,0.97:normal,0.01:high]) := ventlung~=zero, intubation~=onesided.
minvol ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := ventlung~=zero, intubation~=normal.
minvol ~ discrete([0.01:zero,0.01:low,0.97:normal,0.01:high]) := ventlung~=high, intubation~=esophageal.
minvol ~ discrete([0.01:zero,0.01:low,0.01:normal,0.97:high]) := ventlung~=high, intubation~=onesided.
minvol ~ discrete([0.01:zero,0.97:low,0.01:normal,0.01:high]) := ventlung~=high, intubation~=normal.
minvol ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := ventlung~=low, intubation~=esophageal.
minvol ~ discrete([0.6:zero,0.38:low,0.01:normal,0.01:high]) := ventlung~=low, intubation~=onesided.
minvol ~ discrete([0.01:zero,0.01:low,0.01:normal,0.97:high]) := ventlung~=low, intubation~=normal.
minvol ~ discrete([0.5:zero,0.48:low,0.01:normal,0.01:high]) := ventlung~=normal, intubation~=esophageal.
minvol ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := ventlung~=normal, intubation~=onesided.
minvol ~ discrete([0.5:zero,0.48:low,0.01:normal,0.01:high]) := ventlung~=normal, intubation~=normal.

fio2 ~ discrete([0.05:low,0.95:normal]).

pvsat ~ discrete([1.0:low,0.0:normal,0.0:high]) := ventalv~=zero, fio2~=low.
pvsat ~ discrete([0.99:low,0.01:normal,0.0:high]) := ventalv~=zero, fio2~=normal.
pvsat ~ discrete([0.01:low,0.95:normal,0.04:high]) := ventalv~=high, fio2~=low.
pvsat ~ discrete([0.01:low,0.01:normal,0.98:high]) := ventalv~=high, fio2~=normal.
pvsat ~ discrete([0.95:low,0.04:normal,0.01:high]) := ventalv~=low.
pvsat ~ discrete([1.0:low,0.0:normal,0.0:high]) := ventalv~=normal, fio2~=low.
pvsat ~ discrete([0.95:low,0.04:normal,0.01:high]) := ventalv~=normal, fio2~=normal.

sao2 ~ discrete([0.69:low,0.3:normal,0.01:high]) := pvsat~=high, shunt~=high.
sao2 ~ discrete([0.01:low,0.01:normal,0.98:high]) := pvsat~=high, shunt~=normal.
sao2 ~ discrete([0.98:low,0.01:normal,0.01:high]) := pvsat~=low.
sao2 ~ discrete([0.98:low,0.01:normal,0.01:high]) := pvsat~=normal, shunt~=high.
sao2 ~ discrete([0.01:low,0.98:normal,0.01:high]) := pvsat~=normal, shunt~=normal.

pap ~ discrete([0.05:low,0.9:normal,0.05:high]) := pulmembolus~=false.
pap ~ discrete([0.01:low,0.19:normal,0.8:high]) := pulmembolus~=true.

pulmembolus ~ discrete([0.01:true,0.99:false]).

shunt ~ discrete([0.95:normal,0.05:high]) := pulmembolus~=false, intubation~=esophageal.
shunt ~ discrete([0.05:normal,0.95:high]) := pulmembolus~=false, intubation~=onesided.
shunt ~ discrete([0.95:normal,0.05:high]) := pulmembolus~=false, intubation~=normal.
shunt ~ discrete([0.1:normal,0.9:high]) := pulmembolus~=true, intubation~=esophageal.
shunt ~ discrete([0.01:normal,0.99:high]) := pulmembolus~=true, intubation~=onesided.
shunt ~ discrete([0.1:normal,0.9:high]) := pulmembolus~=true, intubation~=normal.

intubation ~ discrete([0.92:normal,0.03:esophageal,0.05:onesided]).

press ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=zero, intubation~=esophageal, kinkedtube~=false.
press ~ discrete([0.01:zero,0.3:low,0.49:normal,0.2:high]) := venttube~=zero, intubation~=esophageal, kinkedtube~=true.
press ~ discrete([0.1:zero,0.84:low,0.05:normal,0.01:high]) := venttube~=zero, intubation~=onesided, kinkedtube~=false.
press ~ discrete([0.01:zero,0.01:low,0.08:normal,0.9:high]) := venttube~=zero, intubation~=onesided, kinkedtube~=true.
press ~ discrete([0.01:zero,0.01:low,0.01:normal,0.97:high]) := venttube~=zero, intubation~=normal, kinkedtube~=false.
press ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=zero, intubation~=normal, kinkedtube~=true.
press ~ discrete([0.01:zero,0.01:low,0.38:normal,0.6:high]) := venttube~=high, intubation~=esophageal, kinkedtube~=false.
press ~ discrete([0.2:zero,0.7:low,0.09:normal,0.01:high]) := venttube~=high, intubation~=esophageal, kinkedtube~=true.
press ~ discrete([0.01:zero,0.01:low,0.01:normal,0.97:high]) := venttube~=high, intubation~=onesided, kinkedtube~=false.
press ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=high, intubation~=onesided, kinkedtube~=true.
press ~ discrete([0.01:zero,0.9:low,0.08:normal,0.01:high]) := venttube~=high, intubation~=normal, kinkedtube~=false.
press ~ discrete([0.2:zero,0.75:low,0.04:normal,0.01:high]) := venttube~=high, intubation~=normal, kinkedtube~=true.
press ~ discrete([0.01:zero,0.01:low,0.08:normal,0.9:high]) := venttube~=low, intubation~=esophageal, kinkedtube~=false.
press ~ discrete([0.01:zero,0.15:low,0.25:normal,0.59:high]) := venttube~=low, intubation~=esophageal, kinkedtube~=true.
press ~ discrete([0.01:zero,0.01:low,0.01:normal,0.97:high]) := venttube~=low, intubation~=onesided, kinkedtube~=false.
press ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=low, intubation~=onesided, kinkedtube~=true.
press ~ discrete([0.01:zero,0.29:low,0.3:normal,0.4:high]) := venttube~=low, intubation~=normal, kinkedtube~=false.
press ~ discrete([0.05:zero,0.25:low,0.25:normal,0.45:high]) := venttube~=low, intubation~=normal, kinkedtube~=true.
press ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=normal, intubation~=esophageal, kinkedtube~=false.
press ~ discrete([0.01:zero,0.97:low,0.01:normal,0.01:high]) := venttube~=normal, intubation~=esophageal, kinkedtube~=true.
press ~ discrete([0.4:zero,0.58:low,0.01:normal,0.01:high]) := venttube~=normal, intubation~=onesided, kinkedtube~=false.
press ~ discrete([0.01:zero,0.01:low,0.97:normal,0.01:high]) := venttube~=normal, intubation~=onesided, kinkedtube~=true.
press ~ discrete([0.01:zero,0.01:low,0.01:normal,0.97:high]) := venttube~=normal, intubation~=normal, kinkedtube~=false.
press ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=normal, intubation~=normal, kinkedtube~=true.

disconnect ~ discrete([0.1:true,0.9:false]).

minvolset ~ discrete([0.05:low,0.9:normal,0.05:high]).

ventmach ~ discrete([0.05:zero,0.01:low,0.01:normal,0.93:high]) := minvolset~=high.
ventmach ~ discrete([0.05:zero,0.93:low,0.01:normal,0.01:high]) := minvolset~=low.
ventmach ~ discrete([0.05:zero,0.01:low,0.93:normal,0.01:high]) := minvolset~=normal.

venttube ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := ventmach~=zero.
venttube ~ discrete([0.01:zero,0.01:low,0.01:normal,0.97:high]) := ventmach~=high, disconnect~=false.
venttube ~ discrete([0.01:zero,0.01:low,0.97:normal,0.01:high]) := ventmach~=high, disconnect~=true.
venttube ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := ventmach~=low.
venttube ~ discrete([0.01:zero,0.97:low,0.01:normal,0.01:high]) := ventmach~=normal, disconnect~=false.
venttube ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := ventmach~=normal, disconnect~=true.

ventlung ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=zero, intubation~=esophageal, kinkedtube~=false.
ventlung ~ discrete([0.95:zero,0.03:low,0.01:normal,0.01:high]) := venttube~=zero, intubation~=esophageal, kinkedtube~=true.
ventlung ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=zero, intubation~=onesided, kinkedtube~=false.
ventlung ~ discrete([0.4:zero,0.58:low,0.01:normal,0.01:high]) := venttube~=zero, intubation~=onesided, kinkedtube~=true.
ventlung ~ discrete([0.3:zero,0.68:low,0.01:normal,0.01:high]) := venttube~=zero, intubation~=normal, kinkedtube~=false.
ventlung ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=zero, intubation~=normal, kinkedtube~=true.
ventlung ~ discrete([0.01:zero,0.01:low,0.97:normal,0.01:high]) := venttube~=high, kinkedtube~=false, intubation~=esophageal.
ventlung ~ discrete([0.01:zero,0.01:low,0.01:normal,0.97:high]) := venttube~=high, kinkedtube~=false, intubation~=onesided.
ventlung ~ discrete([0.01:zero,0.97:low,0.01:normal,0.01:high]) := venttube~=high, kinkedtube~=false, intubation~=normal.
ventlung ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=high, kinkedtube~=true.
ventlung ~ discrete([0.5:zero,0.48:low,0.01:normal,0.01:high]) := venttube~=low, kinkedtube~=false, intubation~=esophageal.
ventlung ~ discrete([0.3:zero,0.68:low,0.01:normal,0.01:high]) := venttube~=low, kinkedtube~=false, intubation~=onesided.
ventlung ~ discrete([0.95:zero,0.03:low,0.01:normal,0.01:high]) := venttube~=low, kinkedtube~=false, intubation~=normal.
ventlung ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=low, kinkedtube~=true.
ventlung ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=normal, intubation~=esophageal, kinkedtube~=false.
ventlung ~ discrete([0.01:zero,0.97:low,0.01:normal,0.01:high]) := venttube~=normal, intubation~=esophageal, kinkedtube~=true.
ventlung ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=normal, intubation~=onesided, kinkedtube~=false.
ventlung ~ discrete([0.01:zero,0.01:low,0.97:normal,0.01:high]) := venttube~=normal, intubation~=onesided, kinkedtube~=true.
ventlung ~ discrete([0.01:zero,0.01:low,0.01:normal,0.97:high]) := venttube~=normal, intubation~=normal, kinkedtube~=false.
ventlung ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := venttube~=normal, intubation~=normal, kinkedtube~=true.

ventalv ~ discrete([0.01:zero,0.97:low,0.01:normal,0.01:high]) := ventlung~=zero, intubation~=esophageal.
ventalv ~ discrete([0.01:zero,0.01:low,0.97:normal,0.01:high]) := ventlung~=zero, intubation~=onesided.
ventalv ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := ventlung~=zero, intubation~=normal.
ventalv ~ discrete([0.01:zero,0.94:low,0.04:normal,0.01:high]) := ventlung~=high, intubation~=esophageal.
ventalv ~ discrete([0.01:zero,0.88:low,0.1:normal,0.01:high]) := ventlung~=high, intubation~=onesided.
ventalv ~ discrete([0.03:zero,0.95:low,0.01:normal,0.01:high]) := ventlung~=high, intubation~=normal.
ventalv ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := ventlung~=low, intubation~=esophageal.
ventalv ~ discrete([0.01:zero,0.97:low,0.01:normal,0.01:high]) := ventlung~=low, intubation~=onesided.
ventalv ~ discrete([0.01:zero,0.01:low,0.01:normal,0.97:high]) := ventlung~=low, intubation~=normal.
ventalv ~ discrete([0.01:zero,0.01:low,0.01:normal,0.97:high]) := ventlung~=normal, intubation~=esophageal.
ventalv ~ discrete([0.97:zero,0.01:low,0.01:normal,0.01:high]) := ventlung~=normal, intubation~=onesided.
ventalv ~ discrete([0.01:zero,0.01:low,0.97:normal,0.01:high]) := ventlung~=normal, intubation~=normal.

artco2 ~ discrete([0.01:low,0.01:normal,0.98:high]) := ventalv~=zero.
artco2 ~ discrete([0.9:low,0.09:normal,0.01:high]) := ventalv~=high.
artco2 ~ discrete([0.01:low,0.01:normal,0.98:high]) := ventalv~=low.
artco2 ~ discrete([0.04:low,0.92:normal,0.04:high]) := ventalv~=normal.

catechol ~ discrete([0.3:normal,0.7:high]) := tpr~=high, artco2~=high, sao2~=high.
catechol ~ discrete([0.1:normal,0.9:high]) := tpr~=high, artco2~=high, sao2~=low.
catechol ~ discrete([0.3:normal,0.7:high]) := tpr~=high, artco2~=high, sao2~=normal, insuffanesth~=false.
catechol ~ discrete([0.1:normal,0.9:high]) := tpr~=high, artco2~=high, sao2~=normal, insuffanesth~=true.
catechol ~ discrete([0.95:normal,0.05:high]) := tpr~=high, artco2~=low, sao2~=high.
catechol ~ discrete([0.7:normal,0.3:high]) := tpr~=high, artco2~=low, sao2~=low.
catechol ~ discrete([0.95:normal,0.05:high]) := tpr~=high, artco2~=low, sao2~=normal, insuffanesth~=false.
catechol ~ discrete([0.7:normal,0.3:high]) := tpr~=high, artco2~=low, sao2~=normal, insuffanesth~=true.
catechol ~ discrete([0.99:normal,0.01:high]) := tpr~=high, artco2~=normal, sao2~=high.
catechol ~ discrete([0.7:normal,0.3:high]) := tpr~=high, artco2~=normal, sao2~=low.
catechol ~ discrete([0.99:normal,0.01:high]) := tpr~=high, artco2~=normal, sao2~=normal, insuffanesth~=false.
catechol ~ discrete([0.7:normal,0.3:high]) := tpr~=high, artco2~=normal, sao2~=normal, insuffanesth~=true.
catechol ~ discrete([0.01:normal,0.99:high]) := tpr~=low, sao2~=high, insuffanesth~=false, artco2~=high.
catechol ~ discrete([0.05:normal,0.95:high]) := tpr~=low, sao2~=high, insuffanesth~=false, artco2~=low.
catechol ~ discrete([0.05:normal,0.95:high]) := tpr~=low, sao2~=high, insuffanesth~=false, artco2~=normal.
catechol ~ discrete([0.01:normal,0.99:high]) := tpr~=low, sao2~=high, insuffanesth~=true.
catechol ~ discrete([0.01:normal,0.99:high]) := tpr~=low, sao2~=low.
catechol ~ discrete([0.01:normal,0.99:high]) := tpr~=low, sao2~=normal.
catechol ~ discrete([0.01:normal,0.99:high]) := tpr~=normal, artco2~=high.
catechol ~ discrete([0.05:normal,0.95:high]) := tpr~=normal, artco2~=low, sao2~=high.
catechol ~ discrete([0.05:normal,0.95:high]) := tpr~=normal, artco2~=low, sao2~=low, insuffanesth~=false.
catechol ~ discrete([0.01:normal,0.99:high]) := tpr~=normal, artco2~=low, sao2~=low, insuffanesth~=true.
catechol ~ discrete([0.05:normal,0.95:high]) := tpr~=normal, artco2~=low, sao2~=normal.
catechol ~ discrete([0.05:normal,0.95:high]) := tpr~=normal, artco2~=normal, sao2~=high.
catechol ~ discrete([0.05:normal,0.95:high]) := tpr~=normal, artco2~=normal, sao2~=low, insuffanesth~=false.
catechol ~ discrete([0.01:normal,0.99:high]) := tpr~=normal, artco2~=normal, sao2~=low, insuffanesth~=true.
catechol ~ discrete([0.05:normal,0.95:high]) := tpr~=normal, artco2~=normal, sao2~=normal.

hr ~ discrete([0.01:low,0.09:normal,0.9:high]) := catechol~=high.
hr ~ discrete([0.05:low,0.9:normal,0.05:high]) := catechol~=normal.

co ~ discrete([0.01:low,0.01:normal,0.98:high]) := hr~=high, strokevolume~=high.
co ~ discrete([0.8:low,0.19:normal,0.01:high]) := hr~=high, strokevolume~=low.
co ~ discrete([0.01:low,0.04:normal,0.95:high]) := hr~=high, strokevolume~=normal.
co ~ discrete([0.3:low,0.69:normal,0.01:high]) := hr~=low, strokevolume~=high.
co ~ discrete([0.98:low,0.01:normal,0.01:high]) := hr~=low, strokevolume~=low.
co ~ discrete([0.95:low,0.04:normal,0.01:high]) := hr~=low, strokevolume~=normal.
co ~ discrete([0.01:low,0.3:normal,0.69:high]) := hr~=normal, strokevolume~=high.
co ~ discrete([0.95:low,0.04:normal,0.01:high]) := hr~=normal, strokevolume~=low.
co ~ discrete([0.04:low,0.95:normal,0.01:high]) := hr~=normal, strokevolume~=normal.

bp ~ discrete([0.01:low,0.09:normal,0.9:high]) := co~=high, tpr~=high.
bp ~ discrete([0.9:low,0.09:normal,0.01:high]) := co~=high, tpr~=low.
bp ~ discrete([0.05:low,0.2:normal,0.75:high]) := co~=high, tpr~=normal.
bp ~ discrete([0.3:low,0.6:normal,0.1:high]) := co~=low, tpr~=high.
bp ~ discrete([0.98:low,0.01:normal,0.01:high]) := co~=low, tpr~=low.
bp ~ discrete([0.98:low,0.01:normal,0.01:high]) := co~=low, tpr~=normal.
bp ~ discrete([0.05:low,0.4:normal,0.55:high]) := co~=normal, tpr~=high.
bp ~ discrete([0.98:low,0.01:normal,0.01:high]) := co~=normal, tpr~=low.
bp ~ discrete([0.1:low,0.85:normal,0.05:high]) := co~=normal, tpr~=normal.


