# Context-Specific Likelihood Weighting
A sampling algorithm for Probabilistic Logic Programs.

Installation
============

This installation manual is tested on Ubuntu 18.04.4 LTS and 16.04 LTS.


1. Install dependencies
```
    $ sudo apt install build-essential pkg-config libgsl-dev
```

2. Install latest version of SWI-Prolog (version above 8.1.30 is required)
```
    $ sudo apt-add-repository ppa:swi-prolog/devel
    $ sudo apt-get update
    $ sudo apt-get install swi-prolog
```

3. Build 
```
    $ cd Sampling
    $ make clean
    $ make all
```

Execution 
=========

4. Go to Examples folder
```
   $ cd ../Examples/
```

You will find one example of DC(B) program, namely, "alarm.pl". 
Alarm: (https://www.bnlearn.com/bnrepository/discrete-medium.html#alarm). 
Note that how tree-CPDs are written in the form of rules in DC(B) program.

5. Open an example in SWI-Prolog
```
   $ swipl -s alarm.pl
```

SWI-Prolog should now be opened without any error or warnings.
First, set the number of samples.

```
   ?- set_sample_size(1000).
```
Second, turn off the debug mode (1 to turn on the debug mode).

```
   ?- set_debug(0).
```

Now query. The first argument is query, second is the list of evidence and P is the output probability

```
   ?- query(bp~=low, [lvfailure~=false, cvp~=normal, hr~=normal, expco2~=low, ventalv~=low, ventlung~=zero], P).
```



