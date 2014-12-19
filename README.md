point-plotter
=============

![Plots displayed by point-plotter](./plotImage.png?raw=true)

![Input prompts for point-plotter players](./inputImage.png?raw=true)

Blank-Canvas program which plots a set of random points on a graph.
Two players are then each asked to write an inequality function so
as to try and cover as many points on the graph as possible.

`runghc point-plotter.hs` to run, and `CTRL+C` to quit.

### Example expressions:

* `y <= [(2,3),(0.5,2)]` is like y <= 2x^3 + (0.5)x^2
* `y >= [(-0.1,0)]` is like y >= (-0.1)


tested on an Ubuntu 14.04 virtualbox under Vagrant.

requires
--------
* Haskell Platform
* [Blank-Canvas](https://hackage.haskell.org/package/blank-canvas):
`cabal install blank-canvas`



a rough to-do list
------------------

- [x] Plot chart setup function
- [x] Function to complete box after function to be filled in
- [x] Generate random numbers within range
- [x] Function to plot random points on graph
- [x] Function to change color of dot if within plot
- [x] Function to check status of MVar for user input
- [x] Data types for simple-parse polynomial expressions
- [x] Function to prompt for user equation
- [ ] Break into server/client threads
- [ ] Data type and parser for better representation of polynomials
- [ ] ~~ Applicative/Monadic point, as in class example~~
    
