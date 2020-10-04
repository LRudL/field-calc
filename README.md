# field-calc
Calculate 3D fields from small/coarse mass/charge distributions, visualise 2D slices of them

## Goals
The goal was to write, in a functional programming style, some rough code for visualising the gravitational fields of arbitrary density distributions in 3D space.

## Results
To get something working fast, I used the simplest possible approach: calculate the force at each point in space by summing up the force contributions over all other points. The performance starts getting intolerable once the grid size exceeds 10x10x10, due to all the looping.

The drawing code can visualise a "slice" of the space, meaning the 2D plane you get from setting either x, y, or z to a constant and letting the other position variables vary. Density is indicated by color (black to white), and the vectors are drawn in red, with length scaling less than linearly with vector magnitude and automatically adjusting so that the longest vector never far exceeds the grid size.

## Future
Some optimisations I have not yet made include accounting for symmetry (when it exists) and looping only over non-zero mass density points (significant speedup only when those are a small fraction of the total volume). In the general case, however, a significant optimisation would likely require switching to fancier methods like multigrid relaxation based on the Poisson equation for the field, so these optimisations (and likewise commenting the existing code) are not a priority.
