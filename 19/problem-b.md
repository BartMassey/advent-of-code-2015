Now that the machine is calibrated, you're ready to begin
molecule fabrication.

Molecule fabrication always begins with just a single
electron, e, and applying replacements one at a time, just
like the ones during calibration.

For example, suppose you have the following replacements:

        e => H
        e => O
        H => HO
        H => OH
        O => HH

If you'd like to make HOH, you start with e, and then make
the following replacements:

        e => O to get O
        O => HH to get HH
        H => OH (on the second H) to get HOH

So, you could make HOH after 3 steps. Santa's favorite
molecule, HOHOHO, can be made in 6 steps.

How long will it take to make the medicine? Given the
available replacements and the medicine molecule in your
puzzle input, what is the fewest number of steps to go from
e to the medicine molecule?
