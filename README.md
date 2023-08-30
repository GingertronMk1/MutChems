# MutChems

This is a Python project I created to help me determine the most optimum way of distributing team chemistries in my Madden Ultimate Team.
Now I just need to work out how to test things...

### Installation

If installing new, run `./bin/install` to install with all additional requirements (defined in requirements.txt).

### Current way it works

- File `input.json` is read, converted into a `Lineup` and some `Change`s
- These `Change`s are applied iteratively to the `Lineup`, to generate a list of `Lineup`s
- These `Lineup`s are then analysed to see which is the best based on the following criteria:
    - Which has the highest maximum tier (multiple of 5 basically)
    - Which has the most `Team`s at that tier
    - Which has the most highest numbers of `Team`s (taking the mean of the square of the count of each `Team`)

### MyPy

https://mypy.readthedocs.io/en/stable/config_file.html
