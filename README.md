# MutChems

This is a Haskell project I created to help me determine the most optimum way of distributing team chemistries in my Madden Ultimate Team.
It did just use Cabal but I decided to switch it up to Stack so I could test things.
Now I just need to work out how to test things...

### Installation

If installing new, run `./bin/install -r 1` to install with all additional requirements (defined in requirements.txt).
If you don't want those requirements (they're not needed for functionality) you can just run `./bin/install`

### Current way it works

- File `input.json` is read, converted into a `JSONBuildObject`
- `JSONBuildObject` is converted into a `BuildObject`
- Then this is spread into all possible `Variation`s of the `Lineup` contained within it
