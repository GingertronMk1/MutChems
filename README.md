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

### Todo

- [ ] `Data` (src/Classes/Data.hs:5)
- [ ] `reverseMap` (src/Functions/Application.hs:124)
- [ ] `splitOnInfix` (src/Functions/Application.hs:127)
- [ ] `splitOnInfix'` (src/Functions/Application.hs:137)
- [ ] `splitOnDoubleLines` (src/Functions/Application.hs:144)
- [ ] `dropFromEndWhile` (src/Functions/Application.hs:147)
- [ ] `removeWhitespaceLines` (src/Functions/Application.hs:154)
- [ ] `argumentsToArgumentList'` (src/Types/ArgumentList.hs:70)
- [ ] `processedArgumentPrefixesAndFunctions` (src/Types/ArgumentList.hs:77)
- [ ] `argumentPrefixesAndFunctions` (src/Types/ArgumentList.hs:83)
- [ ] `comboOfTeams'` (src/Types/TeamOrMultiple.hs:61)
- [ ] `teamsToTeamOrMultiples` (src/Data/Teams.hs:147)
- [ ] `nfcNorthAsTeamOrMultiples` (src/Data/Teams.hs:154)
- [ ] `nfcEastAsTeamOrMultiples` (src/Data/Teams.hs:161)
- [ ] `nfcSouthAsTeamOrMultiples` (src/Data/Teams.hs:168)
- [ ] `nfcWestAsTeamOrMultiples` (src/Data/Teams.hs:175)
- [ ] `afcNorthAsTeamOrMultiples` (src/Data/Teams.hs:182)
- [ ] `afcEastAsTeamOrMultiples` (src/Data/Teams.hs:189)
- [ ] `afcSouthAsTeamOrMultiples` (src/Data/Teams.hs:196)
- [ ] `afcWestAsTeamOrMultiples` (src/Data/Teams.hs:203)
- [ ] `nfc` (src/Data/Teams.hs:208)
- [ ] `afc` (src/Data/Teams.hs:221)
- [ ] `all32Teams` (src/Data/Teams.hs:236)
- [ ] `all32TeamsPlusLegends` (src/Data/Teams.hs:243)
- [ ] `doesPlayerBelongToTeam` (src/Types/Player.hs:129)
- [ ] `printPlayerTeamsInLineup` (src/Types/Lineup.hs:137)
- [ ] `printPlayersBelongingToTeam` (src/Types/Lineup.hs:143)
- [ ] `printPlayersBelongingToTeamsToMarkdown` (src/Types/Lineup.hs:151)
- [ ] `printPlayersAsMarkDownSection` (src/Types/Lineup.hs:158)
- [ ] `printPlayerAsMarkDownRow` (src/Types/Lineup.hs:181)
- [ ] `ppLineup` (src/Types/Lineup.hs:185)
- [ ] `ppPlayer` (src/Types/Lineup.hs:188)
- [ ] `openAndStepInitObject` (src/Types/InitObject.hs:45)
- [ ] `initObjectToBuildObjects` (src/Types/InitObject.hs:75)
- [ ] `printVariationAsHtmlTable` (src/Types/Variation.hs:51)
- [ ] `printVariationAsHtmlTable'` (src/Types/Variation.hs:55)
- [ ] `buildObjectToIntermediateObject` (src/Types/DisplayObject.hs:30)
- [ ] `printDisplayObjectsAsTableBody` (src/Types/DisplayObject.hs:73)
- [ ] `printDisplayObjectsAsTableHead` (src/Types/DisplayObject.hs:81)
- [ ] `printDisplayObjectsAsTableFoot` (src/Types/DisplayObject.hs:91)
- [ ] `getTeamCountsFromDisplayObject` (src/Types/DisplayObject.hs:106)