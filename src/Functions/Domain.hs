-- |
-- Module: Functions.Domain
--
-- Domain functions, i.e. those which are more specific to this project and the
-- data structures it contains
module Functions.Domain where

-- My imports
import qualified Data.Teams as Teams
import Functions.Application
import Type

-- Haskell imports

expandTeamOrMultiple :: TeamOrMultiple -> [Team]
expandTeamOrMultiple (Team t) = [t]
expandTeamOrMultiple (MultipleTeam t i) = replicate i t
expandTeamOrMultiple (Teams t1 t2) = expandTeamOrMultiple t1 ++ expandTeamOrMultiple t2