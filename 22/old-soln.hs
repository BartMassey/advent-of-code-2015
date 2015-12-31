-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


-- Got help debugging from this solution
--   https://www.reddit.com/r/adventofcode/comments/
--     3xspyl/day_22_solutions/cy7l25a
-- although it appears to be wrong on my input.

import Soln

-- | Us vs them.
data XPC = PC | NPC
           deriving (Eq, Show, Ord)

-- | An effect applies to both attacker and defender.
type Effect = (Stats, Stats) -> (Stats, Stats)

-- | Description of XPC.
data Stats = Stats {
      statsType :: XPC,
      statsHP :: Int,
      statsDamage :: Int,
      statsArmor :: Int,
      statsMana :: Int,
      statsManaSpent :: Int }
             deriving Show

-- | Description of Spell.
data Spell = Spell {
      spellName :: String,
      spellCost, spellDuration :: Int,
      spellEffect, spellPostEffect:: Effect }

-- | For debugging.
instance Show Spell where
    show spell = "<<" ++ spellName spell ++
                   "@" ++ show (spellDuration spell) ++ ">>"

-- | Result carries who died and how much mana was spent
-- by the PC.
data Result = Result XPC Int
              deriving (Show, Ord, Eq)

-- | The PC.
pc :: Stats
pc = Stats {
       statsType = PC,
       statsHP = 50,
       statsDamage = 0,
       statsArmor = 0,
       statsMana = 500,
       statsManaSpent = 0 }

-- | Read the NPC description.
parseBoss :: String -> Stats
parseBoss stuff =
    case map words $ lines stuff of
      [[ "Hit", "Points:", hpstr ],
       [ "Damage:", dstr ]] ->
        Stats {
          statsType = NPC,
          statsHP = read hpstr,
          statsDamage = read dstr,
          statsArmor = 0,
          statsMana = 0,
          statsManaSpent = 0 }
      _ -> error "bad stats"

-- | Apply damage adjusted for armor, with minimum 1 damage.
applyDamage :: Int -> Stats -> Stats
applyDamage damage combatant =
    combatant { statsHP =
                    statsHP combatant -
                    (1 `max` (damage - statsArmor combatant)) }

-- | Heal.
applyHealing :: Int -> Stats -> Stats
applyHealing healing combatant =
    combatant { statsHP = statsHP combatant + healing }

-- | Increase mana.
applyMana :: Int -> Stats -> Stats
applyMana mana combatant =
    combatant { statsMana = statsMana combatant + mana }

-- | Damage spell effect.
effectDamage :: Effect
effectDamage (attacker, defender) =
    case statsType attacker of
      NPC -> error "damage applied to PC"
      PC -> (attacker, applyDamage 4 defender)
    
-- | Drain spell effect.
effectDrain :: Effect
effectDrain (attacker, defender) =
    case statsType attacker of
      NPC -> error "drain applied to PC"
      PC -> (applyHealing 2 attacker, applyDamage 2 defender)

-- | Shield spell effect.
effectShield :: Effect
effectShield (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, defender { statsArmor = 7 })
      PC -> (attacker { statsArmor = 7 }, defender)

-- | Shield spell post-effect.
effectUnshield :: Effect
effectUnshield (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, defender { statsArmor = 0 })
      PC -> (attacker { statsArmor = 0 }, defender)

-- | Poison spell effect.
effectPoison :: Effect
effectPoison (attacker, defender) =
    case statsType attacker of
      NPC -> (applyDamage 3 attacker, defender)
      PC -> (attacker, applyDamage 3 defender)

-- | Recharge spell effect.
effectRecharge :: Effect
effectRecharge (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, applyMana 101 defender)
      PC -> (applyMana 101 attacker, defender)

-- | Treat "hard mode" as a spell effect.
effectHardMode :: Effect
effectHardMode (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, defender)
      PC -> (applyDamage 1 attacker, defender)

-- | The spell table.
spellbook :: [Spell]
spellbook = [
  Spell "Magic Missile" 53 0 effectDamage id,
  Spell "Drain"         73 0 effectDrain id,
  Spell "Shield"       113 6 effectShield effectUnshield,
  Spell "Poison"       173 6 effectPoison id,
  Spell "Recharge"     229 5 effectRecharge id ]

-- | Decrease caster mana and record its expenditure.
spendMana :: Spell -> Stats -> Stats
spendMana spell caster =
    let cost = spellCost spell in
    caster {
      statsMana = statsMana caster - cost,
      statsManaSpent = statsManaSpent caster + cost }

-- | Tick off the clock on a spell.
decrementDuration :: Spell -> Spell
decrementDuration spell =
    spell { spellDuration = spellDuration spell - 1 }

-- | If either side has died, report it.
diedFromSpell :: (Stats, Stats) -> Maybe Result
diedFromSpell (attacker, defender) =
    if statsHP attacker <= 0 then
        case statsType attacker of
          PC -> Just $ Result NPC (statsManaSpent attacker)
          NPC -> Just $ Result PC (statsManaSpent defender)
    else if statsHP defender <= 0 then
        case statsType defender of
          PC -> Just $ Result NPC (statsManaSpent defender)
          NPC -> Just $ Result PC (statsManaSpent attacker)
    else
        Nothing

-- | The actual fight.
fightSim :: [Spell] -> Stats -> Stats -> Result
fightSim effects attacker defender =
    let instCombatants = runInstants (attacker, defender) in
      case diedFromSpell instCombatants of
        Just result -> result
        Nothing ->
            let (newEffects, effectedCombatants) =
                    runEffects effects instCombatants
            in
            case diedFromSpell effectedCombatants of
              Just result -> result
              Nothing -> uncurry (fightRound newEffects)
                           effectedCombatants
    where
      runInstants combatants =
          foldr runInstant combatants effects
          where
            runInstant spell curCombatants
                | spellDuration spell /= -1 = curCombatants
                | otherwise = spellEffect spell curCombatants
      runEffects oldEffects combatants =
          let newCombatants = foldr runEffect combatants oldEffects in
          foldr expireEffect ([], newCombatants) oldEffects
          where
            runEffect spell curCombatants
                | spellDuration spell == -1 = curCombatants
                | spellDuration spell <= 0 = error "misqueued spell"
                | otherwise = spellEffect spell curCombatants
            expireEffect spell (newEffects, curCombatants) =
                case spellDuration spell of
                  s | s == -1 ->
                        (spell : newEffects, curCombatants)
                  s | s == 1 ->
                        (newEffects, spellPostEffect spell curCombatants)
                  s | s < 1 ->
                        error "misqueued spell"
                  _ -> (decrementDuration spell : newEffects, curCombatants)

-- | Fight one round.
fightRound :: [Spell] -> Stats -> Stats -> Result
fightRound effects attacker defender =
    case statsType attacker of
      PC ->
          case spellChoices of
            [] -> Result NPC (statsManaSpent attacker)
            cs -> minimum $ map magicalAttack $ sortBy (comparing spellCost) cs
          where
            magicalAttack spell =
                let chargedAttacker = spendMana spell attacker in
                case statsManaSpent chargedAttacker of
                  m | m > 2000 -> Result NPC 1000000
                  _ ->
                    case spellDuration spell of
                      0 ->
                        let (effectedAttacker, effectedDefender) =
                              spellEffect spell (chargedAttacker, defender) in
                        if statsHP effectedDefender <= 0
                        then Result PC (statsManaSpent effectedAttacker)
                        else fightSim effects effectedDefender effectedAttacker
                      _ -> 
                        let addedEffects = spell : effects in
                        fightSim addedEffects defender chargedAttacker
            spellChoices =
                sortBy (comparing spellCost) $ filter okSpell spellbook
                where
                  okSpell spell =
                      spellCost spell <= statsMana attacker &&
                      all (\s -> spellName spell /= spellName s) effects
      NPC ->
          let effectedDefender =
                applyDamage (statsDamage attacker) defender in
          if statsHP effectedDefender <= 0
          then Result NPC (statsManaSpent effectedDefender)
          else fightSim effects effectedDefender attacker

solna :: String -> IO ()
solna stuff = do
  let boss = parseBoss stuff
  print $ fightSim [] pc boss

solnb :: String -> IO ()
solnb stuff = do
  let boss = parseBoss stuff
  print $ fightSim [hardMode] pc boss
  where
    hardMode = Spell "HardMode" 0 (-1) effectHardMode id

main :: IO ()
main = makeMain solna solnb
