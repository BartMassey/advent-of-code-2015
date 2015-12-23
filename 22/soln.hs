-- Copyright © 2015 Bart Massey

-- Got help debugging from this solution
--   https://www.reddit.com/r/adventofcode/comments/
--     3xspyl/day_22_solutions/cy7l25a
-- although it appears to be wrong on my input.

import Soln

data XPC = PC | NPC
           deriving (Eq, Show, Ord)

type Effect = (Stats, Stats) -> (Stats, Stats)

data Stats = Stats {
      statsType :: XPC,
      statsHP :: Int,
      statsDamage :: Int,
      statsArmor :: Int,
      statsMana :: Int,
      statsManaSpent :: Int }
             deriving Show

data Spell = Spell {
      spellName :: String,
      spellCost, spellDuration :: Int,
      spellEffect, spellPostEffect:: Effect }

instance Show Spell where
    show spell = "<<" ++ spellName spell ++
                   "@" ++ show (spellDuration spell) ++ ">>"

data Result = Result XPC Int
              deriving (Show, Ord, Eq)

pc :: Stats
pc = Stats {
       statsType = PC,
       statsHP = 50,
       statsDamage = 0,
       statsArmor = 0,
       statsMana = 500,
       statsManaSpent = 0 }

parseBoss :: String -> Stats
parseBoss stuff
    | ["Hit_Points:", "Damage:"] == head parse =
        let [hpstr, dstr] = last parse in
        Stats {
          statsType = NPC,
          statsHP = read hpstr,
          statsDamage = read dstr,
          statsArmor = 0,
          statsMana = 0,
          statsManaSpent = 0 }
    | otherwise = error "bad stats"
    where
      parse = transpose $ map words $ lines stuff

applyDamage :: Int -> Stats -> Stats
applyDamage damage combatant =
    combatant { statsHP =
                    statsHP combatant -
                    (1 `max` (damage - statsArmor combatant)) }

applyHealing :: Int -> Stats -> Stats
applyHealing healing combatant =
    combatant { statsHP = statsHP combatant + healing }

applyMana :: Int -> Stats -> Stats
applyMana mana combatant =
    combatant { statsMana = statsMana combatant + mana }

effectDamage :: Effect
effectDamage (attacker, defender) =
    case statsType attacker of
      NPC -> error "damage applied to PC"
      PC -> (attacker, applyDamage 4 defender)
    
effectDrain :: Effect
effectDrain (attacker, defender) =
    case statsType attacker of
      NPC -> error "drain applied to PC"
      PC -> (applyHealing 2 attacker, applyDamage 2 defender)

effectShield :: Effect
effectShield (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, defender { statsArmor = 7 })
      PC -> (attacker { statsArmor = 7 }, defender)

effectUnshield :: Effect
effectUnshield (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, defender { statsArmor = 0 })
      PC -> (attacker { statsArmor = 0 }, defender)

effectPoison :: Effect
effectPoison (attacker, defender) =
    case statsType attacker of
      NPC -> (applyDamage 3 attacker, defender)
      PC -> (attacker, applyDamage 3 defender)

effectRecharge :: Effect
effectRecharge (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, applyMana 101 defender)
      PC -> (applyMana 101 attacker, defender)

effectHardMode :: Effect
effectHardMode (attacker, defender) =
    case statsType attacker of
      NPC -> (attacker, defender)
      PC -> (applyDamage 1 attacker, defender)

spellbook :: [Spell]
spellbook = [
  Spell "Magic Missile" 53 0 effectDamage id,
  Spell "Drain"         73 0 effectDrain id,
  Spell "Shield"       113 6 effectShield effectUnshield,
  Spell "Poison"       173 6 effectPoison id,
  Spell "Recharge"     229 5 effectRecharge id ]

spendMana :: Spell -> Stats -> Stats
spendMana spell caster =
    let cost = spellCost spell in
    caster {
      statsMana = statsMana caster - cost,
      statsManaSpent = statsManaSpent caster + cost }

decrementDuration :: Spell -> Spell
decrementDuration spell =
    spell { spellDuration = spellDuration spell - 1 }

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

fightSim :: Int -> Maybe [String] -> [Spell] -> Stats -> Stats -> Result
fightSim limit strat effects attacker defender =
    let instCombatants = runInstants (attacker, defender) in
      case diedFromSpell instCombatants of
        Just result -> result
        Nothing ->
            let (newEffects, effectedCombatants) =
                    runEffects effects instCombatants
            in
            case diedFromSpell effectedCombatants of
              Just result -> result
              Nothing -> uncurry (fightRound limit strat newEffects)
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

fightRound :: Int -> Maybe [String] -> [Spell] -> Stats -> Stats -> Result
fightRound limit strat effects attacker defender =
    case statsType attacker of
      PC ->
          case spellChoices of
            [] -> case strat of
                    Nothing -> Result NPC (statsManaSpent attacker)
                    Just _ -> error $ unlines [
                               "ran out of spell choices before winning",
                                "player " ++ show attacker,
                                "effects " ++ show effects ]
            cs -> case strat of
                    Nothing ->
                        minimum $ map magicalAttack $ sortBy (comparing spellCost) cs
                    Just [] -> error "ran out of strat"
                    Just (s : _) ->
                        magicalAttack $ lookupSpell s cs
          where
            lookupSpell stratSpell candidates =
                case find (\c -> spellName c == stratSpell) candidates of
                  Just s -> s
                  Nothing -> error $ unlines [
                              "failed to find strat spell " ++ stratSpell,
                              "in " ++ show (map spellName candidates),
                              "player " ++ show attacker,
                              "effects " ++ show effects ]
            magicalAttack spell =
                let chargedAttacker = spendMana spell attacker in
                case limit - statsManaSpent attacker of
                  newLimit | newLimit < 0 -> Result NPC 1000000
                  newLimit ->
                    case spellDuration spell of
                      0 ->
                        let (effectedAttacker, effectedDefender) =
                              spellEffect spell (chargedAttacker, defender) in
                        if statsHP effectedDefender <= 0
                        then Result PC (statsManaSpent effectedAttacker)
                        else fightSim newLimit (fmap tail strat)
                               effects effectedDefender effectedAttacker
                      _ -> 
                        let addedEffects = spell : effects in
                        fightSim newLimit (fmap tail strat)
                                 addedEffects defender chargedAttacker
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
          else fightSim limit strat effects effectedDefender attacker

solna :: String -> IO ()
solna stuff = do
  let boss = parseBoss stuff
  print $ fightSim 10000 Nothing [] pc boss

solnb :: String -> IO ()
solnb stuff = do
  let boss = parseBoss stuff
  print $ fightSim 10000 Nothing [hardMode] pc boss
  where
    hardMode = Spell "HardMode" 0 (-1) effectHardMode id

main :: IO ()
main = makeMain solna solnb
