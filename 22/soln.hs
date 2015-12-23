-- Copyright © 2015 Bart Massey

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

fightSim :: Int -> Maybe [String] -> [Spell] -> Stats -> Stats -> Result
fightSim limit strat effects attacker defender =
    let (newEffects, (effectedAttacker, effectedDefender)) =
            runEffects effects (attacker, defender)
    in
      if statsHP effectedAttacker <= 0 then
          case statsType effectedAttacker of
            PC -> error "PC attacker died of spell effects"
            NPC -> Result PC (statsManaSpent effectedDefender)
      else if statsHP effectedDefender <= 0 then
          case statsType effectedDefender of
            PC -> error "PC defender died of spell effects"
            NPC -> Result PC (statsManaSpent effectedAttacker)
      else
          fightRound limit strat newEffects effectedAttacker effectedDefender
    where
      runEffects oldEffects combatants =
          let newCombatants = foldr runEffect combatants oldEffects in
          foldr expireEffect ([], newCombatants) oldEffects
          where
            runEffect spell curCombatants
                | spellDuration spell <= 0 = error "overqueued spell"
                | otherwise = spellEffect spell curCombatants
            expireEffect spell (newEffects, curCombatants) =
                case decrementDuration spell of
                  s | spellDuration s < 0 ->
                        error "mysteriously overqueued spell"
                  s | spellDuration s == 0 ->
                        (newEffects, spellPostEffect spell curCombatants)
                  s -> (s : newEffects, curCombatants)

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
  let strat = [ "Shield", "Recharge", "Poison",
                "Shield", "Recharge", "Poison", "Shield" ] ++
              replicate 6 "Magic Missile"
  print $ fightSim 10000 Nothing [] pc boss

solnb :: String -> IO ()
solnb stuff = do
  print stuff

main :: IO ()
main = makeMain solna solnb
